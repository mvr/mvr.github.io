{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}

module ProfunctorLearners where

import Control.Arrow (Kleisli(..), second)
import Control.Monad.Fix (MonadFix, mfix)
import Data.Profunctor (Profunctor(..), lmap, rmap)

data Institute b b' a a' =
  forall m. Institute
  { implement :: m -> a -> b,
    update    :: m -> a -> b' -> m,
    request   :: m -> a -> b' -> a' }

data School b b' a a' =
  forall m n. School
  { left :: (m, a) -> (n, b),
    right :: (n, b') -> (m, a') }

instituteToSchool :: Institute b b' a a' -> School b b' a a'
instituteToSchool (Institute i u r) = School left right
  where left (m, a) = ((m, a), i m a)
        right ((m , a), b') = (u m a b', r m a b')

schoolToInstitute :: School b b' a a' -> Institute b b' a a'
schoolToInstitute (School left right) = Institute i u r
  where i m a = snd (left (m, a))
        u m a b' = fst (right (fst (left (m, a)), b'))
        r m a b' = snd (right (fst (left (m, a)), b'))

flipClassroom :: School b b' a a' -> School a' a b' b
flipClassroom (School l r) = School r l

schoolCup :: School () () a a
schoolCup = School (\((), a) -> (a, ())) (\(a, ()) -> ((), a))

schoolCap :: School b b () ()
schoolCap = flipClassroom schoolCup

data Appraiser b b' a a' =
  forall n. Appraiser
  { oleft  :: a -> (n, b),
    oright :: (n, b') -> a' }

instance Profunctor (Appraiser b b') where
  dimap f g (Appraiser l r) = Appraiser (lmap f l) (rmap g r)

opticCup :: Appraiser () () a a
opticCup = Appraiser (\a -> (a, ())) (\(a, ()) -> a)

class Profunctor p => Strong p where
  strength :: p a a' -> p (c, a) (c, a')

instance Strong (Appraiser b b') where
  strength (Appraiser l r) = Appraiser l' r'
    where l' ~(c, a) = let (n, b) = l a in ((c, n), b)
          r' ~(~(c, n), b') = let a' = r (n, b') in (c, a')

type Optic a a' b b' = forall p. Strong p => p b b' -> p a a'

appraise :: Appraiser b b' a a' -> Optic a a' b b'
appraise (Appraiser l r) = dimap l r . strength

unappraise :: Optic a a' b b' -> Appraiser b b' a a'
unappraise o = o (Appraiser (\b -> ((), b)) (\((), b') -> b'))

instance Profunctor (School b b') where
  dimap f g (School l r) = School (lmap (second f) l) (rmap (second g) r)

class Profunctor p => Pedagogical p where
  action :: p a a' -> p (c, a) (c, a')
  reaction :: p (d, a) (d, a') -> p a a'

instance Pedagogical (School b b') where
  action (School l r) = School l' r'
    where l' ~(m, ~(c, a))  = let (n, b)  = l (m, a)  in ((n, c), b)
          r' ~(~(n, c), b') = let (m, a') = r (n, b') in (m , (c, a'))
  reaction (School l r) = School l' r'
    where l' ~(~(m, d), a)  = let (n, b)        = l (m, (d, a)) in ((n, d), b)
          r' ~(~(n, d), b') = let (m, ~(d, a')) = r (n, b')     in ((m, d), a')

type Learner a a' b b' = forall p. Pedagogical p => p b b' -> p a a'

inculcate :: School b b' a a' -> Learner a a' b b'
inculcate (School l r) = reaction . dimap l r . action

unculcate :: Learner a a' b b' -> School b b' a a'
unculcate o = o (School id id)

cup :: Learner a a () ()
cup p = dimap (\a -> (a, ())) (\(a, ()) -> a) (action p)

cap :: Learner () () b b
cap p = reaction (dimap (\(b, ()) -> b) (\b -> (b, ())) p)

newtype Flipped b b' a a' = Flipped
  { unFlipped :: Learner b' b a' a }

instance Profunctor (Flipped b b') where
  dimap l r (Flipped o) = Flipped (o . dimap r l)

instance Pedagogical (Flipped b b') where
  action (Flipped o) = Flipped (o . reaction)
  reaction (Flipped o) = Flipped (o . action)

invol :: Learner a a' b b' -> Learner b' b a' a
invol o = unFlipped (o (Flipped id))

instance Pedagogical (->) where
  action f ~(c, a) = (c , f a)
  reaction g a = let (d, a') = g (d, a) in a'

instance MonadFix m => Pedagogical (Kleisli m) where
  action (Kleisli f) = Kleisli $ \ ~(c, a) ->
    do a' <- f a
       return (c, a')
  reaction (Kleisli g) = Kleisli $ \a -> do
    let g' p = g (fst p, a)
    ~(d, a') <- mfix g'
    return a'

judge :: Integer -> Learner [Integer] [Ordering] () ()
judge secret = inculcate (School l r)
  where
    l ~(_, guesses) = (map (\guess -> compare guess secret) guesses, ())
    r ~(orders, _) = ((), orders)

guesser :: Learner Integer [Integer] [Integer] [Ordering]
guesser = inculcate (School l r)
  where
    l ~(orders, initial) = (guesses, guesses)
      where
        guesses = initial : zipWith adjust guesses orders
    r ~(guesses, orders) = (orders, guesses)

    adjust guess LT = guess + 1
    adjust guess EQ = guess
    adjust guess GT = guess - 1

trial :: Learner Integer [Integer] () ()
trial = guesser . judge 10

-- >>> take 12 (trial id 3)
-- [3,4,5,6,7,8,9,10,10,10,10,10]
-- >>> take 8 (trial id 14)
-- [14,13,12,11,10,10,10,10]

-- guesses = initial : zipWith adjust guesses orders
-- orders  = map (\guess -> compare guess 10) guesses

data ISchool b b' m a a' =
  forall n. ISchool
  { ileft  :: (m, a)  -> (n, b),
    iright :: (n, b') -> (m, a') }

class IProfunctor p where
  idimap :: (a -> b) -> (b' -> a') -> p m b b' -> p m a a'

instance IProfunctor (ISchool b b') where
  idimap f g (ISchool l r) = ISchool (lmap (second f) l) (rmap (second g) r)

newtype Para m a a' = Para (m -> a -> a')

instance IProfunctor Para where
  idimap f g (Para h) = Para $ \m -> dimap f g (h m)

class IProfunctor p => IPedagogical p where
  iaction :: p m a a' -> p m (c, a) (c, a')
  ireaction :: p m (d, a) (d, a') -> p (m, d) a a'

instance IPedagogical Para where
  iaction (Para f) = Para $ \m ~(c, a) -> (c , f m a)
  ireaction (Para g) = Para $ \ ~(m, d) a -> snd (g m (d, a))

instance IPedagogical (ISchool b b') where
  iaction (ISchool l r) = ISchool l' r'
    where l' ~(m, ~(c, a))  = let (n, b) = l (m, a) in ((n, c), b)
          r' ~(~(n, c), b') = let (m, a') = r (n, b')  in (m , (c, a'))
  ireaction (ISchool l r) = ISchool l' r'
    where l' ~(~(m, d), a)  = let (n, b)       = l (m, (d, a)) in ((n, d), b)
          r' ~(~(n, d), b') = let (m, ~(d, a')) = r (n, b') in ((m, d), a')

type ILearner a a' b b' m n = forall p. IPedagogical p => p m b b' -> p n a a'

iinculcate :: ISchool b b' n a a' -> ILearner a a' b b' m (m, n)
iinculcate (ISchool l r) = ireaction . idimap l r . iaction

iunculcate :: ILearner a a' b b' n m -> ISchool b b' m a a'
iunculcate o = o (ISchool id id)

newtype Streamed p a a' = Streamed { unStreamed :: p [a] [a'] }

instance Profunctor p => Profunctor (Streamed p) where
  dimap f g (Streamed p) = Streamed (dimap (fmap f) (fmap g) p)

instance Pedagogical p => Pedagogical (Streamed p) where
  action (Streamed p) = Streamed $
    dimap unzip (\ ~(cs, as') -> zip cs as') (action p)
  reaction (Streamed p) = Streamed $
    reaction (dimap (\ ~(ds, as) -> zip ds as) unzip p)

batch :: Learner a a' b b' -> Learner [a] [a'] [b] [b']
batch o p = unStreamed (o (Streamed p))

newtype Captured p m a a' = Captured
  { unCaptured :: p (m, a) (m, a') }

instance Profunctor p => IProfunctor (Captured p) where
  idimap f g (Captured p) = Captured $
    dimap (\ ~(m, a) -> (m, f a))
          (\ ~(m, a') -> (m, g a')) p

instance Pedagogical p => IPedagogical (Captured p) where
  iaction (Captured p) = Captured $
    dimap (\ ~(m, ~(c, a)) -> (c, (m, a)))
          (\ ~(c, ~(m, a')) -> (m, (c, a')))
          (action p)
  ireaction (Captured p) = Captured $
    dimap (\ ~(~(m, d), a) -> (m, (d, a)))
          (\ ~(m, ~(d, a')) -> ((m, d), a')) p

trivialParams :: Profunctor p => p a a' -> Captured p () a a'
trivialParams p = Captured (dimap snd (\a' -> ((), a')) p)

capturedSteps :: ILearner a a' b b' () m
  -> Learner [(m, a)] [(m, a')] [b] [b']
capturedSteps o p = unStreamed (unCaptured (o (trivialParams (Streamed p))))

iterateLearner :: m -> ILearner a a' b b' () m
  -> Learner [a] [a'] [b] [b']
iterateLearner m o p = reaction (dimap seed unzip (capturedSteps o p))
  where seed ~(ms, as) = zip (m:ms) as

layer :: Double -> ILearner Double Double Double Double m (m, Double)
layer rate = iinculcate (ISchool l r)
  where
    l (w, a) = ((w, a), w * a)
    r ((w, a), b') = (w - rate * b' * a, w * b')

inetwork :: ILearner Double Double Double Double () (((), Double), Double)
inetwork = layer 0.05 . layer 0.05

network :: Learner [Double] [Double] [Double] [Double]
network = iterateLearner (((), 2), 1) inetwork

trainingRun :: [Double] -> [Double] -> [Double]
trainingRun ins targets = network judge ins
  where judge results = zipWith (-) results targets

-- >>> take 5 (trainingRun (repeat 1) (repeat 6))
-- [-8.0,-8.9936,-7.703924129257238,-5.047284566497232,-2.6240549262696793]

trainingTrace :: [Double] -> [Double] -> ([Double], [Double])
trainingTrace ins targets = runKleisli (network (Kleisli judge)) ins
  where
    judge results = (results, zipWith (-) results targets)

-- >>> let (outs, grads) = trainingTrace (repeat 1) (repeat 6)
-- >>> take 5 outs
-- [2.0,3.08,4.138453280000001,4.988143715505187,5.525063380141243]
-- >>> take 5 grads
-- [-8.0,-8.9936,-7.703924129257238,-5.047284566497232,-2.6240549262696793]
