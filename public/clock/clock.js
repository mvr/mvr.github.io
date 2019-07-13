// Constants

const clockWidth = 500;
const clockHeight = 300;
const scale = 0.45;

// const verticalSegments = [false, false, true, false, false, true, true]; // ish
// const horizontalSegments = [false, false, false, true, false, true, true, true];
const verticalSegments   = [false, false, true,  false, false, true, true, false, false, false,  false, false, true,  true];
const horizontalSegments = [false, false, false, true,  false, true, true, true,  false, false, false, true,  false, true];

const startRadius = 100;
const verticalWidth = 10;
const horizontalWidth = 50;
const horizontalHeight = 0.05; // In radians
const digitGap = 10;
const ringGap = 30;
const outsidePad = 4;

const spinSpeed = 0.001;

const colonSeparation = 30;
const colonSize = 5;

function totalRadius() {
  return startRadius + 4 * verticalWidth + 2 * horizontalWidth + digitGap;
}

function leftRingCenter() {
  const r = totalRadius() + outsidePad;
  return { x : r, y : r };
}

function rightRingCenter() {
  const y = totalRadius() + outsidePad;
  const x = y + 2 * totalRadius() + ringGap;
  return { x : x, y : y };
}

function middlePoint() {
  const y = totalRadius() + outsidePad;
  const x = y + totalRadius() + ringGap / 2;
  return { x : x, y : y };
}

// function digitToSegments(d) {
//   switch(d) {
//   case 0: return [[true, true], [true, false, true], [true, true]];
//   case 1: return [[false, false], [false, false, false], [true, true]];
//   case 2: return [[false, true], [true, true, true], [true, false]];
//   case 3: return [[false, false], [true, true, true], [true, true]];
//   case 4: return [[true, false], [false, true, false], [true, true]];
//   case 5: return [[true, false], [true, true, true], [false, true]];
//   case 6: return [[true, true], [true, true, true], [false, true]];
//   case 7: return [[false, false], [true, false, false], [true, true]];
//   case 8: return [[true, true], [true, true, true], [true, true]];
//   case 9: return [[true, false], [true, true, true], [true, true]];
//   default: return digitToSegments(8);
//   }
// }

function digitToSegments(d) {
  switch(d) {
  case 0: return [[false, true, true, false], [true, false, true], [false, true, true, false]];
  case 1: return [[false, false, false, false], [false, false, false], [false, true, true, false]];
  case 2: return [[false, false, true, false], [true, true, true], [false, true, false, false]];
  case 3: return [[false, false, false, false], [true, true, true], [false, true, true, false]];
  case 4: return [[false, true, false, false], [false, true, false], [false, true, true, false]];
  case 5: return [[false, true, false, false], [true, true, true], [false, false, true, false]];
  case 6: return [[false, true, true, false], [true, true, true], [false, false, true, false]];
  case 7: return [[false, false, false, false], [true, false, false], [false, true, true, false]];
  case 8: return [[false, true, true, false], [true, true, true], [false, true, true, false]];
  case 9: return [[false, true, false, false], [true, true, true], [false, true, true, false]];
  default: return digitToSegments(8);
  }
}

function drawFatArc(ctx, origin, startAngle, endAngle, innerRadius, outerRadius) {
  ctx.beginPath();
  ctx.arc(scale*origin.x, scale*origin.y, scale*outerRadius, startAngle, endAngle, false);
  ctx.arc(scale*origin.x, scale*origin.y, scale*innerRadius, endAngle, startAngle, true);
  ctx.fillStyle = "black";
  ctx.fill();
}

function drawFilledCircle(ctx, origin, radius) {
  ctx.beginPath();
  ctx.arc(scale*origin.x, scale*origin.y, scale*radius, 0, 2 * Math.PI, false);
  ctx.fillStyle = "black";
  ctx.fill();
}

function drawHorizontalSegments(ctx, segments, origin, angle, innerRadius, outerRadius) {
  const segmentCount = segments.length;
  const segmentSize = 2 * Math.PI / segmentCount;

  for(var i = 0; i < segmentCount; i++) {
    if(segments[i]) {
      const baseAngle  = -angle + segmentSize * (i - 1);
      const startAngle = baseAngle - (horizontalHeight / 2);
      const endAngle   = baseAngle + (horizontalHeight / 2);
      drawFatArc(ctx, origin, startAngle, endAngle, innerRadius, outerRadius);
    }
  }
}

function drawVerticalSegments(ctx, segments, origin, angle, innerRadius, outerRadius) {
  const segmentCount = segments.length;
  const segmentSize = 2 * Math.PI / segmentCount;

  for(var i = 0; i < segmentCount; i++) {
    if(segments[i]) {
      const baseAngle  = -angle + segmentSize * (i - 2); // or 1
      const startAngle = baseAngle - (horizontalHeight / 2);
      const endAngle   = baseAngle + (horizontalHeight / 2) + segmentSize;
      drawFatArc(ctx, origin, startAngle, endAngle, innerRadius, outerRadius);
    }
  }
}

function drawColon(ctx) {
  var middle = middlePoint();
  var topOrigin = {x : middle.x, y : middle.y - colonSeparation / 2};
  drawFilledCircle(ctx, topOrigin, colonSize);
  var botOrigin = {x : middle.x, y : middle.y + colonSeparation / 2};
  drawFilledCircle(ctx, botOrigin, colonSize);
}

const secondStartRadius = startRadius + 2 * verticalWidth + horizontalWidth + digitGap;
var ringProps = [
  {
    innerRadius: startRadius,
    outerRadius: startRadius + verticalWidth,
    orientation: 'vertical'
  },
  {
    innerRadius: startRadius,
    outerRadius: startRadius + 2 * verticalWidth + horizontalWidth,
    orientation: 'horizontal'
  },
  {
    innerRadius: startRadius + verticalWidth + horizontalWidth,
    outerRadius: startRadius + 2 * verticalWidth + horizontalWidth,
    orientation: 'vertical'
  },
  {
    innerRadius: secondStartRadius,
    outerRadius: secondStartRadius + verticalWidth,
    orientation: 'vertical'
  },
  {
    innerRadius: secondStartRadius,
    outerRadius: secondStartRadius + 2 * verticalWidth + horizontalWidth,
    orientation: 'horizontal'
  },
  {
    innerRadius: secondStartRadius + verticalWidth + horizontalWidth,
    outerRadius: secondStartRadius + 2 * verticalWidth + horizontalWidth,
    orientation: 'vertical'
  },
];

// Fuck
var deepCopy = function( o ) {
  return JSON.parse(JSON.stringify( o ));
};

function drawLeftRing(ctx, leftRing) {
  const origin = leftRingCenter();
  for(let i in leftRing) {
    let r = leftRing[i];
    if(r.orientation === 'vertical') {
      drawVerticalSegments(ctx, verticalSegments, origin, r.angle, r.innerRadius, r.outerRadius);
    } else {
      drawHorizontalSegments(ctx, horizontalSegments, origin, r.angle, r.innerRadius, r.outerRadius);
    }
  }
}

function drawRightRing(ctx, rightRing) {
  const origin = rightRingCenter();
  for(let i in rightRing) {
    let r = rightRing[i];
    if(r.orientation === 'vertical') {
      drawVerticalSegments(ctx, verticalSegments, origin, r.angle, r.innerRadius, r.outerRadius);
    } else {
      drawHorizontalSegments(ctx, horizontalSegments, origin, r.angle, r.innerRadius, r.outerRadius);
    }
  }
}

function goalDigits() {
  var currentDate = new Date();
  var hours = currentDate.getHours();
  var minutes = currentDate.getMinutes();
  return [Math.floor(hours / 10), hours % 10, Math.floor(minutes / 10), minutes % 10];
}

function goalSegments() {
  var t = goalDigits().map(digitToSegments);
  return [].concat.apply([], t);
}

function findIndex(arr, target, current) {
  let i = current;
  while(true) {
    let match = true;
    for(let j = 0; j < target.length; j++) {
      if(arr[i + j] != target[j])
        match = false;
    }
    if(match)
      return i;

    i += 1;
    if(i >= arr.length) {
      i = 0;
    }
    // if(i == current) {
    //   console.log("Couldn't find", target, "in", arr, "starting at", current);
    // }
  }
}

function setLeftGoalIndices(leftRing) {
  var goals = goalSegments();
  for(let i in leftRing) {
    let r = leftRing[i];
    if(r.orientation === 'vertical') {
      r.goalIndex = findIndex(verticalSegments, goals[i], r.previousIndex);
      var segmentCount = verticalSegments.length;
    } else {
      r.goalIndex = findIndex(horizontalSegments, goals[i], r.previousIndex);
      var segmentCount = horizontalSegments.length;
    }
    r.previousIndex = r.goalIndex;

    const segmentSize = 2 * Math.PI / segmentCount;
    r.goalAngle = segmentSize * r.goalIndex;
  }
}

function setRightGoalIndices(rightRing) {
  var goals = goalSegments().slice(6).map(function(a) {return a.reverse();}).reverse();
  for(let i in rightRing) {
    let r = rightRing[i];
    if(r.orientation === 'vertical') {
      r.goalIndex = findIndex(verticalSegments, goals[i], r.previousIndex);
      var segmentCount = verticalSegments.length;
    } else {
      r.goalIndex = findIndex(horizontalSegments, goals[i], r.previousIndex);
      var segmentCount = horizontalSegments.length;
    }
    r.previousIndex = r.goalIndex;

    const segmentSize = 2 * Math.PI / segmentCount;
    r.goalAngle = segmentSize * r.goalIndex + Math.PI;
  }
}



function run() {
  var canvas = document.getElementById('canvas');
  var ctx = canvas.getContext('2d');

  canvas.width = clockWidth;
  canvas.height = clockHeight;
  // canvas.style.width  = clockWidth;
  // canvas.style.height = clockHeight;

  var leftRing = deepCopy(ringProps);
  for(let i in leftRing) {
    leftRing[i].angle = 0;
    leftRing[i].previousIndex = 0;
  }

  var rightRing = deepCopy(ringProps);
  for(let i in rightRing) {
    rightRing[i].angle = 0;
    rightRing[i].previousIndex = 0;
  }

  var lastTime = [];
  function checkTime() {
    if(goalDigits().toString() != lastTime.toString()) { // FUCK
      setLeftGoalIndices(leftRing);
      setRightGoalIndices(rightRing);
    }
    lastTime = goalDigits();
  }
  checkTime();
  window.setInterval(checkTime, 1000);


  var lastTimestamp = undefined;
  function draw(timestamp) {
    if(lastTimestamp === undefined) {
      lastTimestamp = timestamp;
      window.requestAnimationFrame(draw);
      return;
    }

    var delta = timestamp - lastTimestamp;
    lastTimestamp = timestamp;

    for(let i in leftRing) {
      leftRing[i].angle += (leftRing[i].goalAngle - leftRing[i].angle) * delta * spinSpeed;
    }
    for(let i in rightRing) {
      rightRing[i].angle += (rightRing[i].goalAngle - rightRing[i].angle) * delta * spinSpeed;
    }

    ctx.clearRect(0, 0, canvas.width, canvas.height);
    drawLeftRing(ctx, leftRing);
    drawRightRing(ctx, rightRing);
    if(Math.floor(timestamp / 1000) % 2 == 0)
      drawColon(ctx);

    window.requestAnimationFrame(draw);
  }

  window.requestAnimationFrame(draw);
}

run();
