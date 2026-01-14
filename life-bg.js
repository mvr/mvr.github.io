(() => {
  const canvas = document.getElementById("life-bg-canvas");
  if (!canvas) return;

  const prefersReducedMotion = window.matchMedia("(prefers-reduced-motion: reduce)");
  if (prefersReducedMotion.matches) return;

  const config = {
    minScreenWidth: 1100,
    cols: 30,
    extraCols: 10, // Off screen
    rows: 30,
    cellSize: 16,
    cellGap: 2,
    seedCols: 6,
    seedRows: 16,
    seedDensity: 0.4,
    seedPadding: 1,
    seedRefreshInterval: 16,
    maxTries: 20,
    testSteps: 40,
    frameMs: 300
  };

  const totalCols = config.cols + config.extraCols;
  let grid = new Uint8Array(totalCols * config.rows);
  let nextGrid = new Uint8Array(totalCols * config.rows);
  let seedMask = new Uint8Array(totalCols * config.rows);
  let ctx = canvas.getContext("2d");
  let running = false;
  let rafId = null;
  let lastFrame = 0;
  let generation = 0;

  const rootStyles = getComputedStyle(document.documentElement);
  const color = rootStyles.getPropertyValue("--life-bg-color").trim() || "#d8e5f1";

  const index = (x, y) => y * totalCols + x;

  const shouldRun = () => window.innerWidth >= config.minScreenWidth;

  const clearGrid = () => {
    grid.fill(0);
    nextGrid.fill(0);
    seedMask.fill(0);
  };

  const seedRandom = () => {
    clearGrid();
    const startCol = Math.min(totalCols - 1, config.cols + config.seedPadding);
    for (let y = 0; y < config.seedRows; y += 1) {
      for (let x = startCol; x < startCol + config.seedCols; x += 1) {
        if (Math.random() < config.seedDensity) {
          const idx = index(x, y);
          grid[idx] = 1;
          seedMask[idx] = 1;
        }
      }
    }
  };

  const step = () => {
    for (let y = 0; y < config.rows; y += 1) {
      for (let x = 0; x < totalCols; x += 1) {
        let count = 0;
        for (let dy = -1; dy <= 1; dy += 1) {
          const ny = y + dy;
          if (ny < 0 || ny >= config.rows) continue;
          for (let dx = -1; dx <= 1; dx += 1) {
            const nx = x + dx;
            if (dx === 0 && dy === 0) continue;
            if (nx < 0 || nx >= totalCols) continue;
            count += grid[index(nx, ny)];
          }
        }
        const alive = grid[index(x, y)] === 1;
        nextGrid[index(x, y)] = count === 3 || (alive && count === 2) ? 1 : 0;
      }
    }
    generation += 1;
    if (generation % config.seedRefreshInterval === 0) {
      for (let i = 0; i < seedMask.length; i += 1) {
        if (seedMask[i] === 1) {
          nextGrid[i] = 1;
        }
      }
    }
    const temp = grid;
    grid = nextGrid;
    nextGrid = temp;
  };

  const findGoodSeed = () => {
    let bestPopulation = -1;
    let bestGrid = null;
    let bestSeedMask = null;
    for (let attempt = 0; attempt < config.maxTries; attempt += 1) {
      seedRandom();
      const initialGrid = grid.slice();
      const initialSeedMask = seedMask.slice();
      for (let t = 0; t < config.testSteps; t += 1) {
        step();
      }
      const visiblePopulation = countVisibleCells();
      if (visiblePopulation > bestPopulation) {
        bestPopulation = visiblePopulation;
        bestGrid = initialGrid;
        bestSeedMask = initialSeedMask;
      }
    }
    if (bestGrid) {
      grid.set(bestGrid);
      nextGrid.fill(0);
      seedMask.set(bestSeedMask);
      generation = 0;
      return true;
    }
    return false;
  };

  const hasVisibleCells = () => {
    for (let y = 0; y < config.rows; y += 1) {
      for (let x = 0; x < config.cols; x += 1) {
        if (grid[index(x, y)] === 1) {
          return true;
        }
      }
    }
    return false;
  };

  const countVisibleCells = () => {
    let count = 0;
    for (let y = 0; y < config.rows; y += 1) {
      for (let x = 0; x < config.cols; x += 1) {
        count += grid[index(x, y)];
      }
    }
    return count;
  };


  const render = () => {
    ctx.clearRect(0, 0, canvas.width, canvas.height);
    ctx.fillStyle = color;
    const cellDraw = Math.max(1, config.cellSize - config.cellGap);
    for (let y = 0; y < config.rows; y += 1) {
      for (let x = 0; x < config.cols; x += 1) {
        if (grid[index(x, y)] === 1) {
          ctx.fillRect(
            x * config.cellSize,
            y * config.cellSize,
            cellDraw,
            cellDraw
          );
        }
      }
    }
  };

  const applySizing = () => {
    canvas.width = config.cols * config.cellSize;
    canvas.height = config.rows * config.cellSize;
    canvas.style.width = `${config.cols * config.cellSize}px`;
    canvas.style.height = `${config.rows * config.cellSize}px`;
  };

  const init = () => {
    applySizing();
    generation = 0;
    findGoodSeed();
    render();
  };

  const loop = (timestamp) => {
    if (!running) return;
    if (timestamp - lastFrame >= config.frameMs) {
      step();
      render();
      lastFrame = timestamp;
    }
    rafId = window.requestAnimationFrame(loop);
  };

  const start = () => {
    if (running) return;
    running = true;
    lastFrame = 0;
    init();
    rafId = window.requestAnimationFrame(loop);
  };

  const stop = () => {
    if (!running) return;
    running = false;
    if (rafId) {
      window.cancelAnimationFrame(rafId);
      rafId = null;
    }
  };

  const handleResize = () => {
    if (!shouldRun()) {
      canvas.style.display = "none";
      stop();
      return;
    }
    canvas.style.display = "block";
    stop();
    start();
  };

  prefersReducedMotion.addEventListener("change", handleResize);
  window.addEventListener("resize", handleResize);

  handleResize();
})();
