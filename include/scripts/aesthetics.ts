const CDN_PATH = "https://cdn.braindump.ing/aesthetics";

type GridItem = HTMLImageElement | HTMLElement;

const loadImage = (url: string): Promise<HTMLImageElement> =>
  new Promise((resolve, reject) => {
    const img = new Image();
    img.onload = () => resolve(img);
    img.onerror = () => reject(new Error(`Failed to load image: ${url}`));
    img.src = url;
  });

// Fisher-Yates shuffle, in place.
const shuffle = <T>(items: T[]): T[] => {
  for (let i = items.length - 1; i > 0; i--) {
    const j = Math.floor(Math.random() * (i + 1));
    const tmp = items[i]!;
    items[i] = items[j]!;
    items[j] = tmp;
  }
  return items;
};

const loadHtml = async (url: string): Promise<HTMLElement> => {
  const res = await fetch(url);
  if (!res.ok) throw new Error(`${res.status} for ${url}`);
  const wrapper = document.createElement("div");
  wrapper.innerHTML = await res.text();
  return wrapper;
};

// Resolve a resource subpath to a ready-to-place element, or null if it can't load.
// Classify by extension only — no HEAD round-trip — so images and HTML start loading
// at the same instant and race fairly (an extra round-trip would bias HTML to land last).
const loadResource = async (subpath: string): Promise<GridItem | null> => {
  try {
    const url = new URL(`${CDN_PATH}/${subpath}`).href;
    return /\.(png|jpe?g|gif|webp|svg|avif)$/i.test(subpath) ? await loadImage(url) : await loadHtml(url);
  } catch (err) {
    // A missing or broken resource shouldn't take down the rest of the grid.
    console.warn(`Skipping resource: ${subpath}`, err);
    return null;
  }
};

window.onload = async () => {
  const resources: string[] = await (await fetch(CDN_PATH)).json();
  shuffle(resources);

  const container = document.querySelector<HTMLElement>("#container")!;
  const stage = document.createElement("div");
  stage.style.cssText = "position:relative;";
  container.appendChild(stage);

  // --- Masonry: fixed-width columns, each item dropped into the shortest one. ---
  const GUTTER = 8;
  const TARGET_COL = 280; // ideal column width; the count falls out of the viewport size
  const MAX_COLS = 3; // cap so laptops top out at 3 rather than tiling indefinitely

  // Column count derived purely from the available width: ~1 on phones, 2 on tablets,
  // 3 on laptops. Recomputed on every (re)layout so rotation/resize adapts.
  const columnCount = (width: number): number =>
    Math.min(MAX_COLS, Math.max(1, Math.floor((width + GUTTER) / (TARGET_COL + GUTTER))));

  let cols = 1;
  let colW = 0;
  let colHeights: number[] = [];

  // Drop a single element into the shortest column. Runs to completion synchronously,
  // so concurrent loads can call it without racing on colHeights.
  const place = (el: GridItem): void => {
    el.classList.add("grid-item");
    el.style.position = "absolute";
    // Fill the column edge-to-edge (images upscale too) so there's no dead margin.
    const itemW = colW;
    el.style.width = `${itemW}px`;

    let c = 0;
    for (let i = 1; i < cols; i++) if (colHeights[i]! < colHeights[c]!) c = i;

    el.style.left = `${c * (colW + GUTTER)}px`;
    el.style.top = `${colHeights[c]!}px`;
    stage.appendChild(el);

    // Images: derive height from the intrinsic ratio (no reflow). HTML: measure it.
    const h = el instanceof HTMLImageElement ? itemW * (el.naturalHeight / el.naturalWidth) : el.offsetHeight;
    colHeights[c] = colHeights[c]! + h + GUTTER;
    stage.style.height = `${Math.max(...colHeights)}px`;
  };

  // (Re)compute geometry for the current width and re-place every settled item.
  const placed: GridItem[] = [];
  const layout = (): void => {
    const width = container.clientWidth || window.innerWidth;
    cols = columnCount(width);
    colW = (width - GUTTER * (cols - 1)) / cols;
    colHeights = new Array<number>(cols).fill(0);
    for (const el of placed) place(el);
  };

  // Load everything in parallel, but place in shuffle order so the on-screen order is
  // the shuffle — not whatever finished downloading first (which clustered text last).
  // We commit a contiguous prefix: an item is placed only once all earlier ones settled,
  // so the layout still streams from the front as fast as the slowest leading item allows.
  const items = new Array<GridItem | null>(resources.length);
  const settled = new Array<boolean>(resources.length).fill(false);
  let next = 0;
  const flushReady = () => {
    while (next < resources.length && settled[next]!) {
      const el = items[next]!;
      if (el) {
        placed.push(el);
        place(el);
      }
      next++;
    }
  };

  layout();

  // Re-flow when the viewport changes (window resize, phone rotation). Debounced so a
  // drag-resize doesn't thrash; recomputes column count and repositions everything.
  let resizeTimer = 0;
  window.addEventListener("resize", () => {
    clearTimeout(resizeTimer);
    resizeTimer = window.setTimeout(layout, 150);
  });

  await Promise.all(
    resources.map(async (subpath, i) => {
      items[i] = await loadResource(subpath);
      settled[i] = true;
      flushReady();
    }),
  );
};
