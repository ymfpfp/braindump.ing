import prism from "prismjs";
import mjAPI from "mathjax-node";
import * as fontkit from "fontkit";
import { Resvg } from "@resvg/resvg-js";
import { readFileSync, writeFileSync, readdirSync, existsSync } from "fs";
import { createHash, createHmac } from "crypto";
import { join, relative } from "path";
import { fileURLToPath } from "url";

const loadLanguages = require("prismjs/components/");

// Stand up the MathJax worker once. We render math as CommonHTML (real text
// spans) rather than SVG so the contents of `\text{}` stay as actual glyphs we
// can restyle with a page font. CommonHTML needs a one-time stylesheet pulling
// its glyph fonts from the MathJax CDN; that lives statically in
// `include/styles/mjx.css` (linked from `wrapper.html`) rather than being
// injected here.
mjAPI.config({ MathJax: {} });
mjAPI.start();

// The code inside `<pre><code>` arrives raw (the Haskell side does not escape
// it), so when we can't highlight we still have to escape it ourselves to keep
// the output valid HTML.
function escapeHtml(s) {
  return s.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;");
}

// Lazily register a grammar, remembering failures so we don't retry an unknown
// language on every block.
const attempted = new Set();
function grammarFor(language) {
  if (!language) return undefined;
  if (prism.languages[language]) return prism.languages[language];
  if (!attempted.has(language)) {
    attempted.add(language);
    try {
      loadLanguages([language]);
    } catch {
      // Unknown language — leave it unregistered and fall back to escaping.
    }
  }
  return prism.languages[language];
}

// Match the exact shape emitted by `Markdown.blockToHtml` for a `CodeBlock`:
// `<pre><code class="language-LANG">RAW_CODE</code></pre>`.
const CODE_BLOCK =
  /<pre><code class="language-([^"]*)">([\s\S]*?)<\/code><\/pre>/g;

function highlightCode(html) {
  return html.replace(CODE_BLOCK, (_match, language, code) => {
    const grammar = grammarFor(language);
    const highlighted = grammar
      ? prism.highlight(code, grammar, language)
      : escapeHtml(code);
    // Mirror the language class onto `<pre>` so Prism themes style it.
    return `<pre class="language-${language}"><code class="language-${language}">${highlighted}</code></pre>`;
  });
}

// Inline math delimited by single dollar signs, e.g. `$x$`. We stay on a single
// line and reject empty/escaped (`\$`) delimiters to avoid swallowing prose.
const INLINE_MATH = /(^|[^\\])\$([^$\n]+?)\$/g;

// Render one TeX snippet to a CommonHTML fragment. A malformed snippet makes
// `typeset` reject, so on any failure we fall back to the original `$...$` —
// a typo degrades to plain text rather than crashing the build.
async function renderInline(tex) {
  try {
    const data = await mjAPI.typeset({
      math: tex,
      format: "inline-TeX",
      html: true,
    });
    return data.html ?? `$${tex}$`;
  } catch {
    return `$${tex}$`;
  }
}

// Math may not occur inside code, so highlight first and only scan the segments
// that fall outside any block (`<pre>...</pre>`) or inline (`<code>...</code>`)
// code. A `$` inside inline code (e.g. `trim $ words`) is literal, not math.
async function transform(html) {
  const highlighted = highlightCode(html);

  // Collect every distinct TeX snippet outside the code blocks, render them
  // concurrently, then substitute. Caching by snippet avoids re-typesetting
  // repeats like `$n$`.
  const CODE = /<pre[\s\S]*?<\/pre>|<code[\s\S]*?<\/code>/g;
  const rendered = new Map();
  const collect = (segment) => {
    let m;
    INLINE_MATH.lastIndex = 0;
    while ((m = INLINE_MATH.exec(segment))) rendered.set(m[2], null);
  };
  splitOutsidePre(highlighted, CODE).forEach(({ inside, text }) => {
    if (!inside) collect(text);
  });
  await Promise.all(
    [...rendered.keys()].map(async (tex) =>
      rendered.set(tex, await renderInline(tex)),
    ),
  );

  return splitOutsidePre(highlighted, CODE)
    .map(({ inside, text }) =>
      inside
        ? text
        : text.replace(
            INLINE_MATH,
            (_match, lead, tex) => `${lead}${rendered.get(tex)}`,
          ),
    )
    .join("");
}

// Break the html into ordered segments, flagging which ones are `<pre>` blocks
// (where math must be left untouched).
function splitOutsidePre(html, preRegex) {
  const segments = [];
  let last = 0;
  let m;
  preRegex.lastIndex = 0;
  while ((m = preRegex.exec(html))) {
    if (m.index > last)
      segments.push({ inside: false, text: html.slice(last, m.index) });
    segments.push({ inside: true, text: m[0] });
    last = m.index + m[0].length;
  }
  if (last < html.length)
    segments.push({ inside: false, text: html.slice(last) });
  return segments;
}

// Recursively collect every `.html` file under a directory.
function htmlFiles(dir) {
  const files = [];
  for (const entry of readdirSync(dir, { withFileTypes: true })) {
    const full = join(dir, entry.name);
    if (entry.isDirectory()) files.push(...htmlFiles(full));
    else if (entry.isFile() && entry.name.endsWith(".html")) files.push(full);
  }
  return files;
}

// ---------------------------------------------------------------------------
// OpenGraph images
//
// Every page gets a 1200x630 card mirroring the site chrome: the green field
// with the rotated `braindump.ing` wordmark over a white article slab holding
// the (optional) date and the title. Cards are rendered here, pushed to the R2
// bucket behind cdn.braindump.ing, and linked from the page's own meta tags.
//
// The whole stage is opt-in: without R2 credentials the build is exactly the
// syntax-highlighting/math pass it was before, so a plain `bun run` still works
// with no secrets around.
// ---------------------------------------------------------------------------

const srcDir = fileURLToPath(new URL(".", import.meta.url));
const repoRoot = fileURLToPath(new URL("../", import.meta.url));

// Minimal `.env` reader — enough for `KEY=value`, `export KEY=value`, quoted
// values and `#` comments. Looks beside this script first, then at the repo
// root. Never clobbers a variable already in the real environment, which is
// what CI hands us.
function loadEnv() {
  const path = [srcDir, repoRoot]
    .map((dir) => join(dir, ".env"))
    .find(existsSync);
  if (!path) return;
  for (const line of readFileSync(path, "utf8").split("\n")) {
    const m = /^\s*(?:export\s+)?([A-Za-z_][A-Za-z0-9_]*)\s*=\s*(.*)$/.exec(
      line,
    );
    if (!m) continue;
    let value = m[2].trim();
    const quote = value[0];
    if (quote === '"' || quote === "'") {
      const end = value.lastIndexOf(quote);
      value = end > 0 ? value.slice(1, end) : value.slice(1);
    } else {
      value = value.split(" #")[0].trim();
    }
    if (process.env[m[1]] === undefined) process.env[m[1]] = value;
  }
}

const CDN = "https://cdn.braindump.ing";
const OG_PREFIX = "writing/og";
const OG_WIDTH = 1200;
const OG_HEIGHT = 630;

// ---- fonts -----------------------------------------------------------------

// Both Source Sans faces ship as variable fonts whose *default* instance is
// ExtraLight, so we pin the weights the stylesheet asks for (300 for headings,
// 400 for the italic date) via `getVariation`. Text is then converted to
// outlines below, which sidesteps resvg's font matching entirely.
const fontDir = join(repoRoot, "out", "fonts");
const loadFont = (file, variation) => {
  const font = fontkit.create(readFileSync(join(fontDir, file)));
  return variation ? font.getVariation(variation) : font;
};

let fonts;
function ogFonts() {
  if (!fonts)
    fonts = {
      heading: loadFont("Roberte-Regular.ttf"),
      sans: loadFont("SourceSans3-Variable.ttf", { wght: 300 }),
      italic: loadFont("SourceSans3-ItalicVariable.ttf", { wght: 400 }),
    };
  return fonts;
}

// Shape `text` and return its glyph outlines as one SVG path, in SVG
// coordinates (y grows downward) with the baseline at y=0 and the pen starting
// at x=0. `tracking` is extra letter-spacing in px, matching the CSS.
function textPath(font, text, size, tracking = 0) {
  const scale = size / font.unitsPerEm;
  const run = font.layout(text);
  const parts = [];
  let x = 0;
  run.glyphs.forEach((glyph, i) => {
    const pos = run.positions[i];
    const d = glyph.path
      .translate(pos.xOffset ?? 0, pos.yOffset ?? 0)
      .scale(scale, -scale)
      .translate(x, 0)
      .toSVG();
    if (d) parts.push(d);
    x += pos.xAdvance * scale + tracking;
  });
  return { d: parts.join(" "), width: x };
}

function textWidth(font, text, size) {
  return (font.layout(text).advanceWidth * size) / font.unitsPerEm;
}

// Greedy word wrap. A single word longer than the line is left to overflow;
// shrinking (below) is what actually keeps such titles on the card.
function wrapText(font, text, size, maxWidth) {
  const lines = [];
  let line = "";
  for (const word of text.split(/\s+/).filter(Boolean)) {
    const next = line ? `${line} ${word}` : word;
    if (line && textWidth(font, next, size) > maxWidth) {
      lines.push(line);
      line = word;
    } else line = next;
  }
  if (line) lines.push(line);
  return lines;
}

// The wordmark hangs from a fixed baseline and the green field is sized to sit
// a constant distance under it, so changing LOGO_TOP moves the header as a unit
// and leaves the descenders where they are relative to the field's bottom edge.
const LOGO_TOP = 44; // clearance above the wordmark's ascenders
const LOGO_BASELINE = LOGO_TOP + 148;
const BAND = LOGO_BASELINE + 37; // height of the green header field
const STRIPE = 20; // sliver of green left of the article slab
const PAD = 126; // uniform text inset inside the slab: left, right and top
const LINE_HEIGHT = 1.2;
const DATE_GAP = 72; // date baseline to first title baseline
const MAX_LINES = 4;
const FLOOR = 40; // last line's descenders stay this far off the bottom edge

// resvg resolves nothing off disk or the network, so the icon rides along in the
// SVG as a data URI. Its bytes therefore feed the card hash, and swapping the
// favicon re-cuts the front page's card on the next build.
let icon;
function ogIcon() {
  if (!icon) {
    const png = readFileSync(join(repoRoot, "out", "images", "dumpster.png"));
    // Straight out of the IHDR chunk, so the aspect ratio survives scaling.
    icon = {
      width: png.readUInt32BE(16),
      height: png.readUInt32BE(20),
      href: `data:image/png;base64,${png.toString("base64")}`,
    };
  }
  return icon;
}

// The icon runs the full height of the slab, inset by the same margin the text
// layout keeps clear at the bottom. It gets a tighter inset than the text's PAD
// because it is the whole content of the card rather than the start of a block:
// at PAD it reads as a small mark adrift in white instead of the subject.
// Left edge still lines up with where the title would begin.
function iconBody() {
  const { width, height, href } = ogIcon();
  const h = OG_HEIGHT - BAND - 2 * FLOOR;
  const w = h * (width / height);
  return `<image x="${STRIPE + PAD}" y="${BAND + FLOOR}" width="${w}" height="${h}" href="${href}"/>`;
}

function textBody({ title, date }) {
  const { sans, italic } = ogFonts();
  const maxWidth = OG_WIDTH - STRIPE - 2 * PAD;
  const dateSize = 30;

  // The block hangs from the top pad rather than being centred, so the gap above
  // the date matches the gap to the left of it. Measure from cap height, not the
  // baseline: padding is only even to the eye if it's to where the ink starts.
  const capDate = dateSize * 0.7;
  const layout = (titleSize) => {
    const lines = wrapText(sans, title, titleSize, maxWidth);
    const step = titleSize * LINE_HEIGHT;
    const top = BAND + PAD + (date ? capDate : titleSize * 0.72);
    const last =
      top +
      (date ? DATE_GAP : 0) +
      (lines.length - 1) * step +
      titleSize * 0.24;
    return {
      lines,
      step,
      top,
      fits: lines.length <= MAX_LINES && last <= OG_HEIGHT - FLOOR,
    };
  };

  // Step the title down until it fits — long titles shrink rather than running
  // out of the bottom of the card.
  let titleSize = 62;
  while (!layout(titleSize).fits && titleSize > 34) titleSize -= 4;
  const { lines, step, top } = layout(titleSize);

  let y = top;
  const body = [];
  if (date) {
    body.push(
      `<path d="${textPath(italic, date, dateSize).d}" fill="#222" transform="translate(${STRIPE + PAD} ${y})"/>`,
    );
    y += DATE_GAP;
  }
  for (const line of lines.slice(0, MAX_LINES)) {
    body.push(
      `<path d="${textPath(sans, line, titleSize).d}" fill="#111" transform="translate(${STRIPE + PAD} ${y})"/>`,
    );
    y += step;
  }
  return body;
}

function ogSvg(card) {
  const { heading } = ogFonts();
  const body = card.icon ? [iconBody()] : textBody(card);

  // The wordmark is an `h1`, so the browser renders Roberte — which ships only
  // a Regular — with synthetic bold. Stroking the outline reproduces that
  // thickening; a plain fill comes out as hairlines.
  const logoSize = 133;
  const logo = textPath(heading, "braindump.ing", logoSize, -0.0625 * logoSize);

  return `<svg xmlns="http://www.w3.org/2000/svg" width="${OG_WIDTH}" height="${OG_HEIGHT}" viewBox="0 0 ${OG_WIDTH} ${OG_HEIGHT}">
<rect width="${OG_WIDTH}" height="${OG_HEIGHT}" fill="#690"/>
<rect x="${STRIPE}" y="${BAND}" width="${OG_WIDTH - STRIPE}" height="${OG_HEIGHT - BAND}" fill="#fff"/>
<path d="${logo.d}" fill="#fff" stroke="#fff" stroke-width="${0.042 * logoSize}" stroke-linejoin="round" transform="translate(24 ${LOGO_BASELINE}) rotate(-5)"/>
${body.join("\n")}
</svg>`;
}

function ogPng(card) {
  return new Resvg(ogSvg(card), {
    fitTo: { mode: "width", value: OG_WIDTH },
  })
    .render()
    .asPng();
}

// ---- reading the card contents out of a page -------------------------------

const ENTITIES = {
  amp: "&",
  lt: "<",
  gt: ">",
  quot: '"',
  apos: "'",
  nbsp: " ",
};
function decodeEntities(s) {
  return s
    .replace(/&#(\d+);/g, (_m, n) => String.fromCodePoint(Number(n)))
    .replace(/&#x([0-9a-f]+);/gi, (_m, n) =>
      String.fromCodePoint(parseInt(n, 16)),
    )
    .replace(/&([a-z]+);/gi, (m, name) => ENTITIES[name.toLowerCase()] ?? m);
}

const stripTags = (s) => decodeEntities(s.replace(/<[^>]*>/g, "")).trim();

// The front page's card shows the site icon rather than its title — "About"
// says nothing a share preview needs, and the burning dumpster is the mark
// people recognise.
const ICON_PAGE = "index";

// Posts carry `<div id="date"><p>DATE</p><h1>TITLE</h1></div>`; the standalone
// pages (index, writing, todo, aesthetics) have no such block, so they fall back
// to the `<title>` element and render without a date.
function cardFor(html, slug) {
  if (slug === ICON_PAGE) return { icon: true };
  const dated = /<div id="date">\s*<p>([^<]*)<\/p>\s*<h1>([\s\S]*?)<\/h1>/.exec(
    html,
  );
  if (dated) return { date: stripTags(dated[1]), title: stripTags(dated[2]) };
  const titled = /<title>([\s\S]*?)<\/title>/.exec(html);
  const title = titled
    ? stripTags(titled[1]).replace(/^braindump\.ing\s*\|\s*/, "")
    : "braindump.ing";
  return { date: null, title };
}

// ---- R2 --------------------------------------------------------------------

const sha256 = (data) => createHash("sha256").update(data).digest("hex");
const hmac = (key, data) => createHmac("sha256", key).update(data).digest();

// SigV4 insists on RFC 3986, which `encodeURIComponent` leaves four characters
// short of.
const uriEscape = (s) =>
  encodeURIComponent(s).replace(
    /[!'()*]/g,
    (c) => `%${c.charCodeAt(0).toString(16).toUpperCase()}`,
  );

// SigV4 for a single request. R2 is S3-compatible and always uses the `auto`
// region. Doing this by hand keeps the AWS SDK (and its dependency tree) out of
// what is otherwise a two-dependency build.
function signedHeaders({
  method,
  host,
  path,
  query = {},
  body,
  contentType,
  accessKeyId,
  secretAccessKey,
}) {
  const now = new Date().toISOString().replace(/[-:]|\.\d{3}/g, "");
  const stamp = now.slice(0, 8);
  const payloadHash = sha256(body ?? "");

  const headers = {
    host,
    "x-amz-content-sha256": payloadHash,
    "x-amz-date": now,
  };
  if (contentType) headers["content-type"] = contentType;

  const names = Object.keys(headers).sort();
  const signed = names.join(";");
  const canonical = [
    method,
    path.split("/").map(uriEscape).join("/"),
    Object.keys(query)
      .sort()
      .map((k) => `${uriEscape(k)}=${uriEscape(query[k])}`)
      .join("&"),
    names.map((n) => `${n}:${headers[n]}`).join("\n") + "\n",
    signed,
    payloadHash,
  ].join("\n");

  const scope = `${stamp}/auto/s3/aws4_request`;
  const toSign = ["AWS4-HMAC-SHA256", now, scope, sha256(canonical)].join("\n");
  const signingKey = ["auto", "s3", "aws4_request"].reduce(
    (key, part) => hmac(key, part),
    hmac(`AWS4${secretAccessKey}`, stamp),
  );

  headers.Authorization =
    `AWS4-HMAC-SHA256 Credential=${accessKeyId}/${scope}, ` +
    `SignedHeaders=${signed}, Signature=${hmac(signingKey, toSign).toString("hex")}`;
  return headers;
}

// The R2 S3 endpoint is per-account (`<account>.r2.cloudflarestorage.com`), but
// the account id is not one of the four credentials. Prefer it straight from the
// environment; the API lookup only works for a broadly-scoped CLOUDFLARE_TOKEN,
// and an R2-only token — the sensible thing to be holding here — cannot list
// accounts at all.
async function accountId(token) {
  if (process.env.CLOUDFLARE_ACCOUNT_ID)
    return process.env.CLOUDFLARE_ACCOUNT_ID;
  const res = await fetch("https://api.cloudflare.com/client/v4/accounts", {
    headers: { Authorization: `Bearer ${token}` },
  });
  const id = (await res.json().catch(() => null))?.result?.[0]?.id;
  if (!id)
    throw new Error(
      "set CLOUDFLARE_ACCOUNT_ID in .env — CLOUDFLARE_TOKEN is not scoped to list accounts, " +
        "so the R2 endpoint host cannot be derived from it. It is in the Cloudflare dashboard " +
        "on the R2 page, or in the dashboard URL after /accounts/.",
    );
  return id;
}

function ogConfig() {
  const token = process.env.CLOUDFLARE_TOKEN;
  const accessKeyId = process.env.CLOUDFLARE_ACCESS_KEY_ID;
  const secretAccessKey = process.env.CLOUDFLARE_SECRET_ACCESS_KEY;
  const bucket = process.env.CLOUDFLARE_BUCKET;
  if (!token || !accessKeyId || !secretAccessKey || !bucket) return null;
  return { token, accessKeyId, secretAccessKey, bucket };
}

async function r2Request(
  config,
  method,
  key,
  { query, body, contentType } = {},
) {
  const host = `${config.account}.r2.cloudflarestorage.com`;
  const path = `/${config.bucket}${key ? `/${key}` : ""}`;
  const headers = signedHeaders({
    method,
    host,
    path,
    query,
    body,
    contentType,
    accessKeyId: config.accessKeyId,
    secretAccessKey: config.secretAccessKey,
  });
  const search = query
    ? "?" +
      Object.keys(query)
        .sort()
        .map((k) => `${uriEscape(k)}=${uriEscape(query[k])}`)
        .join("&")
    : "";
  return fetch(`https://${host}${path}${search}`, { method, headers, body });
}

// Object names carry a hash of the card's contents, so a retitled post gets a
// fresh URL (no CDN cache to bust) and an unchanged one can skip the upload.
async function uploadCard(config, name, png) {
  const key = `${OG_PREFIX}/${name}`;
  const head = await r2Request(config, "HEAD", key);
  if (head.status !== 200) {
    const res = await r2Request(config, "PUT", key, {
      body: png,
      contentType: "image/png",
    });
    if (!res.ok)
      throw new Error(
        `upload of ${key} failed: ${res.status} ${await res.text().catch(() => "")}`,
      );
    console.log(`og: uploaded ${key}`);
  }
  return `${CDN}/${key}`;
}

// Every key currently under the card prefix, following continuation tokens.
async function listCards(config) {
  const keys = [];
  let token;
  do {
    const query = { "list-type": "2", prefix: `${OG_PREFIX}/` };
    if (token) query["continuation-token"] = token;
    const res = await r2Request(config, "GET", "", { query });
    const xml = await res.text();
    if (!res.ok)
      throw new Error(`listing ${OG_PREFIX}/ failed: ${res.status} ${xml}`);
    for (const m of xml.matchAll(/<Key>([^<]+)<\/Key>/g)) keys.push(m[1]);
    token = /<IsTruncated>true<\/IsTruncated>/.test(xml)
      ? /<NextContinuationToken>([^<]+)<\/NextContinuationToken>/.exec(xml)?.[1]
      : undefined;
  } while (token);
  return keys;
}

// A card's name is only stable while its contents are, so a retitled post
// leaves its old hash behind. Sweep the prefix after every build: anything that
// looks like a card we generated but isn't one of the cards this build just
// linked to is a previous version (or belongs to a route that no longer wants a
// card at all) and goes. The shape test is what keeps this from touching
// anything else that might be filed under the prefix.
const CARD_KEY = new RegExp(
  `^${OG_PREFIX}/[A-Za-z0-9._-]+\\.[0-9a-f]{8}\\.png$`,
);

async function pruneCards(config, keep) {
  const stale = (await listCards(config)).filter(
    (key) => CARD_KEY.test(key) && !keep.has(key),
  );
  await Promise.all(
    stale.map(async (key) => {
      const res = await r2Request(config, "DELETE", key);
      // A delete of something already gone reports 204 too; either way it's out.
      if (!res.ok && res.status !== 404)
        throw new Error(`delete of ${key} failed: ${res.status}`);
      console.log(`og: deleted ${key}`);
    }),
  );
  return stale.length;
}

// ---- writing the link back into the page -----------------------------------

// Only pages built from `wrapper.html` carry an `og:image` slot. `aesthetics`
// opts out of SEO entirely and an unwritten draft is a zero-byte file, and
// neither wants a card rendered — let alone uploaded — for it.
const wantsOgImage = (html) => /<meta property="og:image" content="/.test(html);

// Fills the placeholders the layout leaves empty: the `og:image*` meta tags and
// the matching `image` entry in the JSON-LD block. Values are rewritten rather
// than only filled in, so a changed title updates a page that was built before.
function injectOgImage(html, url) {
  for (const [property, value] of [
    ["og:image", url],
    ["og:image:width", OG_WIDTH],
    ["og:image:height", OG_HEIGHT],
  ]) {
    html = html.replace(
      new RegExp(`(<meta property="${property}" content=")[^"]*(")`),
      `$1${value}$2`,
    );
  }

  return html.replace(
    /("image":\s*\[\s*\{\s*"contentUrl":\s*")[^"]*("\s*,\s*"width":\s*")[^"]*("\s*,\s*"height":\s*")[^"]*(")/,
    `$1${url}$2${OG_WIDTH}$3${OG_HEIGHT}$4`,
  );
}

// ---------------------------------------------------------------------------

// Transform every HTML file in the built `out/` directory in place. The path is
// resolved relative to this script so it works regardless of the cwd.
const outDir = fileURLToPath(new URL("../out/", import.meta.url));

loadEnv();
const config = ogConfig();
if (config) config.account = await accountId(config.token);

const pages = htmlFiles(outDir).map((path) => ({
  path,
  // `writing/2026-07-16-graphics-idea.html` -> `writing-2026-07-16-graphics-idea`
  slug: relative(outDir, path)
    .replace(/\.html$/, "")
    .replace(/[\\/]/g, "-"),
}));

// Keys this build linked to; everything else under the prefix is a leftover.
const live = new Set();

await Promise.all(
  pages.map(async ({ path, slug }) => {
    let html = await transform(readFileSync(path, "utf8"));
    if (config && wantsOgImage(html)) {
      const card = cardFor(html, slug);
      const png = ogPng(card);
      const name = `${slug}.${sha256(png).slice(0, 8)}.png`;
      live.add(`${OG_PREFIX}/${name}`);
      html = injectOgImage(html, await uploadCard(config, name, png));
    }
    writeFileSync(path, html);
  }),
);

// Only sweep once every page has claimed its card, so a key is never judged
// stale just because its page hasn't been reached yet.
if (config) await pruneCards(config, live);
