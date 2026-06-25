import prism from "prismjs";
import mjAPI from "mathjax-node";
import { readFileSync, writeFileSync, readdirSync } from "fs";
import { join } from "path";
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

// Transform every HTML file in the built `out/` directory in place. The path is
// resolved relative to this script so it works regardless of the cwd.
const outDir = fileURLToPath(new URL("../out/", import.meta.url));
for (const path of htmlFiles(outDir)) {
  const input = readFileSync(path, "utf8");
  writeFileSync(path, await transform(input));
}
