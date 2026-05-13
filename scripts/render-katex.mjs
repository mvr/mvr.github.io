import fs from "node:fs";
import katex from "katex";

function fromHex(hex) {
  return Buffer.from(hex, "hex").toString("utf8");
}

function toHex(text) {
  return Buffer.from(text, "utf8").toString("hex");
}

function decodeEntities(text) {
  return text.replace(/&(#x[0-9a-f]+|#\d+|amp|lt|gt|quot|apos);/gi, (match, entity) => {
    const lower = entity.toLowerCase();

    if (lower.startsWith("#x")) {
      return String.fromCodePoint(Number.parseInt(lower.slice(2), 16));
    }

    if (lower.startsWith("#")) {
      return String.fromCodePoint(Number.parseInt(lower.slice(1), 10));
    }

    return {
      amp: "&",
      lt: "<",
      gt: ">",
      quot: "\"",
      apos: "'",
    }[lower] ?? match;
  });
}

function renderMacroPreamble(source, macros) {
  for (const line of source.split(/\r?\n/)) {
    const trimmed = line.trim();
    if (!trimmed || trimmed.startsWith("%") || trimmed.startsWith("<!--")) {
      continue;
    }

    try {
      katex.renderToString(trimmed, {
        displayMode: false,
        throwOnError: false,
        macros,
      });
    } catch {
      // Pandoc usually expands LaTeX macros before this point; unsupported
      // preamble commands should not prevent ordinary math from rendering.
    }
  }
}

function renderTex(tex, displayMode, macros) {
  return katex.renderToString(tex, {
    displayMode,
    throwOnError: false,
    fleqn: false,
    macros,
  });
}

function renderHtmlFragment(html) {
  const macros = {};

  return html.replace(
    /<span\s+class="math\s+(inline|display)">([\s\S]*?)<\/span>/g,
    (_match, mode, texHtml) => renderTex(decodeEntities(texHtml), mode === "display", macros)
  );
}

const htmlFileIndex = process.argv.indexOf("--html-file");
const input = htmlFileIndex === -1
  ? fs.readFileSync(0, "utf8")
  : fs.readFileSync(process.argv[htmlFileIndex + 1], "utf8");

if (process.argv.includes("--html") || htmlFileIndex !== -1) {
  const rendered = renderHtmlFragment(input);
  const outFileIndex = process.argv.indexOf("--out");
  if (outFileIndex === -1) {
    process.stdout.write(rendered);
  } else {
    fs.writeFileSync(process.argv[outFileIndex + 1], rendered);
  }
  process.exit(0);
}

const lines = input.split("\n");

const macroSource = fromHex(lines[0] ?? "");
const count = Number.parseInt(lines[1] ?? "0", 10);
const macros = {};
const output = [String(count)];

renderMacroPreamble(macroSource, macros);

for (let i = 0; i < count; i += 1) {
  const line = lines[i + 2] ?? "";
  const tab = line.indexOf("\t");
  const mode = tab === -1 ? "I" : line.slice(0, tab);
  const tex = fromHex(tab === -1 ? "" : line.slice(tab + 1));

  try {
    const html = renderTex(tex, mode === "D", macros);
    output.push(`ok\t${toHex(html)}`);
  } catch (error) {
    output.push(`err\t${toHex(error instanceof Error ? error.message : String(error))}`);
  }
}

process.stdout.write(`${output.join("\n")}\n`);
