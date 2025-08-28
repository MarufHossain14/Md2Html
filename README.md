# md2html
Tiny Markdown → HTML converter in Haskell. Minimal now; growing toward a small static-site tool.

## Features (current)
- H1–H3, paragraphs
- **bold**, *italic*
- Minimal HTML escaping
- UTF-8 I/O (Windows-safe)

## Usage
```bash
ghc Md2Html.hs -o md2html
./md2html input.md output.html
# Windows PowerShell:
# .\md2html.exe .\input.md .\output.html
