"""
Generates the MkDocs source folder (docs_site/) from the repository's own documents, so the documentation site
can never drift from the READMEs:

    README.md                     -> index.md
    <project>/README.md           -> <project>/index.md        (the case study)
    <project>/SUMMARY.md          -> <project>/summary.md      (executive summary)
    <project>/docs/methodology.md -> <project>/methodology.md
    02-.../results/summary.md     -> 02-.../results.md
    <project>/figures/*           -> <project>/figures/*

Relative links are rewritten: links to another document become links to its page; links to code, notebooks and
other files become links to the file on GitHub.

    python tools/build_docs.py && mkdocs build --strict
"""

from __future__ import annotations

import os
import re
import shutil
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
OUT = ROOT / "docs_site"
GITHUB = "https://github.com/etisamhaq/Data-Analysis-Projects"
PROJECTS = sorted(p.name for p in ROOT.glob("[0-9][0-9]-*") if p.is_dir())

LINK = re.compile(r"(!?\[[^\]]*\])\(([^)\s]+)\)")


def page_map() -> dict[str, str]:
    """Repository path -> site path, for every document that becomes a page."""
    pages = {"README.md": "index.md"}
    for p in PROJECTS:
        for source, target in [("README.md", "index.md"), ("SUMMARY.md", "summary.md"),
                               ("docs/methodology.md", "methodology.md"), ("results/summary.md", "results.md")]:
            if (ROOT / p / source).exists():
                pages[f"{p}/{source}"] = f"{p}/{target}"
    return pages


def rewrite_links(text: str, source: str, target: str, pages: dict[str, str]) -> str:
    source_dir, target_dir = Path(source).parent, Path(target).parent

    def fix(match: re.Match) -> str:
        label, href = match.groups()
        if re.match(r"^(https?:|mailto:|#)", href):
            return match.group(0)
        path, _, anchor = href.partition("#")
        repo_path = os.path.normpath(source_dir / path).replace(os.sep, "/")
        if repo_path in (".", ""):
            repo_path = "README.md"
        if (ROOT / repo_path).is_dir() and f"{repo_path}/README.md" in pages:
            repo_path = f"{repo_path}/README.md"
        suffix = f"#{anchor}" if anchor else ""
        if repo_path in pages:
            new = os.path.relpath(pages[repo_path], target_dir).replace(os.sep, "/")
        elif "/figures/" in f"/{repo_path}" and (ROOT / repo_path).is_file():
            new = os.path.relpath(repo_path, target_dir).replace(os.sep, "/")
        elif (ROOT / repo_path).exists():
            kind = "tree" if (ROOT / repo_path).is_dir() else "blob"
            new = f"{GITHUB}/{kind}/main/{repo_path}"
        else:
            raise FileNotFoundError(f"Broken link in {source}: {href}")
        return f"{label}({new}{suffix})"

    return LINK.sub(fix, text)


def main() -> None:
    if OUT.exists():
        shutil.rmtree(OUT)
    pages = page_map()
    for source, target in pages.items():
        dest = OUT / target
        dest.parent.mkdir(parents=True, exist_ok=True)
        dest.write_text(rewrite_links((ROOT / source).read_text(), source, target, pages))
    for p in PROJECTS:
        if (ROOT / p / "figures").is_dir():
            shutil.copytree(ROOT / p / "figures", OUT / p / "figures")
    shutil.copy(ROOT / "tools" / "docs_dashboards.md", OUT / "dashboards.md")
    print(f"Generated {len(pages)} pages in {OUT.relative_to(ROOT)}")


if __name__ == "__main__":
    main()
