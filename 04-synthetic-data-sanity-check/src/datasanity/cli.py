"""
Command line: check one or more data files for signs of synthetic generation.

    datasanity data.csv                      # report to the terminal
    datasanity data.csv --format json        # machine-readable
    datasanity a.csv b.csv --out reports/    # one Markdown report per file
    datasanity data.csv --fail-on-synthetic  # exit code 1 if LIKELY SYNTHETIC (for pipelines)
"""

from __future__ import annotations

import argparse
import sys
from pathlib import Path

from .report import check_file


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(prog="datasanity", description="Check data files for signs of random generation.")
    parser.add_argument("files", nargs="+", help="CSV (optionally .gz) or Excel files")
    parser.add_argument("--format", choices=["markdown", "json"], default="markdown")
    parser.add_argument("--out", help="directory to write one report per file instead of printing")
    parser.add_argument("--fail-on-synthetic", action="store_true", help="exit 1 if any file is LIKELY SYNTHETIC")
    args = parser.parse_args(argv)

    any_synthetic = False
    for path in args.files:
        report = check_file(path)
        any_synthetic |= report.verdict == "LIKELY SYNTHETIC"
        text = report.to_json() if args.format == "json" else report.to_markdown()
        if args.out:
            out_dir = Path(args.out)
            out_dir.mkdir(parents=True, exist_ok=True)
            suffix = ".json" if args.format == "json" else ".md"
            target = out_dir / (Path(path).name.split(".")[0] + "_sanity" + suffix)
            target.write_text(text)
            print(f"{report.verdict:<22} {path} -> {target}")
        else:
            print(text)
    return 1 if (args.fail_on_synthetic and any_synthetic) else 0


if __name__ == "__main__":
    sys.exit(main())
