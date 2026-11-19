"""
Execute every project notebook from a clean kernel and fail if any cell errors.

    python tools/run_notebooks.py            # check only (used in CI)
    python tools/run_notebooks.py --write    # also save the fresh outputs into the notebooks
    python tools/run_notebooks.py 03-nba     # only notebooks whose path contains "03-nba"
"""

import argparse
import sys
import time
from pathlib import Path

import nbformat
from nbclient import NotebookClient
from nbclient.exceptions import CellExecutionError

ROOT = Path(__file__).resolve().parents[1]


def notebooks(pattern: str | None) -> list[Path]:
    found = sorted(p for p in ROOT.glob("[0-9][0-9]-*/*.ipynb") if ".ipynb_checkpoints" not in p.parts)
    return [p for p in found if pattern is None or pattern in str(p.relative_to(ROOT))]


def run(path: Path, write: bool) -> bool:
    nb = nbformat.read(path, as_version=4)
    start = time.time()
    try:
        NotebookClient(nb, timeout=1200, kernel_name="python3", resources={"metadata": {"path": str(path.parent)}}).execute()
    except CellExecutionError as err:
        print(f"FAIL {path.relative_to(ROOT)}\n{str(err)[:2000]}")
        return False
    if write:
        nbformat.write(nb, path)
    print(f"ok   {path.relative_to(ROOT)} ({time.time() - start:.0f}s)")
    return True


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("pattern", nargs="?", help="only run notebooks whose path contains this text")
    parser.add_argument("--write", action="store_true", help="save fresh outputs into the notebooks")
    args = parser.parse_args()
    paths = notebooks(args.pattern)
    if not paths:
        print("No notebooks matched")
        return 1
    results = [run(p, args.write) for p in paths]
    return 0 if all(results) else 1


if __name__ == "__main__":
    sys.exit(main())
