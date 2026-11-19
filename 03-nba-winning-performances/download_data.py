"""
Downloads the team box scores used by this project into data/.

Source: "Historical NBA Data and Player Box Scores" by Eoin A Moore (Kaggle), pinned to version 552
so results are reproducible (the dataset is updated daily). No Kaggle account is needed.

    pip install kagglehub
    python download_data.py
"""

import shutil
from pathlib import Path

import kagglehub

DATASET = "eoinamoore/historical-nba-data-and-player-box-scores/versions/552"
FILE = "TeamStatistics.csv"

if __name__ == "__main__":
    out = Path(__file__).parent / "data"
    out.mkdir(exist_ok=True)
    path = kagglehub.dataset_download(DATASET, path=FILE)
    shutil.copy(path, out / FILE)
    print(f"Saved {out / FILE}")
