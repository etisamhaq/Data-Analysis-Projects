"""
Downloads the Olist Brazilian e-commerce dataset into data/.

Source: "Brazilian E-Commerce Public Dataset by Olist" (Kaggle, olistbr), pinned to version 2. No Kaggle account needed.

    python download_data.py
"""

import shutil
from pathlib import Path

import kagglehub

DATASET = "olistbr/brazilian-ecommerce/versions/2"

if __name__ == "__main__":
    out = Path(__file__).parent / "data"
    out.mkdir(exist_ok=True)
    source = Path(kagglehub.dataset_download(DATASET))
    for csv in sorted(source.glob("*.csv")):
        shutil.copy(csv, out / csv.name)
        print(f"Saved {out / csv.name}")
