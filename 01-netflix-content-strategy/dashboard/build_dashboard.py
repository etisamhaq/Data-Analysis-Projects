"""
Builds dashboard/data.json for the Netflix trends page (dashboard/index.html).

    python dashboard/build_dashboard.py      (from the project folder)

Trends come from the same functions as the notebook. Recommendations are precomputed for every title with the same
ContentRecommender, so the page needs no server.
"""

import json
from pathlib import Path

import numpy as np
import pandas as pd
from netflix_analytics import data, recommend, topics, trends
from sklearn.metrics.pairwise import linear_kernel

OUT = Path(__file__).resolve().parent / "data.json"
TOP_K = 5


def precompute_recommendations(titles: pd.DataFrame, k: int = TOP_K, chunk: int = 500) -> list[list[int]]:
    model = recommend.ContentRecommender().fit(titles)
    types = titles["type"].to_numpy()
    out = []
    for start in range(0, len(titles), chunk):
        sims = linear_kernel(model.matrix[start:start + chunk], model.matrix)
        for row, i in enumerate(range(start, min(start + chunk, len(titles)))):
            s = sims[row].copy()
            s[types != types[i]] = -1          # same type only, as in the notebook
            s[i] = -1
            out.append(np.argsort(s)[::-1][:k].tolist())
    return out


def main():
    df, report = data.load_titles("netflix_titles.csv")
    df = df.reset_index(drop=True)
    recent = data.window(df, 2015, 2021)

    def shares(frame, flag, first=2015):
        s = trends.share_by_year(frame, flag).loc[first:]
        return [{"year": int(y), "share": float(r["share_%"]), "low": float(r["ci_low_%"]), "high": float(r["ci_high_%"]),
                 "n": int(r["n"])} for y, r in s.iterrows()]

    countries = data.explode_list(recent, "countries", "country")
    country_share = pd.crosstab(countries["year_added"], countries["country"], normalize="index") * 100
    country_share.index = country_share.index.astype(int)
    top = countries["country"].value_counts().head(10).index
    by_type = pd.crosstab(recent["year_added"], recent["type"])
    by_type.index = by_type.index.astype(int)
    genres = data.explode_list(recent, "genres", "genre")
    genre_change = trends.share_change(genres, "genre", (2016, 2017), (2020, 2021))
    movies = recent[recent["type"] == "Movie"].assign(fresh=lambda d: (d["content_age"] <= 1).astype(int))

    model = topics.fit_topics(df["description"], k=12)
    df["theme"] = pd.Series(model.dominant_topic(), index=df.index).map(dict(enumerate(model.labels(3))))
    theme_change = trends.share_change(data.window(df, 2016, 2021).dropna(subset=["theme"]), "theme", (2016, 2017), (2020, 2021))
    evaluation = recommend.genre_agreement_eval(df, k=10, n_queries=1500)

    out = {
        "titles_total": int(len(df)), "last_added": str(report["last_date_added"]),
        "additions": [{"year": int(y), "movies": int(r.get("Movie", 0)), "tv": int(r.get("TV Show", 0))} for y, r in by_type.iterrows()],
        "tv_share": shares(recent, "is_tv"),
        "kids_share": shares(recent, "is_kids", 2016),
        "mature_share": shares(recent, "is_mature", 2016),
        "fresh_movies": shares(movies, "fresh", 2016),
        "countries": {c: [{"year": int(y), "share": float(country_share.loc[y, c])} for y in country_share.loc[2016:].index] for c in top},
        "genre_movers": [{"name": g, "change": float(v)} for g, v in pd.concat([genre_change.head(5), genre_change.tail(5)]).items()],
        "theme_movers": [{"name": g, "change": float(v)} for g, v in theme_change.items()],
        "recommender_eval": {k: float(v) for k, v in evaluation.items()},
        "catalogue": [[t, ty, int(y), g] for t, ty, y, g in zip(df["title"].astype(str), df["type"], df["release_year"],
                                                                 df["listed_in"].fillna(""), strict=True)],
        "recs": precompute_recommendations(df),
    }
    OUT.write_text(json.dumps(out, separators=(",", ":"), ensure_ascii=False))
    print(f"Wrote {OUT} ({OUT.stat().st_size / 1024:.0f} KB)")


if __name__ == "__main__":
    main()
