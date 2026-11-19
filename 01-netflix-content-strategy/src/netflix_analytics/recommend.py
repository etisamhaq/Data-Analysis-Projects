"""
Content-based recommendations ("because you watched X") and an honest offline evaluation of them.

There is no viewing data here, so the recommender can't be scored on what people watched next. Instead:
recommend from *descriptions only*, then check how often recommendations share a genre with the title asked
about. Genres aren't in the input, so agreement is earned, not copied.

The baseline matters. Netflix genres are type-specific ("TV Dramas" vs "Dramas"), so simply matching movies
with movies already raises genre agreement a lot. The model is therefore compared with random picks *of the same
type*, and recommends within type, which is also the sensible product behaviour.
"""

from __future__ import annotations

import numpy as np
import pandas as pd
from scipy.sparse import hstack
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.metrics.pairwise import linear_kernel
from sklearn.preprocessing import normalize

SEED = 42


def _people_tokens(series: pd.Series, top: int) -> pd.Series:
    """'A B, C D' -> 'a_b c_d' so each person is one token (and only the first `top` names count)."""
    return series.fillna("").str.split(", ").apply(lambda names: " ".join(n.strip().lower().replace(" ", "_")
                                                                            for n in names[:top] if n.strip()))


class ContentRecommender:
    """
    Recommends similar titles of the same type from a weighted mix of description, genres, director and cast.

    `weights` sets each signal's share of the similarity; signals are L2-normalised before weighting so a long
    description doesn't dominate by sheer length.
    """

    def __init__(self, weights: dict[str, float] | None = None):
        self.weights = weights or {"description": 0.5, "genres": 0.3, "director": 0.1, "cast": 0.1}

    def fit(self, titles: pd.DataFrame) -> ContentRecommender:
        self.titles = titles.reset_index(drop=True)
        fields = {
            "description": (self.titles["description"].fillna(""),
                            TfidfVectorizer(stop_words="english", min_df=2, max_df=0.5, sublinear_tf=True)),
            "genres": (self.titles["listed_in"].fillna("").str.replace(" ", "_").str.replace(",_", " "),
                       TfidfVectorizer(token_pattern=r"\S+")),
            "director": (_people_tokens(self.titles["director"], 2), TfidfVectorizer(token_pattern=r"\S+")),
            "cast": (_people_tokens(self.titles["cast"], 5), TfidfVectorizer(token_pattern=r"\S+")),
        }
        blocks = []
        for name, (text, vectorizer) in fields.items():
            weight = self.weights.get(name, 0)
            if weight > 0:
                blocks.append(normalize(vectorizer.fit_transform(text)) * np.sqrt(weight))
        self.matrix = hstack(blocks).tocsr()
        return self

    def recommend(self, title: str, k: int = 5) -> pd.DataFrame:
        matches = self.titles.index[self.titles["title"].str.lower() == title.lower()]
        if len(matches) == 0:
            raise KeyError(f"Title not found: {title!r}")
        i = matches[0]
        sims = linear_kernel(self.matrix[i], self.matrix).ravel()
        candidates = np.where((self.titles["type"] == self.titles.loc[i, "type"]).to_numpy())[0]
        candidates = candidates[candidates != i]
        best = candidates[np.argsort(sims[candidates])[::-1][:k]]
        out = self.titles.loc[best, ["title", "type", "listed_in", "release_year"]].copy()
        out["similarity"] = sims[best].round(3)
        return out.reset_index(drop=True)


def genre_agreement_eval(titles: pd.DataFrame, k: int = 10, n_queries: int = 1500, n_boot: int = 1000,
                         seed: int = SEED) -> dict:
    """
    Offline evaluation: share of top-k description-only recommendations (same type) sharing at least one genre
    with the query, vs the same share for random same-type titles, with a bootstrap 95% CI on the lift.
    """
    rng = np.random.default_rng(seed)
    t = titles.reset_index(drop=True)
    genres = t["listed_in"].fillna("").str.split(", ").apply(set).tolist()
    types = t["type"].to_numpy()
    X = TfidfVectorizer(stop_words="english", min_df=2, max_df=0.5, sublinear_tf=True).fit_transform(t["description"].fillna(""))
    queries = rng.choice(len(t), size=min(n_queries, len(t)), replace=False)

    model, baseline, any_type = [], [], []
    others = np.arange(len(t))
    for i in queries:
        candidates = np.where((types == types[i]) & (others != i))[0]
        sims = linear_kernel(X[i], X[candidates]).ravel()
        recs = candidates[np.argsort(sims)[::-1][:k]]
        rand = rng.choice(candidates, size=k, replace=False)
        rand_any = rng.choice(others[others != i], size=k, replace=False)
        model.append(np.mean([bool(genres[i] & genres[j]) for j in recs]))
        baseline.append(np.mean([bool(genres[i] & genres[j]) for j in rand]))
        any_type.append(np.mean([bool(genres[i] & genres[j]) for j in rand_any]))
    model, baseline = np.array(model), np.array(baseline)
    lift = model - baseline
    boots = [rng.choice(lift, size=len(lift)).mean() for _ in range(n_boot)]
    return {"k": k, "queries": len(queries), "model_genre_agreement": float(model.mean()),
            "random_same_type_agreement": float(baseline.mean()), "random_any_type_agreement": float(np.mean(any_type)),
            "lift": float(lift.mean()),
            "lift_ci_low": float(np.percentile(boots, 2.5)), "lift_ci_high": float(np.percentile(boots, 97.5))}
