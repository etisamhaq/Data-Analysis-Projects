"""
Themes in title descriptions: TF-IDF + non-negative matrix factorisation (NMF).

NMF splits the description-term matrix into a small set of topics (weighted word lists) and gives each title a
weight on each topic. The number of topics is chosen by *stability*: refitting from different random starts
should find the same topics, otherwise they are artefacts of the initialisation.
"""

from __future__ import annotations

from dataclasses import dataclass

import numpy as np
import pandas as pd
from scipy.optimize import linear_sum_assignment
from sklearn.decomposition import NMF
from sklearn.feature_extraction.text import TfidfVectorizer

SEED = 42


def description_matrix(descriptions: pd.Series, min_df: int = 5, max_df: float = 0.3):
    vectorizer = TfidfVectorizer(stop_words="english", min_df=min_df, max_df=max_df)
    return vectorizer, vectorizer.fit_transform(descriptions.fillna(""))


@dataclass
class TopicModel:
    vectorizer: TfidfVectorizer
    model: NMF
    weights: np.ndarray            # titles x topics

    def top_terms(self, n: int = 8) -> list[list[str]]:
        terms = np.array(self.vectorizer.get_feature_names_out())
        return [terms[comp.argsort()[::-1][:n]].tolist() for comp in self.model.components_]

    def labels(self, n: int = 3) -> list[str]:
        """A readable label per topic from its top terms (generated, so it can't drift from the model)."""
        return [" / ".join(t[:n]) for t in self.top_terms(n)]

    def dominant_topic(self) -> np.ndarray:
        """Each title's strongest topic, or -1 if its description matched no topic at all."""
        dominant = self.weights.argmax(axis=1)
        dominant[self.weights.max(axis=1) == 0] = -1
        return dominant


def fit_topics(descriptions: pd.Series, k: int = 12, seed: int = SEED) -> TopicModel:
    vectorizer, X = description_matrix(descriptions)
    model = NMF(n_components=k, random_state=seed, init="nndsvda", max_iter=500)
    return TopicModel(vectorizer, model, model.fit_transform(X))


def _matched_similarity(a: np.ndarray, b: np.ndarray) -> float:
    """Mean cosine similarity between two sets of topics after optimally pairing them up."""
    a = a / np.linalg.norm(a, axis=1, keepdims=True)
    b = b / np.linalg.norm(b, axis=1, keepdims=True)
    sim = a @ b.T
    rows, cols = linear_sum_assignment(-sim)
    return float(sim[rows, cols].mean())


def topic_stability(descriptions: pd.Series, ks: list[int], seeds: int = 5) -> pd.Series:
    """
    For each k, fit NMF from `seeds` random initialisations and average the matched similarity of every pair of
    runs (1 = identical topics every time). Random init is essential: deterministic init would always score 1.
    """
    _, X = description_matrix(descriptions)
    out = {}
    for k in ks:
        comps = [NMF(n_components=k, random_state=s, init="random", max_iter=800).fit(X).components_ for s in range(seeds)]
        out[k] = np.mean([_matched_similarity(comps[i], comps[j]) for i in range(seeds) for j in range(i + 1, seeds)])
    return pd.Series(out, name="stability")
