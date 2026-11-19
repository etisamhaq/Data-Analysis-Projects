# Methodology

## 1. What the data is, and isn't

A **snapshot**: the 8,807 titles available on Netflix in 2021, each with the date it was added. Titles that were
added and later removed are absent. Consequences:

- Earlier years are under-counted, particularly licensed content, which expires.
- Shares within a year are comparable over time only as "among titles that survived to 2021".
- Back-catalogue licences expire faster than originals, so early years look fresher than they were.

All findings are worded accordingly.

## 2. Cleaning (`data.load_titles`)

| Issue | Handling |
|---|---|
| `date_added` has stray spaces, 10 values unparseable | Strip; parse with an explicit format; unparseable rows kept but excluded from time trends |
| 3 rows have the duration in `rating` ("74 min") | Moved to `duration`; `rating` set to missing |
| For TV shows `release_year` is the latest season | `content_age` (year added − release year) computed for movies only |
| `country` lists several countries | Split; every country counted (`explode_list`) |
| 2021 ends 25 September | Annualised only for total-volume comparisons |

**Validation raises** on duplicate IDs, unexpected types, movies "added" more than a year before release, or future
dates.

## 3. Statistical inference

| Question | Method |
|---|---|
| How uncertain is a yearly share? | Wilson score 95% interval (accurate for small counts and shares near 0 or 1) |
| Is there a trend across years? | Logistic regression of the indicator on year; odds ratio per year with 95% CI and p-value |
| Did a share change between two years? | Chi-square test on the 2×2 table |
| Which genres or themes moved? | Change in share of mentions between two-year windows (2016–17 vs 2020–21) |

With 400–2,000 titles a year, small shifts can be significant without mattering; the README reports effect sizes
(percentage points, odds ratios) alongside p-values.

## 4. Topic modelling (`topics.py`)

1. TF-IDF on descriptions: English stop words removed; terms in fewer than 5 descriptions or more than 30% of them dropped.
2. Non-negative matrix factorisation: X ≈ W · H, with H giving each topic's word weights and W each title's topic weights.
3. Each title's **dominant topic** is the largest weight in its row of W (titles with no matching words get none).
4. **Choosing k by stability.** For k ∈ {8, 12, 16}, fit from 5 random initialisations; for each pair of runs, match
   topics one-to-one (Hungarian algorithm on cosine similarity) and average the matched similarity.

| k | Stability |
|---|---|
| 8 | 0.825 |
| 12 | 0.872 |
| 16 | 0.873 |

k = 12 is as stable as 16 and more interpretable. Random initialisation is essential: with the deterministic NNDSVD
initialisation every run is identical and stability is trivially 1.0. Labels are generated from each topic's top
three words, so they can't drift from the model.

## 5. Recommender (`recommend.py`)

**Model.** Four TF-IDF blocks: description (word unigrams, sublinear TF), genres, director (first 2) and cast
(first 5), with each person as a single token. Each block is L2-normalised and scaled by √weight (0.5 / 0.3 / 0.1 / 0.1),
so the cosine similarity of the stacked vectors is the weighted sum of per-block similarities. Recommendations are
restricted to the same type.

**Offline evaluation.** No viewing data exists, so:
- **Proxy target:** does a recommendation share at least one genre with the query title?
- **Leakage control:** the evaluated model uses descriptions only, so genre agreement can't be copied from the input.
- **Baseline:** random titles of the **same type**. Netflix genres are type-specific ("TV Dramas" vs "Dramas"), so
  matching type alone raises agreement from 23.6% (any random title) to 41.7%. Comparing with an untyped baseline
  would overstate the model's lift almost twofold.
- **Uncertainty:** bootstrap 95% CI over 1,500 query titles.

| | Genre agreement (top 10) |
|---|---|
| Random title, any type | 23.6% |
| Random title, same type | 41.7% |
| Description-only model, same type | 63.4% |

## 6. Limitations

- Snapshot bias (section 1).
- Additions, not viewing: a title added is not a title watched.
- Topic labels are word lists; the human-readable names in the README are interpretations.
- The recommender's proxy measures topical relevance, not engagement; an online A/B test would be the real test.
