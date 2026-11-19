# Signal or Noise: Data Analysis Case Studies

[![ci](https://github.com/etisamhaq/Data-Analysis-Projects/actions/workflows/ci.yml/badge.svg)](https://github.com/etisamhaq/Data-Analysis-Projects/actions/workflows/ci.yml)

Business and data analysis case studies in **Python, SQL and R**, each about telling a real pattern from noise. Every project starts from a question, ends with findings and a recommendation, and says what the data can't support.

| # | Project | Question | Headline finding | Tools |
|---|---|---|---|---|
| 1 | [Netflix content strategy](01-netflix-content-strategy) ([live dashboard](https://netflix-catalogue-trends.vercel.app/)) | What has Netflix prioritised in its catalogue since 2015, and can the catalogue power recommendations? | TV's share rose from 25% to 34% (p < 10⁻⁷); India surged sevenfold then fell 60%; a catalogue-only recommender beats a type-matched random baseline by 22 points | Python, statsmodels, NLP, recommender |
| 2 | [Adani Ports stock analysis](02-adani-ports-stock-analysis) | How did the stock compare with the market, how should its daily risk be measured, and does volume predict price? | Beat the Nifty 50 on return (10.4% vs 7.3% a year) but lost on risk-adjusted return; 73% of its risk is company-specific; GARCH-filtered VaR passes backtests that normal VaR fails | R, GARCH, VaR backtesting |
| 3 | [NBA winning performances](03-nba-winning-performances) ([live dashboard](https://nba-win-probability-jade.vercel.app/)) | What decides games, and can they be predicted before tip-off? | The better-shooting team wins 81% of games; pre-game, a calibrated Elo rating calls 64% and recent-form stats add nothing reliable | Python, scikit-learn, Elo |
| 4 | [Synthetic data sanity check](04-synthetic-data-sanity-check) | Can two popular Kaggle datasets support the findings drawn from them? | No: both are randomly generated. Packaged as `datasanity`, a CLI that flags both and passes all three real datasets | Python, SciPy, CLI |
| 5 | [Olist e-commerce analytics](05-olist-ecommerce-analytics) ([live dashboard](https://olist-marketplace-dashboard.vercel.app/)) | Where does a marketplace's growth come from, and what does late delivery cost? | GMV +140% year on year, almost all new customers (3% ever return); a late delivery costs about 2 review stars, and Rio de Janeiro is late 2.7× as often as São Paulo | SQL (DuckDB), statsmodels, dashboard |

## Concepts and techniques covered

Each project README has a full **Concepts and techniques** section; this is the index. Projects: 1 Netflix · 2 Adani Ports · 3 NBA · 4 datasanity · 5 Olist.

| Area | Concepts and techniques | Projects |
|---|---|---|
| **Data preparation and quality** | Explicit parsing, validation that fails loudly, completeness checks, quality reports of every exclusion, deduplication, domain-aware fixes (share splits, season IDs, TV release years), synthetic-data detection | all, especially 4 |
| **SQL and data modelling** | Layered warehouse (staging, marts), fact and dimension tables, CTEs, window functions (`row_number`, `ntile`), cohort and RFM queries, SQL data tests | 5 |
| **Exploratory and descriptive analytics** | Trends, mix and share analysis, geographic breakdowns, distributions, correlation | 1, 2, 5 |
| **Statistical inference** | Confidence intervals (Wilson, regression, bootstrap), chi-square, Kolmogorov-Smirnov, logistic trend tests, F-tests, multiple-testing correction, effect sizes alongside p-values | 1, 2, 3, 4, 5 |
| **Regression and causal thinking** | OLS with fixed effects and robust standard errors, logistic regression, dose-response, equal follow-up windows, association vs causation, selection and survivorship bias | 1, 2, 3, 5 |
| **Machine learning** | Logistic regression, gradient boosting, Elo ratings, time-based train/validation/test splits, grid search, leakage-free features and leakage tests, concept drift | 3, 5 |
| **Model evaluation** | Log loss, Brier score, AUC, average precision, calibration curves, top-decile lift, baselines, bootstrap model comparison, offline proxy evaluation | 1, 3, 5 |
| **Time series and financial risk** | Returns and CAGR, volatility, drawdowns, CAPM beta, GARCH(1,1) by maximum likelihood, Value-at-Risk with Kupiec/Christoffersen backtests, Granger causality | 2 |
| **NLP and recommender systems** | TF-IDF, NMF topic modelling with stability-based model selection, content-based recommendation, hybrid feature weighting | 1 |
| **Business analytics** | KPI design, year-on-year growth, cohort retention, RFM segmentation, customer lifetime value reasoning, impact sizing, decision-first recommendations | 1, 2, 3, 5 |
| **Communication** | Executive summaries (`SUMMARY.md`), interactive dashboards, consistent colourblind-safe charts, methodology documents | all |
| **Engineering** | Tested packages (pytest, testthat), CLI tool, pinned environments, Docker, CI that re-runs every notebook and analysis | all |

## How these projects are built

- **A question first**, a recommendation last, and limitations stated, not hidden.
- **Data checked before it is analysed.** Each project found a real data problem and handled it: a share split that looked like an 80% crash (Adani Ports), seasons missing from the source (NBA), TV "release years" that are really the latest season (Netflix), duplicate reviews and stray geocodes (Olist), and two datasets that turned out to be fake (sanity check).
- **Results that survive scrutiny.** Claims come with intervals and tests, models are judged on data they never saw, and results that didn't hold up are reported as such: volume doesn't predict prices (Adani), recent form adds nothing beyond Elo (NBA), late deliveries don't measurably cut repeat purchases (Olist).
- **Reproducible.** Every number in a README is printed by the project's notebook or script.

## Engineering

- **Tested packages, thin notebooks.** Analysis logic lives in importable modules under each project's `src/` (or `R/`, `sql/`), covered by pytest, testthat and SQL data tests; notebooks tell the story and call that code.
- **Continuous integration.** Every push runs linting (ruff), the Python tests, a clean re-execution of every notebook, the R tests, and the R analysis, failing if its regenerated results differ from the committed ones.
- **Pinned and reproducible.** Exact dependency versions in `pyproject.toml`, datasets pinned to a Kaggle version, fixed random seeds, R run in a pinned Docker image.

## Quick start

```bash
make setup        # .venv with pinned dependencies and the project packages
make data         # download datasets not stored in the repo (NBA, Olist)
make test         # Python unit and data-validation tests
make r-test       # R tests in Docker
make notebooks    # re-run every notebook from scratch
make dashboard    # rebuild the Olist dashboard data
make help         # all tasks
```

Each project folder has its own README, `SUMMARY.md` (one-page executive summary) and `docs/methodology.md`. Python 3.10-3.12; the R project needs R 4.x with the tidyverse, or Docker.

## License

MIT, see [LICENSE](LICENSE). Datasets keep their original Kaggle licences, linked from each project.
