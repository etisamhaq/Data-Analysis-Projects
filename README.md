# Signal or Noise: Data Analysis Case Studies

Business and data analysis case studies in **Python** and **R**, each about telling a real pattern from noise. Every project starts from a question, ends with findings and a recommendation, and says what the data can't support.

| # | Project | Question | Headline finding | Tools |
|---|---|---|---|---|
| 1 | [Netflix content strategy](01-netflix-content-strategy) | What has Netflix prioritised in its catalogue since 2015? | TV's share of additions rose from 25% (2018) to 34% (2021); India's share jumped sevenfold to 18%, then fell back to 7% | Python, pandas |
| 2 | [Adani Ports stock analysis](02-adani-ports-stock-analysis) | What did holding the stock from 2007 to 2021 take, and does volume predict price? | 10.4% a year, but with an 80% crash that took 6.5 years to recover; volume spikes don't predict the next day's direction | R, tidyverse |
| 3 | [NBA winning performances](03-nba-winning-performances) | What decides games, and can they be predicted before tip-off? | The better-shooting team wins 81% of games; pre-game, a calibrated Elo rating calls 64% and recent-form stats add nothing reliable | Python, scikit-learn, Elo |
| 4 | [Synthetic data sanity check](04-synthetic-data-sanity-check) | Can two popular Kaggle datasets support the findings drawn from them? | No: both are randomly generated (cities in the wrong states, AQI unrelated to PM2.5) | Python, SciPy |

## How these projects are built

- **A question first**, a recommendation last, and limitations stated, not hidden.
- **Data checked before it is analysed.** Each project found a real data problem and handled it: a share split that looked like an 80% crash (Adani Ports), seasons missing from the source (NBA), rows with values in the wrong column (Netflix), and two datasets that turned out to be fake (sanity check).
- **Reproducible.** Every number in a README is printed by the project's notebook or script.

## Engineering

- **Tested packages, thin notebooks.** Analysis logic lives in importable modules under a project's `src/`, covered by pytest unit and data-validation tests; notebooks tell the story and call that code. (NBA so far; the other projects are being migrated.)
- **Continuous integration.** Every push runs linting (ruff), the test suite and a clean re-execution of every notebook on GitHub Actions.
- **Pinned and reproducible.** Exact dependency versions in `pyproject.toml`, datasets pinned to a Kaggle version, fixed random seeds.

## Quick start

```bash
make setup        # .venv with pinned dependencies and the project packages
make data         # download datasets not stored in the repo
make test         # unit and data-validation tests
make notebooks    # re-run every notebook from scratch
make help         # all tasks
```

Each project folder has its own README with details. Python 3.10-3.12; the R project needs R 4.x with the tidyverse, or Docker.

## License

MIT, see [LICENSE](LICENSE). Datasets keep their original Kaggle licences, linked from each project.
