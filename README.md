# Signal or Noise: Data Analysis Case Studies

Business and data analysis case studies in **Python** and **R**, each about telling a real pattern from noise. Every project starts from a question, ends with findings and a recommendation, and says what the data can't support.

| # | Project | Question | Headline finding | Tools |
|---|---|---|---|---|
| 1 | [Netflix content strategy](01-netflix-content-strategy) | What has Netflix prioritised in its catalogue since 2015? | TV's share of additions rose from 25% (2018) to 34% (2021); India's share jumped sevenfold to 18%, then fell back to 7% | Python, pandas |
| 2 | [Adani Ports stock analysis](02-adani-ports-stock-analysis) | How did the stock compare with the market, how should its daily risk be measured, and does volume predict price? | Beat the Nifty 50 on return (10.4% vs 7.3% a year) but lost on risk-adjusted return; 73% of its risk is company-specific; GARCH-filtered VaR passes backtests that normal VaR fails | R, GARCH, VaR backtesting |
| 3 | [NBA winning performances](03-nba-winning-performances) | What decides games, and can they be predicted before tip-off? | The better-shooting team wins 81% of games; pre-game, a calibrated Elo rating calls 64% and recent-form stats add nothing reliable | Python, scikit-learn, Elo |
| 4 | [Synthetic data sanity check](04-synthetic-data-sanity-check) | Can two popular Kaggle datasets support the findings drawn from them? | No: both are randomly generated. Packaged as `datasanity`, a CLI that flags both and passes all three real datasets | Python, SciPy, CLI |

## How these projects are built

- **A question first**, a recommendation last, and limitations stated, not hidden.
- **Data checked before it is analysed.** Each project found a real data problem and handled it: a share split that looked like an 80% crash (Adani Ports), seasons missing from the source (NBA), rows with values in the wrong column (Netflix), and two datasets that turned out to be fake (sanity check).
- **Reproducible.** Every number in a README is printed by the project's notebook or script.

## Engineering

- **Tested packages, thin notebooks.** Analysis logic lives in importable modules under a project's `src/`, covered by pytest unit and data-validation tests; notebooks tell the story and call that code. (The Netflix project is next to be migrated.) The R project follows the same pattern with `R/` functions and testthat tests.
- **Continuous integration.** Every push runs linting (ruff), the Python tests, a clean re-execution of every notebook, the R tests, and the R analysis, failing if its regenerated results differ from the committed ones.
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
