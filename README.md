# Signal or Noise: Data Analysis Case Studies

Business and data analysis case studies in **Python** and **R**, each about telling a real pattern from noise. Every project starts from a question, ends with findings and a recommendation, and says what the data can't support.

| # | Project | Question | Headline finding | Tools |
|---|---|---|---|---|
| 1 | [Netflix content strategy](01-netflix-content-strategy) | What has Netflix prioritised in its catalogue since 2015? | TV's share of additions rose from 25% (2018) to 34% (2021); India's share jumped sevenfold to 18%, then fell back to 7% | Python, pandas |
| 2 | [Adani Ports stock analysis](02-adani-ports-stock-analysis) | What did holding the stock from 2007 to 2021 take, and does volume predict price? | 10.4% a year, but with an 80% crash that took 6.5 years to recover; volume spikes don't predict the next day's direction | R, tidyverse |
| 3 | [NBA winning performances](03-nba-winning-performances) | Which of the Four Factors matters most for winning? | The team that shoots better wins 81% of games; shooting carries about 47% of the weight | Python, scikit-learn |
| 4 | [Synthetic data sanity check](04-synthetic-data-sanity-check) | Can two popular Kaggle datasets support the findings drawn from them? | No: both are randomly generated (cities in the wrong states, AQI unrelated to PM2.5) | Python, SciPy |

## How these projects are built

- **A question first**, a recommendation last, and limitations stated, not hidden.
- **Data checked before it is analysed.** Each project found a real data problem and handled it: a share split that looked like an 80% crash (Adani Ports), seasons missing from the source (NBA), rows with values in the wrong column (Netflix), and two datasets that turned out to be fake (sanity check).
- **Reproducible.** Every number in a README is printed by the project's notebook or script.

## Run a project

Each project folder has its own README with setup steps. Python projects need Python 3.10+ and Jupyter; the R project needs R 4.x with the tidyverse (or Docker, see its README).

## License

MIT, see [LICENSE](LICENSE). Datasets keep their original Kaggle licences, linked from each project.
