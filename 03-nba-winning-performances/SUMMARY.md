# Executive summary: what wins NBA games

**Decision: coaching time should go first to shot quality, on offence and defence, then to ball security; for forecasting, use a simple Elo rating with an adaptive home-court bonus.**

## Why

- **Shooting decides most games.** The team with the better effective field-goal percentage won 81.0% of 27,597 games (2001–02 to 2024–25). Being better on turnovers, offensive rebounds or free throws alone wins only 56–60%.
- **Relative weights:** shooting 47%, turnovers 22%, offensive rebounding 17%, free throws 14%, close to the classic 40/25/20/15. The 3-point era barely changed them.
- **About 64% of games can be called before tip-off** (Elo 64.0% vs 55.5% for "home team always wins"), the realistic ceiling for this kind of data.
- **Home advantage has shrunk** from about 60% home wins (2001–11) to 55% (2019 on). A fixed home bonus over-predicts home wins (59.5% predicted vs 55.5% actual); re-estimating it from recent seasons fixes that (55.1%).
- **Extra statistics add nothing reliable beyond Elo:** recent-form models improve log loss by just 0.0010 (95% CI −0.0046 to +0.0025).

## Actions

1. Prioritise shot selection and shot defence in practice; ball security second.
2. Use the adaptive-home Elo model for forecasts; it is as accurate as more complex models and fully explainable.
3. To beat it, add information Elo lacks (injuries, lineups, travel), not more box-score statistics.

## Confidence

- **High** on the Four Factors weights (27,597 games) and on the forecasting comparison (judged on later seasons never used in training).
- Two seasons are incomplete in the source data and are excluded; results are for the regular season only.

**Details:** [README](README.md) · [Dashboard](https://nba-win-probability-jade.vercel.app/) · [Methodology](docs/methodology.md)
