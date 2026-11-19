# Methodology

## 1. Two questions, two kinds of model

| | Part 1: explain | Part 2: predict |
|---|---|---|
| Question | Which factors separate winners from losers? | Who will win, before tip-off? |
| Information used | Statistics from the game itself | Only games already finished |
| Model | Logistic regression on standardised Four Factor gaps | Elo; logistic regression and gradient boosting on Elo + recent form |
| Judged by | Coefficient size and direction | Log loss, Brier score, accuracy, calibration on unseen seasons |

Mixing the two is a common mistake: a model fed in-game statistics "predicts" 95% of results, but only because it is told how the game went. Part 1 reports that figure as an explanation, never as a forecast.

## 2. Data pipeline

```
TeamStatistics.csv (Kaggle v552, one row per team per game)
   │  load_team_games      keep regular season; add date (UTC) and season label
   ▼
season window 2001-24
   │  season_completeness  drop seasons with < 900 games (source gaps, not short seasons)
   │  inconsistent_games   drop games with tied or 0-0 final scores
   │  validate_team_games  raise if: not 2 rows/game, ≠1 winner, winner scored less, made > attempted
   ▼
clean team games ──► add_four_factors (joins each row to its opponent) ──► Part 1
   │
   ├─► home_view (1 row/game) ──► run_elo ──┐
   └─► team_form (shifted rolling means) ───┴─► pregame_features ──► Part 2
```

Validation **raises** rather than warns: a broken assumption here would silently corrupt every number downstream. Known, explainable defects are removed *before* validation and listed in a report, so nothing disappears unannounced.

## 3. Leakage prevention

| Component | Guarantee | Test |
|---|---|---|
| Elo | Probability for game *n* is computed before game *n* updates ratings | Flipping game *n*'s result leaves its own probability unchanged and changes later ones |
| Rolling form | `shift(1)` before `rolling(10)`, reset each season | Wrecking a game's shooting leaves that game's form features unchanged |
| Adaptive home bonus | Season *s* uses home win rates of seasons < *s* only | First season falls back to the fixed bonus; the next uses exactly the prior rate |
| Model fitting | Fitted on training seasons only | `time_split` test: splits are disjoint and ordered |

## 4. Evaluation protocol

| Split | Seasons | Games scored* | Purpose |
|---|---|---|---|
| Train | 2001-02 to 2016-17 | 18,547 | Fit models; tune Elo K and home bonus |
| Validation | 2017-18 to 2019-20 | 3,296 | Choose fixed vs adaptive home bonus |
| Test | 2020-21, 2022-23 to 2024-25 | 4,648 | Final, one-time report |

\*Each team's first three games of a season have no form features yet; those games are dropped from **every** model and baseline so all are scored on identical games.

**Metrics**
- **Log loss:** penalises confident wrong forecasts heavily; the primary metric.
- **Brier score:** mean squared error of the probability.
- **Accuracy:** share of games where the favourite won; intuitive but blind to confidence.
- **Calibration:** predicted vs observed home-win rate by probability bin.

**Comparing models:** the log-loss difference between two models is bootstrapped over test games (1,000 resamples). An improvement counts only if the 95% interval excludes zero.

## 5. Elo design

Based on FiveThirtyEight's NBA Elo: start at 1500; winner takes `K × MOV multiplier × (result − expected)` from the loser; margin-of-victory multiplier damped for favourites; ratings regress 25% toward 1500 between seasons.

**Home bonus.** Home win rate fell from about 60% to about 55% over the period, so a fixed bonus tuned on early seasons overstates home teams later. The adaptive option converts the previous three seasons' home win rate *p* into Elo points with `400 · log10(p / (1 − p))`.

**Tuning.** K ∈ {15, 20, 25} and fixed bonus ∈ {60, 80, 100} are grid-searched by log loss on training seasons (2001-02 and 2002-03 serve as rating warm-up). Training seasons had a stable home advantage, so they can't separate fixed and adaptive designs; that choice is made on validation, where the decline begins.

## 6. Results summary

| Model (test seasons) | Accuracy | Log loss | Brier |
|---|---|---|---|
| Home team always wins | 55.4% | 0.691 | 0.249 |
| Elo, adaptive home bonus | 63.8% | 0.631 | 0.221 |
| Logistic regression (Elo + form) | 64.5% | 0.630 | 0.220 |
| Gradient boosting (Elo + form) | 64.3% | 0.635 | 0.222 |

Logistic regression's edge over Elo is not significant (log-loss gap −0.0012, 95% CI [−0.0045, +0.0022]).

## 7. Limitations and extensions

- **Missing information:** injuries, rest decisions, lineups and travel are where real forecasting gains lie; box-score form largely re-measures what Elo already knows.
- **Regular season only;** playoff dynamics (series context, rotations) differ.
- **Source gaps:** 2021-22 is absent from version 552.
- **Possible extensions:** player-availability adjustments to Elo, margin-of-victory regression instead of win/loss, and a betting-market benchmark if odds data were available.
