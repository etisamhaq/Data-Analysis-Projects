# Executive summary: Adani Ports risk and return

**Decision: size any Adani Ports position for its drawdowns, not its average return; measure its daily risk with GARCH and filtered historical simulation; don't trade on volume spikes.**

## Why

- **Higher return, worse risk-adjusted return.** 10.4% a year vs 7.3% for the Nifty 50 (Nov 2007 – Apr 2021, price only), but with 1.9× the volatility (42.7% vs 22.4%), so return per unit of risk was lower (0.24 vs 0.32).
- **The falls are severe and slow to recover:** −80% in 2008, which took 6.5 years to recover; two more falls of over 50% (2015–16 and the 2020 COVID crash).
- **Most of the risk is company-specific.** Beta is about 0.99, but the market explains only 27% of daily moves, so index hedges remove only about a quarter of the risk.
- **The right risk model matters.** Over 2,311 days of 99% one-day Value-at-Risk, normal-distribution VaR breached 36 times against 23 expected (it fails); GARCH with filtered historical simulation breached 26 times (it passes), with a smaller average buffer than plain historical VaR (5.99% vs 6.48%).
- **Volume carries no tradable signal:** only 1 of 8 tests was significant, none after a multiple-testing correction, and volume never explained more than 0.52% of next-day moves.

## Actions

1. Set position limits from drawdown tolerance (an 80% fall is in the record).
2. Use GARCH with filtered historical simulation for daily risk limits; retire normal-distribution VaR for this stock.
3. Treat volume spikes as a volatility warning, not a buy or sell signal.

## Confidence

- **High** on the risk measurements and the VaR backtests (long history, formal tests).
- Returns exclude dividends; ratios use a zero risk-free rate, so they compare assets rather than measure absolute performance.
- Data ends April 2021, before the stock's largest later event (January 2023).

**Details:** [README](README.md) · [Results](results/summary.md) · [Methodology](docs/methodology.md)
