# Methodology

## 1. Data preparation

| Step | Method | Why |
|---|---|---|
| Parse | `read_csv(na = c("", "NA", "-"))`, stop on any remaining parsing problem | readr otherwise warns and replaces unparseable values with NA; the index file uses "-" once for an unknown dividend yield |
| Splits | Flag days where Prev Close / Open ≥ 1.8; round to the ratio; divide earlier prices and multiply earlier volumes by it | NSE's Prev Close is unadjusted, so a split appears as a near-integer price ratio |
| Validate | Stop on duplicate or unsorted dates, non-positive prices, High < Low, or any remaining one-day move > 50% | A leftover split or bad row would corrupt every return-based measure |
| Align | Inner-join stock and index log returns by date | Beta and correlation need the same days for both |

## 2. Return and risk measures

- **Annual return:** compound annual growth rate of the price (dividends excluded, for both series).
- **Volatility:** standard deviation of daily log returns × √252.
- **Return / risk:** annualised mean log return ÷ volatility with a zero risk-free rate. This is a *comparison* between two assets over the same period, not an absolute Sharpe ratio; Indian risk-free rates (6–8%) would lower both.
- **Downside return / risk:** the same, dividing by downside deviation only (Sortino-style, zero target).
- **Drawdown:** price ÷ running maximum − 1. Recovery = first day the old peak is regained.

## 3. Market model

OLS of daily stock log returns on Nifty 50 log returns: `stock = α + β · market + ε`.

- **β** measures sensitivity to market moves; the 95% interval comes from the regression.
- **R²** is the share of daily variance the market explains; 1 − R² is company-specific risk, which index hedges can't remove.
- **Rolling β** uses a trailing 252-day window, recomputed daily.
- **α** is reported in `results/market_model.csv` but not interpreted, because it uses a zero risk-free rate.

## 4. GARCH(1,1)

```
r_t = μ + e_t,   e_t = σ_t z_t,   σ²_t = ω + α e²_{t−1} + β σ²_{t−1}
```

Fitted by Gaussian quasi-maximum likelihood with `optim` (BFGS). Constraints are enforced by reparameterising:
ω = exp(θ₂), α = e^θ₃ / (1 + e^θ₃ + e^θ₄), β = e^θ₄ / (1 + e^θ₃ + e^θ₄), which guarantees α, β ≥ 0 and α + β < 1.
Implemented from scratch because `rugarch` is not available in the pinned R image. A unit test simulates 6,000
days with known parameters (α = 0.08, β = 0.90) and checks the fit recovers them.

**Derived quantities:** persistence α + β; half-life of a shock log(0.5) / log(α + β); long-run volatility √(ω / (1 − α − β) · 252).

## 5. Value-at-Risk

**Target:** one-day 99% VaR, the loss exceeded on 1 day in 100.

| Method | Forecast for day t |
|---|---|
| Historical | 1st percentile of the previous 500 returns |
| GARCH, normal | μ + σ_t · z₀.₀₁ with σ_t from GARCH |
| GARCH, filtered historical simulation | μ + σ_t · (1st percentile of the model's standardised residuals) |

**No look-ahead.** Each forecast uses returns up to day t − 1. GARCH is refitted every 250 days on an expanding window and the variance rolled forward daily in between. A unit test inserts a crash on day 800 and checks that forecasts up to and including day 800 are unchanged.

**Backtest:** 2 Jan 2012 to 30 Apr 2021, 2,311 days.
- **Kupiec proportion-of-failures test:** is the breach rate consistent with 1%? (likelihood ratio, χ²(1))
- **Christoffersen independence test:** do breaches cluster, i.e. is a breach more likely the day after a breach? (Markov likelihood ratio, χ²(1))

A method needs p > 0.05 on both. Among methods that pass, lower average VaR means less capital tied up for the same protection.

## 6. Volume and next-day moves

**Abnormal volume:** log volume minus its mean over the previous 20 days (known at the close).

**Granger causality:** nested OLS regressions compared with an F-test, so that lagged volume must add information beyond the series' own lags:

```
restricted:  y_t = c + Σ a_i y_{t−i}
full:        y_t = c + Σ a_i y_{t−i} + Σ b_i vol_{t−i}        (i = 1..k)
```

Run for the next-day **return** (direction) and **absolute return** (size), at k = 1, 3, 5 and 10, which makes eight tests.
Reported alongside each p-value is the **variance added** (R² full − R² restricted), because with 3,300 days a
statistically significant effect can still be economically meaningless.

**Interpretation rule:** with eight tests, about 0.4 false positives at p < 0.05 are expected by chance alone, so
the Bonferroni threshold is 0.05 / 8 = 0.0063. One test at p = 0.033 (5 lags, direction), not replicated at other
lag lengths or in either half of the sample, is treated as noise.

## 7. Limitations

- **Price returns only.** Dividends are excluded for both the stock and the index, which slightly understates both.
- **Zero risk-free rate** in return/risk ratios and α: suitable for comparison, not for absolute performance claims.
- **Data ends April 2021,** before the January 2023 short-seller report on the Adani group, the stock's largest later event.
- **Gaussian likelihood for GARCH.** Consistent for the volatility path, but the normal VaR variant's failure shows the tails are fatter than normal; a Student-t GARCH is a natural extension.
