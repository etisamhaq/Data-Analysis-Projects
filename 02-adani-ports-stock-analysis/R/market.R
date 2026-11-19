# How the stock moves with the market (CAPM-style regression on daily log returns).

#' Beta, correlation, R-squared (share of daily variance explained by the market) and annualised alpha.
#' Alpha uses a zero risk-free rate, so it is the return not explained by market moves, not a risk-adjusted
#' excess return.
market_model <- function(returns) {
  fit <- lm(stock ~ market, data = returns)
  ci <- confint(fit)["market", ]
  dplyr::tibble(
    beta = unname(coef(fit)["market"]), beta_low = ci[1], beta_high = ci[2],
    alpha_annual = unname(coef(fit)["(Intercept)"]) * TRADING_DAYS,
    correlation = cor(returns$stock, returns$market),
    r_squared = summary(fit)$r.squared,
    days = nrow(returns)
  )
}

#' Beta over a trailing window (default one trading year), recomputed every day.
rolling_beta <- function(returns, window = 252) {
  n <- nrow(returns)
  beta <- rep(NA_real_, n)
  for (i in seq(window, n)) {
    w <- (i - window + 1):i
    beta[i] <- stats::cov(returns$stock[w], returns$market[w]) / stats::var(returns$market[w])
  }
  dplyr::mutate(returns, beta = beta)
}
