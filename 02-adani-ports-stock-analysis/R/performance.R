# Return, risk and drawdown measures.

TRADING_DAYS <- 252

#' Compound annual growth rate between the first and last price.
cagr <- function(prices, dates) {
  years <- as.numeric(max(dates) - min(dates)) / 365.25
  (dplyr::last(prices) / dplyr::first(prices))^(1 / years) - 1
}

annualised_volatility <- function(log_returns) sd(log_returns, na.rm = TRUE) * sqrt(TRADING_DAYS)

#' Annualised mean return over volatility. With a zero risk-free rate this is a return-to-risk ratio,
#' useful for comparing two assets over the same period, not an absolute Sharpe ratio.
return_to_risk <- function(log_returns) {
  mean(log_returns, na.rm = TRUE) * TRADING_DAYS / annualised_volatility(log_returns)
}

#' Like return_to_risk, but only penalising downside volatility (Sortino-style, zero target).
downside_return_to_risk <- function(log_returns) {
  downside <- sqrt(mean(pmin(log_returns, 0)^2, na.rm = TRUE)) * sqrt(TRADING_DAYS)
  mean(log_returns, na.rm = TRUE) * TRADING_DAYS / downside
}

#' Fall from the running peak, as a fraction (0 = at a new high, -0.8 = 80% below the peak).
drawdown <- function(prices) prices / cummax(prices) - 1

max_drawdown <- function(prices) min(drawdown(prices))

#' Worst fall within a date window: peak date, trough date, depth and the date the old peak was regained.
drawdown_episode <- function(dates, prices, from, to) {
  dd <- drawdown(prices)
  peak_level <- cummax(prices)
  window <- which(dates >= as.Date(from) & dates <= as.Date(to))
  # Positions use i_ names: inside tibble() a column named `trough` would shadow a variable of the same name
  i_trough <- window[which.min(dd[window])]
  i_peak <- max(which(prices[seq_len(i_trough)] == peak_level[i_trough]))
  after <- which(seq_along(prices) > i_trough & prices >= peak_level[i_trough])
  dplyr::tibble(peak = dates[i_peak], trough = dates[i_trough], fall = dd[i_trough],
                recovered = if (length(after)) dates[min(after)] else as.Date(NA),
                years_to_recover = if (length(after)) as.numeric(dates[min(after)] - dates[i_peak]) / 365.25 else NA_real_)
}
