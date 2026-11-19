# One-day 99% Value-at-Risk: the loss a holder should exceed on only 1 day in 100. Three methods, each
# forecasting tomorrow from data up to today only, then backtested on the days that followed.

#' Rolling one-day VaR forecasts (as a positive loss in log-return units).
#' - "historical":  empirical quantile of the last `window` returns
#' - "garch_normal": GARCH(1,1) volatility forecast x normal quantile
#' - "garch_fhs":   GARCH volatility forecast x empirical quantile of standardised residuals
#'                  (filtered historical simulation: GARCH's timing, the data's own fat tails)
#' GARCH is refitted every `refit_every` days on an expanding window, then rolled forward daily.
rolling_var <- function(returns, start_index, level = 0.99, window = 500, refit_every = 250,
                        methods = c("historical", "garch_normal", "garch_fhs")) {
  n <- length(returns)
  out <- matrix(NA_real_, nrow = n, ncol = length(methods), dimnames = list(NULL, methods))
  q <- 1 - level
  fit <- NULL
  for (t in start_index:n) {
    past <- returns[seq_len(t - 1)]
    if ("historical" %in% methods) out[t, "historical"] <- -quantile(tail(past, window), q, names = FALSE)
    if (any(c("garch_normal", "garch_fhs") %in% methods)) {
      if (is.null(fit) || (t - start_index) %% refit_every == 0) fit <- fit_garch(past)
      # Roll the fitted variance forward through any days since the last refit
      s2 <- garch_variance(past, fit$mu, fit$omega, fit$alpha, fit$beta)
      s2_next <- fit$omega + fit$alpha * (past[t - 1] - fit$mu)^2 + fit$beta * s2[t - 1]
      sigma <- sqrt(s2_next)
      if ("garch_normal" %in% methods) out[t, "garch_normal"] <- -(fit$mu + sigma * qnorm(q))
      if ("garch_fhs" %in% methods) out[t, "garch_fhs"] <- -(fit$mu + sigma * quantile(fit$std_resid, q, names = FALSE))
    }
  }
  out
}

#' Kupiec proportion-of-failures test: is the share of days with losses beyond VaR consistent with 1 - level?
kupiec_test <- function(exceed, level = 0.99) {
  n <- length(exceed); x <- sum(exceed); p <- 1 - level; phat <- x / n
  ll_null <- (n - x) * log(1 - p) + x * log(p)
  ll_alt <- if (x == 0) n * log(1) else if (x == n) n * log(1) else (n - x) * log(1 - phat) + x * log(phat)
  lr <- -2 * (ll_null - ll_alt)
  list(days = n, exceedances = x, expected = n * p, rate = phat, lr = lr, p_value = 1 - pchisq(lr, 1))
}

#' Christoffersen independence test: do exceedances cluster (an exceedance today making one tomorrow more likely)?
christoffersen_test <- function(exceed) {
  e <- as.integer(exceed)
  prev <- head(e, -1); cur <- tail(e, -1)
  n00 <- sum(prev == 0 & cur == 0); n01 <- sum(prev == 0 & cur == 1)
  n10 <- sum(prev == 1 & cur == 0); n11 <- sum(prev == 1 & cur == 1)
  pi0 <- n01 / max(n00 + n01, 1); pi1 <- n11 / max(n10 + n11, 1); pi <- (n01 + n11) / max(n00 + n01 + n10 + n11, 1)
  ll <- function(a, b, p) ifelse(a > 0, a * log(1 - p), 0) + ifelse(b > 0, b * log(p), 0)
  lr <- -2 * (ll(n00 + n10, n01 + n11, pi) - (ll(n00, n01, pi0) + ll(n10, n11, pi1)))
  list(lr = lr, p_value = 1 - pchisq(lr, 1), p_exceed_after_exceed = pi1, p_exceed_after_normal = pi0)
}

#' Backtest table for the columns of a rolling_var() matrix.
backtest_var <- function(returns, var_matrix, level = 0.99) {
  ok <- complete.cases(var_matrix)
  purrr::map_dfr(colnames(var_matrix), function(m) {
    exceed <- -returns[ok] > var_matrix[ok, m]
    k <- kupiec_test(exceed, level); ch <- christoffersen_test(exceed)
    dplyr::tibble(method = m, days = k$days, exceedances = k$exceedances, expected = k$expected,
                  exceed_rate = k$rate, kupiec_p = k$p_value, independence_p = ch$p_value,
                  avg_var = mean(var_matrix[ok, m]))
  })
}
