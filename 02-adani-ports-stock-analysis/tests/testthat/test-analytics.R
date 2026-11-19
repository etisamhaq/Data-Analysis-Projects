library(testthat)
library(dplyr)

make_prices <- function(n = 300, split_at = NULL, ratio = 5, seed = 1) {
  set.seed(seed)
  close <- 100 * exp(cumsum(rnorm(n, 0.0005, 0.015)))
  if (!is.null(split_at)) close[split_at:n] <- close[split_at:n] / ratio
  tibble(Date = as.Date("2015-01-01") + seq_len(n) - 1, Close = close,
         `Prev Close` = c(close[1], head(close, -1)), Open = close, High = close * 1.01, Low = close * 0.99,
         Volume = round(runif(n, 1e5, 2e5)))
}

# ---------------------------------------------------------------- data

test_that("a 5:1 split is detected and adjusted away", {
  p <- make_prices(split_at = 150)
  splits <- detect_splits(p)
  expect_equal(nrow(splits), 1)
  expect_equal(splits$ratio, 5)
  adj <- adjust_for_splits(p, splits)
  expect_lt(max(abs(diff(log(adj$Close)))), 0.1)
  expect_equal(adj$Volume[1], p$Volume[1] * 5)
  expect_silent(validate_prices(adj))
})

test_that("validation stops on unadjusted splits, duplicates and unsorted dates", {
  expect_error(validate_prices(make_prices(split_at = 150)), "unadjusted split")
  p <- make_prices()
  expect_error(validate_prices(bind_rows(p, p[1, ])), "Duplicate")
  expect_error(validate_prices(p[c(2, 1, 3:nrow(p)), ]), "not in order")
})

# ---------------------------------------------------------------- performance

test_that("CAGR, drawdown and recovery are computed correctly", {
  dates <- as.Date(c("2020-01-01", "2021-01-01"))
  expect_equal(cagr(c(100, 200), dates), 2^(365.25 / 366) - 1, tolerance = 1e-9)
  prices <- c(100, 120, 60, 90, 130)
  expect_equal(drawdown(prices), c(0, 0, -0.5, -0.25, 0))
  ep <- drawdown_episode(as.Date("2020-01-01") + 0:4, prices, "2020-01-01", "2020-01-05")
  expect_equal(ep$fall, -0.5)
  expect_equal(ep$peak, as.Date("2020-01-02"))
  expect_equal(ep$recovered, as.Date("2020-01-05"))
})

# ---------------------------------------------------------------- market model

test_that("market_model recovers a known beta", {
  set.seed(2)
  market <- rnorm(3000, 0, 0.01)
  r <- tibble(market = market, stock = 1.4 * market + rnorm(3000, 0, 0.01))
  m <- market_model(r)
  expect_equal(m$beta, 1.4, tolerance = 0.05)
  expect_true(m$beta_low < 1.4 && m$beta_high > 1.4)
  rb <- rolling_beta(r, window = 250)
  expect_true(all(is.na(rb$beta[1:249])))
  expect_equal(mean(rb$beta, na.rm = TRUE), 1.4, tolerance = 0.05)
})

# ---------------------------------------------------------------- GARCH

test_that("fit_garch recovers the parameters of a simulated GARCH(1,1)", {
  r <- simulate_garch(6000, omega = 1e-5, alpha = 0.08, beta = 0.9, seed = 3)
  fit <- fit_garch(r)
  expect_equal(fit$alpha, 0.08, tolerance = 0.25)   # relative tolerance
  expect_equal(fit$beta, 0.9, tolerance = 0.05)
  expect_lt(fit$persistence, 1)
  expect_equal(sd(fit$std_resid), 1, tolerance = 0.05)
})

# ---------------------------------------------------------------- VaR backtests

test_that("Kupiec accepts the right exceedance rate and rejects a wrong one", {
  expect_gt(kupiec_test(c(rep(TRUE, 10), rep(FALSE, 990)), 0.99)$p_value, 0.5)
  expect_lt(kupiec_test(c(rep(TRUE, 40), rep(FALSE, 960)), 0.99)$p_value, 0.001)
  expect_equal(kupiec_test(rep(FALSE, 500), 0.99)$exceedances, 0)
})

test_that("Christoffersen detects clustered exceedances", {
  spread <- rep(FALSE, 1000); spread[seq(50, 1000, by = 100)] <- TRUE
  clustered <- rep(FALSE, 1000); clustered[500:509] <- TRUE
  expect_gt(christoffersen_test(spread)$p_value, 0.1)
  expect_lt(christoffersen_test(clustered)$p_value, 0.001)
})

test_that("rolling VaR only uses past returns", {
  r <- simulate_garch(900, seed = 4)
  v1 <- rolling_var(r, start_index = 600, methods = c("historical", "garch_fhs"))
  r2 <- r; r2[800] <- -0.5                       # a crash on day 800...
  v2 <- rolling_var(r2, start_index = 600, methods = c("historical", "garch_fhs"))
  expect_equal(v1[600:800, ], v2[600:800, ])     # ...can't change forecasts made up to and including that day
  expect_false(isTRUE(all.equal(v1[801, ], v2[801, ])))
})

# ---------------------------------------------------------------- Granger

test_that("granger_test finds real lead-lag effects and not spurious ones", {
  set.seed(5)
  x <- rnorm(3000)
  y_caused <- 0.3 * dplyr::lag(x, 1, default = 0) + rnorm(3000)
  y_independent <- rnorm(3000)
  expect_lt(granger_test(y_caused, x, 3)$p_value, 1e-6)
  expect_gt(granger_test(y_independent, x, 3)$p_value, 0.05)
})

test_that("abnormal volume uses only previous days", {
  v <- c(rep(100, 25), 1000, rep(100, 5))
  av <- abnormal_volume(v, window = 20)
  expect_equal(av[26], log(1000) - log(100))
  expect_true(all(is.na(av[1:20])))
})
