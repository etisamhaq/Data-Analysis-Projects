# GARCH(1,1) volatility model, fitted by (quasi-)maximum likelihood with base R's optim.
#
#   r_t       = mu + e_t,        e_t = sigma_t * z_t
#   sigma_t^2 = omega + alpha * e_{t-1}^2 + beta * sigma_{t-1}^2
#
# Volatility clusters: a big move today raises tomorrow's expected variance (alpha), and that effect decays
# slowly (beta). Constraints omega > 0, alpha, beta >= 0 and alpha + beta < 1 (stationarity) are enforced by
# optimising unconstrained parameters through smooth transforms.

garch_params <- function(theta) {
  ea <- exp(theta[3]); eb <- exp(theta[4])
  list(mu = theta[1], omega = exp(theta[2]), alpha = ea / (1 + ea + eb), beta = eb / (1 + ea + eb))
}

#' Conditional variance path for given parameters; starts at the sample variance.
garch_variance <- function(r, mu, omega, alpha, beta) {
  e <- r - mu
  s2 <- numeric(length(r))
  s2[1] <- var(r)
  for (t in 2:length(r)) s2[t] <- omega + alpha * e[t - 1]^2 + beta * s2[t - 1]
  s2
}

garch_negloglik <- function(theta, r) {
  p <- garch_params(theta)
  s2 <- garch_variance(r, p$mu, p$omega, p$alpha, p$beta)
  0.5 * sum(log(2 * pi) + log(s2) + (r - p$mu)^2 / s2)
}

#' Fit GARCH(1,1) to a return series. Returns parameters, persistence, the variance path and
#' standardised residuals (used for filtered historical simulation).
fit_garch <- function(r) {
  start <- c(mean(r), log(var(r) * 0.05), log(0.08 / 0.04), log(0.88 / 0.04))   # ~ alpha 0.08, beta 0.88
  opt <- optim(start, garch_negloglik, r = r, method = "BFGS", control = list(maxit = 2000))
  if (opt$convergence != 0) warning("GARCH optimisation did not converge")
  p <- garch_params(opt$par)
  s2 <- garch_variance(r, p$mu, p$omega, p$alpha, p$beta)
  c(p, list(persistence = p$alpha + p$beta,
            half_life_days = log(0.5) / log(p$alpha + p$beta),
            long_run_vol_annual = sqrt(p$omega / (1 - p$alpha - p$beta) * TRADING_DAYS),
            variance = s2, std_resid = (r - p$mu) / sqrt(s2), loglik = -opt$value))
}

#' One-step-ahead variance forecast from the last observation of a fitted model.
garch_forecast_variance <- function(fit, last_return) {
  n <- length(fit$variance)
  fit$omega + fit$alpha * (last_return - fit$mu)^2 + fit$beta * fit$variance[n]
}

#' Simulate a GARCH(1,1) path (used to test that fit_garch recovers known parameters).
simulate_garch <- function(n, mu = 0, omega = 1e-5, alpha = 0.08, beta = 0.9, seed = 1) {
  set.seed(seed)
  r <- numeric(n); s2 <- omega / (1 - alpha - beta); e_prev <- 0
  for (t in seq_len(n)) {
    s2 <- omega + alpha * e_prev^2 + beta * s2
    e_prev <- sqrt(s2) * rnorm(1)
    r[t] <- mu + e_prev
  }
  r
}
