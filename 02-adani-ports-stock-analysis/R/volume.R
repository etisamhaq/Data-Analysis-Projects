# Does unusual trading volume carry information about the next day's move?

#' Log volume minus its mean over the previous `window` days (information available at the day's close).
abnormal_volume <- function(volume, window = 20) {
  lv <- log(volume)
  base <- stats::filter(lv, rep(1 / window, window), sides = 1)
  as.numeric(lv - dplyr::lag(as.numeric(base)))
}

#' Granger-causality F-test: do `lags` days of x improve a regression of y on its own `lags` lags?
#' Nested OLS models compared with an F-test (equivalent to lmtest::grangertest).
granger_test <- function(y, x, lags = 5) {
  lagmat <- function(v, k) sapply(seq_len(k), function(i) dplyr::lag(v, i))
  d <- data.frame(y = y, ylag = lagmat(y, lags), xlag = lagmat(x, lags))
  d <- d[complete.cases(d), ]
  restricted <- lm(y ~ ., data = d[, c("y", grep("^ylag", names(d), value = TRUE))])
  full <- lm(y ~ ., data = d)
  a <- anova(restricted, full)
  list(f = a$F[2], p_value = a$`Pr(>F)`[2], lags = lags, days = nrow(d),
       # Effect size: how much extra variance the volume lags explain (statistical significance alone can
       # come from a tiny effect on 3,000+ days)
       r2_added = summary(full)$r.squared - summary(restricted)$r.squared)
}
