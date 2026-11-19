# Loading, cleaning and validating price data.
#
# Validation stops with an error rather than warning: a broken assumption here (unsorted dates, a missed split)
# would silently corrupt every return, beta and VaR number downstream.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(lubridate)
})

#' Find share splits: one-day price ratios that are a whole number (2:1, 5:1, 10:1...) and far outside normal moves.
#' NSE's "Prev Close" is not adjusted, so a split shows up as Prev Close / Open close to an integer >= 2.
detect_splits <- function(prices, min_ratio = 1.8) {
  ratio <- prices$`Prev Close` / prices$Open
  candidates <- which(ratio >= min_ratio)
  tibble(date = prices$Date[candidates], prev_close = prices$`Prev Close`[candidates],
         open = prices$Open[candidates], ratio = round(ratio[candidates]))
}

#' Divide prices before each split by its ratio (and multiply volumes), so the series is comparable over time.
adjust_for_splits <- function(prices, splits) {
  factor <- rep(1, nrow(prices))
  for (i in seq_len(nrow(splits))) {
    before <- prices$Date < splits$date[i]
    factor[before] <- factor[before] * splits$ratio[i]
  }
  prices %>%
    mutate(adj_factor = factor,
           across(any_of(c("Prev Close", "Open", "High", "Low", "Last", "Close", "VWAP")), ~ .x / adj_factor),
           across(any_of(c("Volume", "Deliverable Volume")), ~ .x * adj_factor))
}

#' Stop if the series breaks an assumption the analysis depends on.
validate_prices <- function(prices, max_daily_move = 0.5) {
  if (anyDuplicated(prices$Date)) stop("Duplicate trading dates")
  if (is.unsorted(prices$Date)) stop("Dates are not in order")
  if (any(prices$Close <= 0, na.rm = TRUE) || anyNA(prices$Close)) stop("Missing or non-positive closing prices")
  if (any(prices$High < prices$Low, na.rm = TRUE)) stop("High below Low on some days")
  moves <- abs(diff(log(prices$Close)))
  if (any(moves > log(1 + max_daily_move))) {
    stop(sprintf("A one-day move above %.0f%% remains, likely an unadjusted split", max_daily_move * 100))
  }
  invisible(TRUE)
}

#' Read a price CSV, treating "-" as missing (the index file uses it once, for an unknown dividend yield),
#' and stop on any value that still fails to parse rather than letting readr turn it into NA with a warning.
read_price_file <- function(path) {
  d <- read_csv(path, show_col_types = FALSE, guess_max = Inf, na = c("", "NA", "-"))
  if (nrow(readr::problems(d)) > 0) stop("Parsing problems in ", path, ": ", nrow(readr::problems(d)), " values")
  d %>% mutate(Date = ymd(Date)) %>% arrange(Date)
}

#' Stock prices: read, detect and correct splits, validate. Returns the clean series and what was changed.
load_stock <- function(path) {
  raw <- read_price_file(path)
  splits <- detect_splits(raw)
  clean <- adjust_for_splits(raw, splits)
  validate_prices(clean)
  list(prices = clean, splits = splits,
       symbols = raw %>% group_by(Symbol) %>% summarise(first = min(Date), last = max(Date), .groups = "drop"),
       missing_trades = sum(is.na(raw$Trades)))
}

#' Index levels (no splits), validated the same way.
load_index <- function(path) {
  idx <- read_price_file(path)
  validate_prices(idx)
  idx
}

#' Daily log returns for the stock and the index on the days both traded.
aligned_returns <- function(stock, index) {
  s <- stock %>% transmute(Date, stock = log(Close / lag(Close)))
  m <- index %>% transmute(Date, market = log(Close / lag(Close)))
  inner_join(s, m, by = "Date") %>% filter(!is.na(stock), !is.na(market))
}
