# Adani Ports (NSE: ADANIPORTS) 2007-2021: what did holding the stock actually feel like?
#
# Data: daily NSE prices for one stock, listed as MUNDRAPORT until 16 Jan 2012 and ADANIPORTS
# after (Kaggle "NIFTY-50 Stock Market Data", Rohan Rao). This is a single company, not the
# NIFTY 50 index.
#
# Run from this folder:  Rscript adani_ports_analysis.R
# Writes charts to figures/ and prints every number quoted in the README.

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(scales)
})

dir.create("figures", showWarnings = FALSE)
theme_set(theme_minimal(base_size = 11) +
            theme(plot.title = element_text(face = "bold"), panel.grid.minor = element_blank()))
BLUE <- "#2a78d6"; ORANGE <- "#eb6834"; GREY <- "#9a9993"

raw <- read_csv("ADANIPORTS.csv", show_col_types = FALSE) %>%
  mutate(Date = ymd(Date)) %>%
  arrange(Date)

# ---- 1. Data checks ---------------------------------------------------------------------
# `Trades` was only reported from June 2011. Dropping rows with any NA (na.omit) would
# silently delete the first 3.5 years, including the 2008 crash, so only check, don't drop.
cat(sprintf("Rows: %d, %s to %s\n", nrow(raw), min(raw$Date), max(raw$Date)))
cat(sprintf("Rows with no Trades value: %d (all before %s)\n",
            sum(is.na(raw$Trades)), min(raw$Date[!is.na(raw$Trades)])))

# A one-day fall from 837 to 165.6 on 23 Sep 2010 is a 5-for-1 share split (face value
# Rs 10 -> Rs 2), not a crash. Prices before it are divided by 5, volumes multiplied by 5.
split_day <- raw %>% filter(Close / `Prev Close` < 0.3)
split_ratio <- round(split_day$`Prev Close` / split_day$Open)
cat(sprintf("Split on %s: prev close %.1f, open %.1f -> ratio %d:1\n",
            split_day$Date, split_day$`Prev Close`, split_day$Open, split_ratio))

px <- raw %>%
  mutate(factor = if_else(Date < split_day$Date, split_ratio, 1),
         across(c(Open, High, Low, Close), ~ .x / factor),
         Volume = Volume * factor,
         ret = log(Close / lag(Close)))

# ---- 2. Long-run return -----------------------------------------------------------------
years_held <- as.numeric(max(px$Date) - min(px$Date)) / 365.25
cagr <- (last(px$Close) / first(px$Close))^(1 / years_held) - 1
ann_vol <- sd(px$ret, na.rm = TRUE) * sqrt(252)
cat(sprintf("Price return %s to %s: %.1f%% a year over %.1f years (dividends not included)\n",
            min(px$Date), max(px$Date), cagr * 100, years_held))
cat(sprintf("Annualised volatility of daily returns: %.1f%%\n", ann_vol * 100))

yearly <- px %>%
  group_by(year = year(Date)) %>%
  summarise(return = last(Close) / first(Close) - 1,
            volatility = sd(ret, na.rm = TRUE) * sqrt(252), .groups = "drop")
print(yearly %>% mutate(across(c(return, volatility), ~ percent(.x, accuracy = 1))), n = Inf)

p1 <- ggplot(px, aes(Date, Close)) +
  geom_line(colour = BLUE, linewidth = 0.5) +
  scale_y_log10(labels = label_number(prefix = "Rs ")) +
  labs(title = "Adani Ports share price, split-adjusted (log scale)",
       subtitle = sprintf("%.1f%% a year from listing to April 2021, before dividends", cagr * 100),
       x = NULL, y = NULL, caption = "Source: NSE via Kaggle")
ggsave("figures/01_price_log.png", p1, width = 9, height = 4.5, dpi = 150, bg = "white")

# ---- 3. Drawdowns: how far below its previous peak was the stock? ----------------------
px <- px %>% mutate(peak = cummax(Close), drawdown = Close / peak - 1)

episode <- function(from, to) {
  w <- px %>% filter(Date >= ymd(from), Date <= ymd(to))
  low <- w %>% slice_min(drawdown, n = 1, with_ties = FALSE)
  peak_date <- px %>% filter(Date <= low$Date, Close == low$peak) %>% slice_tail(n = 1) %>% pull(Date)
  recovered <- px %>% filter(Date > low$Date, Close >= low$peak) %>% slice_head(n = 1) %>% pull(Date)
  tibble(episode = paste(from, "to", to), peak = peak_date, trough = low$Date,
         fall = percent(low$drawdown, accuracy = 1),
         recovered = if (length(recovered)) as.character(recovered) else "not by Apr 2021")
}
episodes <- bind_rows(episode("2008-01-01", "2009-12-31"),
                      episode("2015-01-01", "2016-12-31"),
                      episode("2020-01-01", "2020-12-31"))
print(episodes)

p2 <- ggplot(px, aes(Date, drawdown)) +
  geom_area(fill = ORANGE, alpha = 0.85) +
  scale_y_continuous(labels = percent) +
  labs(title = "Drawdown: distance below the previous peak",
       subtitle = "Three falls of more than 50%: 2008, 2015-16 and the 2020 COVID crash",
       x = NULL, y = NULL)
ggsave("figures/02_drawdowns.png", p2, width = 9, height = 4, dpi = 150, bg = "white")

p3 <- ggplot(yearly, aes(factor(year), return, fill = return > 0)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = percent(return, accuracy = 1), vjust = if_else(return > 0, -0.4, 1.3)), size = 3) +
  scale_fill_manual(values = c(`TRUE` = BLUE, `FALSE` = ORANGE), guide = "none") +
  scale_y_continuous(labels = percent, expand = expansion(mult = 0.12)) +
  labs(title = "Calendar-year price return",
       subtitle = "2007 starts at listing (27 Nov); 2021 runs to 30 April", x = NULL, y = NULL)
ggsave("figures/03_yearly_returns.png", p3, width = 9, height = 4.5, dpi = 150, bg = "white")

# ---- 4. Does unusual volume predict the next day's move? -------------------------------
# Abnormal volume = log volume minus its average over the previous 20 trading days.
px <- px %>%
  mutate(log_vol = log(Volume),
         base_vol = lag(as.numeric(stats::filter(log_vol, rep(1 / 20, 20), sides = 1))),
         abnormal_vol = log_vol - base_vol,
         next_ret = lead(ret))

same_day <- cor(px$abnormal_vol, abs(px$ret), use = "complete.obs")
next_day <- cor(px$abnormal_vol, px$next_ret, use = "complete.obs")
spikes <- px %>% filter(abnormal_vol >= quantile(abnormal_vol, 0.95, na.rm = TRUE), !is.na(next_ret))
up_after_spike <- mean(spikes$next_ret > 0)
up_all <- mean(px$next_ret > 0, na.rm = TRUE)
cat(sprintf("Correlation of abnormal volume with same-day size of move |return|: %.2f\n", same_day))
cat(sprintf("Correlation of abnormal volume with next-day return: %.3f\n", next_day))
cat(sprintf("Next day up after the top 5%% volume days: %.1f%% (n = %d) vs %.1f%% on all days\n",
            up_after_spike * 100, nrow(spikes), up_all * 100))

p4 <- px %>%
  filter(!is.na(abnormal_vol), !is.na(next_ret)) %>%
  mutate(volume_group = cut(abnormal_vol, quantile(abnormal_vol, 0:5 / 5), include.lowest = TRUE,
                            labels = c("Lowest 20%", "2", "3", "4", "Highest 20%"))) %>%
  group_by(volume_group) %>%
  summarise(`Same day: average size of move` = mean(abs(ret), na.rm = TRUE),
            `Next day: share of up days - 50%` = mean(next_ret > 0) - 0.5, .groups = "drop") %>%
  pivot_longer(-volume_group) %>%
  mutate(name = factor(name, levels = c("Same day: average size of move", "Next day: share of up days - 50%"))) %>%
  ggplot(aes(volume_group, value)) +
  geom_col(fill = BLUE, width = 0.65) +
  facet_wrap(~ name, scales = "free_y") +
  scale_y_continuous(labels = percent) +
  labs(title = "Unusual volume comes with big moves, but doesn't predict tomorrow's direction",
       subtitle = "Next-day bars are within +/-1.5 points of a coin flip, on about 660 days per group: noise",
       x = "Volume relative to the previous 20 days (quintiles)", y = NULL)
ggsave("figures/04_volume_vs_moves.png", p4, width = 10, height = 4.2, dpi = 150, bg = "white")

cat("Charts written to figures/\n")
