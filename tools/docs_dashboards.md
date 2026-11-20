# Live dashboards

Three projects have interactive dashboards: static pages built from each project's own code, so their numbers match
the case studies.

| Dashboard | What you can do |
|---|---|
| [**NBA win probability**](https://nba-win-probability-jade.vercel.app/) | Pick any two teams and get a pre-game win probability from the Elo model; team ratings and history, the shrinking home advantage, forecast quality and calibration |
| [**Netflix catalogue trends**](https://netflix-catalogue-trends.vercel.app/) | Search any of 8,807 titles for "because you watched" recommendations; catalogue trends with confidence bands, country picker, genre and theme shifts |
| [**Olist marketplace**](https://olist-marketplace-dashboard.vercel.app/) | KPIs, first vs repeat orders, late deliveries and reviews filtered by state, the lateness dose-response, RFM segments and a cohort-retention heatmap |

Each redeploys automatically when its project's dashboard folder changes. The data behind them is rebuilt with
`make dashboard`.
