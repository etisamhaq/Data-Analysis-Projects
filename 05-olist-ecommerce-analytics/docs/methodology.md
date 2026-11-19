# Methodology

## 1. Warehouse design

The analysis runs on a DuckDB warehouse organised the way analytics teams organise dbt projects:

| Layer | Folder | Contents | Rule |
|---|---|---|---|
| Raw | (views) | The nine source CSVs, untouched | Never edited |
| Staging | `sql/01_staging` | One model per source: typed, renamed, cleaned | No joins across sources (except the category translation) |
| Marts | `sql/02_marts` | Business-facing tables | Built only from staging or earlier marts; run in file-name order |
| Tests | `sql/03_tests` | Queries that return rule-breaking rows | Empty result = pass; any rows fail the build |

`warehouse.build()` creates raw views, materialises every model as a table, then runs every test and raises
`DataTestFailure` naming any test that returned rows.

### Key modelling decisions

| Decision | Why |
|---|---|
| Customers identified by `customer_unique_id` | `customer_id` is issued per order, so it would make every customer look new |
| `late` compares **dates** | The promised date has no time; a delivery on the promised day is on time |
| One review per order: the most recent | 547 orders have several reviews; keeping all would weight those orders more than once |
| "Main" item = most expensive | Gives each order one category and one seller for analysis |
| Valid orders exclude `canceled` and `unavailable` | Those never became sales |
| Geocodes outside Brazil's bounding box discarded; median per zip prefix | 42 source points fall outside Brazil (20 in Europe), and each prefix has many points |
| Marts use Jan 2017 – Aug 2018 | Earlier months are sparse; Sep–Oct 2018 contain a handful of orders |

### Data tests

Uniqueness (`order_id`), not-null (customer), ranges (values ≥ 0, review 1–5, distance ≤ 4,500 km, delivery after
purchase), referential integrity (every valid order's customer exists in `dim_customers`), one review per order,
cohort retention starting at 100%, complete-months-only KPIs, and a tolerance test (payment/item mismatches ≤ 1%).

## 2. Late delivery and review scores

**Question:** how much does a late delivery lower the review, holding other differences fixed?

Late orders differ from on-time ones: they're more common in distant states, certain categories and peak months, all of
which could affect reviews on their own. OLS on delivered, reviewed orders in the window (95,560 orders):

```
review = β·late + log(order value) + freight share + items + multi-seller
         + category fixed effects + customer-state fixed effects + month fixed effects + ε
```

with heteroskedasticity-robust (HC1) standard errors. β barely moves from the raw difference (−2.02 → −1.97),
so the gap isn't explained by *which* orders run late.

**Caveat:** this is observational. Unobserved factors (for example a seller who is both slow and sells poor products)
could remain. A randomised test of delivery promises would give a causal answer.

## 3. Repeat purchases

**Equal follow-up.** A customer acquired in July 2018 has had weeks to return; one acquired in January 2017, over a
year. Comparing them would confuse "hasn't returned yet" with "won't return". So each customer's outcome is
**"ordered again within 180 days"**, and only first orders with a full 180 days of data afterwards are used (56,247
customers).

**Late first order → repeat:** logistic regression of repeat on late, log order value, customer state (states with
fewer than 500 customers pooled) and purchase month, fitted with BFGS; the function raises if it doesn't converge.
Result: odds ratio 0.90 (95% CI 0.74–1.10), not significant.

> An earlier draft reported a significant effect (OR 0.78, p = 0.009). It came from a model that hadn't converged
> and from unequal follow-up windows. Both are fixed; the convergence check is now enforced in code.

**Propensity model:** can first-order data identify future repeat buyers?
- Features: order value, late, review score, delivery days, items, instalments, freight, state and category indicators.
- Time split: trained on customers acquired before September 2017, tested on those acquired after.
- Metrics: AUC (ranking quality), average precision (suited to a 3% base rate) and the repeat rate in the top-scored decile.
- Result: AUC 0.589; top decile 5.4% vs 3.0% base rate.

**Why no BG/NBD or Pareto/NBD lifetime-value model?** Those models learn from repeat-purchase patterns; with 97% of
customers buying once there is almost nothing to fit, and expected lifetime value collapses to roughly the first order's
value. Saying so is more useful than presenting an elaborate model that can't be validated.

## 4. RFM segmentation

Recency (days since last order, as of the day after the last order in the data) and monetary value (total spend) are
scored into quintiles with `ntile(5)`. Frequency enters only as "1 order" vs "2+", because it barely varies. Six named
segments combine the three. Segments describe the customer base; they were not validated against a later outcome.

## 5. Limitations

- Observational data throughout; effects are controlled associations, not experiments.
- Reviews are left by most but not all customers; non-reviewers are excluded from review analyses.
- Freight is included in order value (it's part of what the customer pays).
- The data ends in August 2018.
