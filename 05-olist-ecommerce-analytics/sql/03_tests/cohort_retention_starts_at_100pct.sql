select cohort, retention from mart_cohort_retention where months_since = 0 and abs(retention - 1) > 1e-9
