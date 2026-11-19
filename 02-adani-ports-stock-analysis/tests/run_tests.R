# Run from the project folder:  Rscript tests/run_tests.R
suppressPackageStartupMessages(library(testthat))
for (f in list.files("R", full.names = TRUE)) source(f)
results <- test_dir("tests/testthat", reporter = "summary", stop_on_failure = TRUE)
