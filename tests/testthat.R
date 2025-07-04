pkg_name <- "tern.mmrm"
library(testthat)
is_on_ci <- isTRUE(as.logical(Sys.getenv("CI")))
if (is_on_ci) {
  reporter <- MultiReporter$new(list(
    CheckReporter$new()
  ))
  test_results <- test_check(pkg_name, reporter = reporter)
  saveRDS(test_results, "unit_testing_results.rds")
} else {
  reporter <- ParallelProgressReporter$new()
  test_check(pkg_name, reporter = reporter)
}
