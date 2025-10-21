library(testthat)
library(evalvariantnowcasthub)

test_results <- test_check("evalvariantnowcasthub")

if (any(as.data.frame(test_results)$warning > 0)) {
  stop("tests failed with warnings", call. = FALSE)
}
