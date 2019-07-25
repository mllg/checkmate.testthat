context("expectations")

expect_expectation_successful = function(expr, info = NULL, label = NULL) {
  res = tryCatch(expr, expectation = function(e) e)
  expect_is(res, "expectation_success", info = info, label = label)
}

expect_expectation_failed = function(expr, pattern = NULL, info = NULL, label = NULL) {
  x = tryCatch(expr, expectation = function(e) e)
  expect_is(x, "expectation_failure", info = info, label = label)
}

expect_expectation_successful(expect_numeric(1))
expect_expectation_failed(expect_numeric("a"))
