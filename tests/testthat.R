
if (!requireNamespace("testthat", quietly = TRUE)) {
  stop(
    "Package 'testthat' must be installed to use this function.",
    call. = FALSE
  )
}

library(testthat)
test_check("lipdR")
