
if (!requireNamespace("testthat", quietly = TRUE)) {
  stop(
    "Package 'testthat' must be installed to use this function. Install it using `install.packages("testthat")`",
    call. = FALSE
  )
}

library(testthat)
test_check("lipdR")
