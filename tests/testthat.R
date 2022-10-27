
if (!requireNamespace("testthat", quietly = TRUE)) {
  stop(
    "Package 'testthat' must be installed to use this function. Install it from github using `remotes::install_github('neotomadb/neotoma2')`",
    call. = FALSE
  )
}

library(testthat)
test_check("lipdR")
