# A ts tibble is rectangular: every column carries a cell for every indexed
# block any column in the dataset has. Assigning the NA cells materialised an
# empty interpretation -- scope NA and nothing else -- on every column with
# fewer than the dataset's maximum, including age and year columns that cannot
# have one. They then survived the write to file.

test_that("an all-NA indexed block does not materialise an entry", {
  entry <- list(interpretation1_variable = "temperature",
                interpretation1_scope = "climate",
                interpretation2_variable = NA,
                interpretation2_scope = NA)
  l <- list()
  for (k in names(entry)) l <- lipdR:::collapse_block_indexed(entry, l, k)
  expect_length(l, 1)
  expect_equal(l[[1]]$variable, "temperature")
})

test_that("a populated block at a higher index is still built", {
  entry <- list(interpretation1_variable = "temperature",
                interpretation2_variable = NA,
                interpretation3_variable = "precipitation")
  l <- list()
  for (k in names(entry)) l <- lipdR:::collapse_block_indexed(entry, l, k)
  expect_length(l, 3)
  expect_equal(l[[1]]$variable, "temperature")
  expect_equal(l[[3]]$variable, "precipitation")
})

test_that("a column keeps only the interpretations it has", {
  L <- list(dataSetName = "A.Author.2001",
            paleoData = list(list(measurementTable = list(list(
              tableName = "t", filename = "t.csv", missingValue = "NaN",
              d18O = list(variableName = "d18O", TSid = "T1", number = 1,
                          values = c(1, 2, 3),
                          interpretation = list(list(scope = "climate", variable = "temperature"),
                                                list(scope = "isotope", variable = "d18O"))),
              year = list(variableName = "year", TSid = "T2", number = 2,
                          values = c(1, 2, 3),
                          interpretation = list(list(scope = "climate", variable = "temperature"))))))))
  class(L) <- c("lipd", "list")
  n <- function(x) vapply(x$paleoData[[1]]$measurementTable[[1]][c("d18O", "year")],
                          function(c) length(c$interpretation), integer(1))
  expect_equal(unname(n(L)), c(2L, 1L))
  L2 <- suppressWarnings(as.lipd(as.lipdTsTibble(L)))
  expect_equal(unname(n(L2)), c(2L, 1L))
})
