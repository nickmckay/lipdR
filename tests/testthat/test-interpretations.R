context("interpretations")

make_interp_ts <- function(){
  mk <- function(id, ...){
    e <- list(dataSetName = paste0("ds", id), paleoData_TSid = paste0("TS", id),
              paleoData_variableName = "proxy", paleoData_values = c(1, 2, 3),
              age = c(100, 200, 300), mode = "paleo")
    c(e, list(...))
  }
  structure(list(
    # 1: annual temperature as interpretation 1
    mk(1, interpretation1_variable = "temperature", interpretation1_seasonality = "Annual",
       interpretation1_scope = "climate", interpretation1_direction = "positive"),
    # 2: temperature (July) and precipitation (annual) in separate interpretations
    mk(2, interpretation1_variable = "temperature", interpretation1_seasonality = "Jul",
       interpretation2_variable = "precipitation", interpretation2_seasonality = "annual"),
    # 3: isotope interpretation first, climate temperature second, ranked
    mk(3, interpretation1_variable = "precipitationIsotope", interpretation1_scope = "isotope",
       interpretation1_rank = 2,
       interpretation2_variable = "Temperature", interpretation2_scope = "Climate",
       interpretation2_seasonality = "Summer", interpretation2_rank = 1),
    # 4: no interpretations
    mk(4),
    # 5: an environment scope and an interpretation 10
    mk(5, interpretation1_variable = "salinity", interpretation1_scope = "environment",
       interpretation10_variable = "temperature", interpretation10_scope = "climate")
  ), class = c("lipd_ts", "list"))
}

ids <- function(x){
  vapply(as.lipdTs(x), function(e) e[["paleoData_TSid"]], character(1))
}

test_that("getInterpretations gives one row per interpretation, missing scope as climate", {
  ts <- make_interp_ts()
  it <- getInterpretations(ts)
  expect_equal(nrow(it), 7)
  expect_equal(it$TSid, c("TS1", "TS2", "TS2", "TS3", "TS3", "TS5", "TS5"))
  expect_equal(it$number, c(1L, 1L, 2L, 1L, 2L, 1L, 10L))
  expect_equal(it$scope[it$TSid == "TS2"], c("climate", "climate"))
  expect_true(all(c("variable", "seasonality", "direction", "rank") %in% names(it)))
  expect_equal(nrow(getInterpretations(ts[4])), 0)
})

test_that("named conditions must hold within one interpretation, ignoring case", {
  ts <- make_interp_ts()
  expect_equal(ids(filterByInterpretation(ts, variable = "temperature")), c("TS1", "TS2", "TS3", "TS5"))
  # TS2 has temperature and an annual interpretation, but not annual temperature
  expect_equal(ids(filterByInterpretation(ts, variable = "temperature", seasonality = "annual")), "TS1")
  # a vector is OR within the field
  expect_equal(ids(filterByInterpretation(ts, variable = "temperature", seasonality = c("annual", "jul"))),
               c("TS1", "TS2"))
})

test_that("unnamed expressions give OR across fields", {
  ts <- make_interp_ts()
  out <- filterByInterpretation(ts, variable == "salinity" | seasonality == "Summer")
  expect_equal(ids(out), c("TS3", "TS5"))
  expect_error(filterByInterpretation(ts, variable), "TRUE or FALSE")
})

test_that("scope limits the interpretations considered", {
  ts <- make_interp_ts()
  expect_equal(ids(filterByInterpretation(ts, variable = "precipitationIsotope", scope = "climate")), character(0))
  expect_equal(ids(filterByInterpretation(ts, scope = "isotope")), "TS3")
  expect_equal(ids(filterByInterpretation(ts, scope = "environment")), "TS5")
  expect_equal(ids(filterByInterpretation(ts, variable = "temperature", scope = "climate")),
               c("TS1", "TS2", "TS3", "TS5"))
})

test_that("match = all, none and primary", {
  ts <- make_interp_ts()
  expect_equal(ids(filterByInterpretation(ts, variable = "temperature", match = "all")), "TS1")
  expect_equal(ids(filterByInterpretation(ts, variable = "temperature", scope = "climate", match = "all")),
               c("TS1", "TS3", "TS5"))
  expect_equal(ids(filterByInterpretation(ts, variable = "temperature", match = "none")), "TS4")
  expect_equal(ids(filterByInterpretation(ts, scope = "isotope", match = "none")), c("TS1", "TS2", "TS4", "TS5"))
  # TS3's temperature interpretation is number 2 but rank 1; TS5's is number 10
  expect_equal(ids(filterByInterpretation(ts, variable = "temperature", match = "primary")),
               c("TS1", "TS2", "TS3"))
})

test_that("chaining gives AND across interpretations", {
  ts <- make_interp_ts()
  out <- filterByInterpretation(filterByInterpretation(ts, variable = "temperature"), variable = "precipitation")
  expect_equal(ids(out), "TS2")
})

test_that("a field no interpretation has warns and matches nothing", {
  ts <- make_interp_ts()
  expect_warning(out <- filterByInterpretation(ts, seasonalty = "annual"), "No interpretation has a 'seasonalty'")
  expect_length(out, 0)
})

test_that("works on ts-tibbles and keeps the class", {
  ts <- make_interp_ts()
  tib <- as.lipdTsTibble(ts)
  out <- filterByInterpretation(tib, variable = "temperature", seasonality = c("annual", "jul"))
  expect_true(is.lipdTsTibble(out))
  expect_equal(out$paleoData_TSid, c("TS1", "TS2"))

  long <- tidyTs(ts, age.var = "age")
  outl <- filterByInterpretation(long, variable = "temperature", match = "primary")
  expect_equal(class(outl), class(long))
  expect_equal(sort(unique(outl$paleoData_TSid)), c("TS1", "TS2", "TS3"))
})

test_that("split and combine handle 10+ interpretations and other scopes", {
  ts <- make_interp_ts()
  s <- splitInterpretationByScope(ts)
  e5 <- s[[5]]
  expect_equal(e5$environmentInterpretation1_variable, "salinity")
  expect_equal(e5$climateInterpretation1_variable, "temperature")
  expect_false(any(grepl("^interpretation\\d+_", names(e5))))

  c5 <- combineInterpretationByScope(s)[[5]]
  it <- getInterpretations(structure(list(c5), class = c("lipd_ts", "list")))
  expect_setequal(paste(it$scope, it$variable), c("climate temperature", "environment salinity"))
})
