# A dataset with several time columns: a CE year (primary), a BP age, a
# calibrated age in ka with another name, an ensemble without units, and a
# proxy flagged isPrimary.
make_time_lipd <- function(dsn = "TimeCols"){
  L <- create_test_lipd_object(dsn)
  L$paleoData[[1]]$measurementTable[[1]] <- list(
    tableName = "P1M1",
    d18O = list(variableName = "d18O", units = "permil", TSid = paste0(dsn, "-d18O"),
                number = 1, values = c(-5, -6, -7, -8), isPrimary = TRUE),
    year = list(variableName = "year", units = "CE", TSid = paste0(dsn, "-year"),
                number = 2, values = c(1900, 1800, 1700, 1600), primaryAgeColumn = TRUE),
    age = list(variableName = "age", units = "yr BP", TSid = paste0(dsn, "-age"),
               number = 3, values = c(50, 150, 250, 350)),
    calAge = list(variableName = "calAge", units = "ka", TSid = paste0(dsn, "-calAge"),
                  number = 4, values = c(0.05, 0.15, 0.25, 0.35))
  )
  L$chronData <- NULL
  L
}

lpd_jsonld <- function(path){
  d <- tempfile()
  utils::unzip(path, exdir = d)
  paste(readLines(list.files(d, "metadata.jsonld", recursive = TRUE, full.names = TRUE)), collapse = "\n")
}

test_that("every time column gets a time scale and its source", {
  L <- add_time_scales(make_time_lipd())
  mt <- L$paleoData[[1]]$measurementTable[[1]]
  expect_equal(mt$year$timeUnits, "yr AD")
  expect_equal(mt$year$timeDirection, "prograde")
  expect_equal(mt$age$timeDatum, 1950)
  expect_equal(mt$calAge$timeExponent, 3)
  expect_equal(mt$calAge$timeSource, "units")
  expect_null(mt$d18O$timeSource)

  mt$age$units <- NULL
  L$paleoData[[1]]$measurementTable[[1]] <- mt
  L <- add_time_scales(L)
  expect_equal(L$paleoData[[1]]$measurementTable[[1]]$age$timeSource, "assumed")
  expect_equal(L$paleoData[[1]]$measurementTable[[1]]$age$timeUnits, "yr BP")
})

test_that("a scale stated in the file is kept, and derived scales are not written", {
  L <- make_time_lipd()
  # a curator states the scale of an oddly labeled column
  L$paleoData[[1]]$measurementTable[[1]]$calAge$units <- "kyr before 2000"
  L$paleoData[[1]]$measurementTable[[1]]$calAge[c("timeDatum", "timeDirection", "timeExponent")] <-
    list(2000, "retrograde", 3)
  dir <- tempfile(); dir.create(dir)
  suppressMessages(writeLipd(L, path = dir))
  path <- file.path(dir, "TimeCols.lpd")
  json <- lpd_jsonld(path)
  expect_false(grepl("timeSource", json))
  expect_equal(lengths(regmatches(json, gregexpr("timeDatum", json))), 1)

  R <- suppressMessages(readLipd(path))
  mt <- R$paleoData[[1]]$measurementTable[[1]]
  expect_equal(mt$calAge$timeSource, "metadata")
  expect_equal(mt$calAge$timeDatum, 2000)
  expect_equal(mt$age$timeSource, "units")
})

test_that("extractTs carries every time column, and time points to its source", {
  TS <- extractTs(make_time_lipd())
  ts <- TS[[1]]
  expect_equal(ts$timeVariableName, "year")
  expect_equal(ts$time, c(1900, 1800, 1700, 1600))
  expect_equal(ts$timeSource, "units")
  expect_equal(ts$calAge, c(0.05, 0.15, 0.25, 0.35))
  expect_equal(ts$calAgeTimeUnits, "ka")
  expect_equal(ts$ageTimeDatum, 1950)
  expect_equal(ts$yearTimeDirection, "prograde")
  expect_null(ts$d18OTimeUnits)

  # any time variable, on any scale
  expect_equal(as.numeric(getTsTime(ts, "yr BP")), c(50, 150, 250, 350))
  expect_equal(as.numeric(getTsTime(ts, "CE", variable = "calAge")), c(1900, 1800, 1700, 1600))
  expect_equal(attr(getTsTime(ts, "ka", variable = "age"), "timeVariableName"), "age")
  expect_error(getTsTime(ts, variable = "d18O"), "time variables are: time, year, age, calAge")
  expect_equal(tsTimeVariables(ts), c("time", "year", "age", "calAge"))

  # time is a copy; the column it points to wins, with a warning
  ts$year <- ts$year + 10
  expect_warning(tm <- getTsTime(ts, NA), "no longer matches")
  expect_equal(as.numeric(tm), c(1910, 1810, 1710, 1610))
})

test_that("an assumed time scale is flagged and warned about", {
  L <- make_time_lipd()
  L$paleoData[[1]]$measurementTable[[1]]$year$units <- NULL
  TS <- extractTs(L)
  expect_equal(TS[[1]]$timeSource, "assumed")
  expect_warning(getTsTime(TS[[1]], "yr BP"), "assumed")
  expect_no_warning(getTsTime(TS[[1]], "yr BP", warn.assumed = FALSE))
  expect_equal(attr(getTsTime(TS[[1]], "yr BP", warn.assumed = FALSE), "timeSource"), "assumed")
})

test_that("an ensemble without units borrows its age column's scale", {
  ts <- list(dataSetName = "x", paleoData_values = 1:3,
             age = c(1, 2, 3), ageUnits = "ka",
             ageTimeUnits = "ka", ageTimeDatum = 1950, ageTimeDirection = "retrograde",
             ageTimeExponent = 3, ageTimeSource = "units",
             ageEnsemble = matrix(c(1, 2, 3, 1.1, 2.1, 3.1), ncol = 2))
  expect_no_warning(e <- getTsTime(ts, "yr BP"))
  expect_equal(e[, 1], c(1000, 2000, 3000))
})

test_that("collapseTs puts nothing time-related at the dataset root, and writes no derived scales", {
  L <- make_time_lipd()
  TS <- extractTs(L)
  D <- collapseTs(TS)
  expect_length(intersect(names(D), c("time", "timeUnits", "timeVariableName", "calAge", "ageTimeDatum")), 0)
  expect_equal(D$paleoData[[1]]$measurementTable[[1]]$calAge$values, c(0.05, 0.15, 0.25, 0.35))

  dir <- tempfile(); dir.create(dir)
  suppressMessages(writeLipd(D, path = dir))
  json <- lpd_jsonld(file.path(dir, "TimeCols.lpd"))
  expect_false(grepl("timeSource|timeDatum|timeVariableName", json))
})

test_that("convertTsTime converts time and keeps the pointer", {
  TS <- extractTs(make_time_lipd())
  bp <- convertTsTime(TS, "yr BP")
  expect_equal(bp[[1]]$time, c(50, 150, 250, 350))
  expect_equal(bp[[1]]$timeVariableName, "year")
  expect_equal(bp[[1]]$year, TS[[1]]$year)
  expect_no_warning(tm <- getTsTime(bp[[1]], "ka"))
  expect_equal(as.numeric(tm), c(0.05, 0.15, 0.25, 0.35))

  tib <- convertTsTime(ts2tibble(TS), "ka")
  expect_true(all(tib$timeVariableName == "year"))
  expect_equal(as.numeric(getTsTime(tib[1, ], "yr BP")), c(50, 150, 250, 350))
})

test_that("timeScale and isPrograde describe scales", {
  expect_equal(timeScale("ka")$timeExponent, 3)
  expect_equal(timeScale(list(timeDatum = 2000, timeDirection = "retrograde", timeExponent = 0))$timeDatum, 2000)
  expect_true(isPrograde("yr AD"))
  expect_true(isPrograde("CE"))
  expect_false(isPrograde("ka"))
  expect_false(isPrograde("yr b2k"))
})

test_that("getTsTimes converts every record and warns once", {
  a <- list(dataSetName = "a", paleoData_variableName = "x", paleoData_values = 1:2,
            year = c(1900, 1950), yearUnits = "CE")
  b <- list(dataSetName = "b", paleoData_variableName = "x", paleoData_values = 1:2,
            age = c(0, 100))
  c3 <- list(dataSetName = "c", paleoData_variableName = "x", paleoData_values = 1:2)
  TS <- list(a, b, c3)
  w <- character(0)
  out <- withCallingHandlers(getTsTimes(TS, "yr BP"),
                             warning = function(cnd){ w <<- c(w, conditionMessage(cnd)); invokeRestart("muffleWarning") })
  expect_length(w, 2)
  expect_match(w[1], "1 timeseries have no 'time'")
  expect_match(w[2], "was assumed.*b x")
  expect_equal(as.numeric(out[[1]]), c(50, 0))
  expect_null(out[[3]])
  expect_error(getTsTimes(TS, missing = "error", warn.assumed = FALSE), "no 'time'")
  expect_equal(attr(out, "timeUnits"), "yr BP")

  tib <- ts2tibble(extractTs(make_time_lipd()))
  expect_length(getTsTimes(tib, "ka"), nrow(tib))
})

test_that("commonTimeUnits keeps a shared scale, and falls back with a message", {
  TS <- extractTs(make_time_lipd())
  expect_equal(commonTimeUnits(TS), "yr AD")
  mixed <- list(TS[[1]], list(dataSetName = "b", paleoData_values = 1:2, age = c(0, 1), ageUnits = "ka"))
  expect_message(u <- commonTimeUnits(mixed), "different time scales")
  expect_equal(u, "yr BP")
})

test_that("addTimeScales is exported, and a guessed source scale doesn't override time's", {
  L <- addTimeScales(make_time_lipd())
  expect_equal(L$paleoData[[1]]$measurementTable[[1]]$calAge$timeUnits, "ka")
  ts <- list(dataSetName = "x", paleoData_values = 1:2, calAge = c(1, 2),
             time = c(1, 2), timeUnits = "ka", timeDatum = 1950, timeDirection = "retrograde",
             timeExponent = 3, timeSource = "units", timeVariableName = "calAge")
  expect_no_warning(tm <- getTsTime(ts, "yr BP"))
  expect_equal(as.numeric(tm), c(1000, 2000))
})
