test_that("convertTime moves between CE, BP, ka and b2k", {
  expect_equal(convertTime(c(1850, 1950, 2000), from = "CE", to = "yr BP"), c(100, 0, -50))
  expect_equal(convertTime(c(100, 0, -50), from = "yr BP", to = "yr AD"), c(1850, 1950, 2000))
  expect_equal(convertTime(c(1.2, 11.7), from = "ka", to = "yr BP"), c(1200, 11700))
  expect_equal(convertTime(c(1200, 11700), from = "yr BP", to = "ka"), c(1.2, 11.7))
  expect_equal(convertTime(50, from = "yr b2k", to = "yr BP"), 0)
  expect_equal(convertTime(2, from = "Ma", to = "ka"), 2000)
  m <- matrix(c(0, 100, 10, 110), ncol = 2)
  expect_equal(convertTime(m, from = "yr BP", to = "CE"), 1950 - m)
})

test_that("convertTime takes extractTs() metadata as a list", {
  from <- list(timeDatum = 0, timeDirection = "prograde", timeExponent = 0)
  expect_equal(convertTime(1900, from = from, to = "yr BP"), 50)
})

test_that("radiocarbon years are not converted", {
  expect_equal(convertTime(5000, from = "yr 14C BP", to = "yr 14C BP"), 5000)
  expect_error(convertTime(5000, from = "yr 14C BP", to = "yr BP"), "Calibrate")
})

test_that("getTsTime uses time metadata, then age or year", {
  ts <- list(time = c(1900, 1950), timeUnits = "yr AD", timeDatum = 0,
             timeDirection = "prograde", timeExponent = 0,
             paleoData_values = c(1, 2))
  expect_equal(as.numeric(getTsTime(ts, "yr BP")), c(50, 0))
  expect_equal(attr(getTsTime(ts, "yr BP"), "timeUnits"), "yr BP")
  expect_equal(as.numeric(getTsTime(ts, NA)), c(1900, 1950))

  old <- list(age = c(1, 2), ageUnits = "ka", paleoData_values = c(1, 2))
  expect_equal(as.numeric(getTsTime(old, "yr BP")), c(1000, 2000))
  yr <- list(year = c(1900, 2000), yearUnits = "AD", paleoData_values = c(1, 2))
  expect_equal(as.numeric(getTsTime(yr, "yr BP")), c(50, -50))
  expect_error(getTsTime(list(paleoData_values = 1)), "no time, age or year")
})

test_that("getTsTime prefers an ensemble, converted, unless asked not to", {
  ens <- matrix(c(100, 200, 110, 210), ncol = 2)
  ts <- list(time = c(1850, 1750), timeUnits = "yr AD", timeDatum = 0,
             timeDirection = "prograde", timeExponent = 0,
             ageEnsemble = ens, ageEnsembleUnits = "yr BP",
             paleoData_values = c(1, 2))
  expect_equal(unname(unclass(getTsTime(ts, "CE")))[, 1:2], 1950 - ens, ignore_attr = TRUE)
  expect_equal(as.numeric(getTsTime(ts, "yr BP", ensemble = FALSE)), c(100, 200))
})

test_that("getTsTime takes a one-row tibble", {
  tib <- tibble::tibble(age = list(c(0, 10)), ageUnits = "yr BP", paleoData_values = list(c(1, 2)))
  expect_equal(as.numeric(getTsTime(tib, "CE")), c(1950, 1940))
  expect_error(getTsTime(rbind(tib, tib)), "one row")
})

test_that("convertTsTime puts a CE and a BP record on one scale, for lists and tibbles", {
  D <- readLipd(test_path("Carre.Saloum.2018.lpd"))
  TS <- extractTs(D)
  expect_equal(TS[[1]]$timeDirection, "prograde")
  bp <- convertTsTime(TS, "yr BP")
  expect_s3_class(bp, "lipd_ts")
  expect_equal(bp[[1]]$time, 1950 - TS[[1]]$time)
  expect_equal(bp[[1]]$timeUnits, "yr BP")
  expect_equal(bp[[1]]$timeMax, 1950 - TS[[1]]$timeMin)
  expect_equal(bp[[1]]$year, TS[[1]]$year)

  tib <- ts2tibble(TS)
  tbp <- convertTsTime(tib, "yr BP")
  expect_s3_class(tbp, "lipd_ts_tibble")
  expect_equal(tbp$time[[1]], 1950 - TS[[1]]$time)
  expect_true(all(tbp$timeUnits == "yr BP"))
})

test_that("as.lipdTs and as.lipdTsTibble accept unclassed data frames and lists", {
  D <- readLipd(test_path("ODP1098B13.lpd"))
  TS <- extractTs(D)
  tib <- ts2tibble(TS)

  plain <- as.data.frame(tib)
  back <- as.lipdTs(plain)
  expect_s3_class(back, "lipd_ts")
  expect_equal(back[[1]]$age, TS[[1]]$age)

  bare <- unclass(TS)
  expect_s3_class(as.lipdTs(bare), "lipd_ts")
  expect_s3_class(as.lipdTsTibble(bare), "lipd_ts_tibble")
  expect_s3_class(as.lipdTsTibble(plain), "lipd_ts_tibble")

  long <- tidyTs(TS)
  long <- tibble::as_tibble(unclass(long))
  expect_s3_class(as.lipdTs(long), "lipd_ts")
})
