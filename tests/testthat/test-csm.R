# Compilation-specific metadata (csm) round-trip

with_csm <- function(dsn = "TestDSN"){
  L <- create_test_lipd_object(dsn)
  L$paleoData[[1]]$measurementTable[[1]]$temp$inCompilation <- list(
    list(compilationName = "iso2k", compilationVersion = c("1_0_0", "1_0_1"),
         csm = list(certification = "B", UI = "iso-42")),
    list(compilationName = "SISAL-LiPD", compilationVersion = "2_0_0",
         csm = list(entityID = "E-7"))
  )
  L
}

test_that("csmCompilationKey strips characters that would break the key", {
  expect_equal(csmCompilationKey("iso2k"), "iso2k")
  # Hyphens and underscores must go, so <comp>_csm_<field> is the only key
  # shape with two underscores.
  expect_equal(csmCompilationKey("SISAL-LiPD"), "SISALLiPD")
  expect_equal(csmCompilationKey("DAMP21k_Lakes"), "DAMP21kLakes")
  expect_equal(csmCompilationKey("NAm21k-noPollen"), "NAm21knoPollen")
})

test_that("extractTs flattens csm to <compilation>_csm_<field>", {
  ts <- extractTs(with_csm())
  e <- ts[[which(vapply(ts, function(x) identical(x$paleoData_variableName, "temp"), logical(1)))]]

  expect_equal(e[["iso2k_csm_certification"]], "B")
  expect_equal(e[["iso2k_csm_UI"]], "iso-42")
  expect_equal(e[["SISALLiPD_csm_entityID"]], "E-7")
  # The nested form is removed so there is a single representation.
  expect_false(any(grepl("^inCompilation[0-9]+_csm$", names(e))))
  # Membership itself is untouched.
  expect_equal(e[["inCompilation1_compilationName"]], "iso2k")
})

test_that("the compilation, not the array index, keys the name", {
  L <- with_csm()
  # Same two compilations, opposite order. A compilation occupies different
  # indices in different datasets, so index-based names would not be stable.
  L$paleoData[[1]]$measurementTable[[1]]$temp$inCompilation <-
    rev(L$paleoData[[1]]$measurementTable[[1]]$temp$inCompilation)

  ts <- extractTs(L)
  e <- ts[[which(vapply(ts, function(x) identical(x$paleoData_variableName, "temp"), logical(1)))]]
  expect_equal(e[["iso2k_csm_certification"]], "B")
  expect_equal(e[["SISALLiPD_csm_entityID"]], "E-7")
})

test_that("csm survives a full extract/collapse round trip", {
  L <- with_csm()
  D <- collapseTs(extractTs(L))
  ic <- D$paleoData[[1]]$measurementTable[[1]]$temp$inCompilation

  expect_length(ic, 2)
  expect_equal(ic[[1]]$compilationName, "iso2k")
  expect_equal(ic[[1]]$csm$certification, "B")
  expect_equal(ic[[1]]$csm$UI, "iso-42")
  # The sanitised key is matched against existing membership, so the original
  # hyphenated name is preserved rather than reconstructed.
  expect_equal(ic[[2]]$compilationName, "SISAL-LiPD")
  expect_equal(ic[[2]]$csm$entityID, "E-7")
  expect_equal(ic[[1]]$compilationVersion, c("1_0_0", "1_0_1"))
})

# Each compilation's csm is folded into its own entry, so writing one
# compilation's metadata never disturbs another's.
test_that("collapse writes each compilation's csm into its own entry", {
  L <- with_csm()
  ts <- extractTs(L)
  i <- which(vapply(ts, function(x) identical(x$paleoData_variableName, "temp"), logical(1)))
  ts[[i]][["iso2k_csm_certification"]] <- "A"

  D <- collapseTs(ts)
  ic <- D$paleoData[[1]]$measurementTable[[1]]$temp$inCompilation

  expect_equal(ic[[1]]$csm$certification, "A")
  expect_equal(ic[[2]]$csm$entityID, "E-7")   # untouched by the iso2k edit
})

# The contract, stated as a test so it cannot drift: collapseTs writes exactly
# the csm the timeseries carries. It does not consult the original file, so a
# timeseries stripped of some csm keys writes back without them. This is why
# extractTs always emits every compilation's csm and why a filtered timeseries
# must not be collapsed.
#
# Recovery from the stored original was considered and rejected: it would make
# it impossible to ever delete a csm field through the timeseries.
test_that("collapse writes exactly the csm the timeseries carries", {
  L <- with_csm()
  ts <- extractTs(L)
  i <- which(vapply(ts, function(x) identical(x$paleoData_variableName, "temp"), logical(1)))
  ts[[i]][["SISALLiPD_csm_entityID"]] <- NULL

  D <- collapseTs(ts)
  ic <- D$paleoData[[1]]$measurementTable[[1]]$temp$inCompilation

  expect_equal(ic[[1]]$csm$certification, "B")
  # Membership survives regardless; only the dropped metadata is absent.
  expect_equal(ic[[2]]$compilationName, "SISAL-LiPD")
  expect_equal(ic[[2]]$compilationVersion, "2_0_0")
  expect_null(ic[[2]]$csm$entityID)
})

test_that("an edited csm value is written back", {
  L <- with_csm()
  ts <- extractTs(L)
  i <- which(vapply(ts, function(x) identical(x$paleoData_variableName, "temp"), logical(1)))
  ts[[i]][["iso2k_csm_certification"]] <- "A"

  D <- collapseTs(ts)
  expect_equal(D$paleoData[[1]]$measurementTable[[1]]$temp$inCompilation[[1]]$csm$certification, "A")
})

test_that("a csm key naming an absent compilation warns and is not silently dropped", {
  L <- with_csm()
  ts <- extractTs(L)
  i <- which(vapply(ts, function(x) identical(x$paleoData_variableName, "temp"), logical(1)))
  ts[[i]][["NotAComp_csm_field"]] <- "orphan"

  expect_warning(D <- collapseTs(ts), "no matching compilation membership")
  ic <- D$paleoData[[1]]$measurementTable[[1]]$temp$inCompilation
  expect_equal(ic[[1]]$csm$certification, "B")
})

test_that("columns without csm are unaffected", {
  L <- with_csm()
  ts <- extractTs(L)
  age <- ts[[which(vapply(ts, function(x) identical(x$paleoData_variableName, "age"), logical(1)))]]
  expect_false(any(grepl("_csm_", names(age))))

  D <- collapseTs(ts)
  expect_null(D$paleoData[[1]]$measurementTable[[1]]$age$inCompilation)
})

test_that("a dataset with no compilations at all round trips unchanged", {
  L <- create_test_lipd_object()
  D <- collapseTs(extractTs(L))
  expect_equal(D$paleoData[[1]]$measurementTable[[1]]$temp$variableName, "temp")
  expect_null(D$paleoData[[1]]$measurementTable[[1]]$temp$csm)
})

test_that("csmFields summarises what is present", {
  f <- csmFields(extractTs(with_csm()))
  expect_setequal(f$compilation, c("iso2k", "SISALLiPD"))
  expect_setequal(f$field[f$compilation == "iso2k"], c("certification", "UI"))
  expect_true(all(f$n >= 1))
})

test_that("csmFields on a timeseries with no csm returns an empty frame", {
  expect_equal(nrow(csmFields(extractTs(create_test_lipd_object()))), 0)
})
