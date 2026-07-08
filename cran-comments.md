## R CMD check results

0 errors | 0 warnings | 0 notes

* This is a resubmission / new submission of lipdR to CRAN.

## Test environments

* local macOS (R release)
* GitHub Actions: macOS (release), Windows (release), Ubuntu (devel, release, oldrel-1)

## Notes

* The package converts to and from the Neotoma Paleoecology Database via the
  suggested package 'neotoma2'. All 'neotoma2' usage is conditional on the
  package being installed (`requireNamespace()`), and examples/vignette code that
  would contact the Neotoma API is not evaluated during checks.
