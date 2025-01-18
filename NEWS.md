# lipdR (development version)

* readLipd() now optionally can be parallelized.

# lipdR 0.5.6

* Upgraded csv loading to use data.table::fread instead of readr::read_csv. This is much faster and avoids a version check.

# lipdR 0.5.5

* Fixed the bug that made `lipdTSSummary()` fail if no age columns were present

# lipdR 0.5.4

* Fixed a bug with `readLipd()` with multiple file paths as inputs

# lipdR 0.5.3

* Fix the issue where `readLipd()` sometimes fails when getting version information from lipdverse

# lipdR 0.5.2

* make `readLipd()` give an error if a local file doesn't exist.

# lipdR 0.5.1

* Make `readLipd()` fail better when downloading from lipdverse or other urls

# lipdR 0.5.0

* Brought `queryLipdverse()`, neotoma conversion and changelogging files into the main branch.

# lipdR 0.4.3

* Fixed bug to load a vector of local files

# lipdR 0.4.2

* repaired bug in query table update

# lipdR 0.4.1

* package now loads with a more compact query table to reduce loading time
* the full query table is added to your local version with `update_queryTable()`
* future updates to the query table are checked for with each subsequent user query

# lipdR 0.4.0

* new `summary()` and `print()` functions allow for a quick look at LiPDs in all formats
* a new vignette demonstrates these new functions
* a new function, `queryLipdverse()`, allows for searching all of LiPDverse with many filter arguments
* `readLipd()` now allows for input as a vector of dataset IDs
* `readLipd()` and `queryLipdverse()` can be combined to download a target set of LiPDs
* a second new vignette showcases the query functionality


# lipdR 0.3.6

* Fixed bug introduced in 0.3.4 that prevented proper conversion of old LiPD versions

# lipdR 0.3.5

* new function `removeEnsembles()` that will strip all ensembles from a LiPD object for portability
* `readLipd()` now allows a vector of paths as input, to create a multiLipd from mulitple directories
* Progress bar for writeLipd()

# lipdR 0.3.4

* Make lipdR read/write files without changing the working directory.
* Improve messages during reading, including parsing and error warnings

# lipdR 0.3.3

* Added a `createColumn()` function

# lipdR 0.3.2

* Fix metadata mismatch error in `getPaleoDataNeotoma2()`. Thanks to Sarah Ivory for the bug report.

# lipdR 0.3.1

* Can now read/write json objects in pure jsonld

# lipdR 0.3.0

* Implement S3 classes into lipdR

# lipdR 0.2.4

* readr 2.0.0 is dramatically slowing down readLipd(), for now, I've added an option to use the earlier version. See details here: https://www.tidyverse.org/blog/2021/07/readr-2-0-0/.


# lipdR 0.2.3

* Added a `NEWS.md` file to track changes to the package.
