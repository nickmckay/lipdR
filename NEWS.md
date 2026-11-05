# lipdR 0.7.0

* Reading a file with no `lipdVersion` key no longer prompts. It asked on every such file, which made reading a directory interactive, and answered nothing useful when it could not ask: a non-interactive `readline()` returns `""`, which matched neither the yes nor the no branch, so the version was left `NA` and written back into the file that way. Files without a version key are now assumed to be current (1.3), and an unrecognised version warns instead of printing.
* Fixed `collapse_block_indexed()` materialising empty interpretations on every column of a rectangular time series tibble, including `age` and `year` columns that cannot have one. This is the mechanism behind the empty interpretation shells that accumulated in the LiPDverse database, and it fired on every file passing through `as.lipdTsTibble()`/`as.lipd()`.

* Prepared the package for CRAN (passes `R CMD check --as-cran` cleanly).
* `writeLipd()` is substantially faster for large files. Zip compression is now configurable via a new `compression_level` argument (default `2`), which writes datasets with large ensemble tables roughly 3-4x faster for slightly larger files. Pass `compression_level = 6-9` to prioritize file size over speed.
* `writeLipd()` gained a `parallel` option for writing a `multiLipd` of many datasets in parallel (uses `furrr`; call `future::plan()` first, as with `readLipd(parallel = TRUE)`).
* Reading with `dont.load.ensemble = TRUE` no longer extracts the archive twice, speeding up loads of ensemble-heavy files.
* Fixed a bug where string and logical measurement columns (e.g. `labID`, `materialDated`, `notes`) were silently written as `NaN`, causing data loss on `writeLipd()`. Columns now keep their type through a read/write round trip.
* Fixed the mirror image of that bug: a numeric column whose values are all `NaN` was typed logical on re-read, because an all-missing CSV column carries no type information and `fread` defaults to logical. The file then failed `validLipd()` with e.g. "depth values are not numeric". All-missing columns are now read as numeric. This affected 14 of the 7,177 files in the LiPDverse database.
* Fixed paleo ensemble tables doubling in width on a read/write round trip (1000 members became 2000). `merge_csv_columns()` decided which CSV columns a metadata entry with no `number` should take by looking only at the columns processed so far, so a numberless column listed *before* the column that claims the data was handed the whole table, and the claiming column then took it as well. Claims are now gathered across all columns before any assignment. Chron ensembles were unaffected, because their columns declare disjoint `number` ranges.
* `extractTs()` now adds standardized time metadata (`time`, `timeUnits`, `timeDatum`, `timeDirection`, `timeExponent`, `timeMin`/`timeMax`) to every entry, and an optional `calculateResolution` argument computes per-column resolution.
* Renamed `inCompilationBeta` to `inCompilation`, with automatic migration of legacy keys on read. Added a new `filterByCompilation()` function.
* Added support for compilation-specific metadata (`csm`), stored under `csm` inside each `inCompilation` entry. `extractTs()` flattens these to `<compilation>_csm_<field>` (e.g. `iso2k_csm_certification`), keyed by compilation name rather than array index, and `collapseTs()` folds them back into the matching entry. `csmFields()` lists what a time series carries. Note that `extractTs()` always emits every compilation's `csm`, and `collapseTs()` writes back exactly what the time series carries, so a filtered time series should not be collapsed.
* Fixed the LiPD-to-Neotoma conversion (`lipd2neotoma()`), which is now working and tested.
* Fixed a crash in `print()`/`summary()` for objects with no measurement table.
* Added `getTables()`, which extracts every measurement, summary, ensemble, and/or distribution table from a LiPD file as a named list of data.frames. Generalizes `getMeasurementTables()` to all table types and correctly expands matrix-valued columns (e.g. ensemble table draws) into one output column each.

# lipdR 0.6.0

* a handful of improvements and error handling
* new option for `readLipd()`: `dont.load.ensemble()`. This option doesn't load in ensemble data, but stores them in a temporary directory. If when that object is then written back out using `writeLipd()`, if that temporary directory still exists it will add the ensemble data back in.

# lipdR 0.5.7

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
