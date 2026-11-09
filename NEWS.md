# lipdR 0.8.3

* `pullTsVariable()` and `pushTsVariable()`: a near match such as "variableName" again finds `paleoData_variableName`. Since 0.8.0 it also matched the new `timeVariableName` and stopped with "multiple near matches". When several fields match, the `paleoData_` (or `chronData_`) one is used. Field names are also no longer listed twice.

# lipdR 0.8.2

* New exported `addTimeScales()` adds time scales to the time columns of a LiPD dataset, for columns created in memory (for example by an age model).
* `getTsTime()`: when the column `time` points to has no units (so its scale would be guessed), but `time` has a stated scale, `time`'s scale is used.

# lipdR 0.8.1

Helpers so analysis packages handle time scales the same way:

* `timeScale()` describes a time scale (from units, time metadata, or a timeseries), and `isPrograde()` says whether it counts forward (year CE) or back (yr BP, ka, b2k).
* `getTsTimes()` gets one time variable of every timeseries on one scale, with a single warning listing records whose scale was assumed or that have no time.
* `commonTimeUnits()` returns the scale shared by a set of timeseries, or a default (yr BP) with a message when they differ.

# lipdR 0.8.0

Every time column now carries its time scale, so lipdR and the analysis packages can convert between time axes without guessing.

* `readLipd()` adds a time scale to every time column (`age`, `year`, their ensembles, and any column with calendar time units such as yr BP, ka, CE or b2k): `timeUnits`, `timeDatum`, `timeDirection`, `timeExponent`, and `timeSource`, which says where the scale came from: `"metadata"` (the file states it), `"units"` (read from the units) or `"assumed"` (the units don't say, so it was guessed from the variable name). `writeLipd()` writes only scales the file stated, so a curator can state the scale of an oddly labeled column by giving it `timeDatum`, `timeDirection` and `timeExponent`.
* `extractTs()` copies every time column of a table into each entry, with its scale (`ageTimeDatum`, `calAgeTimeUnits`, ...), not only `age`, `year` and `ageEnsemble`. The new `timeVariableName` says which column `time` came from, and `timeSource` says how its scale was found.
* `time` is now a copy of the column `timeVariableName` points to. `getTsTime()` reads that column, so edits to it after `extractTs()` are respected; if `time` no longer matches, it warns and uses the column.
* `getTsTime()` gains `variable`, to get any time variable on any scale (e.g. `getTsTime(ts, "CE", variable = "age")`), and `warn.assumed`: it warns when a scale was assumed. The result's attributes give the variable used and its `timeSource`. New `tsTimeVariables()` lists the time variables of an entry.
* An ensemble whose units don't say its scale uses its `age` or `year` column's scale.
* Fixed: `collapseTs()` put `time`, `timeUnits` and the other time fields at the dataset root, so `writeLipd()` wrote them into files (since 0.7.0). They, and the copied time columns, are no longer collapsed.

# lipdR 0.7.4

* `extractTs()` no longer puts depth, or the proxy, in `time` for some records. It took the first column flagged `isPrimary` as the time axis, but in LiPD `isPrimary` usually marks a table's primary variable, so records with depth flagged first (many lake and speleothem records) got depth in `time`, labeled yr BP. Now only time columns are candidates: columns named `age` or `year`, or with calendar time units (yr BP, ka, yr AD, CE, b2k, ...), excluding radiocarbon and uncertainty columns. Among them, a column flagged `primaryAgeColumn` comes first, then `isPrimary`, then `age`, then `year`. This matters because lipdViz, compositeR, actR and ens use `time` by default.

# lipdR 0.7.3

* New `convertTime()`, `getTsTime()` and `convertTsTime()` use the time metadata from `extractTs()` (`timeDatum`, `timeDirection`, `timeExponent`) to put records on one time scale, for example yr BP, ka or year CE. `getTsTime()` returns one timeseries' time axis on a chosen scale, using an age or year ensemble when there is one. `convertTsTime()` converts the `time` of a whole TS list or timeseries tibble. Radiocarbon years are never converted.
* `as.lipdTs()` and `as.lipdTsTibble()` now accept timeseries tibbles that have lost their class (as dplyr verbs can do), plain data frames, and unclassed lists of timeseries.
* `ts2tibble()` no longer warns about, or adds a spurious `chronData_TSid` to, timeseries without a `mode` field (for example ones built by hand).
* `extractTs()` no longer needs R 4.4 or later: it used `%||%`, which base R only gained in 4.4.

# lipdR 0.7.2

* `readLipd()` gives downloads at least an hour, instead of R's default 60 seconds, so large compilation zips (such as the 285 MB hydroclimate2k) no longer fail partway. A longer `options(timeout)` set by the user is kept, and the setting is restored afterwards.
* On Windows, downloads now use R's built-in downloader (libcurl), as on macOS and Linux, instead of the external curl program. Windows downloads now respect `options(timeout)` and `R_LIBCURL_SSL_REVOKE_BEST_EFFORT`.
* A failed download now says why. A timeout explains how to allow longer, and a failed certificate revocation check on Windows names the `R_LIBCURL_SSL_REVOKE_BEST_EFFORT` workaround. On Windows, libcurl reports a revocation failure only as "SSL connect error", so that gets the hint too.
* Fixed a spurious `no non-missing arguments to max` warning when reading a single file from a URL.

# lipdR 0.7.1

* `readLipd()` given a dataset id now gets the current version from the dataset's `version.txt` on lipdverse, falling back on the dataset page where that file doesn't exist yet.
* A dataset id whose version can't be found is now an error. Previously it built a url containing `NA` and failed later with a confusing download error.
* `readLipd()` given several dataset ids now retries a version lookup only for temporary problems (timeouts, and HTTP 408, 429 and 5xx). It used to retry every failure for several minutes per id, including ones that can't recover, such as a missing dataset or a bad certificate.
* Ids whose lookup fails are now left out and named, and the rest still load. If every lookup fails, `readLipd()` stops and gives the reason. Previously a failure became a missing path that broke the whole read with an unrelated error.
* `readLipd()` given a data frame of dataset ids with no `datasetVersion` column no longer fails. The placeholder versions were counted by column rather than by row.

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
