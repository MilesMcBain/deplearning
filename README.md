# deplearning
> Utilise the synergy of algorithms and heuristics to unlock knowledge of R script/package dependencies.

Your first forays into understanding someone else's R analysis often involve flailing around trying to obtain all the required dependencies. This package will manage that for you by identifying your missing and out of date dependencies and offering a choice to automatically install & update.

# Usage

WORK IN PROGRESS

## Identify and Analyse Dependencies
`depl_check(source_path)` will examine the code found at or below `source_path` and produce a dependency report:

```
> depl_check()
[deplearning] Starting dependency check.
[deplearning] Searching path . 
[deplearning] Scanning 7 R source files... done.
[deplearning] Found 29 dependencies.
[deplearning] Fetching remote data... CRAN... GitHub... done.
[deplearning] ✔ 14 Installed and up to date.

 purrr, memoise, jsonlite, rstudioapi, tibble, clisymbols, knitr, tidyr, packagemetrics, xml2, devtools, testthat 

[deplearning] ✖ 2 Installed but behind CRAN release.

 package installed_ver CRAN_ver
  visdat    0.0.5.9000    0.1.0
   dplyr         0.7.1    0.7.2

[deplearning] ✖ 8 Missing CRAN packages.

  datapasta, A3, abc, switchr, dejaVu, Rborist, kpmt, fasjem 

[deplearning] ✖ 4 Missing GitHub packages.

  njtierney/naniar, maelle/rtimicropem, benmarwick/rrtools, MilesMcBain/packup 

[deplearning] ✖ 1 Missing packages from untracked repositories.

  notARealPackage 

[deplearning] ✔ Minimum R version to update & install is 3.3.1, you have 3.4.0.
[deplearning] ℹ  Update & install will include 18 new recursive dependencies.

  maps, gistr, assertive.base, assertive.files, assertive.numbers, assertive.properties, assertive.reflection, assertive.strings, assertive.types, pbapply, abc.data, quantreg, locfit, SparseM, MatrixModels, RJSONIO, RcppArmadillo, matrixStats 

[deplearning] Would you like to update & install old and missing dependencies?

1: Yes
2: No

Selection: 2
[deplearning] Dependency check finished without install & update.
```

There is also an RStudio addin hook to search the dependencies in the currently active window.

Dependencies are identified using by pattern matching against common methods: `library(...)`, `require(...)`, `p_load(..., ..., )`, `...::func()`

## Future Work
The plan is to extend this idea to:
   * Estimation of time until code can run based on analysis of recursive dependencies. 
