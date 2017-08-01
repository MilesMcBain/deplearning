# deplearning
> Utilise the synergy of algorithms and heuristics to unlock knowledge of R script/package dependencies.

Your first forays into understanding someone else's R analysis often involve flailing around trying to obtain all the required dependencies. This package will manage that for you by identifying your missing and out of date dependencies and offering a choice to automatically install & update.

# Usage

WORK IN PROGRESS

## Identify and Analyse Dependencies
`depl_check(source_path)` will examine the code found at or below `source_path` and produce a dependency report:

```
> depl_check()
[deplearning] Starting check_run.
[deplearning] Searching path . 
[deplearning] Scanning 7 R source files... done.
[deplearning] Found 24 dependencies.
[deplearning] Fetching remote data... CRAN... GitHub... done.
[deplearning] ✔ 15 Installed and up to date.

 purrr, memoise, jsonlite, rstudioapi, tibble, clisymbols, knitr, datapasta, tidyr, dplyr, xml2, devtools, testthat 

[deplearning] ✖ 1 Installed but behind CRAN release.

 package installed_ver CRAN_ver
 switchr        0.9.24   0.11.2

[deplearning] ✖ 1 Installed but behind GitHub version.

      GH_repository installed_ver GH_ver
 maelle/rtimicropem           1.2    1.3

[deplearning] ✖ 3 Missing CRAN packages.

  A3, abc, visdat 

[deplearning] ✖ 2 Missing GitHub packages.

  njtierney/narnia, MilesMcBain/packup 

[deplearning] ✖ 2 Missing packages from untracked repositories.

  notARealPackage, packagemetrics 

[deplearning] ✔ Minimum R version to update & install is 3.3.1, you have 3.4.0.
[deplearning] ℹ  Update & install will include 8 new recursive depenencies.

  pbapply, abc.data, quantreg, locfit, SparseM, MatrixModels, viridisLite, gridExtra 

[deplearning] Would you like to update & install old and missing dependencies?

1: Yes
2: No

Selection: 2
[deplearning] depl_check Finished without install & update.
```

There is also an RStudio addin hook to search the dependencies in the currently active window.

Dependencies are identified using by pattern matching against common methods: `library(...)`, `require(...)`, `p_load(..., ..., )`, `...::func()`

## Future Work
The plan is to extend this idea to:
   * Estimation of time until code can run based on analysis of recursive dependencies. 
