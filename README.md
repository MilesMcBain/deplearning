# deplearning
Utilise the synergy of algorithms and heuristics to unlock knowledge of R script/package dependencies.

# Usage

WORK IN PROGRESS

## Identify and Analyse Dependencies
`depl_check_run()` will examine the code in the currently active RStudio pane and produce a dependency report:

```
> depl_check_run()
[deplearning] Starting check_run.
[deplearning] Found 14 dependencies.
[deplearning] Fetching remote data... done.
[deplearning] ✔ 7 Installed and up to date.

 datapasta, tidyr, rstudioapi, dplyr, xml2, jsonlite, devtools 

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
```

Dependencies are identified using pattern matching against common methods: `library(...)`, `require(...)`, `p_load(..., ..., )`, `...::func()`

## Future Work
The plan is to extend this idea to:
   
   * Analysing a file argument or all files below a root folder argument.
   * A prompt to automatically update and installed required dependencies.
   * Estimation of time until code run based on analysis recursive dependencies. 
