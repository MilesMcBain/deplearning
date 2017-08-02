# Given a data.frame containing a package column, append package dependency metadata from
# CRAN to all packages.
get_CRAN_data <- function(CRAN_install_candidates){
  CRAN_packs <- get_CRAN_pkgs()
  CRAN_install_candidates$R_ver <-
    purrr::map_chr(CRAN_install_candidates$package, ~get_R_dependency(CRAN_packs$Depends[CRAN_packs$Package == .]))
  CRAN_install_candidates$recur_dependencies <-
    tools::package_dependencies(packages = CRAN_install_candidates$package,
                                recursive = TRUE)
  CRAN_install_candidates
}

# Fetch available CRAN packages.
get_CRAN_pkgs <- memoise::memoise(function(){
  tibble::as_tibble(available.packages())
})
