#' Check R source dependencies.
#'
#' `depl_check` analyses R source for dependency references:
#' `library(<package>)`, `require(<package>)`, `<package>::func`,
#' `pload(<package>, <package>)`. The dependencies are then located in either the
#' local library, CRAN, or GitHub. Installed versions are compared with repository versions,
#' with CRAN taking precendence over GitHub (Even if a local package was installed from GitHub).
#'
#' A report is output to the console containing information about missing and out of date dependencies.
#' If there are any missing or out of date, a prompt is made to automatically update and install dependencies.
#'
#' @param source_path a file or directory path.
#'
#' @return A list of dataframes containing package metadata. Convenient for diagnostics or further processing.
#' @export
#'
#' @examples
#' \dontrun{
#' depl_check("~/repos/my_analysis")
#' depl_check("./report.Rmd")
#' }
#'
depl_check <- function(source_path = "."){
  stopifnot(is.character(source_path))
  cat("[deplearning] Starting dependency check.\n")

  if(is_R_file(source_path)){
    cat("[deplearning] Scanning file", source_path)
    doc <- readLines(con = source_path)
    lib_list <- find_doc_libs(doc)

  }else{
    cat("[deplearning] Searching path", source_path, "\n")
    R_files <- dir(".", pattern = "\\.(rmd|r)$" , recursive = TRUE, ignore.case = TRUE)
    if(length(R_files > 0)){
      cat("[deplearning] Scanning", length(R_files), "R source files...")
      lib_list <-
        purrr::map(R_files, readLines) %>%
        purrr::map(find_doc_libs) %>%
        unlist() %>%
        unique()
    }else{
      cat("[deplearning] Did not find any R source files in", source_path, ".\n")
      return()
    }
  }
  cat(" done.\n")
  depl_check_run(lib_list)
}

#' Check R dependencies in the currently active Rstudio pane.
#'
#' See `depl_check()` for details.
#'
#' @return A list of dataframes containing package metadata. Convenient for diagnostics or further processing.
#' @export
depl_check_addin <- function(){
  cat("[deplearning] Starting check run.\n")
  cat("[deplearning] Scanning RStudio pane...")
  lib_list <- find_doc_libs(rstudioapi::getActiveDocumentContext()$contents)
  cat(" done.\n")
  depl_check_run(lib_list)
}

# Subfunctuion of depl_check to allow the same body to be used for RStudio addin
# and file path version.
depl_check_run <- function(lib_list){
  if(length(lib_list) == 0){
    cat("[deplearning] found no dependencies in this code.\n")
    return()
  }
  #remove base packages if present
  lib_list <- lib_list[!lib_list %in% c("base","tools","utils")]

  cat(sprintf("[deplearning] Found %i dependencies.\n", length(lib_list)))
  pkg_install_status <-
    tibble::tibble(
      package = lib_list,
      installed = are_installed(lib_list)
    )
  cat("[deplearning] Fetching remote data...")
  installed_pkg_df <- get_installed_data(pkg_install_status)
  n_behind_CRAN <- sum(installed_pkg_df$CRAN_up_to_date == -1, na.rm = TRUE)
  n_behind_GH <- sum(installed_pkg_df$GH_up_to_date == -1, na.rm = TRUE)
  n_missing <- sum(!pkg_install_status$installed)
  n_uptodate_installed <-
    sum(installed_pkg_df$CRAN_up_to_date >= 0, na.rm = TRUE) +
    sum(installed_pkg_df$GH_up_to_date >= 0, na.rm = TRUE)

  # Is there anything to install or update?
  if(n_missing + n_behind_CRAN + n_behind_GH > 0){
    install_candidates <- installed_pkg_df[which(installed_pkg_df$installed == FALSE |
                                               installed_pkg_df$CRAN_up_to_date == -1 |
                                               installed_pkg_df$GH_up_to_date == -1),]
    cat(" CRAN... ")
    CRAN_df <- get_CRAN_data(install_candidates[install_candidates$on_CRAN,])

    # Are there packages we need that are not found on CRAN? Try GitHub.
    if(nrow(CRAN_df) < length(lib_list)){
      cat("GitHub... ")
      GH_df <- get_gh_data(install_candidates[!(install_candidates$package %in% CRAN_df$package),c("package","installed","installed_ver")])
    } else GH_df <- data.frame()
    cat("done.\n")

    # Report installed and up to date dependencies.
    if(n_uptodate_installed > 0){
      cat("[deplearning] ",clisymbols::symbol$tick," ",n_uptodate_installed, " Installed and up to date.\n\n", sep = "")
      cat("", paste0(installed_pkg_df$package[which(installed_pkg_df$installed == TRUE &
                                                  (installed_pkg_df$CRAN_up_to_date >= 0 |
                                                     installed_pkg_df$GH_up_to_date >= 0))], collapse = ", "),"\n\n")
    }

    # Report dependencies found on CRAN that are behind CRAN version.
    if(n_behind_CRAN > 0){
      cat("[deplearning] ",clisymbols::symbol$cross," ",n_behind_CRAN, " Installed but behind CRAN release.\n\n", sep = "")
      print(as.data.frame(installed_pkg_df[which(installed_pkg_df$CRAN_up_to_date == -1),
                                       c("package", "installed_ver","CRAN_ver")]),
            row.names = FALSE)
      cat("\n")
    }

    # Report Dependencies found on Github (not on CRAN), that are behind
    # Github version.
    if(n_behind_GH > 0){
      cat("[deplearning] ",clisymbols::symbol$cross," ",n_behind_GH, " Installed but behind GitHub version.\n\n", sep = "")
      print(as.data.frame(installed_pkg_df[which(installed_pkg_df$GH_up_to_date == -1),
                                       c("GH_repository", "installed_ver","GH_ver")]),
            row.names = FALSE)
      cat("\n")
    }

    # Report uninstalled dependencies found on CRAN.
    if(sum(!CRAN_df$installed) > 0){
      cat("[deplearning] ", clisymbols::symbol$cross," ", sum(!CRAN_df$installed), " Missing CRAN packages.\n\n", sep = "")
      cat(" ", paste0(CRAN_df$package[!CRAN_df$installed], collapse = ", "), "\n\n")
    }

    # Report uninstalled dependencies found on Github (not on CRAN).
    if(sum(!GH_df$installed) > 0){
      cat("[deplearning] ", clisymbols::symbol$cross," ", sum(!GH_df$installed), " Missing GitHub packages.\n\n", sep = "")
      cat(" ", paste0(GH_df$repository[!GH_df$installed], collapse = ", "), "\n\n")
    }

    # Report dependencies that could not be found in a tracked repository.
    # Tracked repositories are CRAN and GitHub.
    lost_pkgs <- lib_list[!(lib_list %in% c(installed_pkg_df$package[installed_pkg_df$installed], CRAN_df$package, GH_df$package))]
    if(length(lost_pkgs) > 0){
      cat("[deplearning] ", clisymbols::symbol$cross," ", length(lost_pkgs), " Missing packages from untracked repositories.\n\n", sep = "")
      cat(" ", paste0(lost_pkgs, collapse = ", "), "\n\n")
    }

    # If we found any packages that are uninstalled or are behind repository version:
    # * Check that the installed R version is compatible with maximum
    #   R dependency. I.e. that we can install & update without updating R.
    # If install & update is possible:
    # * If GitHub packages need to be installed, check that devtools is intalled,
    #   If not, add it to CRAN dependencies and report this.
    # * Report the set of all new recursive dependencies from CRAN and GitHub
    #   that would be installed along with detected dependencies.
    # * Prompt user with choice to install & update missing and out of date
    #   dependencies. Install from CRAN, then GitHub as required.
    if((nrow(GH_df) + nrow(CRAN_df)) > 0){
      # Report minimum R version to install & update.
      required_R_ver <- max_R_version(c(CRAN_df$R_ver, GH_df$R_ver))
      current_R_ver <- paste0(version$major,".",version$minor)
      cat("[deplearning] ",ifelse(compare_version(current_R_ver,required_R_ver) <= 0,
                                  clisymbols::symbol$cross,
                                  clisymbols::symbol$tick),
          " Minimum R version to update & install is ", required_R_ver, ", you have ", current_R_ver,".\n",sep="")

      if(compare_version(current_R_ver,required_R_ver) >= 0){

        if(nrow(GH_df) > 0){
          GH_df_recur_deps_CRAN <- unlist(purrr::map(GH_df$recur_dependencies, `[`, "CRAN_deps"))
          GH_df_recur_deps_GH <- unlist(purrr::map(GH_df$recur_dependencies, `[`, "GH_deps"))

          # If we might have to install Github dependencies, check for devtools and
          # add it as a CRAN dependency if not installed.
          if(!requireNamespace("devtools", quietly = TRUE)){
            cat("[deplearning] ",clisymbols::symbol$info,"  devtools package is required to install from GitHub. Added to CRAN dependencies.", sep = "")
            CRAN_df[nrow(CRAN_df)+1, "package"] <- "devtools"
            CRAN_df[nrow(CRAN_df)+1, "recur_dependencies"] <- tools::package_dependencies(packages = "devtools",
                                                                                        recursive = TRUE)
          }
        }else{
          GH_df_recur_deps_CRAN <- vector()
          GH_df_recur_deps_GH <- vector()
        }
        if(nrow(CRAN_df) > 0){
          CRAN_df_recur_deps <- unlist(CRAN_df$recur_dependencies)
        }else{
          CRAN_df_recur_deps <- vector()
        }

        # Deterimine missing recursive dependencies
        # if else guards around deps vectors ensure they are not NULL and
        # can be passed to below.
        all_recur_CRAN_deps <- unique(c(GH_df_recur_deps_CRAN,
                                 CRAN_df_recur_deps))

        if(length(all_recur_CRAN_deps) > 0 ){
          #Filter out installed recusrive dependencies.
          missing_recur_CRAN_deps <- all_recur_CRAN_deps[!are_installed(all_recur_CRAN_deps)]
        } else{
          missing_recur_CRAN_deps <- list()
        }
        all_recur_GH_deps <- unique(GH_df_recur_deps_GH)

        if(length(all_recur_GH_deps) > 0){
          # Get just the package name so we can filter out installed recusrive
          # dependencies.
          GH_pack_names <- all_recur_GH_deps %>%
            strsplit(split="/") %>%
            purrr::map(`[`,2)
          # Check the Github
          missing_recur_GH_deps <- all_recur_GH_deps[!are_installed(GH_pack_names)]

        }else{
          missing_recur_GH_deps <- list()
        }
        all_missing_recur_deps <- c(missing_recur_CRAN_deps, missing_recur_GH_deps)

        # Report missing recursive dependencies that will be installed if choice
        # is made to install & update.
        if(length(all_missing_recur_deps) > 0){
          cat("[deplearning] ",clisymbols::symbol$info,"  Update & install will include ", length(all_missing_recur_deps)," new recursive dependencies.\n\n", sep = "")
          cat(" ", paste0(all_missing_recur_deps, collapse = ", "), "\n\n")
        }

        # Prompt for choice to install and update
        cat("[deplearning] Would you like to update & install old and missing dependencies?\n")
        install_choice <- menu(c("Yes", "No"))
        if(install_choice == 1){
          #install CRAN deps
          if(nrow(CRAN_df) > 0){
            cat("[deplearning] Installing from CRAN.\n")
            install.packages(CRAN_df$package)
          }
          if(nrow(GH_df) > 0){
            cat("[deplearning] Installing from GitHub.\n")
            if(requireNamespace("devtools")){
              purrr::walk(GH_df$repository, ~devtools::install_github(.))
            }
          }
          cat("[deplearning] Dependency check finished install & update. Check for errors/warnings.\n")
        }else{
          cat("[deplearning] Dependency check finished without install & update.\n")
        }

      }else{
        # The R version is too old to update some dependencies
        cat("[deplearning] R version needs to be updated to install & update dependencies.")
        cat("[deplearning] Dependency check finished without install & update.\n")
      }
    }
  }else{
    # All dependencies present and and up to date.
    cat(" done.\n")
    cat("[deplearning] ", clisymbols::symbol$cross, " All available dependencies installed & up to date.\n\n", sep = "")
    cat(" ",paste0(lib_list,collapse = ", "),"\n", sep="")
  }

  # Finally return the lists of packages found installed, and in tracked repositories
  # for diagnostics or further processing. These may be empty dataframes.
  invisible(list(install_status = installed_pkg_df,  CRAN_packs = CRAN_df, GH_packs = GH_df))
}

# This helper appends metadata from CRAN and GitHub to locally installed dependencies.
get_installed_data <- function(installed_list){
  CRAN_packs <- get_CRAN_pkgs()
  installed_list$on_CRAN <-
    purrr::map_lgl(installed_list$package, ~ . %in% CRAN_packs$Package )
  installed_list$installed_ver <-
    map_ifelse( .x = installed_list$package,
                .p = installed_list$installed,
                .f = ~packageDescription(., fields = "Version", drop = TRUE),
                .e = NA) %>% unlist()
  installed_list$CRAN_ver <-
    map_ifelse(.x = installed_list$package,
               .p = installed_list$on_CRAN,
               .f = ~CRAN_packs$Version[CRAN_packs$Package == .],
               .e = NA) %>% unlist()
  installed_list$GH_acct <-
    map_ifelse(.x = installed_list$package,
               .p = !installed_list$on_CRAN & installed_list$installed,
               .f = ~packageDescription(., fields = "GithubUsername", drop = TRUE),
               .e = NA) %>% unlist()
  installed_list$GH_packname <-
    map_ifelse(.x = installed_list$package,
               .p = !installed_list$on_CRAN & installed_list$installed,
               .f = ~packageDescription(., fields = "GithubRepo", drop = TRUE),
               .e = NA) %>% unlist()
  installed_list$GH_repository <-
    map_ifelse(.x = paste0(installed_list$GH_acct,"/",installed_list$GH_packname),
               .p = !is.na(installed_list$GH_packname),
               .f = ~.,
               .e = NA) %>% unlist()
  installed_list$GH_ver <-
    map_ifelse(.x = installed_list$GH_repository,
               .p = !is.na(installed_list$GH_packname),
               .f = ~get_gh_DESCRIPTION_data(.)$version,
               .e = NA) %>% unlist()
  installed_list$CRAN_up_to_date <-
    purrr::map2(.x = installed_list$installed_ver,
                .y = installed_list$CRAN_ver,
                .f = compare_version) %>% unlist()
  installed_list$GH_up_to_date <-
    purrr::map2( .x = installed_list$installed_ver,
                 .y = installed_list$GH_ver,
                 .f = compare_version) %>% unlist()
  installed_list

}




















