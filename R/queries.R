depl_check <- function(source_path = "."){
  stopifnot(is.character(source_path))
  cat("[deplearning] Starting check_run.\n")

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

depl_check_addin <- function(){
  cat("[deplearning] Starting check run.\n")
  cat("[deplearning] Scanning RStudio pane...")
  lib_list <- find_doc_libs(rstudioapi::getActiveDocumentContext()$contents)
  cat(" done.\n")
  depl_check_run(lib_list)
}

depl_check_run <- function(lib_list){
  if(length(lib_list) == 0){
    cat("[deplearning] found no depenencies in this code.\n")
    return()
  }
  cat(sprintf("[deplearning] Found %i dependencies.\n", length(lib_list)))
    install_status <-
      tibble::tibble(
        package = lib_list,
        installed = are_installed(lib_list)
      )
  cat("[deplearning] Fetching remote data...")
    installed_df <- get_installed_data(install_status)
    n_behind_CRAN <- sum(installed_df$CRAN_up_to_date == -1, na.rm = TRUE)
    n_behind_GH <- sum(installed_df$GH_up_to_date == -1, na.rm = TRUE)
    n_missing <- sum(!install_status$installed)
    n_uptodate_installed <- length(lib_list) - (n_missing + n_behind_CRAN + n_behind_GH)

    if(n_missing + n_behind_CRAN + n_behind_GH > 0){
      install_candidates <- installed_df[which(installed_df$installed == FALSE |
                                       installed_df$CRAN_up_to_date == -1 |
                                       installed_df$GH_up_to_date == -1),]
      CRAN_df <- get_CRAN_data(install_candidates[install_candidates$on_CRAN,])
      if(nrow(CRAN_df) < n_missing + n_behind_CRAN + n_behind_GH){
         GH_df <- get_gh_data(install_candidates[!(install_candidates$package %in% CRAN_df$package),c("package","installed","installed_ver")])
      } else GH_df <- data.frame()
      cat(" done.\n")
      if(n_uptodate_installed > 0){
         cat("[deplearning] ",clisymbols::symbol$tick," ",n_uptodate_installed, " Installed and up to date.\n\n", sep = "")
         cat("", paste0(installed_df$package[which(installed_df$installed == TRUE &
                                                     (installed_df$CRAN_up_to_date >= 0 |
                                                     installed_df$GH_up_to_date >= 0))], collapse = ", "),"\n\n")
       }
       if(n_behind_CRAN > 0){
         cat("[deplearning] ",clisymbols::symbol$cross," ",n_behind_CRAN, " Installed but behind CRAN release.\n\n", sep = "")
         print(as.data.frame(installed_df[which(installed_df$CRAN_up_to_date == -1),
                                          c("package", "installed_ver","CRAN_ver")]),
               row.names = FALSE)
         cat("\n")
       }
      if(n_behind_GH > 0){
          cat("[deplearning] ",clisymbols::symbol$cross," ",n_behind_GH, " Installed but behind GitHub version.\n\n", sep = "")
          print(as.data.frame(installed_df[which(installed_df$GH_up_to_date == -1),
                                           c("GH_repository", "installed_ver","GH_ver")]),
                row.names = FALSE)
          cat("\n")
      }
      if(sum(!CRAN_df$installed) > 0){
        cat("[deplearning] ", clisymbols::symbol$cross," ", sum(!CRAN_df$installed), " Missing CRAN packages.\n\n", sep = "")
        cat(" ", paste0(CRAN_df$package[!CRAN_df$installed], collapse = ", "), "\n\n")
      }
      if(sum(!GH_df$installed) > 0){
        cat("[deplearning] ", clisymbols::symbol$cross," ", sum(!GH_df$installed), " Missing GitHub packages.\n\n", sep = "")
        cat(" ", paste0(GH_df$repository[!GH_df$installed], collapse = ", "), "\n\n")
      }
      lost_pkgs <- lib_list[!(lib_list %in% c(installed_df$package[installed_df$installed], CRAN_df$package, GH_df$package))]
      if(length(lost_pkgs) > 0){
        cat("[deplearning]", clisymbols::symbol$cross, length(lost_pkgs), "Missing packages from untracked repositories.\n\n")
        cat(" ", paste0(lost_pkgs, collapse = ", "), "\n\n")
      }
      required_R_ver <- max_R_version(c(CRAN_df$R_ver, GH_df$R_ver))
      current_R_ver <- paste0(version$major,".",version$minor)
      cat("[deplearning] ",ifelse(compare_version(current_R_ver,required_R_ver) <= 0,
                                 clisymbols::symbol$cross,
                                 clisymbols::symbol$tick),
          " Minimum R version to update & install is ", required_R_ver, ", you have ", current_R_ver,".\n",sep="")

    }else{
      cat(" done.\n")
      cat("[deplearning] ", clisymbols::symbol$cross, " All dependencies installed & up to date.\n\n", sep = "")
      cat(" ",paste0(lib_list,collapse = ", "),"\n", sep="")
  }
}

get_CRAN_data <- function(CRAN_install_candidates){
  CRAN_packs <- get_CRAN_pkgs()
  CRAN_install_candidates$R_ver <-
    purrr::map_chr(CRAN_install_candidates$package, ~get_R_dependency(CRAN_packs$Depends[CRAN_packs$Package == .]))
  CRAN_install_candidates$recur_dependencies <-
    tools::package_dependencies(packages = CRAN_install_candidates$package,
                                recursive = TRUE)
 CRAN_install_candidates
}

are_installed <- function(pack_list){
  if(is.character(pack_list) & !is.null(pack_list)){
    purrr::map_lgl(pack_list, ~find_package(package = .))
  }else{
    logical(0)
  }
}

find_package <- function(package){
  result <- find.package(package = package, quiet = TRUE)
  ifelse(length(result > 0), TRUE, FALSE)
}

get_gh_data <- function(GH_install_candidates){
  gh_packs <- get_gh_pkgs()
  GH_install_candidates$on_GH <- purrr::map_lgl(GH_install_candidates$package, ~ . %in% gh_packs$pkg_name)
  GH_install_candidates <- GH_install_candidates[GH_install_candidates$on_GH,]
  if(!any(GH_install_candidates$on_GH)){
    return(GH_install_candidates)
  }
  GH_install_candidates$repository <-
    purrr::map(GH_install_candidates$package, ~gh_packs$pkg_location[gh_packs$pkg_name == .]) %>%
    purrr::map_chr(`[`,1) #could return multiple repositories, if it has moved it will have a redirect anyway.
  GH_install_candidates$description_data <-
    purrr::map(GH_install_candidates$repository, get_gh_DESCRIPTION_data)
  GH_install_candidates$R_ver <-
    purrr::map_chr(GH_install_candidates$description_data, ~get_R_dependency(.$depends))
  GH_install_candidates$recur_dependencies <-
    purrr::map(GH_install_candidates$description_data, gh_recursive_deps)

  GH_install_candidates
}

# From: jimhester/autoinst/R/package.R
get_gh_pkgs <- memoise::memoise(function() {
  res <- jsonlite::fromJSON("http://rpkg.gepuro.net/download")
  res <- res$pkg_list
  res$pkg_location <- res$pkg_name
  res$pkg_org <- vapply(strsplit(res$pkg_location, "/"), `[[`, character(1), 1)
  res$pkg_name <- vapply(strsplit(res$pkg_location, "/"), `[[`, character(1), 2)
  res[!(res$pkg_org == "cran" | res$pkg_org == "Bioconductor-mirror"), ]
})

get_CRAN_pkgs <- memoise::memoise(function(){
  tibble::as_tibble(available.packages())
})

get_gh_DESCRIPTION_data <- function(repo){
  desc_url = url(paste0("https://raw.githubusercontent.com/",repo,"/master/DESCRIPTION"))
  desc_data <- read.dcf(desc_url)
  close(desc_url)
  names(desc_data) <- dimnames(desc_data)[[2]]
  desc_data <- as.list(desc_data)
  compact_data <- desc_data[c("Package","Imports","Depends","LinkingTo","Remotes","Version")] %>%
    purrr::map(~ifelse(is.null(.),"",.)) %>%
    purrr::map(~strsplit(x = ., split = ",\n*")) %>%
    purrr::map(unlist)
  names(compact_data) <- c("package","imports","depends","linkingto","remotes","version")
  compact_data
}

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

gh_recursive_remotes <- function(repo, repo_list = new.env()){
  #TODO replace with safer Recursion. :(
  if(!exists(x = repo, envir = repo_list, inherits = FALSE)){
    assign(x = repo, value = get_gh_DESCRIPTION_data(repo), envir = repo_list)
    if(length(get(x = repo, envir = repo_list)$remotes) > 0){
      purrr::walk(get(x = repo, envir = repo_list)$remotes, ~gh_recursive_remotes(.,repo_list))
    }
  }
  as.list(repo_list)
}

gh_recursive_deps <- function(description_data){
  CRAN_deps <- vector()
  GH_remotes <- vector()

  if(length(description_data$remotes) > 0){
    gh_deps <- purrr::map(description_data$remotes, gh_recursive_remotes) %>%
      purrr::flatten()

    CRAN_deps <-
      gh_deps %>%
      purrr::map(`[`, c("depends","imports","linkingto")) %>%
      unlist() %>%
      sanitise_deps() %>%
      unique()

    GH_remotes <-
      gh_deps %>%
      purrr::map(`[`, "remotes") %>%
      unlist()
  }

  CRAN_deps <-
    c(CRAN_deps,
      sanitise_deps(c(description_data$depends, description_data$imports,
                    description_data$linkingto))
      ) %>%
      unique()

 result <- list(CRAN_deps = CRAN_deps, GH_remotes = GH_remotes)
 result
}


















