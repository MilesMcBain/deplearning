depl_check_run <- function(){
  doc <- rstudioapi::getActiveDocumentContext()$contents
  lib_list <- find_doc_libs(doc)

  install_status <-
    tibble::tibble(
      package = lib_list,
      installed = are_installed(lib_list)
    )
  installed_df <- get_installed_data(install_status[install_status$installed == TRUE,])

  n_missing <- sum(!install_status$installed)
  if(n_missing > 0){
    message(sprintf("Found %i package(s) that need to be installed to run this script \n",n_missing))
      install_list <- install_status[install_status$installed == FALSE,]

      CRAN_df <- get_CRAN_data(install_list)
      if(nrow(CRAN_df) < n_missing){
         GH_df <- get_gh_data(install_list[!(install_list$package %in% CRAN_df$package),])
      } else GH_df <- NA



      # message("From CRAN: \n------------------------ \n")

      # purrr::pwalk(install_list[install_list$on_CRAN == TRUE,],
      #   function(package, to_install, ...){
      #     message(paste0(package,"\n"))
      #     if(length(to_install > 0 )){
      #       message(paste0("    - and missing deps: ",
      #                    paste0(to_install, collapse = " "), "\n"))
      #     }
      #   }
      # )

    if(sum(install_list$on_CRAN) < n_missing){
      #We're gonna look on github as well

      message("From GitHub: \n------------------------ \n")

        install_list <- append_gh_data(install_list)


    }

  }else{
    print("Your library contains all packages mentioned this code. :)")
  }
}

get_CRAN_data <- function(CRAN_install_list){
  CRAN_packs <- available.packages() %>% tibble::as_tibble()
  CRAN_install_list$on_CRAN <- purrr::map_lgl(CRAN_install_list$package, ~ . %in% CRAN_packs$Package )
  CRAN_install_list <- CRAN_install_list[CRAN_install_list$on_CRAN,]
  CRAN_install_list$R_ver <-
    purrr::map_chr(CRAN_install_list$package, ~get_R_dependency(CRAN_packs$Depends[CRAN_packs$Package == .]))
  CRAN_install_list$recur_dependencies <-
    tools::package_dependencies(packages = CRAN_install_list$package,
                                recursive = TRUE)
 CRAN_install_list
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

get_gh_data <- function(GH_install_list){
  gh_packs <- get_gh_pkgs()
  GH_install_list$on_gh <- purrr::map_lgl(GH_install_list$package, ~ . %in% gh_packs$pkg_name)
  GH_install_list <- GH_install_list[GH_install_list$on_gh,]
  GH_install_list$repository <-
    purrr::map(GH_install_list$package, ~gh_packs$pkg_location[gh_packs$pkg_name == .]) %>%
    purrr::map_chr(`[`,1) #could return multiple repositories, if it has moved it will have a redirect anyway.
  GH_install_list$description_data <-
    purrr::map(GH_install_list$repository, get_gh_DESCRIPTION_data)
  GH_install_list$R_ver <-
    purrr::map_chr(GH_install_list$description_data, ~get_R_dependency(.$depends))
  GH_install_list$CRAN_dependencies <-
    purrr::map(GH_install_list$description_data, ~c(.$depends, .$imports, .$linkingto))
  GH_install_list$GH_dependencies <-
    purrr::map(GH_install_list$description_data, ~.$remotes)

  GH_install_list
}

# From: jimhester/autoinst/R/package.R
get_gh_pkgs <-function() {
  res <- jsonlite::fromJSON("http://rpkg.gepuro.net/download")
  res <- res$pkg_list
  res$pkg_location <- res$pkg_name
  res$pkg_org <- vapply(strsplit(res$pkg_location, "/"), `[[`, character(1), 1)
  res$pkg_name <- vapply(strsplit(res$pkg_location, "/"), `[[`, character(1), 2)
  res[!(res$pkg_org == "cran" | res$pkg_org == "Bioconductor-mirror"), ]
}

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
  CRAN_packs <- available.packages() %>% tibble::as_tibble()
  installed_list$on_CRAN <-
    purrr::map_lgl(installed_list$package, ~ . %in% CRAN_packs$Package )
  installed_list$installed_ver <-
    purrr::map_chr(installed_list$package, ~packageDescription(., fields = "Version", drop = TRUE))
  installed_list$CRAN_ver <-
    map_ifelse(.x = installed_list$package,
               .p = installed_list$on_CRAN,
               .f = ~CRAN_packs$Version[CRAN_packs$Package == .],
               .e = NA) %>% unlist()
  installed_list$GH_acct <-
    map_ifelse(.x = installed_list$package,
               .p = !installed_list$on_CRAN,
               .f = ~packageDescription(., fields = "GithubUsername", drop = TRUE),
               .e = NA) %>% unlist()
  installed_list$GH_repo <-
    map_ifelse(.x = installed_list$package,
               .p = !installed_list$on_CRAN,
               .f = ~packageDescription(., fields = "GithubRepo", drop = TRUE),
               .e = NA) %>% unlist()
  installed_list$GH_ver <-
    map_ifelse(.x = paste0(installed_list$GH_acct,"/",installed_list$GH_repo),
               .p = !is.na(installed_list$GH_repo),
               .f = ~get_gh_DESCRIPTION_data(.)$version,
               .e = NA) %>% unlist()
  installed_list$CRAN_up_to_date <-
    purrr::map2(.x = installed_list$CRAN_ver,
                .y = installed_list$installed_ver,
                .f = compare_version) %>% unlist()
  installed_list$GH_up_to_date <-
    purrr::map2( .x = installed_list$installed_ver,
                 .y = installed_list$GH_ver,
                 .f = compare_version) %>% unlist()
  installed_list
}

gh_recursive_remotes <- function(repo){
  deps <- list()
  deps[[1]] <- get_gh_DESCRIPTION_data(repo)
  if(length(deps[[1]]$remotes) > 0){
    results <- purrr::map(deps[[1]]$remotes, ~gh_recursive_remotes(.)) %>%
      purrr::flatten()
  } else{ results <- list() }
  return_list <- c(deps, results)
  return_list
}

gh_recursive_deps <- function(description_data){

  if(length(description_data$remotes) > 0){
    gh_deps <-
      purrr::map(description_data$remotes, gh_recursive_remotes) %>%
      flatten()
  }
  deps <- c(description_data$depends, description_data$imports,
            description_data$linkingto)
  sanitise_deps(deps)

}

get_R_dependency <- function(dep_spec){
  R_spec_match <- regexec(pattern = "R\\s*\\(>=\\s*([0-9.]+)\\)",
          text = dep_spec
          )
  R_spec <- regmatches(dep_spec, R_spec_match)[[1]][[2]]
}




















