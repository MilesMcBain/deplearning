# Given a data.frame containing a package column, append GitHub metadata for
# each package.
get_gh_data <- function(GH_install_candidates){
  gh_packs <- get_gh_pkgs(GH_install_candidates$package)
  GH_install_candidates$on_GH <- purrr::map_lgl(GH_install_candidates$package, ~ . %in% gh_packs$pkg_name)

  # Filter out packages not on Github
  GH_install_candidates <- GH_install_candidates[GH_install_candidates$on_GH,]

  # If no packages were found on Github, return a empty dataframe with
  # consistent structure.
  if(!any(GH_install_candidates$on_GH)){
    GH_install_candidates$repository <- character(0)
    GH_install_candidates$R_ver <- character(0)
    GH_install_candidates$recur_dependencies <- character(0)
    return(GH_install_candidates)
  }
  GH_install_candidates$repository <-
    purrr::map(GH_install_candidates$package, ~gh_packs$pkg_location[gh_packs$pkg_name == .]) %>%
    purrr::map_chr(`[`,1) # could return multiple repositories, if it has moved it will have a redirect anyway.
  GH_install_candidates$description_data <-
    purrr::map(GH_install_candidates$repository, get_gh_DESCRIPTION_data)
  GH_install_candidates$R_ver <-
    purrr::map_chr(GH_install_candidates$description_data, ~get_R_dependency(.$depends))
  GH_install_candidates$recur_dependencies <-
    purrr::map(GH_install_candidates$description_data, gh_recursive_deps)

  GH_install_candidates
}

# Given a package repository, read the DESCRIPTION file from GitHub and
# get dependencies and version.
get_gh_DESCRIPTION_data <- function(repo){
  desc_url = url(paste0("https://raw.githubusercontent.com/",repo,"/master/DESCRIPTION"))
  desc_data <- read.dcf(desc_url)
  close(desc_url)
  names(desc_data) <- dimnames(desc_data)[[2]] # Fix structure of this wacky list.
  desc_data <- as.list(desc_data)
  compact_data <- desc_data[c("Package","Imports","Depends","LinkingTo","Remotes","Version")] %>%
    purrr::map(~ifelse(is.null(.),"",.)) %>%
    purrr::map(~strsplit(x = ., split = ",\n*")) %>%
    purrr::map(unlist)
  names(compact_data) <- c("package","imports","depends","linkingto","remotes","version")
  compact_data
}

# A recursive function to follow and collect DESCRIPTION data from Github
# packages referenced in the LinkingTo field. Not protected against stack overflow, but deep
# recursion in this field is unlikely.
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

# Given dependencies from a DESCRIPTION file, determine the recursive dependencies from
# CRAN and GitHub.
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
      sanitise_deps( c(description_data$depends,
                       description_data$imports,
                       description_data$linkingto)) ) %>%
    unique() %>%
    purrr::map(~tools::package_dependencies(packages = ., recursive = TRUE)) %>%
    unlist() %>%
    unique()


  result <- list(CRAN_deps = CRAN_deps, GH_remotes = GH_remotes)
  result
}

# Adapted from: jimhester/autoinst/R/package.R
# Call the Gepuro package list API to find github repository associated
# with a package. Where multiple exact matches are found it just chooses the first. Forks
# are not included in the search.
get_gh_pkgs <- function(package_list){
  res <-
    purrr::map(package_list,
               ~get_gepuro_data(.)) %>%
    purrr::map(head,1) %>%
    purrr::reduce(rbind, .init=tibble::tibble())
  if(nrow(res) > 0){
    res$pkg_location <- res$pkg_name
    res$pkg_org <- vapply(strsplit(res$pkg_location, "/"), `[[`, character(1), 1)
    res$pkg_name <- vapply(strsplit(res$pkg_location, "/"), `[[`, character(1), 2)
    res
  }
  else{
    res <- list()
  }
}

# The gepuro list is used as a name resolution database for mapping package names
# to github repositories. @Gepuro has kindly provided a search API.
get_gepuro_data <- memoise::memoise(function(package_name, mirrors = FALSE){
  query_url <- "http://rpkg-api.gepuro.net/rpkg?q="
  search_result <- jsonlite::fromJSON(paste0(query_url, package_name))
  if(length(search_result) > 0){
    exact_matches <- grepl(pattern = paste0(".*/",package_name,"$"),
                           x = search_result$pkg_name)
    search_result <- search_result[exact_matches,]
    if(!mirrors){
      mirror_matches <- grepl(pattern = "^cran|^Bioconductor-mirror",
                              x = search_result$pkg_name)
      search_result <- search_result[!mirror_matches,]
    }
    if(nrow(search_result) == 0){
      search_result <- list()
    }
  }
  search_result
})
