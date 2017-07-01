depl_2run <- function(){
  doc <- rstudioapi::getActiveDocumentContext()$contents
  lib_list <- find_doc_libs(doc)

  install_status <-
    tibble::tibble(
      package = lib_list,
      installed = are_installed(lib_list)
    )
  n_missing <- sum(!install_status$installed)
  if(n_missing > 0){
    message(sprintf("Found %i package(s) that need to be installed to run this script \n",n_missing))
      install_list <- install_status[install_status$installed == FALSE,]


      CRAN_df <- get_CRAN_data(install_list)
      if(nrow(CRAN_df) < n_missing){
        GH_df <- get_gh_DESCRIPTION_data()
      }


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

get_CRAN_data <- function(install_list){
  CRAN_packs <- available.packages(fields = "Remotes") %>% tibble::as_tibble()
  install_list$on_CRAN <- purrr::map_lgl(install_list$package, ~ . %in% CRAN_packs$Package )
  install_list$dependencies <-
    purrr::map(install_list$package, ~paste(na.omit(CRAN_packs$Depends[CRAN_packs$Package == .]),
                                       na.omit(CRAN_packs$Imports[CRAN_packs$Package == .]))) %>%
    purrr::map(~gsub(x = ., pattern = "R\\s*\\(>=\\s*[0-9.]+\\),*", replacement = "", perl = TRUE)) %>%
    purrr::map(~gsub(x = ., pattern = "\\(>=\\s*[0-9.]+\\)", replacement = "", perl = TRUE)) %>%
    purrr::map(~gsub(x = ., pattern = "\\s*", replacement = "")) %>%
    purrr::map(~strsplit(x = ., split = ",")) %>%
    purrr::map(unlist)

  install_list$to_install <-
    purrr::map(install_list$dependencies, ~.[!are_installed(.)])

  install_list[install_list$on_CRAN == TRUE, c("package","dependencies","to_install")]
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

append_gh_data <- function(install_list){
  gh_packs <- get_gh_pkgs()
  install_list$on_gh <- purrr::map_lgl(install_list$package, ~ . %in% gh_packs$pkg_name)
  installing_from_gh <- install_list$on_CRAN == FALSE & install_list$on_gh
  install_list[installing_from_gh] %>%
    purrr::map(get)
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
  names(desc_data) <- dimnames(desc_data)[[2]]
  desc_data <- as.list(desc_data)
  desc_data[c("Package","Imports","Depends","Remotes","Version")] %>%
    purrr::map(~strsplit(x = ., split = ",\n*"))
}
