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

    message("From CRAN: \n------------------------ \n")
      CRAN_packs <- available.packages() %>% tibble::as_tibble()
      install_list <- append_CRAN_data(install_list, CRAN_packs)

      purrr::pwalk(install_list[install_list$on_CRAN == TRUE,],
        function(package, to_install, ...){
          message(paste0(package,"\n"))
          if(length(to_install > 0 )){
            message(paste0("    - and missing deps: ",
                         paste0(to_install, collapse = " "), "\n"))
          }
        }
      )

    if(length(packs_from_CRAN < n_missing)){
      #We're gonna look on github as well

    }

  }else{
    print("Your library contains all packages mentioned this code. :)")
  }
}

append_CRAN_data <- function(install_list, CRAN_packs){

  install_list$on_CRAN <- purrr::map_lgl(install_list$package, ~ . %in% CRAN_packs$Package )
  install_list$dependencies <-
    purrr::map(install_list$package, ~CRAN_packs$Depends[CRAN_packs$Package == .]) %>%
    purrr::map(~gsub(x = ., pattern = "R\\s\\(.*\\),*", replacement = "")) %>%
    purrr::map(~gsub(x = ., pattern = "\\s*", replacement = "")) %>%
    purrr::map(~strsplit(x = ., split = ",")) %>%
    purrr::map(unlist)

  install_list$to_install <-
    purrr::map(install_list$dependencies, ~.[!are_installed(.)])

  install_list
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
