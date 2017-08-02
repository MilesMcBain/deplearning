#' Pipe operator
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

# Like map_if but with the option to specify a value for false cases.
# E.g. NA, character(0) etc.
map_ifelse <- function(.x, .p, .f, .e, ...) {
  if (purrr::is_logical(.p)) {
    stopifnot(length(.p) == length(.x))
    sel <- .p
  } else {
    sel <- purrr::map_lgl(.x, .p, ...)
  }
  .x[sel] <- purrr::map(.x[sel], .f, ...)
  .x[!sel] <- .e
  .x
}

#A compare version that bails if either arg is NA
compare_version <- function(a, b){
  if(anyNA(c(a,b))){ NA }
  else{ compareVersion(a,b) }
}

sanitise_deps <- function(deps){
  dep_ver_matches <- regexec(pattern = "\\s*\\(>=\\s*[0-9.]+\\)",
          text = deps)
  #Remove Versions
  regmatches(deps, dep_ver_matches) <- ""

  #find and remove R
  R_matches <- regexec(pattern = "R\\S*", text = deps)
  deps <- deps[R_matches == -1]

}

max_R_version <- function(vers){
  vers <- sanitise_versions(vers)
  vers_comps  <-
    vers %>%
    purrr::map(~strsplit(x = ., split = "\\.")) %>%
    purrr::flatten()
  vers_major <-
    vers_comps %>%
    purrr::map(`[[`,1) %>%
    purrr::flatten() %>%
    as.numeric()
  vers_minor <-
    vers_comps %>%
    purrr::map(`[`,c(2,3)) %>%
    purrr::map(~paste0(., collapse=".")) %>%
    purrr::flatten() %>%
    as.numeric()
  max_ver <- vers[order(vers_major, vers_minor, decreasing = TRUE)][[1]]
}

sanitise_versions <- function(vers){
num_dots <-
    vers %>%
    purrr::map(~gregexpr(text = ., pattern = "\\.", perl=TRUE)) %>%
    purrr::flatten() %>%
    purrr::map(length)
vers[num_dots < 2] <- paste0(vers[num_dots < 2],".0")
vers
}

get_R_dependency <- function(dep_spec){
  if(!(is.character(dep_spec) & length(dep_spec) > 0)) return("0.0.0")
  R_spec_match <- regexec(pattern = "R\\s*\\(>=\\s*([0-9.]+)\\)",
                          text = dep_spec
  )
  if(any(unlist(R_spec_match) == 1)){
    R_spec <- regmatches(dep_spec, R_spec_match)[[1]][[2]]
  }else{
    "0.0.0"
  }
}

is_R_file <- function(filename){
  regexpr(pattern =  "\\.([Rr]{1}[Mm]{1}[Dd]{1})|([Rr]{1})$",
          text = filename) > 0
}

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


