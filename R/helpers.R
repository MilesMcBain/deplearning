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

# A compare version that returns NA if either arg is NA.
compare_version <- function(a, b){
  if(anyNA(c(a,b))){ NA }
  else{ compareVersion(a,b) }
}

# Remove R (>= <ver>) and other version qualifiers from
# list of package dependencies.
sanitise_deps <- function(deps){
  dep_ver_matches <- regexec(pattern = "\\s*\\(>=\\s*[0-9.]+\\)",
          text = deps)
  #Remove Versions
  regmatches(deps, dep_ver_matches) <- ""

  #find and remove R
  R_matches <- regexec(pattern = "R\\S*", text = deps)
  deps <- deps[R_matches == -1]

}

# Given a list of R version dependencies, find the maximum version.
# Split 2 lists of major integer and minor decimal and order.
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

# Given a list of version numbers, make them sets of
# 3 numbers delimited by dots. Some early R versions use 2 numbers.
sanitise_versions <- function(vers){
num_dots <-
    vers %>%
    purrr::map(~gregexpr(text = ., pattern = "\\.", perl=TRUE)) %>%
    purrr::flatten() %>%
    purrr::map(length)
vers[num_dots < 2] <- paste0(vers[num_dots < 2],".0")
vers
}

# Given a list of Depends entries, extract the version associated
# with R or return version 0.0.0.
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

# Test if a file has an rmd or R extension.
is_R_file <- function(filename){
  regexpr(pattern =  "\\.([Rr]{1}[Mm]{1}[Dd]{1})|([Rr]{1})$",
          text = filename) > 0
}

# Given a list or vector of package names, determine if they are installed,
# returning a logical vector of indicators.
are_installed <- function(pack_list){
  if(is.character(pack_list) & !is.null(pack_list)){
    purrr::map_lgl(pack_list, ~find_package(package = .))
  }else{
    logical(0)
  }
}

# Given a package name, determine if it is installed, returning a logical indicator.
find_package <- function(package){
  result <- find.package(package = package, quiet = TRUE)
  ifelse(length(result > 0), TRUE, FALSE)
}




