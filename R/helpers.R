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

get_R_dependency <- function(dep_spec){
  R_spec_match <- regexec(pattern = "R\\s*\\(>=\\s*([0-9.]+)\\)",
                          text = dep_spec
  )
  R_spec <- regmatches(dep_spec, R_spec_match)[[1]][[2]]
}
