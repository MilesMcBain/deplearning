#' Find libraries referenced in a document containing R code
#'
#' @param doc a character vector or list of character vectors containing R code.
#'
#' @return A vector of characters containing library references present in the Code
#' @export
#'
#' @examples
find_doc_libs <- function(doc){
  patterns <- list(
    library = "(?<=library\\()\"*[a-zA-Z0-9]+\"*(?=\\))",
    require   = "(?<=require\\()\"*[a-zA-Z0-9]+\"*(?=\\))",
    p_load = "(?<=p_load\\()\"*[a-zA-Z0-9]+\"*[\\s*\\,\\s*\"*[a-zA-Z0-9]+\"*]*(?=\\))",
    `::` = "[a-zA-Z0-9]+(?=::)"
  )
  match_pos <- purrr::map(patterns, ~regexec(., doc, perl = TRUE))
  lib_matches <- purrr::map(match_pos, ~regmatches(doc, .)) %>%
    lapply(unlist)

  #post processing cleanup of matches
  final_matches <-
    lib_matches %>%
    purrr::map(~gsub(x = ., pattern = "\"", replacement ="")) %>%
    purrr::map(~strsplit(x = . , split = ",\\s*" )) %>%
    unlist() %>%
    unique()

  final_matches
}

find_proj_libs <- function(path){}

