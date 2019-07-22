#' Locate the position of one of a series of patterns in a string.
#' 
#' Vectorized over \code{string}; compares each element of \code{string} to every element of \code{patterns}.
#' Expects that only one of the patterns provided will match the input string,
#' and returns the position of said match. If the match is of length 0,
#' (e.g. from a special match like $) end will be one character less than start.
#' If more than one of patterns provided match input string, returns position of first
#' matching element from patterns provided. Wraps \code{\link[stringr]{str_locate}} with \code{\link[purrr]{map}}.
#' 
#' @export
#' @importFrom dplyr as_tibble coalesce mutate select starts_with
#' @importFrom magrittr %>%
#' @importFrom purrr map reduce
#' @importFrom stringr str_detect str_locate
#' 
#' @param patterns Patterns to look for, as a character vector. Each string in
#' the vector may be used as \code{pattern} would be in \code{\link[stringr]{str_locate}}.
#' @inheritParams stringr::str_locate
#' @return An integer matrix. First column gives start position of whichever pattern matched,
#' and second column gives end position.
#' 
#' @examples
#' fruit <- c("apple", "banana", "pear", "pineapple")
#' str_locate_whichever(fruit, c("ap", "ba", "pe"))
#' 

str_locate_whichever <- function (string, patterns) {
  n_matches <- map(patterns, ~str_detect(string, .)) %>%
    as.data.frame() %>%
    rowSums()
  
  if (any(n_matches > 1)) warning("Some elements of 'string' have multiple matches in 'patterns',\nreturning positions of first match found in order by elements of 'patterns'!")
  
  out <- map(patterns, ~str_locate(string, .)) %>%
    reduce(cbind) %>%
    as_tibble(.name_repair = "universal") %>%
    mutate(starts = coalesce(!!! select(., starts_with("start"))),
           ends = coalesce(!!! select(., starts_with("end"))))
  
  return (cbind(start = out$starts, end = out$ends))
}