
#' Calculate row sums & means tidyverse-safely.
#' 
#' Wrapper around \code{\link[base]{rowSums}} etc. to allow
#' calculating row sums and means of flexibly supplied column names
#' from a df with \code{link[dplyr]{select}}-style syntax. Safe to
#' use inside of \code{\link[dplyr]{mutate}}.
#' 
#' @export
#' @importFrom dplyr select
#' @importFrom magrittr %>%
#' @importFrom purrr map_lgl
#' @importFrom rlang exprs quo_name UQS !!!
#' 
#' @param ... One or more unquoted variable names. Does not currently accept
#' \code{\link[tidyselect]{select_helpers}} (hopefully to be added soon).
#' @return A vector of equal length to each of the input vectors/columns.
#' 

tidy_row_sums <- function (...) {
  
  # TODO: add tidyselect support. How to do this without forcibly writing
  # a function that only works inside of mutate?
  
  # @param .data Hidden argument to capture the calling environment.
  # Necessary to allow the function to accept \code{\link[tidyselect]{select_helpers}}.
  # \emph{Do not change this argument!}
  
  dots <- exprs(...)
  return (rowSums(cbind(...)))

}

#' @rdname tidy_row_sums
#' @export

tidy_row_means <- function (...) {
  
  dots <- exprs(...)
  return (rowMeans(cbind(...)))
  
}