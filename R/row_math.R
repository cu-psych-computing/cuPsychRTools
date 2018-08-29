#' Calculate row sums & means mutate-safely.
#' 
#' Wrapper around \code{\link[base]{rowSums}} etc. to allow
#' calculating row sums and means of flexibly supplied column names
#' from a df. Designed for use inside of \code{\link[dplyr]{mutate}}.
#' 
#' @export
#' @importFrom purrr pmap_dbl
#' @importFrom rlang list2
#' 
#' @param ... One or more unquoted variable names.
#' Does not accept \code{\link[tidyselect]{select_helpers}}, but is usable inside of \code{\link[dplyr]{mutate}}.
#' @param na.rm a logical value indicating whether NA values should be stripped
#' before the computation proceeds.
#' @return A vector of equal length to each of the input vectors/columns.
#' 
#' @examples 
#' data <- tibble::tibble(x1 = 1:5, x2 = 1:5, x3 = 1:5)
#' 
#' dplyr::mutate(data,
#' xsum = tidy_row_sums(x1, x2, x3),
#' xmean = tidy_row_means(x1, x2, x3))

tidy_row_sums <- function (..., na.rm = FALSE) {
  
  return (pmap_dbl(list2(...), function (...) sum(c(...), na.rm = na.rm)))
}

#' @rdname tidy_row_sums
#' @export

tidy_row_means <- function (..., na.rm = FALSE) {
  
  return (pmap_dbl(list2(...), function (...) mean(c(...), na.rm = na.rm)))
}

#' Calculate row sums & means tidyselect-safely.
#' 
#' Wrapper around \code{\link[base]{rowSums}} etc. to allow
#' calculating row sums and means of flexibly supplied column names
#' from a df. Accepts \code{\link[tidyselect]{select_helpers}}.
#' Designed for external use in pipe-chains, \emph{outside} of \code{\link[dplyr]{mutate}}.
#' 
#' @export
#' @importFrom dplyr mutate select
#' @importFrom rlang exprs sym !! !!! :=
#' 
#' @param .data A tbl: a \code{data.frame} or \code{tibble}.
#' @param ... One or more unquoted variable names. Accepts \code{\link[tidyselect]{select_helpers}}.
#' @param .value The name of the new column, as a string.
#' @param na.rm a logical value indicating whether NA values should be stripped
#' before the computation proceeds.
#' @return An object of the same class as \code{.data}, with the computed column added.
#' 
#' @examples 
#' data <- tibble::tibble(x1 = 1:5, x2 = 1:5, x3 = 1:5)
#' 
#' tidyselect_row_sums(data, dplyr::starts_with("x"))
#' tidyselect_row_means(data, dplyr::starts_with("x"))

tidyselect_row_sums <- function (.data, ..., .value = "row_sum", na.rm = FALSE) {
  
  dots <- exprs(...)
  value <- sym(.value)
  cols <- select(.data, !!!dots)
  return (mutate(.data, !!value := rowSums(cols, na.rm = na.rm)))
}

#' @rdname tidyselect_row_sums
#' @export

tidyselect_row_means <- function (.data, ..., .value = "row_mean", na.rm = FALSE) {
  
  dots <- exprs(...)
  value <- sym(.value)
  cols <- select(.data, !!!dots)
  return (mutate(.data, !!value := rowMeans(cols, na.rm = na.rm)))
}