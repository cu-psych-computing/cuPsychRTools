#' Spread key-values pairs across multiple columns.
#' 
#' Spread key-values pairs, where one key maps analogously onto multiple values,
#' into across multiple columns. Equivalent to calling \code{\link[tidyr]{spread}} separately
#' for dataframes with the same key and different values,
#' and joining them column-wise back into one dataframe.
#' 
#' @export
#' @importFrom magrittr %>%
#' @importFrom rlang enquo exprs UQ UQS
#' @importFrom tidyr gather unite spread
#' 
#' @param data A data frame.
#' @param key Column names, \emph{unquoted,} passed in using \code{\link[tidyselect]{vars_select}}.
#' Accepts \code{\link[dplyr]{select_helpers}}. To specify multiple key columns
#' all by name, group unquoted variable names using \code{\link[base]{c}}.
#' @param ... A selection of columns containing values to be spread.
#' again, unquoted. Accepts \code{\link[dplyr]{select_helpers}}. Variable names
#' do not need to be grouped using \code{\link[base]{c}}; all names passed in
#' as \code{...} are assumed to be value columns.
#' @param name_order Which identifier comes first in final colname?
#' Choose \code{"key_first"} or \code{"value_first"}. Defaults to \code{"key_first"}.
#' @param sep Separator to use between values, ultimately ending up in colnames.
#' Passed to \code{\link[tidyr]{unite}}.
#' @inheritParams tidyr::spread
#' @return A data frame, "fully" spread by all indicated columns.
#' 
#' @examples
#' data <- expand.grid(
#' id = 1L:10L,
#' condition = c("a", "b"),
#' value_1 = 1L,
#' value_2 = 0L)
#' 
#' super_spread(data, condition, value_1:value_2)
super_spread <- function (data, key, ..., name_order = "key_first", sep = "_",
                          fill = NA, convert = FALSE, drop = TRUE) {
  dots <- exprs(...)
  key <- enquo(key)
  output <- gather(data, gkey, value, UQS(dots))
  
  if (name_order == "key_first") {
    output <- unite(output, ukey, UQ(key), gkey, sep = sep)
  } else if (name_order == "value_first") {
    output <- unite(output, ukey, gkey, UQ(key), sep = sep)
  }
  
  output <- spread(output, ukey, value, fill = fill, convert = convert, drop = drop)
  
  return (output)
}