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
#' Choose \code{"key_first"} or \code{"value_first"}. Defaults to \code{"value_first"}.
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

super_spread <- function (data, key, ..., name_order = "value_first", sep = "_",
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

#' Gather columns into key-values pairs.
#' 
#' Partially collapse multiple columns into key-values pairs. Use \code{super_gather()}
#' when you have have multiple wide-form variables that you want to partially collapse,
#' but you want those gathered variables to stay in separate columns. Equivalent to
#' calling \code{\link[tidyr]{gather}} separately for dataframes with the \emph{same key} and
#' different values, and joining them column-wise back into one dataframe. Requires the common
#' key(s) to be spelled identically in names of columns to be gathered.
#' 
#' @export
#' @importFrom dplyr as_tibble if_else matches mutate rename select
#' @importFrom magrittr %>%
#' @importFrom rlang is_character
#' @importFrom stringr str_sub
#' @importFrom tidyr gather separate spread
#' 
#' @param data A data frame.
#' @param key Name of new key column, as string.
#' @param key_names Names of key levels present in column names to be gathered,
#' as character vector. Specify either this or \code{value_names} but not both.
#' If both this and \code{value_names} are specified, will \emph{only} use
#' key names to identify columns to gather.
#' @param value_names Names of value levels present in column names to be gathered,
#' as character vector. Specify either this or \code{key_names} but not both.
#' @param name_order Which identifier comes first in existing colnames? Used to assist in
#' identifying which columns to gather. Choose \code{"key_first"} or \code{"value_first"}.
#' @param delim_nchar How many characters delimit between key and value names? Defaults to 1.
#' @return A data frame, with all indicated columns gathered, but separate.
#' 
#' @details
#' Implements a strategy akin to calling \code{\link[tidyr]{gather}}, \code{\link[tidyr]{separate}}, and
#' \code{\link[tidyr]{spread}} in succession. As a result, may coerce some columns to character
#' under the hood. Please pay attention to warnings.
#' 
#' @examples
#' data <- data.frame(id = 1:10,
#' value1_cond1 = "a",
#' value1_cond2 = "b",
#' value2_cond1 = 0L,
#' value2_cond2 = 1L)
#' 
#' super_gather(data, "condition", key_names = c("cond1", "cond2"), name_order = "value_first")
#' super_gather(data, "condition", value_names = c("value1", "value2"), name_order = "value_first")

super_gather <- function (data, key = "key", key_names = NULL, value_names = NULL, name_order, delim_nchar = 1) {
  
  stopifnot(is_character(key), (!is.null(key_names) | !is.null(value_names)))
  if (!is.null(key_names) & !is.null(value_names)) {
    warning("Both key names and value names specified! Defaulting to select with key names.")
    value_names <- NULL
  }
  gather_names <- c(key_names, value_names)
  
  if (!is.null(key_names)) {
    intos = list(key = "name_part",
                 skey = "other_part")
    if (name_order == "key_first") {
      gather_regexp  <- paste0("^", gather_names, collapse = "|")
    } else if (name_order == "value_first") {
      gather_regexp  <- paste0(gather_names, "$", collapse = "|")
    }
  } else if (!is.null(value_names)) {
    intos = list(skey = "name_part",
                 key = "other_part")
    if (name_order == "key_first") {
      gather_regexp <- paste0(gather_names, "$", collapse = "|")
    } else if (name_order == "value_first") {
      gather_regexp <- paste0("^", gather_names, collapse = "|")
    }
  }
  
  output <- data %>%
    gather(gkey, value, dplyr::matches(gather_regexp)) %>%
    cbind(str_locate_whichever(.$gkey, gather_names)) %>%
    as_tibble() %>%
    mutate(name_part = str_sub(gkey, start = start, end = end),
           other_part = if_else(start != 1,
                                str_sub(gkey, end = start - (delim_nchar + 1)),
                                str_sub(gkey, start = end + (delim_nchar + 1)))) %>%
    rename(!!! intos) %>%
    select(-gkey, -start, -end) %>%
    spread(skey, value)
  
  
  return (output)
}