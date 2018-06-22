#' Read and bind multiple data files at once.
#' 
#' Wrapper around \code{\link[readr]{read_delim}} to read multiple files at once
#' and return a single \code{\link[tibble]{tibble}} where all files have been combined row-wise.
#' For example, in a psychological experiment where one task is run repeatedly
#' for multiple subjects, use this function to read all of their data into one
#' fully long dataframe at once. As with \code{\link[readr]{read_csv}} and \code{\link[readr]{read_tsv}},
#' the \code{read_*_multi} versions are special cases of \code{read_delim_multi} for common
#' types of flat file data, CSV and TSV.
#' 
#' @export
#' @importFrom magrittr %>%
#' @importFrom rlang exprs
#' @importFrom stringr str_sub
#' 
#' @param files A character vector containing a series of paths to files.
#' @param ... other arguments passed on to \code{\link[readr]{read_delim}}.
#' @return A data frame, with file contents concatenated row-wise.
#' Contains a column \code{file} with the file name of origin for each line of data.
#' 
#' @details
#' \code{read_delim_multi} requires all files to have the same file ending, and thus
#' the same delimiter.
#' 
#' All functions first attempt to use \code{\link[dplyr]{bind_rows}} to combine data, in order
#' to preserve column data types as rigorously as possible. If this fails, \code{\link[plyr]{rbind.fill}}
#' will instead be used to combine data, which may silently coerce some columns to character.
#' 
#' @examples
#' \dontrun{
#' read_csv_multi(files = c("subject1.csv", "subject2.csv", "subject3.csv"))
#' 
#' these_files <- endsWith(list.files("path/to/data/folder"), ".csv")
#' read_csv_multi(these_files)
#' }

read_delim_multi <- function (files, ...) {
  
  dots <- exprs(...)
  if (length(unique(str_sub(files, -3L, -1L))) > 1) stop("Specified files have different file types, may have different delimiters!")
  out <- tidy_read_files(files, read_delim, dots) %>% try_bind_rows()
  
  return (out)
}

#' @rdname read_delim_multi
#' @export

read_csv_multi <- function (files, ...) {
  
  dots <- exprs(...)
  if (any(!endsWith(files, "csv"))) stop("Not all specified files are CSVs!")
  out <- tidy_read_files(files, read_csv, dots) %>% try_bind_rows()
  
  return (out)
}

#' @rdname read_delim_multi
#' @export

read_tsv_multi <- function (files, ...) {
  
  dots <- exprs(...)
  if (any(!endsWith(files, "tsv"))) stop("Not all specified files are TSVs!")
  out <- tidy_read_files(files, read_tsv, dots) %>% try_bind_rows()
  
  return (out)
}

#' @keywords internal
#' 
#' @param FUN name of \code{\link[readr]{readr}} file reading function to use
#' @param dots captured additional args to pass into \code{FUN}
#' @return A list of dataframes.
#' 
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#' @importFrom purrr map
#' @importFrom readr read_delim read_csv read_tsv
#' @importFrom rlang UQS
#' @importFrom tibble tibble

tidy_read_files <- function (files, FUN, dots) {
  
  out <- tibble(this_file = files) %>%
    mutate(data = map(this_file, ~ FUN(.x, UQS(dots)) %>%
                       mutate(file = .x)))
  
  return (out$data)
}

#' @keywords internal
#' 
#' @importFrom dplyr bind_rows
#' @importFrom plyr rbind.fill
#' 
#' @param x list of dataframes, or vector of dataframes from \code{\link[dplyr]{do}}.
#' @return A long dataframe.

try_bind_rows <- function (x) {
  out <- tryCatch({
    bind_rows(x)
  }, error = function(e) {
    warning("Detected column data types not consistent between files!\nSome cols may be converted to character; please examine output dataframe.")
    return (plyr::rbind.fill(x))
  })
  
  return (out)
}
