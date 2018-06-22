# cu-psych-r-tools
Miscellaneous R helper functions

## Installation

The package can be installed as a library for everyday use via `devtools::install_github("cu-psych-r-users/cuPsychRTools")`. If you'd like to play around with the source code and modify/add functionality, please feel free to fork this repo and submit a pull request.

## Contents

This package currently functions as a catch-all package for various data reading/tidying/manipulation helpers. As best as possible, all functions are built on `tidyverse` code and should work with the pipe `%>%`. Functions currently supported include:

* `read_delim_multi(files, ...)`: Quickly read in multiple delimited files that contain similar data into one long dataframe. For example, if you have behavioral experiment data from multiple subjects who have completed the same task, read all their data into R at once using these functions.
    + `read_csv_multi()` wraps `readr::read_csv()`
    + `read_tsv_multi()` wraps `readr::read_tsv()`
    + `read_delim_multi()` wraps `readr::read_delim()` for your nonspecific delimited needs
* `super_spread()`: A wrapper around `tidyr::spread()` that allows you to simultaneously spread multiple columns of values that correspond to the same key column.
* `super_gather()`: A wrapper around `tidyr::gather()` that allows you to simultaneously gather multiple groups of columns that will correspond to the same key column.

## Contact

Currently maintained in the Columbia University psychology department, care of [Monica Thieu](https://github.com/monicathieu). Please create pull requests/issues in this repo to communicate about code stuff, or [email Monica](mailto:monica.thieu@columbia.edu) for general questions or thoughts.