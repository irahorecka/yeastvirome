#' Load Yeast Virome CSV Data
#'
#' Reads every `.csv` file in `inst/extdata/` and returns a named list
#' of tibbles.  File names become list names.
#'
#' @return A named `list` of `tibble`s.
#' @examples
#' data_list <- load_yeast_virome()
#' names(data_list)
#' @export
load_yeast_virome <- function() {
  csv_dir   <- system.file("extdata", package = "yeastvirome")
  csv_files <- list.files(csv_dir, pattern = "\\.csv$", full.names = TRUE)

  csv_files %>%
    purrr::set_names(basename(.)) %>%
    purrr::map(readr::read_csv, show_col_types = FALSE)
}
