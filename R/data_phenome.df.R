# File: R/data_phenome.df.R
# ------------------------------------------------------------
#  Loads the full phenotype matrix for 1,011 yeast strains.
#  From: https://github.com/SakshiKhaiwal/Genotype-to-phenotype-mapping-in-yeast/tree/main/data
#  "Finalset_223phenotypes_1011.csv" renamed to phenome223_1011.csv"
#  Accessed June 2025
# ------------------------------------------------------------

#' Read the wide 1,011-strain phenotype matrix
#'
#' Loads **inst/extdata/phenome223_1011.csv**, returning a
#' wide-format tibble with one row per yeast strain and ~223 phenotype
#' columns.
#'
#' @param path Optional override path. Defaults to internal file.
#'
#' @return A tibble with:
#' \describe{
#'   \item{std_name}{Strain identifier (character)}
#'   \item{<phenotype columns>}{Numeric values or NA}
#' }
#'
#' @examples
#' \dontrun{
#' pheno_tbl <- read_phenotype_matrix()
#' dim(pheno_tbl)
#' }
#' @export
read_phenotype_matrix <- function(
    path = system.file("extdata",
                       "phenome223_1011.csv",
                       package = "yeastvirome")
) {
  if (!file.exists(path) || path == "") {
    stop("Missing file: phenome223_1011.csv")
  }

  # Use readr to auto-detect everything except std_name
  suppressMessages({
    df <- readr::read_csv(
      file = path,
      col_types = readr::cols(
        std_name = readr::col_character(),
        .default = readr::col_double()
      ),
      progress = FALSE,
      show_col_types = FALSE,
      na = c("", "NA", "NaN")
    )
  })

  stopifnot("std_name" %in% names(df))
  df
}

#' 1,011-Strain Phenotype Matrix (pre-loaded)
#'
#' This tibble contains ~223 phenotypes measured across 1,011 yeast strains.
#' Automatically loaded via `.onLoad()`. Use \code{read_phenotype_matrix()}
#' for explicit reading.
#'
#' @format A tibble with 1,011 rows and ~224 columns
#' @seealso \code{\link{read_phenotype_matrix}}
#' @export
phenome.df <- NULL
