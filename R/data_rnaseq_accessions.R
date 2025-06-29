#' Read RNA-seq accession metadata
#'
#' Loads **inst/extdata/rnaseq_accessions.tsv**, containing metadata for
#' RNA-seq runs in the yeast virome project.
#'
#' @param path Optional override path.
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{tree_order}{Integer tree order for plotting/ordering.}
#'   \item{std_name}{Yeast strain name (character).}
#'   \item{accession}{ENA accession ID (character).}
#'   \item{replicate}{Replicate number (integer).}
#' }
#'
#' @export
read_rnaseq_accessions <- function(
    path = system.file("extdata", "rnaseq_accessions.tsv", package = "yeastvirome")
) {
  if (!file.exists(path) || path == "") {
    stop("Missing file: rnaseq_accessions.tsv")
  }

  df <- readr::read_tsv(
    file           = path,
    col_types      = readr::cols(
      tree_order        = readr::col_number(),
      std_name          = readr::col_character(),
      accession         = readr::col_character(),
      replicate         = readr::col_number()
      # date              = readr::col_character()
      # std_name          = readr::col_character(),
      # alias             = readr::col_character(),
      # source            = readr::col_character(),
      # ENA_run_accession = readr::col_character(),
      # read_type         = readr::col_factor(levels = NULL),
      # adapter           = readr::col_character(),
      # index             = readr::col_character(),
      # date              = readr::col_character()
    ),
    na             = c("", "NA", "NaN"),
    progress       = FALSE,
    show_col_types = FALSE
  )

  stopifnot("std_name" %in% names(df))
  return(df)
}

#' RNA-seq Run Metadata (preloaded)
#'
#' Metadata table describing RNA-seq samples: one row per replicate,
#' including ENA accessions and replicate numbers.
#'
#' @format A tibble with columns:
#' \describe{
#'   \item{tree_order}{Tree order (numeric).}
#'   \item{std_name}{Strain name (character).}
#'   \item{accession}{ENA run accession (character).}
#'   \item{replicate}{Replicate index (numeric).}
#' }
#' @seealso \code{\link{read_rnaseq_accessions}}
#' @export
rnaseq.accessions <- NULL
