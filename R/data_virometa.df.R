#' Read viral contig taxonomy metadata
#'
#' Loads the viral metadata file \code{virometa250531.tsv}, which includes
#' taxonomic classifications for viral contigs.
#'
#' @param path Optional file path override.
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{rvlabel}{Contig label (character)}
#'   \item{accession}{Yeast strain accession (character)}
#'   \item{species}{Viral species classification (character)}
#'   \item{family}{Viral family classification (character)}
#' }
#' @export
read_virometa.df <- function(
    path = system.file("extdata", "virometa250531.tsv", package = "yeastvirome")
) {
  if (!file.exists(path) || path == "") {
    stop("Missing file: virometa250531.tsv")
  }

  # Explicitly set column types based on the actual TSV headers
  df <- readr::read_tsv(
    file           = path,
    col_types      = readr::cols(
      rvlabel   = readr::col_character(),
      accession = readr::col_character(),
      species   = readr::col_character(),
      family    = readr::col_character()
    ),
    na             = c("", "NA", "NaN"),
    show_col_types = FALSE,
    progress       = FALSE
  )

  # Sanity check for expected columns
  required_cols <- c("rvlabel", "accession", "species", "family")
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols)) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  return(df)
}

#' Viral contig taxonomy metadata (preloaded)
#'
#' Preloaded tibble with viral contig metadata, including strain accession and
#' taxonomy (species and family).
#'
#' @seealso \code{\link{read_virometa.df}}
#'
#' @format A tibble with the columns:
#' \describe{
#'   \item{rvlabel}{Contig label (character)}
#'   \item{accession}{Yeast strain accession (character)}
#'   \item{species}{Viral species classification (character)}
#'   \item{family}{Viral family classification (character)}
#' }
#' @export
virometa.df <- NULL
