#' Read wide-format contig abundance matrix
#'
#' Loads **inst/extdata/virome250531.tsv**, which contains virome abundance
#' data for each yeast strain. Each row corresponds to a yeast strain and each
#' column (after \code{accession}) corresponds to a viral contig.
#'
#' @param path Optional path override. Defaults to internal data.
#'
#' @return A wide-format tibble with columns:
#' \describe{
#'   \item{accession}{Strain name (character).}
#'   \item{<contig ids>}{Numeric abundance values (e.g. RPM).}
#' }
#' @export
read_virome.df <- function(
    path = system.file("extdata", "virome250531.tsv", package = "yeastvirome")
) {
  if (!file.exists(path) || path == "") {
    stop("Missing file: virome250531.tsv")
  }

  df <- readr::read_tsv(
    file           = path,
    col_types      = readr::cols(
      accession = readr::col_character(),
      .default  = readr::col_double()
    ),
    na             = c("", "NA", "NaN"),
    show_col_types = FALSE,
    progress       = FALSE
  )

  return(tibble::as_tibble(df))
}

#' Virome contig abundance matrix (preloaded)
#'
#' Wide-format tibble containing RNA abundance values (e.g. RPM) for each
#' viral contig across 1,011 yeast strains. One row per strain.
#'
#' @format A tibble: rows = strains; columns = contigs
#' @seealso \code{\link{read_virome_matrix}}
#' @export
virome.df <- NULL
