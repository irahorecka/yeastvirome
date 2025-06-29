#' Read yeast strain ecological metadata
#'
#' Loads ecological metadata from **strain_ecodata.csv**, containing strain identifiers and ecological annotations.
#'
#' @param path Optional path override.
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{std_name}{Strain identifier (character)}
#'   \item{StrainName}{Original strain name (character)}
#'   \item{Reference}{Source/reference (factor)}
#'   \item{SRA}{SRA accession (character)}
#'   \item{Clade}{Genetic clade (factor)}
#'   \item{ecological_value}{Ecological annotation (character)}
#'   \item{eco_cat1}{Primary ecological category (factor)}
#'   \item{eco_cat2}{Secondary ecological category (factor)}
#'   \item{eco_cat3}{Tertiary ecological category (factor)}
#' }
#'
#' @export
read_ecodata.df <- function(
    path = system.file("extdata", "strain_ecodata.csv", package = "yeastvirome")
) {
  if (!file.exists(path) || path == "") {
    stop("Missing file: strain_ecodata.csv")
  }

  df <- readr::read_csv(
    file = path,
    col_types = readr::cols(
      std_name         = readr::col_character(),
      StrainName       = readr::col_character(),
      Reference        = readr::col_factor(),
      SRA              = readr::col_character(),
      Clade            = readr::col_factor(),
      ecological_value = readr::col_character(),
      eco_cat1         = readr::col_factor(),
      eco_cat2         = readr::col_factor(),
      eco_cat3         = readr::col_factor()
    ),
    na = c("", "NA", "NaN"),
    show_col_types = FALSE,
    progress = FALSE
  )

  required_cols <- c("std_name", "ecological_value")
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols)) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  return(df)
}

#' Yeast strain ecological metadata (preloaded)
#'
#' Ecological annotations for yeast strains, including hierarchical ecological categories.
#'
#' @format A tibble with columns:
#' \describe{
#'   \item{std_name}{Strain identifier (character)}
#'   \item{StrainName}{Original strain name (character)}
#'   \item{Reference}{Source/reference (factor)}
#'   \item{SRA}{SRA accession (character)}
#'   \item{Clade}{Genetic clade (factor)}
#'   \item{ecological_value}{Ecological annotation (character)}
#'   \item{eco_cat1}{Primary ecological category (factor)}
#'   \item{eco_cat2}{Secondary ecological category (factor)}
#'   \item{eco_cat3}{Tertiary ecological category (factor)}
#' }
#'
#' @seealso \code{\link{read_ecodata.df}}
#' @export
ecodata.df <- NULL
