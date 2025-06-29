#' Read yeast strain geographic metadata
#'
#' Loads strain geographic metadata from **strain_geodata.csv**, containing
#' strain identifiers, geographic coordinates, and associated metadata.
#'
#' @param path Optional path override.
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{std_name}{Strain identifier (character)}
#'   \item{StrainName}{Original strain name (character)}
#'   \item{Reference}{Reference/source (character)}
#'   \item{SRA}{SRA accession (character)}
#'   \item{Clade}{Genetic clade (factor)}
#'   \item{geographic_value}{Geographical descriptor (character)}
#'   \item{city_region_value}{City or region (character)}
#'   \item{province_state_value}{Province/state (character)}
#'   \item{country_value}{Country (character)}
#'   \item{geo_cc}{Geographic country code (factor)}
#'   \item{geo_country}{Geographic country name (factor)}
#'   \item{geo_region}{Geographic region (factor)}
#'   \item{geo_lat}{Latitude (numeric)}
#'   \item{geo_lon}{Longitude (numeric)}
#'   \item{geo_label}{Geographic label (factor)}
#' }
#'
#' @export
read_geodata.df <- function(
    path = system.file("extdata", "strain_geodata.csv", package = "yeastvirome")
) {
  if (!file.exists(path) || path == "") {
    stop("Missing file: strain_geodata.csv")
  }

  df <- readr::read_csv(
    file = path,
    col_types = readr::cols(
      std_name            = readr::col_character(),
      StrainName          = readr::col_character(),
      Reference           = readr::col_factor(),
      SRA                 = readr::col_character(),
      Clade               = readr::col_factor(),
      geographic_value    = readr::col_character(),
      city_region_value   = readr::col_character(),
      province_state_value= readr::col_character(),
      country_value       = readr::col_character(),
      geo_cc              = readr::col_factor(),
      geo_country         = readr::col_factor(),
      geo_region          = readr::col_factor(),
      geo_lat             = readr::col_double(),
      geo_lon             = readr::col_double(),
      geo_label           = readr::col_character()
    ),
    na = c("", "NA", "NaN"),
    show_col_types = FALSE,
    progress = FALSE
  )

  required_cols <- c("std_name", "geo_lat", "geo_lon")
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols)) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  return(df)
}

#' Yeast strain geographic metadata (preloaded)
#'
#' Contains geographic coordinates and metadata for yeast strains.
#'
#' @seealso \code{\link{read_geodata.df}}
#'
#' @format A tibble with columns:
#' \describe{
#'   \item{std_name}{Strain identifier (character)}
#'   \item{StrainName}{Original strain name (character)}
#'   \item{Reference}{Reference/source (factor)}
#'   \item{SRA}{SRA accession (character)}
#'   \item{Clade}{Genetic clade (character)}
#'   \item{geographic_value}{Geographical descriptor (character)}
#'   \item{city_region_value}{City or region (character)}
#'   \item{province_state_value}{Province/state (character)}
#'   \item{country_value}{Country (character)}
#'   \item{geo_cc}{Geographic country code (factor)}
#'   \item{geo_country}{Geographic country name (factor)}
#'   \item{geo_region}{Geographic region (factor)}
#'   \item{geo_lat}{Latitude (numeric)}
#'   \item{geo_lon}{Longitude (numeric)}
#'   \item{geo_label}{Geographic label (factor)}
#' }
#'
#' @export
geodata.df <- NULL
