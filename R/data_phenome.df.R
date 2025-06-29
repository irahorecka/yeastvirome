#' Read the wide 1,011‑strain phenotype matrix
#'
#' Uses \code{phenome.layout} to coerce each column to its declared type.
#' Any extra columns are kept as character; any missing columns trigger an
#' informative error.
#'
#' @param path CSV path (defaults to internal copy).
#' @return A tibble with typed columns and rows ordered by \code{Tree_order}.
#' @export
read_phenotype_matrix <- function(
    path = system.file("extdata", "phenome223_1011.csv", package = "yeastvirome")
) {
  if (!file.exists(path) || path == "") {
    stop("File not found: phenome223_1011.csv")
  }

  if (!exists("phenome.layout", inherits = TRUE)) {
    stop("phenome.layout must be loaded before reading the phenotype matrix.")
  }

  # -------------------------------------------------------------------------
  # 1. Read everything as character first (never lose data on import)
  # -------------------------------------------------------------------------
  suppressMessages({
    df <- readr::read_csv(
      file           = path,
      col_types      = readr::cols(.default = readr::col_character()),
      na             = c("", "NA", "NaN"),
      progress       = FALSE,
      show_col_types = FALSE,
      name_repair    = "minimal"
    )
  })

  # -------------------------------------------------------------------------
  # 2. Standard‑ise the primary key column name
  # -------------------------------------------------------------------------
  std_candidates <- c("std_name", "Standard_name", "strain", "Strain")
  present        <- intersect(std_candidates, names(df))
  if (length(present) == 0) {
    stop("Could not find a strain identifier column in the CSV.")
  }
  names(df)[match(present[1], names(df))] <- "std_name"

  # -------------------------------------------------------------------------
  # 3. Re‑type each phenotype column according to phenome.layout
  # -------------------------------------------------------------------------
  layout_map <- phenome.layout |>
    dplyr::select(col.id, value.type) |>
    dplyr::mutate(value.type = as.character(value.type))  # ensure character

  unknown_cols <- setdiff(layout_map$col.id, names(df))
  if (length(unknown_cols)) {
    stop("These columns listed in phenome.layout are missing from the CSV: ",
         paste(unknown_cols, collapse = ", "))
  }

  # Prepare typed mutations
  numeric_cols <- layout_map$col.id[layout_map$value.type == "numeric"]
  factor_cols  <- layout_map$col.id[layout_map$value.type == "factor"]
  string_cols  <- layout_map$col.id[layout_map$value.type == "string"]

  df <- df |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(numeric_cols), ~ suppressWarnings(as.numeric(.x))),
      dplyr::across(dplyr::all_of(factor_cols),  ~ factor(.x)),
      dplyr::across(dplyr::all_of(string_cols),  ~ as.character(.x))
    )

  # -------------------------------------------------------------------------
  # 4. Order by Tree_order if the column exists
  # -------------------------------------------------------------------------
  if ("Tree_order" %in% names(df)) {
    df <- df[order(as.integer(df$Tree_order)), ]
  }

  # Ensure std_name survived and return
  stopifnot("std_name" %in% names(df))
  return(tibble::as_tibble(df))
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
