# File: R/data_phenome.layout.R
# ------------------------------------------------------------
#  Helpers & data for the phenome-experiment layout table
# ------------------------------------------------------------

#' Read the phenome-experiment layout table
#'
#' Loads **inst/extdata/phenome.experiment.layout.tsv** and returns it as
#' a tidy tibble with proper column types.
#' See **`?phenome.layout`** for field descriptions.
#'
#' @param path Optional override; defaults to the table shipped inside the
#'   package.
#'
#' @return A tibble.
#' @export
read_phenome_experiment_layout <- function(
    path = system.file("extdata",
                       "phenome.experiment.layout.tsv",
                       package = "yeastvirome")
) {
  if (!file.exists(path) || path == "") {
    stop("Cannot locate 'phenome.experiment.layout.tsv' – check installation.")
  }

  df <- readr::read_tsv(
    file       = path,
    col_types  = readr::cols(.default = readr::col_character()),
    show_col_types = FALSE,
    progress   = FALSE
  )

  expected <- c("display.order", "col.id",
                "experimental.group", "value.type")

  missing_cols <- setdiff(expected, names(df))
  if (length(missing_cols)) {
    stop("Missing expected columns: ",
         paste(missing_cols, collapse = ", "))
  }

  df <- dplyr::mutate(
    df,
    display.order = suppressWarnings(as.integer(display.order)),
    value.type    = factor(
      value.type,
      levels = c("numeric", "factor", "string"),
      ordered = FALSE
    )
  )

  dplyr::relocate(df, dplyr::all_of(expected))
}

#' Phenome-experiment layout (pre-loaded)
#'
#' A tibble describing every phenotype measured in the 1,011-strain yeast
#' collection.  It is available immediately after \pkg{yeastvirome} is
#' attached, because it is populated at load-time by \code{.onLoad()}.
#'
#' @format A tibble with (at least) four columns:
#' \describe{
#'   \item{display.order}{Integer sort key.}
#'   \item{col.id}{Phenotype identifier.}
#'   \item{experimental.group}{High-level category.}
#'   \item{value.type}{Factor with levels \emph{numeric}, \emph{factor},
#'         \emph{string}.}
#' }
#' Additional columns, if present in the source TSV, are retained as
#' character vectors so that no information is lost.
#' @seealso \code{\link{read_phenome_experiment_layout}}
#' @export
phenome.layout <- NULL      # will be populated in .onLoad()
