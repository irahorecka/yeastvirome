#' Summarise virome counts by viral *species*
#'
#' Converts a wide \code{virome_df} (one row per strain, columns = contigs)
#' into a wide table where each viral species is a column containing the sum
#' of all contig abundances that belong to that species.
#'
#' @param virome_df A wide tibble or data frame; first column \code{std_name},
#'   remaining numeric columns store abundance (e.g. RPM) for each viral contig.
#' @param vmeta_df  Metadata tibble with columns \code{import_label} (contig id)
#'   and \code{species}.
#'
#' @return A wide tibble with \code{std_name} + one column per species.
#' @examples
#' \dontrun{
#' species_tbl <- summarise_virome_species(virome_df, vmeta_df)
#' }
#' @export
summarise_virome_species <- function(virome_df, vmeta_df) {
  virome_long <- virome_df %>%
    tidyr::pivot_longer(
      cols = -std_name,
      names_to  = "import_label",
      values_to = "value"
    ) %>%
    dplyr::left_join(
      dplyr::select(vmeta_df, import_label, species),
      by = "import_label"
    )

  species_sum <- virome_long %>%
    dplyr::group_by(std_name, species) %>%
    dplyr::summarise(
      sum_value = sum(value, na.rm = TRUE),
      .groups   = "drop"
    )

  tidyr::pivot_wider(
    species_sum,
    names_from  = species,
    values_from = sum_value,
    values_fill = list(sum_value = 0)
  )
}
