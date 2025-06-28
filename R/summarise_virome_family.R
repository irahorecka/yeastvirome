#' Summarise virome counts by viral *family*
#'
#' Aggregates contig-level abundances into family-level sums.
#'
#' @inheritParams summarise_virome_species
#' @return A wide tibble with one column per viral family.
#' @export
summarise_virome_family <- function(virome_df, vmeta_df) {
  virome_long <- virome_df %>%
    tidyr::pivot_longer(
      cols      = -std_name,
      names_to  = "import_label",
      values_to = "value"
    ) %>%
    dplyr::left_join(
      dplyr::select(vmeta_df, import_label, family),
      by = "import_label"
    )

  family_sum <- virome_long %>%
    dplyr::group_by(std_name, family) %>%
    dplyr::summarise(
      sum_value = sum(value, na.rm = TRUE),
      .groups   = "drop"
    )

  tidyr::pivot_wider(
    family_sum,
    names_from  = family,
    values_from = sum_value,
    values_fill = list(sum_value = 0)
  )
}
