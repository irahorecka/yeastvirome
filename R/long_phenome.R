#' Prepare Phenotype × Family Data for Visualization
#'
#' Converts wide-format phenotype and virome family abundance data into a tidy
#' long-format suitable for ggplot visualizations such as boxplots or violin plots.
#'
#' @param threshold Numeric threshold for minimum family abundance to consider as present.
#'
#' @return A tidy tibble with columns: `std_name`, `phenotype`, `value`, and `family`.
#' @import dplyr
#' @import tidyr
#' @export
long_phenome <- function(threshold = 5) {
  # Exclude non-numeric columns explicitly
  numeric_cols <- phenome.df %>%
    dplyr::select(where(is.numeric), -std_name) %>%
    colnames()

  phenotype_long <- phenome.df %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(numeric_cols),
      names_to = "phenotype",
      values_to = "value"
    )

  virome_family_df <- summarize_virome_family(virome.df, virometa.df)

  family_long <- virome_family_df %>%
    tidyr::pivot_longer(
      cols = -std_name,
      names_to = "family",
      values_to = "family_abundance"
    ) %>%
    dplyr::filter(family_abundance > threshold) %>%
    dplyr::select(std_name, family)

  background_df <- tibble::tibble(
    std_name = setdiff(unique(phenotype_long$std_name), unique(family_long$std_name)),
    family = "Background"
  )

  combined_family_df <- dplyr::bind_rows(family_long, background_df)

  phenotype_long %>%
    dplyr::inner_join(combined_family_df, by = "std_name") %>%
    dplyr::mutate(
      family = factor(
        family,
        levels = c("Background", sort(setdiff(unique(family), "Background")))
      )
    )
}
