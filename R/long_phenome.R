#' Prepare Phenotype × Family Data for Visualization
#'
#' Converts wide-format phenotype and virome family abundance data into a tidy
#' long-format suitable for ggplot visualizations such as boxplots or violin plots.
#'
#' @param phenome.df Wide-format phenotype numeric matrix; first column is `std_name`.
#' @param virome_family_df Wide-format family-level abundance matrix; first column is `std_name`.
#' @param threshold Numeric threshold for minimum family abundance to consider as present.
#'
#' @return A tidy tibble with columns: `std_name`, `phenotype`, `value`, and `family`.
#' @import dplyr
#' @import tidyr
#' @export
long_phenome <- function(phenome.df, virome_family_df, threshold = 5) {
  phenotype_long <- phenome.df %>%
    tidyr::pivot_longer(
      cols = -std_name,
      names_to = "phenotype",
      values_to = "value"
    )

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
