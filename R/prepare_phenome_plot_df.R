#' Prepare long-format phenotype × family data
#'
#' Converts wide phenotype and virome-family matrices into a tidy long
#' data frame suitable for ggplot-based visualisations (box/violin plots).
#'
#' @param ymeta_numeric_df Wide phenotypic numeric matrix; first column std_name.
#' @param virome_fam_df    Wide family-level abundance matrix; first column std_name.
#' @param p_threshold      Minimum family abundance to consider “present”.
#'
#' @return A tibble with columns: \code{std_name}, \code{phenotype},
#'   \code{value}, \code{family}.
#' @export
prepare_phenome_plot_df <- function(ymeta_numeric_df,
                                    virome_fam_df,
                                    p_threshold = 5) {
  pheno_long <- ymeta_numeric_df %>%
    tidyr::pivot_longer(
      cols      = -std_name,
      names_to  = "phenotype",
      values_to = "value"
    )

  fam_long <- virome_fam_df %>%
    tidyr::pivot_longer(
      cols      = -std_name,
      names_to  = "family",
      values_to = "fam_sum"
    ) %>%
    dplyr::filter(fam_sum > p_threshold) %>%
    dplyr::select(std_name, family)

  bg_df <- tibble::tibble(
    std_name = setdiff(unique(pheno_long$std_name), unique(fam_long$std_name)),
    family   = "Background"
  )

  combined_fam <- dplyr::bind_rows(fam_long, bg_df)

  pheno_long %>%
    dplyr::inner_join(combined_fam, by = "std_name") %>%
    dplyr::mutate(
      family = factor(
        family,
        levels = c("Background",
                   sort(unique(family[family != "Background"])))
      )
    )
}
