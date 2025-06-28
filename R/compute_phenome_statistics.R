#' Phenotype × family statistics (KS + FDR)
#'
#' Performs two-sample Kolmogorov–Smirnov tests between each viral family and
#' background for every phenotype, then adjusts p-values for multiple testing.
#'
#' @param plot_df Output of \code{prepare_phenome_plot_df()}.
#' @param alpha   FDR threshold (default 0.01).
#' @param adjust_method Method for \code{p.adjust} (default "BH").
#'
#' @return A tibble including effect sizes, raw & adjusted p-values,
#'   significance status and directionality.
#' @export
compute_phenome_statistics <- function(plot_df,
                                       alpha         = 0.01,
                                       adjust_method = "BH") {
  bg_stats <- plot_df %>%
    dplyr::filter(family == "Background") %>%
    dplyr::group_by(phenotype) %>%
    dplyr::summarise(
      bg_values = list(value),
      n_bg      = dplyr::n(),
      .groups   = "drop"
    )

  fam_stats <- plot_df %>%
    dplyr::filter(family != "Background") %>%
    dplyr::group_by(phenotype, family) %>%
    dplyr::summarise(
      fam_values = list(value),
      n_fam      = dplyr::n(),
      .groups    = "drop"
    )

  stats_df <- fam_stats %>%
    dplyr::left_join(bg_stats, by = "phenotype") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      avg_fam     = mean(unlist(fam_values), na.rm = TRUE),
      sd_fam      = stats::sd(unlist(fam_values),  na.rm = TRUE),
      avg_bg      = mean(unlist(bg_values),  na.rm = TRUE),
      sd_bg       = stats::sd(unlist(bg_values),   na.rm = TRUE),
      fold_change = ifelse(avg_bg > 0, avg_fam / avg_bg, NA_real_),
      z_score     = (avg_fam - avg_bg) / sd_bg,
      p_value     = if (n_fam >= 2 && n_bg >= 2)
        stats::ks.test(unlist(fam_values),
                       unlist(bg_values),
                       alternative = "two.sided")$p.value
      else NA_real_
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      p_adj       = stats::p.adjust(p_value, method = adjust_method),
      significant = !is.na(p_adj) & p_adj < alpha,
      direction   = dplyr::case_when(
        significant & z_score >  0 ~ "Upregulated",
        significant & z_score <  0 ~ "Downregulated",
        TRUE                        ~ "Unchanged"
      )
    ) %>%
    dplyr::select(
      phenotype, family,
      n_fam, n_bg,
      avg_fam, avg_bg,
      z_score, fold_change,
      p_value, p_adj,
      significant, direction
    )

  stats_df
}
