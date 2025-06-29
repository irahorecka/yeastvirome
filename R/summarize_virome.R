#' Summarize Virome Data by Species
#'
#' @param virome.df The original wide data.frame (samples x viruses)
#' @param vmeta.df The metadata data.frame mapping viruses to species and family
#' @return Species-level summary data.frame
#' @import dplyr
#' @import tidyr
#' @export
summarize_virome_species <- function(virome.df, vmeta.df) {
  virome.df %>%
    filter(replicate == 1) %>%
    select(-tree_order, -accession, -replicate) %>%
    pivot_longer(
      cols = -std_name,
      names_to = "import_label",
      values_to = "value"
    ) %>%
    left_join(vmeta.df %>% select(import_label, species), by = "import_label") %>%
    group_by(std_name, species) %>%
    summarise(sum_value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(
      names_from = species,
      values_from = sum_value,
      values_fill = list(sum_value = 0)
    )
}

#' Summarize Virome Data by Family
#'
#' @param virome.df The original wide data.frame (samples x viruses)
#' @param vmeta.df The metadata data.frame mapping viruses to species and family
#' @return Family-level summary data.frame
#' @import dplyr
#' @import tidyr
#' @export
summarize_virome_family <- function(virome.df, vmeta.df) {
  virome.df %>%
    filter(replicate == 1) %>%
    select(-tree_order, -accession, -replicate) %>%
    pivot_longer(
      cols = -std_name,
      names_to = "import_label",
      values_to = "value"
    ) %>%
    left_join(vmeta.df %>% select(import_label, family), by = "import_label") %>%
    group_by(std_name, family) %>%
    summarise(sum_value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(
      names_from = family,
      values_from = sum_value,
      values_fill = list(sum_value = 0)
    )
}

#' Summarize Total Virome Data
#'
#' @param virome.df The original wide data.frame (samples x viruses)
#' @param vmeta.df The metadata data.frame mapping viruses to species and family
#' @return Total summary data.frame
#' @import dplyr
#' @import tidyr
#' @export
summarize_virome_total <- function(virome.df, vmeta.df) {
  virome.df %>%
    filter(replicate == 1) %>%
    select(-tree_order, -accession, -replicate) %>%
    pivot_longer(
      cols = -std_name,
      names_to = "import_label",
      values_to = "value"
    ) %>%
    group_by(std_name) %>%
    summarise(total_value = sum(value, na.rm = TRUE), .groups = "drop")
}
