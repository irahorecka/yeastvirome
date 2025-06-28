#' Total viral load per strain
#'
#' Sums every numeric column (excluding \code{std_name}) to compute the
#' overall abundance of viral sequences per yeast strain.
#'
#' @param virome_df Output from a virome loader; first column \code{std_name}.
#' @return A two-column tibble: \code{std_name}, \code{total_virome}.
#' @export
summarise_virome_total <- function(virome_df) {
  virome_df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      total_virome = sum(dplyr::c_across(where(is.numeric)), na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(std_name, total_virome)
}
