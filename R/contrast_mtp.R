#' Subtracts the first \code{mtp} object in a list of \code{mtp} objects from the remaining objects
#'
#' @param list_of_mtp a list of \code{mtp} objects
#'
#' @return Returns a list of contrasted \code{mtp} objects
#' @export
#'
#' @examples
#' contrast_list <- contrast_mtp_list(df_mtp$mtp[1:2])
#' plot_mtp_list(contrast_list)
#' plot_mtp_list(smooth_mtp_list(contrast_list), y_name = "fit")
#'
contrast_mtp_list <- function(list_of_mtp){
  ref_mtp <- list_of_mtp[[1]] %>%
    dplyr::select(Hor., Ver., hours, ref_value = value, ref_fit = fit)


  mtp <- purrr::map_df(list_of_mtp[-1], bind_rows,.id = "index")

  contrast_df <- left_join(mtp, ref_mtp) %>%
    dplyr::mutate(value = value - ref_value,
                  fit = fit - ref_fit,
                  resid = 0) %>%
    dplyr::select(-ref_value, -ref_fit)

  contrast_list <- split(contrast_df, contrast_df$index)
  purrr::map(contrast_list, as_mtp)
}

