#' Combines \code{mtp} objects in a list
#'
#' @param list_of_mtp A list of \code{mtp} objects
#'
#' @return Returns a list of combined \code{mtp} objects
#' @export
#' @examples
#' # df_mtp is supplied with the package
#' combined <- combine_mtp_list(df_mtp$mtp)
#' plot_mtp_list(combine_mtp_list(df_mtp$mtp))
#'
#' # using dplyr
#' library(dplyr)
#' combined <- df_mtp %>% dplyr::mutate(cmtp = combine_mtp_list(.$mtp)) # recycling
#' combined <- df_mtp %>% dplyr::group_by(file) %>% dplyr::summarize(cmtp = combine_mtp_list(mtp)) #condensing
#' plot_mtp_list(combined$cmtp)
combine_mtp_list <- function(list_of_mtp){

  mtp_names <- names(list_of_mtp)
  if(is.null(mtp_names)) {
    mtp_names <- paste("mtp", seq(1,length(list_of_mtp)))
  }

  combined <- purrr::map_df(list_of_mtp, dplyr::bind_rows) %>%
    dplyr::mutate(curve_id = as.integer(factor(paste(group,Hor.,Ver.))))

  #dplyr::data_frame(list(as_mtp(combined)))
  list(as_mtp(combined))
}

#set_group_mtp_list
