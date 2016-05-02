#' Produces the median \code{mtp} objects from a list of \code{mtp} objects
#'
#' @param list_of_mtp A list of \code{mtp} objects
#'
#' @return Returns a list of median \code{mtp} objects
#' @export
#'
#' @examples
#' # df_mtp is supplied with the package
#' combined01 <- combine_mtp_list(df_mtp$mtp)
#' plot_mtp_list(combine_mtp_list(df_mtp$mtp))
#'
#' # using dplyr
#' library(dplyr)
#' combined02 <- df_mtp %>% dplyr::group_by(file) %>% dplyr::summarize(mtp = median_mtp_list(mtp))
#' plot_mtp_list(combined$mtp)
median_mtp_list <- function(list_of_mtp){
  combined <- purrr::map_df(list_of_mtp, dplyr::bind_rows)
  combined <- combined %>% group_by(Hor., Ver., hours) %>%
    summarize(n = n(),
              value = median(value),
              fit = median(fit),
              resid = 0,
              group = paste(sort(unique(group)), collapse = ","),
              Name = paste(sort(unique(Name)), collapse = ",")) %>%
    ungroup() %>%
    filter(n == max(n)) %>%
    mutate(curve_id = as.integer(factor(paste(Hor., Ver.)))) %>%
    select(-n)

  list(as_mtp(combined))
}
