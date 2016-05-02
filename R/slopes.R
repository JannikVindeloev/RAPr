#' finds the min and max segment slope within a time window
#'
#' @param mtp A \code{mtp} object
#' @param start start a \code{scalar} defining the start of the search
#' @param end a \code{scalar} defining the end of the search
#'
#' @return Returns a data frame of features
#' @export
#'
#' @examples
#' slopes <- df_mtp$mtp[[1]]
#' slopes <- smooth_mtp(slopes)
#' mtp_slopes(slopes)
mtp_slopes <- function(mtp, start = NULL, end = NULL){

  assertthat::assert_that(inherits(mtp, "mtp"))

  mtp <- mtp %>%
    dplyr::filter(!is.na(hours) )

  min_hours <- min(mtp$hours, na.rm = TRUE)
  max_hours <- max(mtp$hours, na.rm = TRUE)

  if(is.null(start)) {
    start = min_hours
  }

  if(is.null(end)){
    end = max_hours
  }

  assertthat::assert_that(start >= min_hours,
                          end <= max_hours,
                          start < end)

  mtp %>%
    dplyr::filter(dplyr::between(hours, start, end)) %>%
    dplyr::group_by(Hor., Ver., Name) %>%
    dplyr::arrange(hours) %>%
    dplyr::summarize(max_slope_value = max(diff(value)/diff(hours), na.rm = TRUE),
                     max_slope_fit = max(diff(fit)/diff(hours), na.rm = TRUE),
                     min_slope_value = min(diff(value)/diff(hours), na.rm = TRUE),
                     min_slope_fit = min(diff(fit)/diff(hours), na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    tidyr::gather(feature, value, -Hor., -Ver., -Name)

}
