#' The value and location of curve extremas within the search window
#'
#' @param mtp An \code{mtp} object
#' @param start a \code{scalar} defining the start of the search
#' @param end a \code{scalar} defining the end of the search
#'
#' @return Returns a data frame of features
#' @export
#'
#' @examples
#' extrema <- df_mtp$mtp[[1]]
#' mtp_extrema(extrema, start = 2, end = 10)extrema
mtp_extrema <- function(mtp, start = NULL, end = NULL){

  assertthat::assert_that(inherits(mtp, "mtp"))

  mtp <- mtp %>%
      dplyr::filter(!is.na(hours))

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
    dplyr::summarize(min_value = min(value, na.rm = TRUE),
              min_value_hours = hours[which.min(value)],
              min_fit = min(fit, na.rm = TRUE),
              min_fit_hours = hours[which.min(fit)],
              max_value = max(value, na.rm = TRUE),
              max_value_hours = hours[which.max(value)],
              max_fit = max(fit, na.rm = TRUE),
              max_fit_hours = hours[which.max(fit)],
              range_value = max_value - min_value,
              range_fit = max_fit - min_fit,
              extreme_value = ifelse(abs(min_value) >= abs(max_value),
                                   min_value,
                                   max_value),
              extreme_value_hours = ifelse(extreme_value == max_value, max_value_hours, min_value_hours),
              extreme_fit = ifelse(abs(min_fit) >= abs(max_fit),
                                   min_fit,
                                   max_fit),
              extreme_fit_hours = ifelse(extreme_fit == max_fit, max_fit_hours, min_fit_hours)) %>%
    dplyr::ungroup() %>%
    tidyr::gather(feature, value, -Hor., -Ver., -Name)
}
