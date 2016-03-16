#' Interpolation between curve points
#'
#' The function will currently find the mean \code{y} for a given \code{x}. Only
#' works for monotonic functions. A future enhancement could be to loop through
#' \code{y} segments to find \code{y} for a given \code{x} within each segment.
#' This would be equivalent to rough root finding function. Such segments could
#' be found using: \code{rlen(x > x_out)}
#'
#' @param df_mtp The data frame containing \code{x} and \code{y} to interpolate
#' @param x_out the \code{x} to evaluate \code{y}
#' @param x_name the name of \code{x} (chr) in the data frame
#' @param y_name the name of \code{y} (chr) in the data frame
#' @param feat_name the root name of the feature, defaults to \code{x_name}
#'
#' @return returns a data frame with \code{feature} and \code{value}
#' @seealso \code{\link{mtp_feature}}
#' @export
#'
#' @examples
#' # An example
#' df <- dplyr::data_frame(hours = 1:10, fit = 2*hours)
#' class(df) <- c("mtp", "data.frame", "tbl_df")
#' ggplot2::qplot(x= hours, y = fit, data = df, geom = "line")
#' fun_xy(df, c(2.5, 3.2, 4.1))
#'
fun_xy <- function(df_mtp, x_out, x_name = "hours", y_name = "fit", feat_name = x_name){
  x <- df_mtp[[x_name]]
  y <- df_mtp[[y_name]]
  ord <- order(x)
  x <- x[ord]
  y <- y[ord]

  assertthat::assert_that(is.numeric(x), !any(is.na(x)),
                          is.numeric(y), !any(is.na(y)),
                          is.numeric(x_out),
                          is.character(x_name),
                          is.character(y_name))

  df <- dplyr::as_data_frame(approx(x = x, y = y, xout = x_out, ties  = mean))
  names(df) <- c("feature", "value")
  df %>% dplyr::mutate(feature = paste(feat_name, feature, sep = ""))
}

#' Find feature points for a MTP
#'
#' @param mtp a data frame of class mtp
#' @param ... parameters for the \code{\link{fun_xy}} function
#' @seealso \code{fun_xy} and \code{df_mtp}
#'
#' @return a data frame
#' @export
#'
#' @examples
#' # pH a 6 hrs
#' mtp_feature(df_mtp$mtp[[1]], x_out = 6, y_name = "value")
#'
#' # time to pH 5.5
#' mtp_feature(df_mtp$mtp[[1]], x_out = 5.5, x_name = "value", y_name = "hours")
#'
#' # reducing data set to whole hours
#' reduced_data <- mtp_feature(df_mtp$mtp[[1]], x_out = c(1:10), y_name = "value")
#'
#' # Calculating features for the whole data frame of mtp's
#' # Make sure that each piece feed to the do() function is a mtp object.
#' # the . is a place holder.
#' library(dplyr)
#' df_mtp %>%
#' group_by(file, sheet, Plate) %>%
#' do(mtp_feature(.$mtp[[1]], x_out = c(1,6)))
#'
mtp_feature <- function(mtp, ...){
  assertthat::assert_that(inherits(mtp, "mtp"))

  mtp %>%
    dplyr::group_by(Hor., Ver., Name) %>%
    dplyr::do(fun_xy(., ...))
    }

#' calculate pH after x hours
#'
#' @param mtp A \code{mtp} object
#' @param x_out the hours in which to evaluate pH
#'
#' @return Returns a data frame
#' @export
#'
#' @examples
#' pHhrs <- df_mtp$mtp[[1]]
#' mtp_pH_after_hours(pHhrs) # error
#' mtp_pH_after_hours(smooth_mtp(pHhrs))
#' mtp_pH_after_hours(smooth_mtp(pHhrs),x_out = c(1,6,12))
#'
mtp_pH_after_hours <- function(mtp, x_out = NULL){
  assertthat::assert_that(inherits(mtp, "mtp"))
  if(is.null(x_out)){
    message("x_out defaulting to 6")
    x_out = 6
  }

  df_value <- mtp_feature(mtp, x_out = x_out, y_name = "value", feat_name = "hrs_val")
  df_fit <- mtp_feature(mtp, x_out = x_out, y_name = "fit", feat_name = "hrs_fit")
  dplyr::bind_rows(df_value, df_fit)
}
