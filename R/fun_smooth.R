#' Loess smoothing of a well curve
#'
#' @param df takes a \code{mtp} object as input
#' @param x_name the name of the x variable of type character
#' @param y_name the name of the y variable of type character
#' @param no_points The number of points in the loess smooth window of type numeric. Defaults to 20\% of the length of the x variable
#' @param fit_degree The degree of the loess smooth polynomial of type numeric must be less than or equal to 2
#' @family smoothing functions
#' @seealso \code{\link{smooth_mtp}} for mtp objects and \code{\link{smooth_mtp_list}} for lists of mtp objects
#' @return returns a \code{mtp} object
#' @export
#' @examples
#' df <- df_mtp$mtp[[1]]
#' curve <- subset(df, curve_id == 1)
#' fun_smooth(curve)
#' fun_smooth(curve, no_points = 40)
fun_smooth <- function(df, x_name = "hours", y_name = "value", no_points = NULL, fit_degree = 1) {
    x <- df[[x_name]]
    y <- df[[y_name]]

    if(fit_degree < 1 | fit_degree > 2) {stop("fit_degree is not 1 or 2")}

    if(is.null(no_points)){
      no_points <- as.integer(round(0.2*length(y)))
    }

    my_span <- no_points/length(y)

    assertthat::assert_that(is.numeric(x),
                            is.numeric(y),
                            x_name %in% names(df),
                            y_name %in% names(df),
                            is.numeric(no_points),
                            is.numeric(fit_degree),
                            length(x) == length(y),
                            length(x) > no_points,
                            my_span <= 1,
                            fit_degree <= 2,!any(is.na(x)),!any(is.na(y)))

    model <- loess(y ~ x, span = my_span, degree = fit_degree)

    dplyr::data_frame(hours = x,
                      value = y,
                      fit = fitted(model),
                      resid = residuals(model))
}

#' Smooths the individual curves of a \code{mtp} object.
#'
#' @param df_mtp A \code{mtp} object
#' @param ... parameter passed on to \code{\link{fun_smooth}}
#'
#' @return a \code{mtp} object
#' @family smoothing functions
#' @seealso \code{\link{smooth_mtp_list}} for lists of mtp objects and \code{\link{fun_smooth}} for individual curves
#' @export
#'
#' @export
#'
#' @examples
#' # df_mtp is data frame of mtp objects (in the .$mtp column) that comes with the package
#' smooth <- smooth_mtp(df_mtp$mtp[[1]])
#' plot_mtp(smooth)
#' plot_mtp(smooth, y_name = "fit")
#' plot_mtp(smooth, y_name = "resid")
#' plot_mtp(smooth, x_name = "value", y_name = "resid")
#' smooth <- smooth_mtp(df_mtp$mtp[[1]], no_points = 100)
#' plot_mtp(smooth, y_name = "resid")
#' smooth <- smooth_mtp(df_mtp$mtp[[1]], no_points = 100, fit_degree = 2)
#' mtp_plot(smooth, y_name = "resid")

smooth_mtp <- function(mtp, ...){
  smooth <- mtp %>%
    dplyr::group_by(curve_id, group, Name, Hor., Ver.) %>%
    dplyr::do(fun_smooth(., ...)) %>%
    dplyr::ungroup()

  structure(smooth, class = c("mtp", "tbl_df", "tbl", "data.frame"))
}


#' A function to smooth a list of data frames of the mtp type
#'
#' @param list_of_mtp A list of data frames of class mtp
#' @param ... Parameters passed on to \code{\link{fun_smooth}}
#' @return Returns a list of smoothed mtp objects
#' @family smoothing functions
#' @seealso \code{\link{smooth_mtp}} for mtp objects and \code{\link{fun_smooth}} for individual curves
#' @export
#'
#' @examples
#' ds <- smooth_mtp_list(df_mtp$mtp)
#' ds <- smooth_mtp_list(df_mtp$mtp, no_points = 40)
#' plot_mtp_list(ds, y_name = "fit")
smooth_mtp_list <- function(list_of_mtp, ...){
  purrr::map(list_of_mtp, smooth_mtp, ...)
}
