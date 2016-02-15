#' Loess smothing of a well curve
#'
#' @param df takes a data frame of class mtp as input
#' @param x_name the name of the x variable of type character
#' @param y_name the name of the y variable of type character
#' @param no_points The number of points in the smooth window of type numeric
#' @param fit_degree The degree of the smooth polynomial of type numeric. must be less than or equal to 2
#'
#' @return returns a data frame of class smooth
#'
#' @export
#'
#' @examples
#' df <- dl[[1]]
#' df <- subset(df, Hor. == "A" & Ver. == "1")
#' fun_smooth(df)
#'
fun_smooth <- function(df, x_name = "hours", y_name = "value", no_points = 40, fit_degree = 1) {

  x <- df[[x_name]]
  y <- df[[y_name]]
  my_span = no_points/length(y)

  assertthat::assert_that(is.numeric(x),
                          is.numeric(y),
                          x_name %in% names(df),
                          y_name %in% names(df),
                          is.numeric(no_points),
                          is.numeric(fit_degree),
                          length(x) == length(y),
                          length(x) > no_points,
                          my_span <= 1,
                          fit_degree <=2,
                          !any(is.na(x)),
                          !any(is.na(y)))

  model <- loess(y ~ x, span = my_span, degree = fit_degree)

dplyr::data_frame(hours = x,
           value = y,
           fit = fitted(model),
           resid = residuals(model))
}


#' smooth the well curves of a data frame of class mtp
#'
#' @param df_mtp a data frame of class mtp and attribute type = "raw"
#' @param ... parameter passed on to fun_smooth
#'
#' @return a data_frame of class mtp and attribute type = "fit"
#'
#' @seealso \code{fun_smooth}
#'
#' @export
#'
#' @examples
#' # df_mtp is data frame of mtp objects (in the .$mtp column) that comes with the package
#' smooth <- mtp_smooth(df_mtp$mtp[[1]])
#' mtp_plot(smooth)
#' mtp_plot(smooth, y_name = "fit")
#' mtp_plot(smooth, y_name = "resid")
#' mtp_plot(smooth, x_name = "value", y_name = "resid")
#' smooth <- mtp_smooth(df_mtp$mtp[[1]], no_points = 100)
#' mtp_plot(smooth, y_name = "fit")
#' mtp_plot(smooth, y_name = "resid")
#' mtp_plot(smooth, x_name = "value", y_name = "resid")
#' smooth <- mtp_smooth(df_mtp$mtp[[1]], no_points = 100, fit_degree = 2)
#' mtp_plot(smooth, y_name = "fit")
#' mtp_plot(smooth, y_name = "resid")
#' mtp_plot(smooth, x_name = "value", y_name = "resid")
#' smooth <- mtp_smooth(df_mtp$mtp[[1]], no_points = 30, fit_degree = 1)
#' mtp_plot(smooth, y_name = "fit")
#' mtp_plot(smooth, y_name = "resid")
#' mtp_plot(smooth, x_name = "value", y_name = "resid")
#' # now we loop through all the mtp
#' list_of_mtp <- df_mtp$mtp
#' list_of_smooth_mtp <- purrr::map(list_of_mtp, mtp_smooth)
#'
#' # To keep the structure of df_map we can do the following (be sure to have one mtp per row)
#' smooth <- df_mtp %>% group_by(file, sheet, ID, Plate) %>% do(smtp = mtp_smooth(.$mtp[[1]]))
#' smooth <- df_mtp %>% group_by(-mtp) %>% do(smtp = mtp_smooth(.$mtp[[1]]))
#'
#' mtp_plot(smooth$smtp[[1]], y_name = "fit")


mtp_smooth <- function(mtp, ...){
  smooth <- mtp %>%
    dplyr::group_by(curve_id, Name, Hor., Ver.) %>%
    dplyr::do(fun_smooth(., ...)) %>%
    dplyr::ungroup()

  as_mtp(smooth)
}


#' A function to smooth a list of data frames of the mtp type
#'
#' @param list_df_mtp A list of data frames of class mtp
#' @param ... Smoother parameters
#'
#' @return a list of smoothed data frames of class mtp
#' @export
#'
#' @examples
#' ds <- smooth_all_mtp(dl)
#'
smooth_all_mtp <- function(list_df_mtp, ...){
  purrr::map(list_df_mtp, smooth_mtp)
}
