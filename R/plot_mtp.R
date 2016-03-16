#' Plots the curves of a \code{mtp} object
#'
#' @param mtp A \code{mtp} object
#' @param x_name The name of the variable to go on the x-axis, defaults to
#'   \code{hours}
#' @param y_name The name of the variable to go on the y-axis, defaults to
#'   \code{value}
#' @param title The title of the plot, defaults to "my plot"
#' @return Returns a \code{ggplot} object that can be manipulated or printed
#' @family plot functions
#' @seealso \code{\link{plot_mtp_list}} for creating a list of \code{ggplot}
#'   objects and \code{\link{plot_to_pdf}} for plotting a list of mtp objects to
#'   pdf
#' @export
#' @examples
#' # df_mtp is a data frame of class mtp that comes with the package
#' str(df_mtp)
#'
#' # plot a specific mtp object
#' df_plot <- df_mtp$mtp[[1]]
#' df_plot$Name <- as.character(1:96)
#' plot_mtp(df_plot, title = "a MTP data frame")


plot_mtp <- function(mtp, x_name = "hours", y_name = "value", title = "my title") {
  y_min <- min(mtp[y_name])
  x_min <- min(mtp[x_name])
  mtp$Name <- ifelse(is.na(mtp$Name), "", mtp$Name)

  df_names <- mtp %>%
    dplyr::select(Hor., Ver., Name) %>%
    dplyr::group_by(Hor., Ver.) %>%
    dplyr::summarize(Name = paste(sort(unique(Name)), collapse = ","))

  mtp %>%
    ggplot2::ggplot(ggplot2::aes_string(x = x_name, y = y_name))+
    ggplot2::facet_grid(Hor. ~ Ver.)+
    ggplot2::geom_text(data = df_names,
                       x = x_min, y = y_min, hjust = 0, vjust = 0, size = 3,
                       ggplot2::aes(label = Name), show.legend = FALSE)+
    ggplot2::geom_line(ggplot2::aes(group = curve_id, colour = factor(group)))+
    ggplot2::ggtitle(title)+
    ggplot2::labs(colour = "group")
}

#' Creates a list of \code{ggplot} objects from a list of \code{mtp} objects
#'
#' @param list_of_mtp A list of (named) \code{mtp} objects
#' @param ... Parameters passed on to \code{\link{plot_mtp}}
#' @details List elements can have names, and the title of the \code{ggplot} objects defaults to
#'   the name of the elements in the list of \code{mtp} objects
#' @return Returns a list of \code{ggplot} objects
#' @seealso \code{\link{plot_mtp}} for plotting a \code{mtp} object and
#'   \code{\link{plot_to_pdf}} for plotting a list of mtp objects to pdf
#' @family plot functions
#' @export
#' @examples
#' # df_mtp is a data frame of class mtp that comes with the package
#' names(df_mtp$mtp) <- paste("mtp", seq(1:length(mtps)))
#' names(df_mtp$mtp)
#' plots <- plot_mtp_list(df_mtp$mtp)
#'
#' # to print all the plots individually using the \code{purrr} library
#' purrr::walk(plots, print)
#'
plot_mtp_list <- function(list_of_mtp, ...){
  plot_names <- names(list_of_mtp)

  if(is.null(plot_names)) {
    plot_names <- rep("title", length(list_of_mtp))
  }
  purrr::map2(list_of_mtp, plot_names, ~plot_mtp(.x, title = .y, ...))
}

#' Takes a list of \code{mtp} objects and plots them to pdf
#'
#' @param list_of_mtp A list of \code{mtp} objects
#'
#' @return nothing but saves a pdf files of \code{ggplot}'s
#' @seealso \code{\link{plot_mtp}} for plotting a \code{mtp} object and
#'   \code{\link{plot_mtp_list}} for making a list of \code{ggplot} objects
#' @family plot functions
#' @export
#'
#' @examples
#' library(dplyr)
#' df_plots <- df_mtp %>%
#' transmute(title = paste("file", .$file[1], "sheet", .$sheet[1], "plate", .$Plate[1]), mtp = mtp)
#'
#' dl <- df_plots$mtp
#' names(dl) <- df_plots$title
#'
#' plot_to_pdf(dl)
#'
plot_to_pdf <- function(list_of_mtp, file_name = "my_plots.pdf", ...){
  gg <- plot_mtp_list(list_of_mtp, ...)
  pdf(file = file_name, paper = "a4r", width = 11, height = 8)
  purrr::walk(gg, print)
  dev.off()
}
