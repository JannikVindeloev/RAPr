#' Plots the curves of a \code{mtp} object
#'
#' @param df_mtp A data frame of class \code{mtp}
#' @param x_name The name of the variable to go on the x-axis, defaults to \code{hours}
#' @param y_name The name of the variable to go on the y-axis, defaults to \code{value}
#' @param title The title of the plot, defaults to "my plot"
#'
#' @return Returns a ggplot object that can be manipulated or printed
#' @seealso \code{\link{plot_to_pdf}}
#' @export
#'
#' @examples
#' # df_mtp is a data frame of class mtp that comes with the package
#' str(df_mtp)
#'
#' # Plot a MTP
#' df_plot <- df_mtp$mtp[[1]]
#' mtp_plot(df_plot, title = "a MTP data frame")
#'
#' # The function uses the package \code{ggplot2} to render the graphs.
#' # The package \code{dplyr} makes manipulation of data frames easy and uses the pipe \code{%>%} operator.
#' # create a data frame of \code{ggplot} objects
#' library(dplyr)
#' df <- df_mtp %>%
#' group_by(file, sheet, Plate) %>%
#' mutate(title = paste("file", .$file[1], "sheet", .$sheet[1], "plate", .$Plate[1])) %>%
#' do(gg = mtp_plot(.$mtp[[1]], title = .$title))
#'
#' # plot the first MTP
#' df$gg[[1]]
#'
#' # The \code{purrr} package makes list manipulation easy.
#' # plot all MTP
#' df$gg %>% purrr::walk(print)
#'
#' # print all files to a pdf file
#' # NOT RUN
#' pdf(file = "plots.pdf", paper = "a4r", width = 11, height = 8)
#' purrr::walk(df$gg, print)
#' dev.off()
#'
#' # modify parts of the plot using ggplot2 syntax:
#' df$gg[[1]] + ggplot2::geom_vline(xintercept = 5, colour = "red")
#' df$gg[[1]] + ggplot2::geom_hline(yintercept = 6, colour = "blue", linetype = 2) + ggplot2::ggtitle("my modified plot")

mtp_plot <- function(mtp, x_name = "hours", y_name = "value", title = "my title") {
  mtp %>%
    ggplot2::ggplot(ggplot2::aes_string(x = x_name, y = y_name))+
    ggplot2::facet_grid(Hor. ~ Ver.)+
    ggplot2::geom_line()+
    ggplot2::ggtitle(title)
}

#' Takes a list of data frames of class \code{mtp} and plots them to pdf
#'
#' @param dl A list of \code{mtp} data frames
#'
#' @return nothing but saves a pdf files of \code{ggplot}'s
#' @seealso \code{\link{plot_mtp}}, \code{\link{df_mtp}}
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
plot_to_pdf <- function(dl, file_name = "my_plots.pdf", ...){
  gg <- purrr::map2(dl, names(dl), ~ mtp_plot(.x, title = .y))
  pdf(file = file_name, paper = "a4r", width = 11, height = 8)
  purrr::walk(gg, print)
  dev.off()
}
