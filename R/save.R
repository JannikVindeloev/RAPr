#' saves for CH 16:9 template in long format
#'
#' @param filename defaults to plot.png
#' @param plot - specify the ggplot object to save. Defaults to last_plot()

save_169_long <- function(filename = "plot.png", plot = last_plot()) {
  ggsave(filename, width = 22.68, plot, height = 9.8, units = "cm", dpi = 300)
}

#' saves for CH 16:9 template in square format
#'
#' @param filename defaults to plot.png
#' @param plot - specify the ggplot object to save. Defaults to last_plot()
#'
save_169_square <- function(filename = "plot.png", plot = last_plot()) {
  ggsave(filename, width = 9.8, plot, height = 9.8, units = "cm", dpi = 300)
}

#' saves for A4 horizontal template in square format
#'
#' @param filename defaults to plot.png
#' @param plot - specify the ggplot object to save. Defaults to last_plot()
#'
save_A4_hor <- function(filename = "plot.png", plot = last_plot()) {
  ggsave(filename, width = 29.7, plot, height = 21, units = "cm", dpi = 300)
}

#' saves for A4 vertical template in square format
#'
#' @param filename defaults to plot.png
#' @param plot - specify the ggplot object to save. Defaults to last_plot()
#'
save_A4_ver <- function(filename = "plot.png", plot = last_plot()) {
  ggsave(filename, width = 21, plot, height = 29.7, units = "cm", dpi = 300)
}
