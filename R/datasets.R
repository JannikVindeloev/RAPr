#' a data frame containing mtp imported mtp data
#'
#' @format A data frame with  24000 rows and 10 variables:
#' \describe{
#'    \item{file}{the file the data set was sourced from, must be character}
#'    \item{sheet}{the sheet in that particular file, must be character}
#'    \item{ID}{The ID of the file, must be character}
#'    \item{Plate}{The position on the scanner (1 to 4), must be character}
#'    \item{mtp}{data frames of mtp class} containg (curve_id:value):
#'    \item{curve_id}{An unique id for each well, must be integer}
#'    \item{Hor.}{The horisontal identifier for the MTP, must be factor}
#'    \item{Ver.}{The vertical identifier for the MTP, must be factor}
#'    \item{Name}{The name of the well, must be a character}
#'    \item{hours}{the time in hours, must be numeric}
#'    \item{value}{the value, must be numeric}
#'     }
"df_mtp"
