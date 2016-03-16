#' Reads a single \code{.xls} or \code{.xlsx} file containing \code{RAP} data from disk.
#'
#' @param path The path of the file containing the mtp data. Defaults to file.choose()
#' @param sheets The name of the sheet containing the mtp data. Defaults to readxl::excel_sheets(path)
#'
#' @return Returns a data frame of format \code{file}, \code{sheet}, \code{ID}, \code{Plate}, and \code{mtp} which contains the \code{mtp} plate objects.
#' @seealso \code{\link{xls_folder_read}}
#' @export
#'
#' @examples
#' # specifying the file path. \code{system.file} is a function to search e.g. within a package.
#' df_mtp <- xls_read(path = system.file("extdata", "crystalban_salt_01.xls", package = "RAPr"))
#' df_mtp
#' df_mtp$mtp[1] # a list item containing a mtp object
#' class(df_mtp$mtp[1])
#' df_mtp$mtp[[1]] # a mtp object
#' class(df_mtp$mtp[[1]])
#'
#' # browsing for files in the current directory
#' df_mtp <- read_mtp_xls()
#'
#' # reading all files in a directory starting from the working directory
#' # replace with your own path to work
#' ## NOT RUN
#' my_path <- "./inst/extdata/"
#' files <- list.files(path = my_path)
#' df_mtp <- purrr::map_df(files, function(file_name) xls_read(path = paste(my_path, file_name, sep = "")))
#' df_mtp
#' ## END NOT RUN
xls_read <- function(path = file.choose(), sheets = readxl::excel_sheets(path)) {
  # make data frame with sheet names
  df <- purrr::map_df(sheets,
                      function(sheet) readxl::read_excel(path = path, sheet = sheet),
                      .id = "sheet") %>%
    dplyr::mutate(file = basename(path)) %>%
    dplyr::select(file, sheet, everything())

  # Check that the imported data frame has the right columns
  # assertthat::assert_that(has_columns(df))

  # coerce into correct columns
  df <- df %>%
    dplyr::mutate(Hor. = as.factor(Hor.),
                  Ver. = as.factor(Ver.),
                  Plate = as.character(Plate),
                  Name = as.character(Name))


  # add unique curve id, and colour per MTP, fold, and remove NA values
  df <- df %>%
    dplyr::mutate(curve_id = as.integer(seq(1, n())),
                  group = as.integer(factor(paste(file, sheet, ID, Plate)))) %>%
    dplyr::select(file, sheet, ID, Plate, curve_id, group, everything()) %>%
    tidyr::gather(hours, value, -file:-Name) %>%
    dplyr::mutate(hours = as.numeric(hours)/60) %>%
    dplyr::filter(!is.na(value)) %>%
    group_by(file, sheet, ID, Plate) %>%
    do(mtp = as_mtp(.))

  # return a data_frame of class mtp
  return(df)
}


#' Reads \code{.xls} or \code{.xlsx} files containing \code{RAP} data from a given directory on disk.
#'
#' @param my_path
#'
#' @return Returns a data frame of format \code{file}, \code{sheet}, \code{ID}, \code{Plate}, and \code{mtp} which contains the \code{mtp} plate objects.
#' @seealso \code{\link{xls_read}}
#' @export
#'
#' @examples
#' # interactively read a folder
#' df_mtp <- xls_folder_read()
#' df_mtp
#' ## NOT RUN
#' df_mtp <- xls_folder_read("./inst/extdata")
#' df_mtp
#' ## END NOT RUN
xls_folder_read <- function(my_path = dirname(file.choose())) {
  files <- list.files(path = my_path)
  df <- purrr::map_df(files, function(file_name) xls_read(path = paste(my_path, file_name, sep = "/")))
}
