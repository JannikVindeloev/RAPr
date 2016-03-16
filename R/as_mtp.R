
#' The function extracts mtp objects from a file sourced from BO (screening
#' data)
#'
#' @param data_frame A data frame that must contain \code{Filename},
#'   \code{Scanner}, \code{`Plate Setup`}, \code{Plate}, \code{Name},
#'   \code{Hor.}, \code{Ver.}, as well as columns with numerical headings in
#'   hours.
#'
#' @return a data frame with a list column \code{mtp} containing the \code{mtp} objects
#' @export
#'
#' @examples
#' # to be made
mtp_extract <- function(data_frame) {
  #standardise names
  names(data_frame) <- str_replace_all(names(data_frame),"(\\[|\\])", "")
  data_frame <- data_frame %>%
    rename(Hor. = Hor, Ver. = Ver) %>%
    mutate(curve_id = seq(1:n()),
           group = as.integer(factor(paste(Filename, Scanner, Plate)))) %>%
    select(Filename, Scanner, `Plate Setup`, Plate, curve_id, group, Name = Chcc, Hor., Ver., matches("\\d")) %>%
    gather(hours, value, matches("\\d")) %>%
    filter(!is.na(value)) %>%
    group_by(Filename, Scanner, Plate, `Plate Setup`) %>%
    mutate(Hor. = factor(Hor.),
           Ver. = factor(Ver.),
           Name = as.character(Name),
           hours = as.numeric(hours)) %>%
    do(mtp = as_mtp(.))
}

#' Converts a data frame to a data frame of class mtp
#'
#'
#' @param df a data frame to convert. It must be a data frame containing the
#'   following variables: \code{Hor.}, \code{Ver.}, \code{Name}, \code{hours},
#'   and \code{value}.
#' @param type is an attribute to be set to show what kind of manipulations have been done (currently not used)
#' @return Returns a df of class mtp
#' @export
#'
#' @examples
#' df_mtp <- xls_read(path = system.file("extdata", "crystalban_salt_01.xls", package = "RAPr"))
#' mtp <- df_mtp$mtp
#' class(df_mtp)
#' str(df_mtp)
#' df_mtp
#' summary(df_mtp)
#'
as_mtp <- function(df, type = "raw"){
  has_columns(df)
  has_format(df)
  df_names <- names(df)
  if(!("fit" %in% df_names)) df$fit <- NA_real_
  if(!("resid" %in% df_names)) df$resid <- NA_real_
  df %>%
    dplyr::select(curve_id, group, Name, Hor., Ver., hours, value, fit, resid) %>%
    structure(.,
              type = type,
              class = c("mtp", "tbl_df", "tbl", "data.frame"))
}

#' Internal function to check if the data frame contains the as a minimum the right columns.
#'
#' @param df a data frame
#'
#' @return TRUE or error
#'
has_columns <- function(df) {
  df_names <- names(df)
  assertthat::assert_that(#"ID" %in% df_names,
                          #"Plate" %in% df_names,
                          "Hor." %in% df_names,
                          "Ver." %in% df_names,
                          "group" %in% df_names,
                          "curve_id" %in% df_names
                          )
}


#' Internal function to check if the data frame columns have the right formats.
#'
#' @param df a data frame
#'
#' @return TRUE or an error
#'
has_format <- function(df) {
  assertthat::assert_that(#is.character(df$ID),
                          #is.character(df$Plate),
                          is.integer(df$curve_id),
                          is.factor(df$Hor.),
                          is.factor(df$Ver.),
                          is.character(df$Name),
                          is.numeric(df$hours),
                          is.numeric(df$value))
}
