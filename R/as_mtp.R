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
#' df <- read_mtp_xls(path = system.file("extdata", "crystalban_salt_01.xls", package = "RAPr"))
#' # select the first mtp
#' df <- subset(df, Plate == "1")
#' df_mtp <- as_mtp(df)
#' class(df_mtp)
#' str(df_mtp)
#' df_mtp
#' summary(df_mtp)
#'
as_mtp <- function(df, type = "raw"){
  has_columns(df)
  has_format(df)
  df %>% structure(.,
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
                          "Ver." %in% df_names
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
