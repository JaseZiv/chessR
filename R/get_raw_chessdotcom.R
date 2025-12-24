#' Get Raw chess.com Game Data
#'
#' This function returns the raw json data for a player's or list of players'
#' chess.com data as a data frame, for all or select months played
#'
#' @param usernames A vector of a valid username or usernames from chess.com
#' @param year_month An integer of YYYYMM
#'
#' @return a dataframe of chessdotcom data
#'
#' @export
#'
#' @examples
#' \dontrun{
#' get_raw_chessdotcom(usernames = "JaseZiv")
#' get_raw_chessdotcom(usernames = "JaseZiv", year_month = c(202112:202201))
#' get_raw_chessdotcom(usernames = c("JaseZiv", "Smudgy1"), year_month = 202201)
#' }
get_raw_chessdotcom <- function(usernames, year_month = NA_integer_) {
  df <- map2_df(usernames, year_month, get_each_player_chessdotcom)
  return(df)
}
