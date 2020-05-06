#' Get Top 50 on chess.com Leaderboards
#'
#' This function takes in one parameter, the game_type, and returns a
#' data frame of the top 50 players on chess.com.
#'
#' The leaderboard options (games) include:
#'
#' \emph{"daily"}, \emph{"daily960}", \emph{"live_rapid"},
#' \emph{"live_blitz"}, \emph{"live_bullet"}, \emph{"live_bughouse"},
#' \emph{"live_blitz960"}, \emph{"live_threecheck" }, \emph{"live_crazyhouse"},
#' \emph{"live_kingofthehill"}, \emph{"lessons"}, \emph{"tactics"}
#'
#' @param game_type A valid chess.com game type to return the leaderboard for
#'
#' @import magrittr
#' @import jsonlite
#'
#' @export
chessdotcom_leaderboard <- function(game_type = "daily") {
  df <- jsonlite::fromJSON("https://api.chess.com/pub/leaderboards")[game_type] %>% unname() %>% data.frame()
  df$X.id <- NULL
  return(df)
}
