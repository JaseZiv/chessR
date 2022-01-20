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
#' @return a dataframe of the chess.com top 50 players based on game_type selected
#'
#' @importFrom magrittr %>%
#'
#' @export
#'
#' @examples
#' \dontrun{
#' chessdotcom_leaderboard(game_type = "daily")
#' }
chessdotcom_leaderboard <- function(game_type = "daily") {
  df <- jsonlite::fromJSON("https://api.chess.com/pub/leaderboards")[game_type] %>% unname() %>% data.frame()
  df$X.id <- NULL
  return(df)
}



#' Get top players on Lichess leaderboards
#'
#' This function takes in two parameters; how many players you want
#' returned (max 200) and the speed variant. The result is a data
#' frame for each game type
#'
#' The leaderboard speed variant options include:
#'
#' \emph{"ultraBullet"}, \emph{"bullet}", \emph{"blitz"},
#' \emph{"rapid"}, \emph{"classical"}, \emph{"chess960"},
#' \emph{"crazyhouse"}, \emph{"antichess" }, \emph{"atomic"},
#' \emph{"horde"}, \emph{"kingOfTheHill"}, \emph{"racingKings"},
#' \emph{"threeCheck"}
#'
#' @param top_n_players The number of players (up to 200) you want returned
#' @param speed_variant A valid lichess speed variant to return the leaderboard for
#'
#' @return a dataframe of the lichess top players based on speed_variant and top_n_players selected
#'
#' @examples
#' \dontrun{
#' top10_blitz <- lichess_leaderboard(top_n_players = 10, speed_variant = "blitz")
#' leaderboards <- purrr::map2_df(top_n_players = 10, c("ultraBullet", "bullet"), lichess_leaderboard)
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
lichess_leaderboard <- function(top_n_players, speed_variant) {
  # extract and convert to DF
  top_leaders <- xml2::read_html(paste0("https://lichess.org/player/top/", top_n_players, "/", speed_variant)) %>%
    rvest::html_table() %>%
    data.frame()
  # player names come with the players title at the beginning of the string, need to remove,
  # but to do that, need to know what the titles are
  player_status_codes <- gsub( "\\s.*", "", top_leaders$X2[grep("\\s", top_leaders$X2)]) %>% unique()
  # create a new column for just the player's username
  top_leaders$Usernames <- gsub(paste(player_status_codes, collapse="|"), "", top_leaders$X2) %>% gsub("\\s", "", .)
  colnames(top_leaders) <- c("Rank", "TitleAndName", "Rating", "Progress", "Username")

  # function to extract the player's title from the Player name string
  extract_title <- function(x){
    if(stringr::str_detect(x, "\\s+")){
      x <- gsub( "\\s.*", "", x)
    } else{
      x <- NA
    }
  }
  # extract the title
  top_leaders$Title <- mapply(extract_title, top_leaders$TitleAndName)
  # reorder the columns in the df to flow
  top_leaders <- top_leaders %>% dplyr::select(.data$Rank, .data$Title, .data$Username, .data$Rating, .data$Progress)
  # add the speed variant as a column for when multiple variants looped through the function
  top_leaders$SpeedVariant <- speed_variant

  return(top_leaders)
}
