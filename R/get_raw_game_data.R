#' Get Single Player Raw chess.com Game Data
#'
#' This function returns the raw json data for a player's
#' chess.com data as a data frame
#'
#' @param username A valid unsername from chess.com
#'
#' @import magrittr
#' @import purrr
#' @import jsonlite
#' @import dplyr
#' @import tidyr
#'
get_each_raw <- function(username) {
  .Deprecated("get_raw_chessdotcom")
  cat("Extracting ", username, " Data, please wait\n")



  # this function gets a list of all year/months the player(s) has played on chess.com
  get_game_urls <- function(){
    jsonlite::fromJSON(paste0("https://api.chess.com/pub/player/", username, "/games/archives"))$archives
  }

  # this function will parse the list of game urls and extract a json blob
  get_games <- function(y) {
    y <- jsonlite::fromJSON(y)
  }


  convert_to_df <- function(games_list) {

    # function to extract the game and moves data required for analysis
    extract_pgn <- function(x){
      tryCatch( {x <- x$games$pgn}, error = function(x) {x <- NA}) %>% as.character() %>% data.frame() %>% dplyr::mutate_if(is.factor, as.character)
    }
    pgn <- games_list %>%
      purrr::map_df(extract_pgn)

    # function to extract the rules of each game
    extract_rules <- function(x){
      tryCatch( {x <- x$games$rules}, error = function(x) {x <- NA}) %>% as.character() %>% data.frame() %>% dplyr::mutate_if(is.factor, as.character)
    }

    # function to extract the time class of each game (ie blitz, bullet, daily, etc)
    extract_time_class <- function(x){
      tryCatch( {x <- x$games$time_class}, error = function(x) {x <- NA}) %>% as.character() %>% data.frame() %>% dplyr::mutate_if(is.factor, as.character)
    }

    rules <- games_list %>%
      purrr::map_df(extract_rules)

    time_class <- games_list %>%
      purrr::map_df(extract_time_class)

    df <- cbind(rules, time_class, pgn) %>% data.frame()
    colnames(df) <- c("rules", "time_class", "pgn")


    cleaned_df <- df[grep("\\{", df$pgn),]

    cleaned_df <- cleaned_df %>% dplyr::filter(.data$rules == "chess")
    cleaned_df <- cleaned_df %>% dplyr::filter(.data$time_class %in% c("blitz", "bullet",  "daily",  "rapid"))
    cleaned_df <- cleaned_df %>% dplyr::filter(!stringr::str_detect(.data$pgn, "Tournament"))
    cleaned_df <- cleaned_df %>% dplyr::filter(!stringr::str_detect(.data$pgn, "club/matches"))

    cleaned_df <- cleaned_df %>%
      tidyr::separate(.data$pgn, into = c("Event", "Site", "Date", "Round", "White", "Black", "Result", "ECO", "ECOUrl", "CurrentPosition", "Timezone",
                                          "UTCDate", "UTCTime", "WhiteElo", "BlackElo", "TimeControl", "Termination", "StartTime", "EndDate", "EndTime",
                                          "Link", "Moves"), sep = "]\n")


    # create a vector of the variables that contains the data we need withing double quotes
    vars_to_extract <- c("Event", "Site", "Date", "Round", "White", "Black", "Result", "ECO", "ECOUrl", "CurrentPosition", "Timezone",
                         "UTCDate", "UTCTime", "WhiteElo", "BlackElo", "TimeControl", "Termination", "StartTime", "EndDate", "EndTime",
                         "Link")
    # function to extract the data contained within the double quotes
    extract_data <- function(x) {sub('[^\"]+\"([^\"]+).*', '\\1', x)}
    # extract the data
    cleaned_df <- cleaned_df %>%
      dplyr::mutate_at(vars_to_extract, extract_data) %>% dplyr::mutate_if(is.factor, as.character)

    cleaned_df <- cleaned_df %>% dplyr::mutate(Username = username)

  }



  output <- get_game_urls() %>%
    purrr::map(get_games) %>%
    convert_to_df() %>% dplyr::distinct(.keep_all = TRUE)

  cat("Data extracted\n")


  return(output)

}


#' Get Raw chess.com Game Data
#'
#' This function returns the raw json data for a player's or list of players'
#' chess.com data as a data frame
#'
#' @param usernames A vector of a valid unsername or usernames from chess.com
#'
#' @import magrittr
#' @import purrr
#'
#' @export
get_raw_game_data <- function(usernames) {
  .Deprecated("get_raw_chessdotcom")
  df <- purrr::map_df(usernames, get_each_raw)

  return(df)
}
