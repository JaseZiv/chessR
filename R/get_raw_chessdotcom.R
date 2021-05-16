#' Get Single Player Raw chess.com Game Data
#'
#' This function returns the raw json data for a player's
#' chess.com data as a data frame
#'
#' @param username A valid unsername from chess.com
#'
#' @importFrom magrittr %>%
#'
get_each_player_chessdotcom <- function(username) {
  cat("Extracting ", username, " Data, please wait\n")

  # this function gets a list of all year/months the player(s) has played on chess.com
  get_month_urls <- function(){
    jsonlite::fromJSON(paste0("https://api.chess.com/pub/player/", username, "/games/archives"))$archives
  }
  # apply function to get a character vector of game urls
  month_urls <- get_month_urls()

  # this function will parse the list of game urls and extract a json blob
  get_games <- function(y) {
    y <- jsonlite::fromJSON(y)
  }
  # apply function to get a list of all the games and game data
  games <- month_urls %>% purrr::map(get_games)

  # function to parse and extract game metadata
  extract_pgn <- function(x){
    tryCatch( {x <- x$games$pgn}, error = function(x) {x <- NA})
  }
  # apply to get a list of all games' metadata
  extracted_pgns <- games %>% purrr::map(extract_pgn)
  # function to create a single list to prepare for converting to a data frame
  create_pgn_list <-function(x) {
    x <- unlist(x) %>% as.list()
  }
  # apply the function to result in a list of each individual game
  pgn_list <- create_pgn_list(extracted_pgns)

  # Additional metadata:
  # function to extract the rules of each game
  extract_rules <- function(x){
    tryCatch( {x <- x$games$rules}, error = function(x) {x <- NA}) %>% as.character() %>% data.frame() %>% dplyr::mutate_if(is.factor, as.character)
  }
  GameRules <- games %>% purrr::map_df(extract_rules)
  # function to extract the time class of each game (ie blitz, bullet, daily, etc)
  extract_time_class <- function(x){
    tryCatch( {x <- x$games$time_class}, error = function(x) {x <- NA}) %>% as.character() %>% data.frame() %>% dplyr::mutate_if(is.factor, as.character)
  }
  TimeClass <- games %>%  purrr::map_df(extract_time_class)

  extra_df <- cbind(GameRules, TimeClass) %>% data.frame()
  colnames(extra_df) <- c("GameRules", "TimeClass")

  # function to extract all elements as columns, and all games as row in a data frame
  convert_to_df <- function(exp_list) {
    if(is.na(exp_list)) {
      df <- data.frame(Event=NA_character_)
    } else {
      pgn_list <- strsplit(exp_list, "\n") %>% unlist()
      tab_names <- c(gsub( "\\s.*", "", pgn_list[grep("\\[", pgn_list)][-c(length(pgn_list), (length(pgn_list)-1))]) %>% gsub("\\[", "", .), "Moves")
      tab_values <- gsub(".*[\"]([^\"]+)[\"].*", "\\1", pgn_list[grep("\\[", pgn_list)])
      if(length(tab_names) != length(tab_values)) {
        tab_values <- c(tab_values, NA)
      }
      #create the df of values
      df <- rbind(tab_values) %>% data.frame(stringsAsFactors = F)
      colnames(df) <- tab_names
      # remove the row names
      rownames(df) <- c()
      # need to clean up date variables
      df$Date <-  gsub("\\.", "-", df$Date)
      df$EndDate <- gsub("\\.", "-", df$EndDate)
    }

    return(df)
  }
  # convert the lists to data frames
  df <- pgn_list %>% purrr::map_df(convert_to_df)
  df <- cbind(extra_df, df)
  df$Username <- username
  # output the final data frame for each player
  return(df)
}


#' Get Raw chess.com Game Data
#'
#' This function returns the raw json data for a player's or list of players'
#' chess.com data as a data frame
#'
#' @param usernames A vector of a valid unsername or usernames from chess.com
#'
#' @importFrom magrittr %>%
#'
#' @export
get_raw_chessdotcom <- function(usernames) {
  df <- usernames %>% purrr::map_df(get_each_player_chessdotcom)
}
