#' Extract Single Player's Game Data
#'
#' \code{get_each_player} returns a dataframe of all of the games played by one player
#'
#'  This function will take in a single player's username and return the
#'  data on all the games they have played on chess.com
#'
#' @param username A string value of a player's name
#'
#' @import jsonlite
#' @import magrittr
#' @import dplyr
#' @import tidyr
#' @import stringr
#' @import lubridate
#' @import purrr

get_each_player <- function(username) {

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
    return(df)

  }

  # clean each game string, separate columns and convert to df
  clean_pgn <- function(df) {
    # notes:
    # this function will excluded "abandoned" games that didn't have a move recorded.
    # if it was abandoned and an opening was created, then it will be included in the results

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

    # create a variable to indicate which colour won the game
    cleaned_df <- cleaned_df %>%
      dplyr::mutate(winner = ifelse(Result == "0-1", "Black", ifelse(Result == "1-0", "White", "Draw")))

    # create a username variable for analysis purposes
    cleaned_df$Username <- username

    # function to extract the number of moves in each game
    get_num_moves <- function(moves_string) {
      n_moves <- suppressWarnings(stringr::str_extract_all(moves_string, "[^... ]+")[[1]] %>% as.numeric() %>% max(na.rm = T))
      return(n_moves)
    }

    # function to extract the ending in the ending url
    ending <- function(user, string, opponent) {
      x <- if(grepl(user, string)) {
        gsub(user, "", string)
      } else {
        x <- gsub(opponent, "", string)
      }
      x <- gsub("won ", "", x)
      x <- gsub(" \\- ", "", x)
      x <- stringr::str_squish(x)

      return(x)
    }


    # data cleaning and preprocessing
    cleaned_df <- cleaned_df %>%
      # convert date variables to ymd using lubridate::ymd()
      dplyr::mutate(Date = lubridate::ymd(Date),
                    EndDate = lubridate::ymd(EndDate)) %>%
      # feature engineering of some new features for analysis
      dplyr::mutate(n_Moves = mapply(get_num_moves, Moves),
                    UserOpponent = ifelse(White == Username, Black, White),
                    UserColour = ifelse(Username == White, "White", "Black"),
                    OpponentColour = ifelse(UserOpponent == White, "White", "Black"),
                    UserELO = as.numeric(ifelse(Username == White, WhiteElo, BlackElo)),
                    OpponentELO = as.numeric(ifelse(Username != White, WhiteElo, BlackElo))) %>%
      dplyr::mutate(UserResult = ifelse(Result == "0-1", "Black", ifelse(Result == "1-0", "White", "Draw")),
                    UserResult = ifelse(UserColour == UserResult, "Win", ifelse(UserResult == "Draw", "Draw", "Loss"))) %>%
      dplyr::mutate(DaysTaken = EndDate - Date) %>%
      dplyr::mutate(GameEnding = mapply(ending, Username, Termination, UserOpponent)) %>%
      dplyr::mutate(Opening = gsub(".*?/", "", ECOUrl),
                    Opening = sub("^.*?-", "", Opening))

  }

  output <- get_game_urls() %>%
    purrr::map(get_games) %>%
    convert_to_df() %>%
    clean_pgn() %>% dplyr::distinct(.keep_all = TRUE)

  cat("Data extracted\n")


  return(output)

}






#' Extract Chess Game Data
#'
#' \code{get_chess_data} returns a dataframe of game data for either a sinlge user
#' or a list of usernames
#'
#' This function will take in a list of player usernames and return
#' a dataframe of game metadata
#'
#' @param usernames A character vector of player usernames from chess.com
#'
#' @import purrr
#'
#' @export
get_game_data <- function(usernames) {
  df <- purrr::map_df(usernames, get_each_player)

  return(df)
}
