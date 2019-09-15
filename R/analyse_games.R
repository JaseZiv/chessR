#' Extract Single Player's Game Data
#'
#' \code{analyse_player_games} returns a dataframe of all of the games played by one player
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
#'
#' @export
analyse_player_games <- function(username) {

  cat("Extracting ", username, " Data, please wait\n")



  # this function gets a list of all year/months the player(s) has played on chess.com
  get_game_urls <- function(){
    jsonlite::fromJSON(paste0("https://api.chess.com/pub/player/", username, "/games/archives"))$archives
  }

  # this function will parse the list of game urls and extract a json blob
  get_games <- function(y) {
    y <- jsonlite::fromJSON(y)
  }


  # function to extract the game and moves data required for analysis
  extract_pgn <- function(x){
    tryCatch( {x <- x$games$pgn}, error = function(x) {x <- NA}) %>% as.character() %>% data.frame() %>% dplyr::mutate_if(is.factor, as.character)

  }

  # clean each game string, separate columns and convert to df
  clean_pgn <- function(df) {

    colnames(df) <- "each_game"
    # split column into the columns contained in the data
    cleaned_df <- suppressWarnings(tidyr::separate(df, each_game, into = c("Event", "Site", "Date", "Round", "White", "Black", "Result", "ECO", "ECOUrl", "CurrentPosition", "Timezone",
                                                                           "UTCDate", "UTCTime", "WhiteElo", "BlackElo", "TimeControl", "Termination", "StartTime", "EndDate", "EndTime",
                                                                           "Link", "Moves"), sep = "]\n"))

    # create a vector of the variables that contains the data we need withing double quotes
    vars_to_extract <- c("Event", "Site", "Date", "Round", "White", "Black", "Result", "ECO", "ECOUrl", "CurrentPosition", "Timezone",
                         "UTCDate", "UTCTime", "WhiteElo", "BlackElo", "TimeControl", "Termination", "StartTime", "EndDate", "EndTime",
                         "Link")
    # function to extract the data contained within the double quotes
    extract_data <- function(x) {sub('[^\"]+\"([^\"]+).*', '\\1', x)}
    # extract the data
    cleaned_df <- cleaned_df %>%
      dplyr::mutate_at(vars_to_extract, extract_data) %>% dplyr::mutate_if(is.factor, as.character)

    # print a message to indicate how many observations there are without analysis data
    print(paste0("There were ", sum(is.na(cleaned_df$Moves), na.rm = T) + sum(cleaned_df$Event == "Live Chess - Chess960", na.rm = T), " records removed due to there being no analysis data"))

    # filter the records that don't have analysis data for
    cleaned_df <- cleaned_df %>% dplyr::filter(!is.na(Moves))
    cleaned_df <- cleaned_df %>% dplyr::filter(Event != "Live Chess - Chess960")

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
                    UserELO = as.numeric(ifelse(Username == White, WhiteElo, BlackElo)),
                    OpponentELO = as.numeric(ifelse(Username != White, WhiteElo, BlackElo))) %>%
      dplyr::mutate(UserResult = ifelse(Result == "0-1", "Black", ifelse(Result == "1-0", "White", "Draw")),
                    UserResult = ifelse(UserColour == UserResult, "Win", ifelse(UserResult == "Draw", "Draw", "Loss"))) %>%
      dplyr::mutate(DaysTaken = EndDate - Date) %>%
      dplyr::mutate(GameEnding = mapply(ending, Username, Termination, UserOpponent)) %>%
      dplyr::mutate(Openings = gsub(".*?/", "", ECOUrl))

  }

  output <- get_game_urls() %>%
    purrr::map(get_games) %>%
    purrr::map_df(extract_pgn) %>%
    clean_pgn() %>% distinct(.keep_all = TRUE)

  cat("Data extracted\n")


  return(output)

}






#' Extract Multiple Player's Game Data
#'
#' \code{analyse_multiple_players} returns a dataframe of game data for a list of usernames
#'
#' This function will take in a list of player usernames and return
#' a dataframe of game metadata
#'
#' @param usernames A list of player usernames from chess.com
#'
#' @import purrr
#'
#' @export
analyse_multiple_players <- function(usernames) {
  df <- purrr::map_df(usernames, analyse_player_games)

  return(df)
}
