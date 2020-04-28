#' Get Raw chess.com Game Data
#'
#' This function returns the raw json data for a player's or list of players'
#' chess.com data as a date frame
#'
#' @param usernames A vector of a valid unsername or usernames from chess.com
#'
#' @import magrittr
#' @import purrr
#' @import jsonlite
#' @import dplyr
#' @import tidyr
#'
#' @export
get_raw_game_data <- function(usernames) {

  cat("Extracting Data, please wait\n")
  usernames <- usernames


  get_game_urls <- function(){

    user_names <- usernames
    part1 <- "https://api.chess.com/pub/player/"
    part2 <- "/games/archives"

    archive_url <- paste0(part1, user_names, part2)
    archive_urls <- purrr::map(archive_url, jsonlite::fromJSON) %>% unlist() %>% unname()

    return(archive_urls)
  }

  get_games <- function(y) {
    tryCatch( {y <- jsonlite::fromJSON(y)}, error = function(x) {y <- NA})
  }

  extract_pgn <- function(x){
    tryCatch( {x <- x$games$pgn}, error = function(x) {x <- NA}) %>% as.character() %>% data.frame() %>% dplyr::mutate_if(is.factor, as.character)

  }

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


  }

  output <- get_game_urls() %>%
    purrr::map(get_games) %>%
    purrr::map_df(extract_pgn) %>%
    clean_pgn()

  cat("Data extracted\n")


  return(output %>% dplyr::distinct(.keep_all = TRUE))

}
