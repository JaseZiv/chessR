#' Extract Single Player's Game Data
#'
#' \code{get_each_player} returns a dataframe of all of the games played by one player
#'
#'  This function will take in a single player's username and return the
#'  data on all the games they have played on chess.com
#'
#' @param username A string value of a player's name
#'
#' @export
get_each_player <- function(username) {

  ## If the function took too long we could consider uncommenting these messages, otherwise just remove.

  # cat("Extracting ", username, " Data, please wait\n")

  output <- get_game_urls(username) |>
    map(get_games) |>
    convert_to_tibble() |>
    clean_pgn() |> distinct(.keep_all = TRUE)

  # cat("Data extracted\n")

  return(output)
}



#' Extract Chess Game Data
#'
#' \code{get_game_data} returns a dataframe of game data for either a single user
#' or a list of usernames
#'
#' This function will take in a list of player usernames and return
#' a dataframe of game metadata
#'
#' @param usernames A character vector of player usernames from chess.com
#'
#' @return a dataframe of chess.com data plus additional analysis columns
#'
#'
#' @examples
#' \dontrun{
#' chess_analysis_single <- get_game_data(usernames = "JaseZiv")
#' chess_analysis_multiple <- get_game_data(usernames = c("JaseZiv", "Smudgy1"))
#' }
#'
#' @export
get_game_data <- function(usernames) {
  df <- purrr::map_df(usernames, get_each_player)

  return(df)
}

## I propose to either move the other function to internals and make this the
## only front game_data function or just remove this entirely, I don't see the point
## of keeping such a function in the r package, everyone with the package is going to
## know to use map_df
