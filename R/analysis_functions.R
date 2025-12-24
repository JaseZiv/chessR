#' Return the number of moves in a game
#'
#' This function returns the number of moves played in each game. The function
#' accepts a vector of chess Moves data in PGN notation, usually called 'Moves'
#'
#' @param moves_string A character vector of chess Moves data in PGN notation usually called 'Moves' in extracted data
#'
#' @examples
#' \dontrun{
#' return_num_moves(moves_string = df$Moves)
#' }
#'
#' @return A numeric vector of the number of moves in each game
#'
#' @export
return_num_moves <- function(moves_string) {

  if (!is.character(moves_string)) stop("Input must be a character vector")
  ## First of all, making sure the strings are character.

  is_valid <- sapply(moves_string, is.pgn)
  ## Making sure that all elements provided are pgn

  if(any(!is_valid)) stop("text is not a PGN")

  n_moves <- sapply(moves_string, count_moves)
  return(n_moves)
}



#' Return the game ending
#'
#' This function returns a character vector of how the game ended from chess.dom.
#'
#' @param termination_string A character vector in the chess.com extracted data frame called 'Termination'
#'
#' @examples
#' \dontrun{
#' get_game_ending(termination_string = df$Termination, df$White, df$Black)
#' }
#'
#' @return A character vector of the game ending for each game
#'
#' @export
get_game_ending <- function(raw_data = NULL) {

  termination_string <- raw_data$Termination

  if (!is.character(termination_string)) stop("The termination string must be a character vector")
  ## First of all, making sure all the arguments are character.

  if (!any(str_detect(termination_string, "Normal|Time forfeit|resignation|checkmate|time|agreement"))) stop("Termination string does not have any official game endings")
  ## Give error if there are no terminations

  y <- str_extract_all(termination_string, "Normal|Time forfeit|resignation|checkmate|time|agreement") |> unlist()
  ## I'm not entirely sure I want to unlist, on one hand I would like every
  ## termination to be univocal, but on the other hand I risk lengthening the
  ## vector in strings like "x and y draw by insufficient material vs time".

  return(y)
}




#' Return the game winner
#'
#' This function returns a character vector of the usernames of the game winners
#'
#' @param result_column A character vector in the extracted data frame called 'Result'
#' @param white A character vector in the extracted data frame called 'White' for the player on white
#' @param black A character vector in the extracted data frame called 'Black' for the player on black
#'
#' @examples
#' \dontrun{
#' get_winner(df$Result, df$White, df$Black)
#' }
#'
#' @return A character vector of the game ending for each game
#'
#' @export
get_winner <- function(result_column, white, black){

  if (!is.character(result_column)) stop("The result column must be a character vector")
  ## It's fine if White and Black attributes are whatever

  if (!all(result_column %in% c("0-1", "1-0", "1/2-1/2"))) warning("Some results are invalid. NAs added by coercion")

  return(case_when(
    result_column == "0-1"     ~ black,
    result_column == "1-0"     ~ white,
    result_column == "1/2-1/2" ~ "Draw"
  ))
}
