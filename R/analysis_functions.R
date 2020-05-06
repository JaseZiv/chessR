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
#' @return
#' A numeric vector of the number of moves in each game
#'
#' @import magrittr
#'
#' @export
return_num_moves <- function(moves_string) {
  moves_string <- moves_string
  moves_fun <- function(x) {
    if(is.na(x)) {
      n_moves <- NA
    } else {
      n_moves <- suppressWarnings(gsub("\\{.*?\\}", "", x, perl=TRUE) %>% strsplit(., "\\s+") %>% unlist() %>%  as.numeric() %>% max(na.rm = T))
    }
  }
  n_moves <- mapply(moves_fun, moves_string)
  return(n_moves)
}



#' Return the game ending
#'
#' This function returns a character vector of how the game ended from chess.dom.
#'
#' @param termination_string A character vector in the chess.com extracted data frame called 'Termination'
#' @param white A character vector in the chess.com extracted data frame called 'White' for the player on white
#' @param black A character vector in the chess.com extracted data frame called 'Black' for the player on black
#'
#' @examples
#' \dontrun{
#' get_game_ending(termination_string = df$Termination, df$White, df$Black)
#' }
#'
#' @return
#' A character vector of the game ending for each game
#'
#' @import magrittr
#' @import stringr
#'
#' @export
get_game_ending <- function(termination_string, white, black) {
  string <- termination_string
  usernames <- c(white, black)
  usernames <- paste0("\\b(", paste(usernames, collapse="|"), ")\\b")

  x <- gsub(usernames, "", string)
  x <- gsub("won ", "", x)
  x <- gsub(" \\- ", "", x)
  x <- stringr::str_squish(x)

  return(x)
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
#' @return
#' A character vector of the game ending for each game
#'
#' @export
get_winner <- function(result_column, white, black){
  a <- ifelse(result_column == "0-1", black, ifelse(result_column == "1-0", white, "Draw"))
  return(a)
}
