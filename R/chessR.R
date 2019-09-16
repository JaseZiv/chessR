#' chessR: A package for extracting and analysing chess game data
#'
#' This package is designed to aid in the extraction and analysis of game data from the popular chess.com
#'
#' @docType package
#' @name chessR
"_PACKAGE"

utils::globalVariables("clean_pgn")
utils::globalVariables(c("each_game", "Moves", "Event", "Date", "EndDate", "White", "Username", "Black", "WhiteElo", "BlackElo", "Result",
                         "UserColour", "UserResult", "Termination", "UserOpponent", "ECOUrl", "n_Moves", "GameEnding", "Opening"))
