#' Extract moves from a game as a data.frame
#'
#' @param moves_string string containing moves built by `chessR` (e.g. from \url{https://www.chess.com/})
#'     or the filename of a PGN file
#'
#' @return cleaned moves as a data.frame
#' @export
pgn_to_dataframe <- function(moves_string) {

  # check length and type of the input
  if(length(moves_string) != 1L | !is.character(moves_string)) stop("only a character string of length 1 can be provided")

  # read PGN filefile
  if (file.exists(moves_string)) {
    moves_string <- read_pgn(moves_string)
  }

  # check validity of pgn
  if(!is.pgn(moves_string)) stop("the provided string must be a pgn OR a pgn file name")

  # remove alternative lines
  clean <- str_remove_all(moves_string, "\\(.*?\\)")

  # remove annotations and ending
  noclock <- str_remove_all(clean, "\\{.*?\\}")
  remove_ending <- str_remove(noclock, "[0-9]-[0-9]")

  # construct the dataframe
  parsed <- separate_rows(data.frame(move = remove_ending), move, sep = "[0-9]+\\.")
  parsed <- parsed[-1, ]
  if (nrow(parsed) %% 2 == 1) {
    # end game early or white wins
    parsed <- rbind(parsed, data.frame(move = ""))
  }
  moves <- data.frame(white = parsed$move[c(TRUE, FALSE)],
                      black = parsed$move[c(FALSE, TRUE)])
  moves$white <- trimws(moves$white)
  moves$black <- trimws(stringr::str_remove(moves$black, stringr::fixed(".. ")))

  return(moves)
}


#' Extract moves and create `chess` game
#'
#' @param game a single row of a `data.frame` provided by `chessR` containing move information
#'    or the filename of a PGN file
#'
#' @return a [chess::game()] game object
#' @export
pgn_to_game <- function(game) {
  if (!requireNamespace("chess", quietly = TRUE)) {
    stop("This function requires the {chess} package to be installed.")
  }
  moves <- if (length(game) == 1 && file.exists(game)) {
    gamedata <- read_pgn(game)
    pgn_to_dataframe(gamedata)
  } else {
    stopifnot("only a single game can be converted" = nrow(game) == 1L)
    pgn_to_dataframe(game$Moves)
  }
  c_moves <- c(as.matrix(t(moves)))
  c_moves <- c_moves[c_moves != ""]
  do.call(chess::move, c(list(chess::game()), as.list(c_moves)))
}


#' Plot a game
#'
#' @param game a [chess::game()] object, likely with moves identified
#' @param interactive wait for 'Enter' after each move? Turn off to use in a gif
#' @param sleep how long to wait between moves
#'
#' @return `NULL`, (invisibly) - called for the side-effect of plotting
#'
#' @examples
#' \dontrun{
#' hikaru <- get_each_player_chessdotcom("hikaru", "202112")
#' m <- pgn_to_game(hikaru[11, ])
#' plot_moves(m)
#' }
plot_moves <- function(game, interactive = TRUE, sleep = 1) {
  if (!requireNamespace("chess", quietly = TRUE)) {
    stop("This function requires the {chess} package to be installed.")
  }
  step <- chess::root(game)
  plot(step)
  for (i in seq_len(2*chess::move_number(game))) {
    if (chess::is_checkmate(step)) break
    step <- chess::forward(step)
    plot(step)
    if (interactive) {
      readline("Press enter to continue...")
    } else {
      Sys.sleep(sleep)
    }
  }
  return(invisible(NULL))
}

## I advise against running plot_moves. I believe rsvg_format is broken for chess
## package or fails nonetheless. The example isn't reproducible and there aren't
## any tests to back it up either.


