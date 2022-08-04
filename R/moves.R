#' Extract moves from a game as a data.frame
#'
#' @param moves_string string containing moves built by `chessR` (e.g. from \url{https://www.chess.com/})
#'     or the filename of a PGN file
#'
#' @return cleaned moves as a data.frame
#' @export
extract_moves <- function(moves_string) {
  stopifnot("only a single moves_string can be provided" = length(moves_string) == 1L)

  # read PGN filefile
  if (file.exists(moves_string)) {
    moves_string <- extract_moves_from_pgn(moves_string)
  }

  # remove newlines
  clean <- stringr::str_remove_all(moves_string, "\\\n")
  # remove explored lines
  clean <- stringr::str_remove_all(moves_string, "\\(.*?\\)")
  # remove annotations
  noclock <- stringr::str_remove_all(clean, "\\{.*?\\}")
  remove_ending <- stringr::str_remove(noclock, "[0-9]-[0-9]")
  parsed <- tidyr::separate_rows(data.frame(move = remove_ending), .data$move, sep = "[0-9]+\\.")
  parsed <- parsed[-1, ]
  if (nrow(parsed) %% 2 == 1) {
    # end game early or white wins
    parsed <- rbind(parsed, data.frame(move = ""))
  }
  moves <- data.frame(white = parsed$move[c(TRUE, FALSE)],
                      black = parsed$move[c(FALSE, TRUE)])
  moves$white <- trimws(moves$white)
  moves$black <- trimws(stringr::str_remove(moves$black, stringr::fixed(".. ")))
  moves
}


#' Extract moves and create `chess` game
#'
#' @param game a single row of a `data.frame` provided by `chessR` containing move information
#'    or the filename of a PGN file
#'
#' @return a [chess::game()] game object
#' @export
extract_moves_as_game <- function(game) {
  if (!requireNamespace("chess", quietly = TRUE)) {
    stop("This function requires the {chess} package to be installed.")
  }
  moves <- if (length(game) == 1 && file.exists(game)) {
    gamedata <- extract_moves_from_pgn(game)
    extract_moves(gamedata)
  } else {
    stopifnot("only a single game can be converted" = nrow(game) == 1L)
    extract_moves(game$Moves)
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
#' @export
#'
#' @examples
#' \dontrun{
#' hikaru <- get_each_player_chessdotcom("hikaru", "202112")
#' m <- extract_moves_as_game(hikaru[11, ])
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

extract_moves_from_pgn <- function(filename) {
  stopifnot(length(filename) == 1 && is.character(filename))
  d <- readLines(filename, encoding = "UTF-8")
  d[grep("^1\\.", d)]
}
