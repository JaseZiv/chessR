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
#' \donttest{
#' hikaru <- get_each_player("hikaru")
#' if (nrow(hikaru) > 0) {
#'   m <- extract_moves_as_game(hikaru[1, ])
#'   plot_moves(m)
#' }
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

#' Analyze a chess game using Stockfish engine
#'
#' This function integrates with the {chess} and {stockfish} packages to provide
#' analysis of chess games extracted using chessR functions.
#'
#' @param game a single row of a `data.frame` provided by `chessR` containing move information,
#'    a [chess::game()] object, or the filename of a PGN file
#' @param depth the depth of analysis for Stockfish (higher = more accurate but slower)
#' @param time_limit time limit for analysis per position in seconds
#' @param annotate logical, whether to return annotated moves with evaluations
#'
#' @return a list containing the original game, position evaluations, and optionally annotated moves
#' @export
#'
#' @examples
#' \donttest{
#' # Analyze a game from chess.com data
#' hikaru <- get_each_player("hikaru")
#' if (nrow(hikaru) > 0) {
#'   analysis <- analyse_game(hikaru[1, ], depth = 15)
#'   print(analysis$evaluations)
#' }
#' }
analyse_game <- function(game, depth = 10, time_limit = 1.0, annotate = FALSE) {
  if (!requireNamespace("chess", quietly = TRUE)) {
    stop("This function requires the {chess} package to be installed.")
  }
  if (!requireNamespace("stockfish", quietly = TRUE)) {
    stop("This function requires the {stockfish} package to be installed.")
  }
  
  # Convert game to chess object if needed
  if (is.data.frame(game)) {
    chess_game <- extract_moves_as_game(game)
  } else if (inherits(game, "character") && length(game) == 1) {
    chess_game <- extract_moves_as_game(game)
  } else if (inherits(game, "chess.game")) {
    chess_game <- game
  } else {
    stop("game must be a data.frame from chessR, a chess::game object, or a PGN filename")
  }
  
  # Initialize Stockfish engine
  engine <- stockfish::fish$new()
  
  # Set engine parameters
  engine$set_depth(depth)
  engine$set_time_limit(time_limit)
  
  # Analyze each position
  evaluations <- list()
  step <- chess::root(chess_game)
  move_count <- 0
  
  # Analyze starting position
  engine$set_fen(chess::fen(step))
  eval_result <- engine$get_evaluation()
  evaluations[[1]] <- list(
    move_number = 0,
    fen = chess::fen(step),
    evaluation = eval_result$value,
    evaluation_type = eval_result$type,
    best_move = engine$get_best_move()
  )
  
  # Analyze each position after moves
  while (move_count < chess::move_number(chess_game) * 2) {
    if (chess::is_checkmate(step) || chess::is_stalemate(step)) break
    
    step <- chess::forward(step)
    move_count <- move_count + 1
    
    engine$set_fen(chess::fen(step))
    eval_result <- engine$get_evaluation()
    
    evaluations[[move_count + 1]] <- list(
      move_number = ceiling(move_count / 2),
      color = if (move_count %% 2 == 1) "White" else "Black",
      fen = chess::fen(step),
      evaluation = eval_result$value,
      evaluation_type = eval_result$type,
      best_move = if (!chess::is_checkmate(step)) engine$get_best_move() else NA
    )
  }
  
  # Close engine
  engine$quit()
  
  result <- list(
    game = chess_game,
    evaluations = evaluations
  )
  
  if (annotate) {
    # Add move annotations based on evaluation changes
    result$annotations <- annotate_moves(evaluations)
  }
  
  return(result)
}

# Helper function to annotate moves based on evaluation changes
annotate_moves <- function(evaluations) {
  if (length(evaluations) < 2) return(character(0))
  
  annotations <- character(length(evaluations) - 1)
  
  for (i in 2:length(evaluations)) {
    prev_eval <- evaluations[[i-1]]$evaluation
    curr_eval <- evaluations[[i]]$evaluation
    
    # Handle evaluation from perspective of the player who just moved
    if (evaluations[[i]]$color == "Black") {
      # Flip evaluation for Black's perspective
      prev_eval <- -prev_eval
      curr_eval <- -curr_eval
    }
    
    eval_change <- curr_eval - prev_eval
    
    annotation <- if (eval_change >= 200) {
      "!! (Brilliant)"
    } else if (eval_change >= 50) {
      "! (Good)"
    } else if (eval_change >= -50) {
      "(Book)"
    } else if (eval_change >= -100) {
      "?! (Inaccuracy)"
    } else if (eval_change >= -200) {
      "? (Mistake)"
    } else {
      "?? (Blunder)"
    }
    
    annotations[i-1] <- annotation
  }
  
  return(annotations)
}

extract_moves_from_pgn <- function(filename) {
  stopifnot(length(filename) == 1 && is.character(filename))
  d <- readLines(filename, encoding = "UTF-8")
  d[grep("^1\\.", d)]
}