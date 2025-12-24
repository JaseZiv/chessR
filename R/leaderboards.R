#' Get Top 50 on chess.com Leaderboards
#'
#' This function takes in one parameter, the game_type, and returns a
#' data frame of the top 50 players on chess.com.
#'
#' The leaderboard options (games) include:
#'
#' \emph{"daily"}, \emph{"daily960}", \emph{"live_rapid"},
#' \emph{"live_blitz"}, \emph{"live_bullet"}, \emph{"live_bughouse"},
#' \emph{"live_blitz960"}, \emph{"live_threecheck" }, \emph{"live_crazyhouse"},
#' \emph{"live_kingofthehill"}, \emph{"lessons"}, \emph{"tactics"}
#'
#' @param game_type A valid chess.com game type to return the leaderboard for
#'
#' @return a dataframe of the chess.com top 50 players based on game_type selected
#'
#' @export
#'
#' @examples
#' \dontrun{
#' chessdotcom_leaderboard(game_type = "daily")
#' }
chessdotcom_leaderboard <- function(game_type) {
  valid_game_types <- c(
    "daily", "daily960", "live_rapid", "live_blitz", "live_bullet",
    "live_bughouse", "live_blitz960", "live_threecheck", "live_crazyhouse",
    "live_kingofthehill", "lessons", "tactics"
  )

  if (!game_type %in% valid_game_types) stop("incorrect `game_type`")

  df <- jsonlite::fromJSON("https://api.chess.com/pub/leaderboards")[game_type] |> unname() |> data.frame() |> as_tibble()

  return(df)
}



#' Get top players on Lichess leaderboards
#'
#' This function takes in two parameters; how many players you want
#' returned (max 200) and the speed variant. The result is a data
#' frame for each game type
#'
#' The leaderboard speed variant options include:
#'
#' \emph{"ultraBullet"}, \emph{"bullet}", \emph{"blitz"},
#' \emph{"rapid"}, \emph{"classical"}, \emph{"chess960"},
#' \emph{"crazyhouse"}, \emph{"antichess" }, \emph{"atomic"},
#' \emph{"horde"}, \emph{"kingOfTheHill"}, \emph{"racingKings"},
#' \emph{"threeCheck"}
#'
#' @param top_n_players The number of players (up to 200) you want returned
#' @param speed_variant A valid lichess speed variant to return the leaderboard for
#'
#' @return a dataframe of the lichess top players based on speed_variant and top_n_players selected
#'
#' @examples
#' \dontrun{
#' top10_blitz <- lichess_leaderboard(top_n_players = 10, speed_variant = "blitz")
#' leaderboards <- purrr::map2_df(top_n_players = 10, c("ultraBullet", "bullet"), lichess_leaderboard)
#' }
#'
#' @export
lichess_leaderboard <- function(speed_variant) {
  # extract and convert to DF
  top_leaders <- xml2::read_html(paste0("https://lichess.org/player/top/", speed_variant)) |>
    (\(x) rvest::html_table(x)[[1]])() |>
    drop_na() |>
    rename(Rank = X1,
           Name = X2,
           Rating = X3,
           Delta = X4) |>
    mutate(speed_variant = speed_variant)
  # player names come with the players title at the beginning of the string.
  # knowing that spaces aren't allowed in usernames, we can separate
  tidy_tl <- top_leaders |> separate(Name, into = c("Title", "Name"), sep = "\u00A0", fill = "left", extra = "merge")

  return(tidy_tl)
}
