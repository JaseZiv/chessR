## Environment for avoiding calling each variable 20+ times

library(tidyverse)

chessdotcom_raw_hikaru <- get_raw_chessdotcom(
  usernames = "Hikaru",
  year_month = c(202104:202105)
)

lichess_raw_jaseziv <- get_raw_lichess("JaseZiv")
lichess_raw_checkmate <- get_raw_lichess("checkmate")

chessdotcom_gamedata_jaseziv <- get_game_data(usernames = "JaseZiv")

chessdotcom_leaderboard_blitz <- chessdotcom_leaderboard("live_blitz")
lichess_leaderboard_blitz <- lichess_leaderboard("blitz")
