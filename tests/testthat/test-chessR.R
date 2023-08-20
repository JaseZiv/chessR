context("Testing chessR functions")


test_that("get_game_data() works", {
  testthat::skip_on_cran()
  game_data <- get_game_data(usernames = "JaseZiv")
  expect_type(game_data, "list")
  expect_true(nrow(game_data) != 0)
})


test_that("get_raw_chessdotcom() works", {
  testthat::skip_on_cran()
  chessdotcom_hikaru_recent <- get_raw_chessdotcom(usernames = "Hikaru", year_month = c(202104:202105))
  expect_type(chessdotcom_hikaru_recent, "list")
  expect_true(nrow(chessdotcom_hikaru_recent) != 0)
})


test_that("get_raw_lichess() works", {
  testthat::skip_on_cran()
  lichess_game_data <- get_raw_lichess("JaseZiv")
  expect_type(lichess_game_data, "list")
  expect_true(nrow(lichess_game_data) != 0)
  # tests for date parameters
  lichess_game_data <- get_raw_lichess("JaseZiv", since = "2020-11-01", until = "2020-11-03")
  expect_type(lichess_game_data, "list")
  expect_true(nrow(lichess_game_data) == 13)
  expect_true(all(lichess_game_data$Date %in% paste0("2020.11.0", 1:3)))
  # no games for the chosen dates
  lichess_game_data <- get_raw_lichess("JaseZiv", until = "2019-01-01")
  expect_type(lichess_game_data, "list")
  expect_true(nrow(lichess_game_data) == 0)
})


test_that("get_top50_leaderboard() works", {
  testthat::skip_on_cran()
  chessdotcom_leaders <- chessdotcom_leaderboard(game_type = "daily")
  expect_type(chessdotcom_leaders, "list")
  expect_true(nrow(chessdotcom_leaders) != 0)
})


# test_that("lichess_leaderboard() works", {
#   testthat::skip_on_cran()
#   lichess_leaders <- lichess_leaderboard(top_n_players = 10, speed_variant = "blitz")
#   expect_type(chessdotcom_leaders, "list")
#   expect_true(nrow(chessdotcom_leaders) != 0)
# })


test_that("return_num_moves() works", {
  testthat::skip_on_cran()
  chessdotcom_hikaru_recent <- get_raw_chessdotcom(usernames = "Hikaru", year_month = c(202104:202105))
  chessdotcom_hikaru_recent$nMoves <- return_num_moves(moves_string = chessdotcom_hikaru_recent$Moves)

  expect_type(chessdotcom_hikaru_recent$nMoves, "double")
})


test_that("get_game_ending() works", {
  testthat::skip_on_cran()
  chessdotcom_hikaru_recent <- get_raw_chessdotcom(usernames = "Hikaru", year_month = c(202104:202105))
  chessdotcom_hikaru_recent$Ending <- mapply(get_game_ending,
                                             termination_string = chessdotcom_hikaru_recent$Termination,
                                             white = chessdotcom_hikaru_recent$White,
                                             black = chessdotcom_hikaru_recent$Black)

  expect_type(chessdotcom_hikaru_recent$Ending, "character")
})


test_that("get_game_ending() works", {
  testthat::skip_on_cran()
  chessdotcom_hikaru_recent <- get_raw_chessdotcom(usernames = "Hikaru", year_month = c(202104:202105))
  chessdotcom_hikaru_recent$Winner <- mapply(get_winner,
                                              result_column = chessdotcom_hikaru_recent$Result,
                                             white = chessdotcom_hikaru_recent$White,
                                             black = chessdotcom_hikaru_recent$Black)

  expect_type(chessdotcom_hikaru_recent$Winner, "character")
})


test_that("get_lichess_clock_move_time() works", {
  testthat::skip_on_cran()
  lichess_game_data <- get_raw_lichess("JaseZiv")

  expect_type(lichess_game_data$Moves, "character")

  lichess_game_data_with_times <- lichess_clock_move_time(games_list = lichess_game_data)

  expect_type(lichess_game_data_with_times$colour, "character")
})
