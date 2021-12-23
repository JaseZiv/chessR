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


