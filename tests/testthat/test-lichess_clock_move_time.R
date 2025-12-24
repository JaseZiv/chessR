## lichess_clock_move_time tests

test_that("get_lichess_clock_move_time() works", {
  testthat::skip_on_cran()

  expect_type(lichess_raw_jaseziv$Moves, "character")
  lichess_game_data_with_times <- lichess_clock_move_time(games_list = lichess_raw_jaseziv)
  expect_type(lichess_game_data_with_times$colour, "character")
})
