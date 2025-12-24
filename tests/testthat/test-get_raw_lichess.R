## get_raw_lichess tests

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
