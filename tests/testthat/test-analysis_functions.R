## return_num_moves tests

test_that("return_num_moves() fails with non-character objects", {
  testthat::skip_on_cran()
  negative_string <- 24

  expect_error(return_num_moves(negative_string), "Input must be a character vector")
})

test_that("return_num_moves() fails with non-pgn text", {
  testthat::skip_on_cran()
  negative_string <- "abcdefg 12. 12"

  expect_error(return_num_moves(negative_string))
})

test_that("return_num_moves() works with pgn", {
  testthat::skip_on_cran()
  test_string <- chessdotcom_raw_hikaru |> subset(!is.na(Moves))

  expect_equal(return_num_moves(moves_string = test_string$Moves)[[1]], 19)
})


## get_game_ending tests

test_that("get_game_ending() fails with non-char", {
  testthat::skip_on_cran()
  negative_string <- iris

  expect_error(get_game_ending(negative_string), "The termination string must be a character vector")
})

test_that("get_game_ending() fails with a non-termination string", {
  testthat::skip_on_cran()
  negative_string <- list(Termination = "Hikaru loses by determination",
                          White = "Hikaru",
                          Black = "Falete")

  expect_error(get_game_ending(negative_string), "Termination string does not have any official game endings")
})

test_that("get_game_ending() works with raw data", {
  testthat::skip_on_cran()

  expect_equal(get_game_ending(chessdotcom_raw_hikaru)[1], "resignation")
})


## get_winner tests

test_that("get_winner() fails when a result_column is not character", {
  testthat::skip_on_cran()
  negative_string <- iris

  negative_string$Result <- 1
  negative_string$White <- 1
  negative_string$Black <- 1

  expect_error(get_winner(negative_string$Result,
                          negative_string$White,
                          negative_string$Black),
               "The result column must be a character vector")
})

test_that("get_winner() warns about non-results in the result column", {
  testthat::skip_on_cran()

  chessdotcom_raw_hikaru$Result[222] <- "0-0"

  expect_warning(get_winner(chessdotcom_raw_hikaru$Result,
                            chessdotcom_raw_hikaru$White,
                            chessdotcom_raw_hikaru$Black),
               "Some results are invalid. NAs added by coercion")

  expect_true(get_winner(chessdotcom_raw_hikaru$Result,
                          chessdotcom_raw_hikaru$White,
                          chessdotcom_raw_hikaru$Black)[222] |>
                is.na() |>
                suppressWarnings())
})

test_that("get_winner() works", {
  testthat::skip_on_cran()

  expect_equal(get_winner(chessdotcom_raw_hikaru$Result,
                          chessdotcom_raw_hikaru$White,
                          chessdotcom_raw_hikaru$Black)[436],
               "DanielNaroditsky")
})
