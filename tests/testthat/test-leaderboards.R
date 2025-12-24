## chessdotcom_leaderboard tests

test_that("chessdotcomleaderboard() works", {
  testthat::skip_on_cran()
  expect_s3_class(chessdotcom_leaderboard_blitz, "tbl_df")
  expect_false(any(is.na(chessdotcom_leaderboard_blitz)))
  expect_true(mean(chessdotcom_leaderboard_blitz |> pull(score)) > 2000)
})

test_that("chessdotcomleaderboard() fails with non variant", {
  testthat::skip_on_cran()
  expect_error(chessdotcom_leaderboard("test"), "incorrect `game_type`")})


## lichess_leaderboard tests

test_that("lichess_leaderboard() works", {
  testthat::skip_on_cran()
  expect_s3_class(lichess_leaderboard_blitz, "tbl_df")
  expect_false(any(is.na(lichess_leaderboard_blitz$Rank)))
  expect_true(mean(lichess_leaderboard_blitz |> pull(Rating)) > 2000)
})

test_that("lichess_leaderboard() fails with non variant", {
  testthat::skip_on_cran()
  expect_error(lichess_leaderboard("test"))
})

