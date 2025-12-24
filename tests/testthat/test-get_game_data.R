## get_each_player tests

test_that("get_each_player() works", {

  expect_silent(get_each_player("hikaru"))
})


## get_game_data tests

test_that("get_game_data() works", {
  testthat::skip_on_cran()

  expect_type(chessdotcom_gamedata_jaseziv, "list")
  expect_true(nrow(chessdotcom_gamedata_jaseziv) != 0)
})
