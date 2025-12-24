## get_each_player_chessdotcom tests (tbd)


## get_raw_chessdotcom tests

test_that("get_raw_chessdotcom() works", {
  testthat::skip_on_cran()

  expect_type(chessdotcom_raw_hikaru, "list")
  expect_true(nrow(chessdotcom_raw_hikaru) != 0)
})
