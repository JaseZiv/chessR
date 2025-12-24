## pgn_to_dataframe tests

test_that("pgn_to_dataframe() errors with non pgn string", {
  testthat::skip_on_cran()

  expect_error(pgn_to_dataframe(123), "only a character string of length 1 can be provided")
  # "", NULL, NA, etc fail the pgn test as well
})

test_that("pgn_to_dataframe() errors with multiple moves strings", {
  testthat::skip_on_cran()

  expect_error(pgn_to_dataframe(chessdotcom_raw_hikaru[, "Moves"]), "only a character string of length 1 can be provided")
})

test_that("pgn_to_dataframe() errors with nonexistent file", {
  testthat::skip_on_cran()

  expect_error(pgn_to_dataframe("nonexistent_file.pgn"), "the provided string must be a pgn OR a pgn file name")
})

test_that("pgn_to_dataframe() errors with nonpgn character string", {
  testthat::skip_on_cran()

  expect_error(pgn_to_dataframe("e4 d5 c6"), "the provided string must be a pgn OR a pgn file name")
})

test_that("pgn_to_dataframe() produces a data.frame from a pgn object", {
  move_df <- pgn_to_dataframe(chessdotcom_raw_hikaru[1, "Moves"])

  testthat::skip_on_cran()
  expect_s3_class(move_df, "data.frame")
  expect_identical(colnames(move_df), c("white", "black"))
  expect_equal(nrow(move_df), 19)
  expect_true(grepl("e4", move_df[1, 1]))
  expect_true(grepl("g6", move_df[1, 2]))
})

test_that("pgn_to_dataframe() produces a data.frame from a pgn file", {
  temp_file <- withr::local_tempfile(fileext = ".pgn")
  writeLines("1. e4 { [%eval 0.00] [%clk 0:10:00] } 1... e5 { [%eval 0.00] [%clk 0:10:00] } 2. Qh5 { [%eval 0.25] [%clk 0:09:58] } 2... Nc6 { [%eval 0.20] [%clk 0:09:59] } 3. Bc4 { [%eval 0.45] [%clk 0:09:55] } 3... Nf6?? { [%eval -3.00] [%clk 0:09:57] } 4. Qxf7# { [%eval 999.00] [%clk 0:09:53] } 1-0
",
             temp_file)
  move_df <- pgn_to_dataframe(temp_file)

  testthat::skip_on_cran()
  expect_s3_class(move_df, "data.frame")
  expect_identical(colnames(move_df), c("white", "black"))
  expect_equal(nrow(move_df), 4)
  expect_true(grepl("e4", move_df[1, 1]))
  expect_true(grepl("Qxf7#", move_df[4, 1]))
})


## pgn_to_game tests (tbd)


## plot_moves tests (tbd)


## extract_moves_from_pgn tests (tbd)
