#' Read a PGN file
#'
#' @keywords internal
read_pgn <- function(filename) {
  stopifnot(length(filename) == 1 && is.character(filename))
  d <- readLines(filename, encoding = "UTF-8")
  d[grep("^1\\.", d)]
}

#' Create a single list to prepare for converting to a data frame
#'
#'  @keywords internal
create_pgn_list <- function(x) {
  x <- unlist(x) |> as.list()
}

#' Extract the rules of each game
#'
#'  @keywords internal
extract_rules <- function(x){
  tryCatch( {x <- x$games$rules}, error = function(x) {x <- NA}) |> as.character() |> data.frame() |> mutate_if(is.factor, as.character)
}

#' Extract all elements as columns, and all games as row in a data frame
#'
#'  @keywords internal
convert_to_df <- function(exp_list) {
    pgn_list <- strsplit(exp_list, "\n") |> unlist()
    tab_names <- c(str_remove(pgn_list[grep("\\[", pgn_list)][-c(length(pgn_list), (length(pgn_list)-1))], "\\s.*") |> str_remove("\\["),
                   "Moves")
    tab_values <- str_extract(pgn_list[grep("\\[", pgn_list)], '(?<=\\").*?(?=\\")')
    if(length(tab_names) != length(tab_values)) {
      tab_values <- c(tab_values, NA)
    }
    #create the df of values
    df <- rbind(tab_values) |> data.frame(stringsAsFactors = F)
    colnames(df) <- tab_names
    # remove the row names
    rownames(df) <- c()
    # need to clean up date variables
    df$Date <- df$Date |> str_replace_all("\\.", "-")
    df$EndDate <- df$EndDate |> str_replace_all("\\.", "-")

  return(df)
}

#' Parse and extract game metadata
#'
#'  @keywords internal
extract_pgn <- function(x){
  tryCatch( {x <- x$games$pgn}, error = function(x) {x <- NA})
}

#' Parse the list of game urls and extract a json blob
#'
#'  @keywords internal
get_games <- function(y) {
  y <- jsonlite::fromJSON(y)
}

#' Extract the time class of each game (ie blitz, bullet, daily, etc)
#'
#'  @keywords internal
extract_time_class <- function(x){
  tryCatch( {x <- x$games$time_class}, error = function(x) {x <- NA}) |> as.character() |> data.frame() |> dplyr::mutate_if(is.factor, as.character)
}

#' Get the raw json data for a player's chess.com data as a tibble
#'
#' @keywords internal
get_each_player_chessdotcom <- function(username, year_month) {

  # this function gets a list of all year/months the player(s) has played on chess.com
  get_month_urls <- function(username){
    # jsonlite::fromJSON(paste0("https://api.chess.com/pub/player/", username, "/games/archives"))$archives
    resp <- httr::GET(url = paste0("https://api.chess.com/pub/player/", username, "/games/archives"))
    check_status(resp)
    resp <- resp |> httr::content()
    resp <- resp$archives
    return(resp)
  }
  # apply function to get a character vector of game urls
  if(is.na(year_month)) {
    month_urls <- get_month_urls(username)
  } else {
    month_urls <- get_month_urls(username) |> unlist()
    year_mon <- str_sub(month_urls, start=-7) |> str_remove("/") |> as.numeric()
    month_urls <- data.frame(year_mon, month_urls)
    month_urls <- month_urls |> dplyr::filter(year_mon %in% year_month) |> dplyr::pull(month_urls)
  }

  if(length(month_urls) > 0) {

    # apply function to get a list of all the games and game data
    games <- month_urls |> map(get_games)

    # apply to get a list of all games' metadata
    extracted_pgns <- games |> map(extract_pgn)

    # apply the function to result in a list of each individual game
    pgn_list <- create_pgn_list(extracted_pgns)

    # Additional metadata:
    GameRules <- games |> map_df(extract_rules)

    TimeClass <- games |> purrr::map_df(extract_time_class)

    extra_df <- cbind(GameRules, TimeClass) |> data.frame()

    colnames(extra_df) <- c("GameRules", "TimeClass")

    # convert the lists to data frames
    df <- pgn_list |> purrr::map_df(convert_to_df)
    df <- cbind(extra_df, df)
    df$Username <- username

  } else {
    df <- data.frame()
  }

  # output the final data frame for each player
  return(df)
}

#' Check if a string is valid PGN
#'
#' @keywords internal
is.pgn <- function(x) {
  has_numbers <- str_detect(x, "\\b\\d+\\.")
  has_legal_moves <- str_detect(x, "\\b(?:[KQRNB])?(?:[a-h1-8])?(?:x)?[a-h][1-8](?:=[QRNB])?[+#]?\\b")

  return(has_numbers & has_legal_moves)
}

#' Extract the number of moves from a PGN
#'
#' @keywords internal
count_moves <- function(x) {
  y <- x |>
    str_replace_all("\\{.*?\\}", "") |>
    str_split("\\s+") |>
    unlist() |>
    as.numeric() |>
    suppressWarnings() |>
    max(na.rm = TRUE)

  return(y)
}

#' Check status function
#'
#' @param res Response from API
#' @keywords internal
check_status <- function(res) {
  x = httr::status_code(res)

  if(x != 200) stop("The API returned an error", call. = FALSE)
}

