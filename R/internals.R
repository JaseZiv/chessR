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
#' @param y list of game urls
#' @keywords internal
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
#' @param user the desired username,
#' @param year_month the desired year and month in format yyyymm,
#' @keywords internal
get_each_player_chessdotcom <- function(username, year_month) {

  # apply function to get a character vector of game urls
  if(is.na(year_month)) {
    month_urls <- get_game_urls(username)
  } else {
    month_urls <- get_game_urls(username) |> unlist()
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
#' @param url link to be checked
#' @keywords internal
check_status <- function(url) {
  res <- httr::status_code(url)

  if(res != 200) stop("The API returned an error", call. = FALSE)
}

#' Get a list of all year/months the player(s) has played on chess.com
#'
#' @param username the desired player's nickname
#' @keywords internal
get_game_urls <- function(username){

player_api_url <- httr::GET(url = paste0("https://api.chess.com/pub/player/", username, "/games/archives"))

## Stop if the url is not responsive
check_status(player_api_url)

player_api_content <- player_api_url |> httr::content()
dates_list <- player_api_content$archives
return(dates_list)
}

#' Parse the list of game urls and extract a json blob (for get_game_data)
#'
#' @param dates_list a list of game urls by date
#' @keywords internal
get_games <- function(y) {
  y <- jsonlite::fromJSON(y)
}

#' Extract the game and moves data required for analysis (for get_game_data)
#'
#' @param x list of pgn games
#' @keywords internal
extract_pgn <- function(x){
  tryCatch( {x <- x$games$pgn}, error = function(x) {x <- NA}) |> as.character() |> data.frame() |> mutate(across(where(is.factor), as.character))
}

#' Extract the rules of each game
#'
#' @keywords internal
extract_rules <- function(x){
  tryCatch( {x <- x$games$rules}, error = function(x) {x <- NA}) |> as.character() |> data.frame() |> mutate(across(where(is.factor), as.character))
}

#' Extract the time class of each game (ie blitz, bullet, daily, etc)
#'
#' @keywords internal
extract_time_class <- function(x){
  tryCatch( {x <- x$games$time_class}, error = function(x) {x <- NA}) |> as.character() |> data.frame() |> mutate(across(where(is.factor), as.character))
}

#' Extract the ending in the ending url
#'
#' @keywords internal
ending <- function(user, string, opponent) {

  return(string |>
           str_remove_all(paste0(user, "|", opponent, "|won |\\-")) |>
           str_squish())
}

#' Extract rules, time class and pgn of each of the games in a list and concentrate them in a tibble
#'
#' @param games_list the list of games to extract information from
#' @keywords internal
convert_to_tibble <- function(games_list) {

  rules <- games_list |>
    map_df(extract_rules)

  time_class <- games_list |>
    map_df(extract_time_class)

  pgn <- games_list |>
    map_df(extract_pgn)

  t <- tibble(
    rules = rules,
    time_class = time_class,
    pgn = pgn
  )

  return(t)
}

#' clean each pgn string and separate its columns
#'
#' @param t tibble coming from convert_to_tibble
#' @keywords internal
clean_pgn <- function(t) {
  # notes:
  # this function will exclude "abandoned" games that didn't have a move recorded.
  # if it was abandoned and an opening was created, then it will be included in the results

  cleaned_df <- df[grep("\\{", df$pgn),]

  cleaned_df <- cleaned_df |> filter(rules == "chess",
                                     time_class %in% c("blitz", "bullet",  "daily",  "rapid"),
                                     str_detect(pgn, "Tournament", negate = TRUE),
                                     str_detect(pgn, "club/matches", negate = TRUE)) |>
    separate(pgn, into = c("Event", "Site", "Date", "Round", "White", "Black", "Result", "CurrentPosition", "Timezone", "ECO", "ECOUrl",
                           "UTCDate", "UTCTime", "WhiteElo", "BlackElo", "TimeControl", "Termination", "StartTime", "EndDate", "EndTime",
                           "Link", "Moves"), sep = "]\n")


  # create a vector of the variables that contains the data we need withing double quotes
  vars_to_extract <- c("Event", "Site", "Date", "Round", "White", "Black", "Result", "ECO", "ECOUrl", "CurrentPosition", "Timezone",
                       "UTCDate", "UTCTime", "WhiteElo", "BlackElo", "TimeControl", "Termination", "StartTime", "EndDate", "EndTime",
                       "Link")
  # function to extract the data contained within the double quotes
  extract_data <- function(x) {str_replace('[^\"]+\"([^\"]+).*', '\\1', x)}

  # extract the data
  cleaned_df <- cleaned_df |>
    mutate(across(vars_to_extract), extract_data) |> mutate(across(where(is.factor)), as.character)

  # create a variable to indicate which colour won the game
  cleaned_df <- cleaned_df |>
    mutate(winner = ifelse(Result == "0-1", "Black", ifelse(Result == "1-0", "White", "Draw")))

  # create a username variable for analysis purposes
  cleaned_df$Username <- username

  # data cleaning and preprocessing
  cleaned_df <- cleaned_df |>
    # convert date variables to ymd using lubridate::ymd()
    mutate(Date = ymd(Date),
           EndDate = ymd(EndDate)) |>
    # feature engineering of some new features for analysis
    mutate(n_Moves = return_num_moves(Moves),
           UserOpponent = ifelse(White == Username, Black, White),
           UserColour = ifelse(Username == White, "White", "Black"),
           OpponentColour = ifelse(Username == White, "Black", "White"),
           UserELO = as.numeric(ifelse(Username == White, WhiteElo, BlackElo)),
           OpponentELO = as.numeric(ifelse(Username != White, WhiteElo, BlackElo))) |>
    mutate(UserResult = ifelse(Result == "0-1", "Black", ifelse(Result == "1-0", "White", "Draw")),
           UserResult = ifelse(UserColour == UserResult, "Win", ifelse(UserResult == "Draw", "Draw", "Loss"))) |>
    mutate(DaysTaken = EndDate - Date) |>
    mutate(GameEnding = mapply(ending, Username, Termination, UserOpponent)) |>
    mutate(Opening = str_remove_all(ECOUrl, ".*?/"),
           Opening = str_remove(Opening, "^.*?-"))

  return(cleaned_df)
}

