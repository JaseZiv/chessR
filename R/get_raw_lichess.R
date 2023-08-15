#' Get Raw Lichess Game Data
#'
#' This function returns the raw json data for a player's or list of players'
#' Lichess data as a data frame
#'
#' @param player_names A vector of a valid username or usernames from Lichess
#' @param since Games played since this date. An object of class `Date` or a
#'   string of the form `"yyyy-mm-dd"`. If `NULL`, defaults to account creation
#'   date.
#' @param until Games played until this date. An object of class `Date` or a
#'   string of the form `"yyyy-mm-dd"`. If `NULL`, defaults to now.
#'
#' @return a dataframe of lichess data
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' \dontrun{
#' georges_data <- get_raw_lichess(player_names = "Georges")
#' the_knife_data <- get_raw_lichess("the_knife", since = "2023-08-13", until = "2023-08-14")
#' }
get_raw_lichess <- function(player_names, since = NULL, until = NULL) {

  get_file <- function(player_name) {

    # cat("Extracting ", player_name, " games. Please wait\n")

    since_query <- ""
    if (!is.null(since)) {
      # API parameters since awaits number of milliseconds since 1970-01-01
      since_integer <- as.integer(as.Date(since)) * 86400 * 1000
      since_query <- paste0("&since=", since_integer)
    }

    until_query <- ""
    if (!is.null(until)) {
      # API parameters until awaits number of milliseconds since 1970-01-01
      until_integer <- ((as.integer(as.Date(until)) + 1) * 86400 * 1000) - 1
      until_query <- paste0("&until=", until_integer)
    }

    # download the tmp file
    tmp <- tempfile()
    curl::curl_download(
      paste0("https://lichess.org/api/games/user/", player_name, "?evals=true&clocks=true&opening=true", since_query, until_query),
      tmp
    )
    # read in the file
    read_in_file <- readLines(tmp)
    # cleaning steps of the file
    collapsed_strings <- paste(read_in_file, collapse = "\n") %>% strsplit(., "\n\n\n") %>% unlist()
    games_list <- strsplit(collapsed_strings, "\n")

    return(games_list)
  }


  # Create DF of games lists ------------------------------------------------
  create_games_df <- function(games_list) {

    first_test <- games_list
    # there are some elements of the list that are blank (""), want to remove these first
    first_test <- first_test[-which(first_test == "")]
    # create a vector with the column names. The moves column doesn't have a title in the code so create one called "Moves"
    # the moves are always the last element so need to pull that out manually
    tab_names <- c(gsub( "\\s.*", "", first_test[grep("\\[", first_test)]) %>% gsub("\\[", "", .), "Moves")
    # then extract the values for each key above. Manually grab the moves value also and append to vector
    tab_values <- c(gsub(".*[\"]([^\"]+)[\"].*", "\\1", first_test[grep("\\[", first_test)]), first_test[length(first_test)])
    #create the df of values
    df <- rbind(tab_values) %>% data.frame(stringsAsFactors = F)
    # then the header for table
    colnames(df) <- tab_names
    # remove the row names
    rownames(df) <- c()
    # because of a wierd handling of moves with the Evals, need to drop these suprious columns, that are indicated by a number
    # in the column name
    errorneous_columns <- which(grepl("[0-9]", colnames(df)))
    df[, errorneous_columns] <- NULL
    # remove the "+" sign and convert RatingDiff columns to numeric
    column_names <- colnames(df) %>% paste0(collapse = ",")
    if(grepl("WhiteRatingDiff", column_names)) {
      df$WhiteRatingDiff <- gsub("\\+", "", df$WhiteRatingDiff)
    }

    if(grepl("WhiteRatingDiff", column_names)) {
      df$BlackRatingDiff <- gsub("\\+", "", df$BlackRatingDiff)
    }
    return(df)
  }
  # Now create the data frame for each player
  final_output <- data.frame()
  # loop through each player - have used a loop because it was the only way I could
  #get the username of the player of interest to be correct in a column
  #when more than one player's data is to be extracted
  for(each_player in player_names) {

    output <- get_file(each_player) %>%
      purrr::map_df(create_games_df)
    # apply the user's names
    output$Username <- each_player
    # filter out games where the variant is 'From Position'
    output <- output %>% dplyr::filter(.data$Variant != "From Position")

    final_output <- dplyr::bind_rows(final_output, output)
  }
  return(final_output)
}
