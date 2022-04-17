# function to extract the time stamps for a game
lichess_game_data <- get_raw_lichess("LordyLeroy")

# function to add increment column
add_increment <- function(games_list){

  suppressWarnings(
  df_with_increment <- games_list %>%
    # add increment
    dplyr::mutate(Increment = as.integer(
      # only include characters after the + symbol of the TimeControl column
      stringr::str_remove(TimeControl, ".*\\+"))
    ))

}

# function to extract time control

test <- lichess_game_data %>%
  # remove correspondence games, as not relevant
  dplyr::filter(!(grepl("Correspondence", Event))) %>%
  add_increment()

x <- test %>%
  dplyr::slice(1) %>%
  dplyr::select(Moves)

v <- stringr::str_split(x, "\\[|\\}")[[1]] %>%
  dplyr::as_tibble() %>%
  dplyr::rename(time = value) %>%
  dplyr::filter(grepl("clk", time)) %>%
  dplyr::mutate(time = stringr::str_remove_all(time, "%clk "),
                time = stringr::str_remove_all(time, "\\] ")) %>%
  dplyr::mutate(colour = ifelse(
    dplyr::row_number() %% 2 == 0,
    "Black",
    "White"),
    move_number = floor((1 + dplyr::row_number()) / 2),
    move_time = )

ggplot2::ggplot(v,
                ggplot2::aes(x = move_number,
                             y = time,
                             colour = colour)) +
  ggplot2::geom_point()


