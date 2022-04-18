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

# function to extract time data
lichess_clock_move_time <- function(games_list){

  # remove correspondence games and add increment
  games_with_increment <- games_list %>%
    # remove correspondence games, as not relevant
    dplyr::filter(!(grepl("Correspondence", Event))) %>%
    add_increment()

  # get clock data for each game
  add_times <- function(site_url){

    # extract one game at a time
    individual_game <- games_with_increment %>%
      dplyr::filter(Site == site_url) %>%
      dplyr::select(Moves, Increment)

    clock_data <- stringr::str_split(individual_game, "\\[|\\}")[[1]] %>%
      dplyr::as_tibble() %>%
      dplyr::rename(clock_time = value) %>%
      dplyr::filter(grepl("clk", clock_time)) %>%
      dplyr::mutate(clock_time = stringr::str_remove_all(clock_time, "%clk "),
                    clock_time = stringr::str_remove_all(clock_time, "\\] ")) %>%
      dplyr::full_join(games_with_increment %>%
                         dplyr::filter(Site == site_url) %>%
                         dplyr::select(Site, Increment, White, Black),
                       by = character()) %>%
      dplyr::mutate(colour = ifelse(
        dplyr::row_number() %% 2 == 0,
        "Black",
        "White"),
        move_number = floor((1 + dplyr::row_number()) / 2),
        clock_time = lubridate::seconds(lubridate::hms(clock_time)),
        move_time = lubridate::seconds(ifelse(dplyr::row_number() <= 2,
                                              0,
                                              Increment - clock_time + dplyr::lag(clock_time, 2)))) %>%
      dplyr::select(Site, White, Black,
                    colour, move_time, move_number)

  }

  df_out <- purrr::map_dfr(.x = games_with_increment$Site,
                           .f = add_times) %>%
    # some bugs in lichess mean there are some negative move times. Have set those move times to 0. Could add a flag to the rows that have been fixed in this way?
  dplyr::mutate(move_time = ifelse(move_time < 0,
                                   0,
                                   move_time))

}

test <- lichess_clock_move_time(games_list = lichess_game_data)


# ggplot2::ggplot(v,
#                 ggplot2::aes(x = move_time,
#                              fill = colour)) +
#   ggplot2::geom_histogram()

# move time by move number
ggplot2::ggplot(test %>%
                  dplyr::filter(
                    (White == "LordyLeroy" & colour == "White") |
                      (Black == "LordyLeroy" & colour == "Black"),
                    dplyr::between(move_number, 2, 9),
                    move_time <= 100),
                ggplot2::aes(x = move_time,
                             fill = as.factor(move_number))) +
  ggplot2::geom_density() +
  ggplot2::coord_flip() +
  ggplot2::labs(x = "Move time (seconds)",
                y = "Density",
                fill = "Move number",
                title = "Density of move time by colour (white or black)") +
  ggplot2::theme_minimal() +
  ggplot2::facet_wrap(~ colour)
