# function to extract time data
#' Get Time Information from Lichess Game Data
#'
#' This function returns a data frame
#' of Lichess data with clock and move times
#'
#' @param games_list A data frame of lichess data which can be generated from chessR::get_raw_lichess("username")
#'
#' @return a data frame of lichess data with move time, clock time, and move numbers
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' \dontrun{
#' lordy_leroy_data <- get_raw_lichess(player_names = "LordyLeroy")
#' lordy_leroy_data_with_times <- lichess_clock_move_time(games_list = lordy_leroy_data)
#' }
lichess_clock_move_time <- function(games_list){

  # Intermediate function to add increment from the TimeControl
  add_increment <- function(games_list){

    suppressWarnings(
      df_with_increment <- games_list %>%
        # add increment
        dplyr::mutate(Increment = as.integer(
          # only include characters after the + symbol of the TimeControl column
          stringr::str_remove(.data$TimeControl, ".*\\+"))
        ))

  }

  # remove games without clk data and add increment
  games_with_increment <- games_list %>%
    # remove games without "clk" included in the Moves column
    dplyr::filter(grepl("clk", .data$Moves)) %>%
    add_increment()

  # Print that can't extract move times if no rows with clock data
  if(nrow(games_with_increment) == 0){

    print("No games with clock times included within this lichess games data frame")

  } else {

    # get clock data for each game
    add_times <- function(site_url){

      # extract one game at a time
      individual_game <- games_with_increment %>%
        dplyr::filter(.data$Site == site_url) %>%
        dplyr::select(.data$Moves, .data$Increment)

      clock_data <- stringr::str_split(individual_game, "\\[|\\}")[[1]] %>%
        dplyr::as_tibble() %>%
        dplyr::rename(clock_time = .data$value) %>%
        dplyr::filter(grepl("clk", .data$clock_time)) %>%
        dplyr::mutate(clock_time = stringr::str_remove_all(.data$clock_time, "%clk "),
                      clock_time = stringr::str_remove_all(.data$clock_time, "\\] ")) %>%
        dplyr::full_join(games_with_increment %>%
                           dplyr::filter(.data$Site == site_url) %>%
                           dplyr::select(.data$Site, .data$Increment, .data$White, .data$Black),
                         by = character()) %>%
        dplyr::mutate(colour = ifelse(
          dplyr::row_number() %% 2 == 0,
          "Black",
          "White"),
          move_number = floor((1 + dplyr::row_number()) / 2),
          clock_time = lubridate::as.duration(
            lubridate::hms(.data$clock_time)),
          move_time = ifelse(dplyr::row_number() <= 2,
                             0,
                             .data$Increment - .data$clock_time + dplyr::lag(.data$clock_time, 2)),
          # some bugs in lichess mean there are some negative move times. Have set those move times to 0. Could add a flag to the rows that have been fixed in this way?
          move_time = ifelse(.data$move_time < 0,
                             0,
                             .data$move_time),
          move_time = lubridate::as.duration(.data$move_time)) %>%
        dplyr::select(.data$Site, .data$White, .data$Black,
                      .data$colour, .data$move_number, .data$clock_time,
                      .data$move_time)

    }

    df_out <- purrr::map_dfr(.x = games_with_increment$Site,
                             .f = add_times)

  }

}

# test <- lichess_clock_move_time(games_list = lichess_game_data)
#
# ggplot2::ggplot(v,
#                 ggplot2::aes(x = move_time,
#                              fill = colour)) +
#   ggplot2::geom_histogram()

# # move time by move number
# username <- "LordyLeroy"
#
# ggplot2::ggplot(test %>%
#                   dplyr::filter(
#                     (White == username & colour == "White") |
#                       (Black == username & colour == "Black"),
#                     dplyr::between(move_number, 2, 9),
#                     move_time <= 100),
#                 ggplot2::aes(x = move_time,
#                              fill = as.factor(move_number))) +
#   ggplot2::geom_density() +
#   ggplot2::coord_flip() +
#   ggplot2::labs(x = "Move time (seconds)",
#                 y = "Density",
#                 fill = "Move number",
#                 title = "Density of move time by colour (white or black)",
#                 subtitle = paste0("User: ", username)) +
#   ggplot2::theme_minimal() +
#   ggplot2::facet_wrap(~ colour)
