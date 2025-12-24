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

      df_with_increment <- games_list |>
        # add increment
        dplyr::mutate(Increment = as.integer(
          # only include characters after the + symbol of the TimeControl column
          stringr::str_remove(TimeControl, ".*\\+"))
        )

    return(df_with_increment)
  }

  # remove games without clk data and add increment
  games_with_increment <- games_list |>
    # remove games without "clk" included in the Moves column
    dplyr::filter(grepl("clk", Moves)) |>
    add_increment()

  # Print that can't extract move times if no rows with clock data
  if(nrow(games_with_increment) == 0){

    print("No games with clock times included within this lichess games data frame")

  } else {

    # get clock data for each game
    add_times <- function(site_url){

      # extract one game at a time
      individual_game <- games_with_increment |>
        dplyr::filter(Site == site_url) |>
        dplyr::select(Moves, Increment)

      clock_data <- stringr::str_split(individual_game, "\\[|\\}")[[1]] |>
        dplyr::as_tibble() |>
        dplyr::rename(clock_time = value) |>
        dplyr::filter(grepl("clk", clock_time)) |>
        dplyr::mutate(clock_time = stringr::str_remove_all(clock_time, "%clk "),
                      clock_time = stringr::str_remove_all(clock_time, "\\] ")) |>
        dplyr::cross_join(games_with_increment |>
                           dplyr::filter(Site == site_url) |>
                           dplyr::select(Site, Increment, White, Black)) |>
        dplyr::mutate(colour = ifelse(
          dplyr::row_number() %% 2 == 0,
          "Black",
          "White"),
          move_number = floor((1 + dplyr::row_number()) / 2),
          clock_time = lubridate::as.duration(
            lubridate::hms(clock_time)),
          move_time = ifelse(dplyr::row_number() <= 2,
                             0,
                             Increment - clock_time + dplyr::lag(clock_time, 2)),
          # some bugs in lichess mean there are some negative move times. Have set those move times to 0. Could add a flag to the rows that have been fixed in this way?
          move_time = ifelse(move_time < 0,
                             0,
                             move_time),
          move_time = lubridate::as.duration(move_time)) |>
        dplyr::select(Site, White, Black,
                      colour, move_number, clock_time,
                      move_time)

    }

    df_out <- purrr::map_dfr(.x = games_with_increment$Site,
                             .f = add_times)

  }

}
