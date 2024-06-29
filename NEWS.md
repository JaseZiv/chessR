# chessR 1.5.4

Bugfix: prevent scientific notation in time queries for `get_raw_lichess()`
(@py-b #18)

# chessR 1.5.3

Feature: `get_raw_lichess()` can now be filtered by date with the parameters
`since` and `until` (@py-b #16)

***

# chessR 1.5.2

CRAN suggestions for functions to fail gracefully.

***

# chessR 1.5.1

Bugfix: `plot_moves()` now correctly shows _all_ moves (@jonocarroll)

Feature: `plot_moves()` now takes a `sleep` argument which can be used to alter the speed of plot increments, 
  e.g. slower/faster for interactive use, or `sleep = 0` for producing a gif (@jonocarroll)

Feature: `extract_moves()` and `extract_moves_as_game()` can now take a local PGN file as input (@jonocarroll)

Feature: explored variations are now stripped from move input (@jonocarroll)

New function `lichess_clock_move_time` created to extract clock and move times from Lichess game data

***

# chessR 1.5.0

Package documentation upgrades to address further CRAN submission feedback

***

# chessR 1.2.4 (2021-12-30)

The following functions were created to aid in the visualisation of matches played:

* `extract_moves()`
* `extract_moves_as_game()`
* `plot_game()`

***

# chessR 1.2.3 (2021-12-23)

* `get_raw_game_data()` previously deprecated, now removed

***

# chessR 1.2.2 (2021-05-16)

* `get_raw_chessdotcom()` now accepts a YYYYMM argument to limit which months to extract game data for [#2](https://github.com/JaseZiv/chessR/issues/2)

***

# chessR 1.2.1 (2021-05-16)

* fixed bug in `get_raw_chessdotcom()` to do with game type bughouse [#9](https://github.com/JaseZiv/chessR/issues/9)

***

# chessR 1.2.0 (2020-05-10)

* Lichess online platform extractions now integrated, including `get_raw_lichess` for games and `lichess_leaderboard` for the top leaders [#5](https://github.com/JaseZiv/chessR/issues/5)

***

# chessR 1.1.0 (2020-05-06)

* New function created (`get_raw_chessdotcom`) deprecate `get_raw_game_data` [#6](https://github.com/JaseZiv/chessR/issues/6)
* Various analysis functions created
* `get_top50_leaderboard` deprecated, replaced with `chessdotcom_leaderboard()` [#6](https://github.com/JaseZiv/chessR/issues/6)

***

# chessR 1.0.0 (2020-04-27)

Initial release of the `chessR` package

* Added function for extracting raw chess.com data in a data frame
* Added a function that in addition to extracting the raw data, also included additional features
* Added a function to extract the top 50 leaderboard depending on the game type selected
