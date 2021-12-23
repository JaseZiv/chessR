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

Intial release of the `chessR` package

* Added function for extracting raw chess.com data in a data frame
* Added a function that in addition to extracting the raw data, also included additional features
* Added a function to extract the top 50 leaderboard depending on the game type selected
