
chessR
======

<!-- badges: start -->
[![Travis build status](https://travis-ci.org/JaseZiv/chessR.svg?branch=master)](https://travis-ci.org/JaseZiv/chessR) <!-- badges: end -->

Overview
--------

This package is designed to allow users to extract their game data from the popular online chess platform [chess.com](https://www.chess.com/).

The website offers a very convenient set of APIs to be able to access and documentation to these can be found [here](https://www.chess.com/news/view/published-data-api).

Installation
------------

You can install the chessR package from github with:

``` r
# install.packages("devtools")
devtools::install_github("JaseZiv/chessR")
```

Functions
---------

There are a number of functions available in this package, both for extracting data and also for analysing and visualising data.

### Extraction

The following extraction functions will get json formatted game data and clean and convert it to a data frame.

#### Raw Data

``` r
# install.packages("devtools")
raw_chess <- get_raw_game_data("JaseZiv")
```

#### Analysis Data

The following two functions will extract the same data that the `get_raw_game_data()` function will, however these functions will also include additional columns to make analysing data easier.

``` r
chess_analysis_single <- analyse_player_games("JaseZiv")

chess_analysis_multiple <- analyse_multiple_players(c("JaseZiv", "elroch"))
```

#### Get Top Players' usernames

The below function allows the user to extract the top 50 leaders on the leaderboards for a number of different game types.

The game types include:

-   "daily"
-   "daily960""
-   "live\_rapid"
-   "live\_blitz"
-   "live\_bullet"
-   "live\_bughouse"
-   "live\_blitz960"
-   "live\_threecheck"
-   "live\_crazyhouse"
-   "live\_kingofthehill"
-   "lessons"
-   "tactics"

The usernames that are contained in the results can then be passes to the chess game data extraction function outlined above.

``` r
daily_leaders <- get_top50_leaderboard("daily")
```