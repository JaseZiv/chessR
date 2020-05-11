
<!-- README.md is generated from README.Rmd. Please edit that file -->

# chessR <a href='https:/jaseziv.github.io/chessR'><img src='man/figures/logo.png' align="right" height="219.5" /></a>

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/JaseZiv/chessR.svg?branch=master)](https://travis-ci.org/JaseZiv/chessR)
<!-- badges: end -->

## Overview

This package is designed to allow users to extract game data from
popular online chess platforms. The platforms currently supported in
this package include:

  - [chess.com](https://www.chess.com/)
  - [Lichess](https://lichess.org/)

These websites offer a very convenient set of APIs to be able to access
data and documentation to these can be found [here for
chess.com](https://www.chess.com/news/view/published-data-api) and [here
for Lichess](https://lichess.org/api).

## Installation

You can install the `chessR` package from github with:

``` r
# install.packages("devtools")
devtools::install_github("JaseZiv/chessR")
```

## Usage

The functions available in this package are designed to enable the
extraction of chess game data.

### Data Extraction

The functions detailed below relate to extracting data from the chess
gaming sites currently supported in this package.

#### Raw Game Data

The game extraction functions can take a vector of either single or
multiple usernames. It will output a data frame with all the games
played by that user.

The functions are below.

**Note:** These functions query an API, which is rate limited. The
limiting rates for chess.com are unknown. For Lichess, the limit is
throttled to 15 games per second. Queries could therefore take a few
minutes if you’re querying a lot of games.

``` r
# function to extract chess.com game data
chessdotcom_game_data <- get_raw_chessdotcom(c("JaseZiv", "Smudgy1"))

# function to extract lichess game data
lichess_game_data <- get_raw_lichess("Georges")
```

#### Analysis Data

The following function will extract the same data that the
`get_raw_chessdotcom()` function will, however this function will also
include additional columns to make analysing data easier.

The function can be used either on a single player, or a character
vector of multiple players.

**Note:** This is only available for chess.com extracts

``` r
chess_analysis_single <- get_game_data("JaseZiv")

chess_analysis_multiple <- get_game_data(c("JaseZiv", "elroch"))
```

### Leaderboards

The leaderboards of each game platform can be extracted for a number of
different games available on each platform. Each are discussed below:

#### Chess.com

The below function allows the user to extract the top 50 players of each
game type specified. Game types available include:

> *“daily”,“daily960”, “live\_rapid”, “live\_blitz”, “live\_bullet”,
> “live\_bughouse”, “live\_blitz960”, “live\_threecheck”,
> “live\_crazyhouse”, “live\_kingofthehill”, “lessons”, “tactics”*

The usernames that are contained in the results can then be passed to
`get_raw_chessdotcom` outlined above.

``` r
chessdotcom_leaders <- get_top50_leaderboard(game_type = "daily")
```

#### Lichess

The `get_lichess_leaderboard()` function takes in two parameters; how
many players you want returned (with a max of 200 being returned) and
the speed variant. Speed variants include;

> *“ultraBullet”, “bullet”, “blitz”, “rapid”, “classical”, “chess960”,
> “crazyhouse”, “antichess”, “atomic”, “horde”, “kingOfTheHill”,
> “racingKings”, “threeCheck”*

``` r
lichess_leaders <- (how_many_players = 10, speed_variant = "blitz")
```

For a detailed guide to using the package and the functions for
analysis, see the package
[vignette](https://jaseziv.github.io/chessR/articles/using_chessR_package.html)
