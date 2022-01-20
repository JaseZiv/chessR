## Release summary

This is a minor release that:

Addresses feedback from CRAN submission process below:

```  
   Missing Rd-tags:
      chessdotcom_leaderboard.Rd: \value
      get_game_data.Rd: \value
      get_raw_chessdotcom.Rd: \value
      get_raw_lichess.Rd: \value
      lichess_leaderboard.Rd: \value

You have examples for unexported functions.
Please either omit these examples or export these functions.
Used ::: in documentation:
      man/plot_moves.Rd:
         hikaru <- chessR:::get_each_player_chessdotcom("hikaru", "202112")
```

## Test environments
* local R installation, R 4.1.0
* ubuntu 16.04 (on travis-ci), R 4.1.0
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
