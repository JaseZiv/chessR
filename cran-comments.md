## Release summary

This is a minor release that:

Bugfix: `plot_moves()` now correctly shows _all_ moves (@jonocarroll)
Feature: `plot_moves()` now takes a `sleep` argument which can be used to alter the speed of plot increments, 
  e.g. slower/faster for interactive use, or `sleep = 0` for producing a gif (@jonocarroll)
Feature: `extract_moves()` and `extract_moves_as_game()` can now take a local PGN file as input (@jonocarroll)
Feature: explored variations are now stripped from move input (@jonocarroll)
New function `lichess_clock_move_time` created to extract clock and move times from Lichess game data


## Test environments
* local R installation, R 4.1.0
* ubuntu 16.04 (on travis-ci), R 4.1.0
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
