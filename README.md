# Advent of Code

This repository contains Haskell programs to solve Advent of Code puzzles. It is intended to contain solutions to all events. Information on the currently completed puzzles can be found in the following table (⭐ first part, 🌟 both parts):

|      | 01 | 02 | 03 | 04 | 05 | 06 | 07 | 08 | 09 | 10 | 11 |  12 | 13 | 14 | 15 | 16 | 17 | 18 | 19 | 20 | 21 | 22 | 23 | 24 | 25 |
|------|----|----|----|----|----|----|----|----|----|----|----|-----|----|----|----|----|----|----|----|----|----|----|----|----|----|
| 2022 | 🌟 | 🌟 | 🌟 | 🌟 | 🌟 | 🌟 | 🌟 | 🌟 | 🌟 |    |    |     |    |    |    |    |    |    |    |    |    |    |    |    |    |
| 2021 | 🌟 | 🌟 | 🌟 | 🌟 | 🌟 | 🌟 | 🌟 | 🌟 | 🌟 | 🌟 | 🌟 | 🌟 | 🌟 | 🌟 | 🌟 | 🌟 | 🌟 | 🌟 | 🌟 | 🌟 | 🌟 | ⭐ |    |    |    |
| 2020 | 🌟 | 🌟 | 🌟 | 🌟 | 🌟 | 🌟 | 🌟 | 🌟 | 🌟 | 🌟 | 🌟 | 🌟 | 🌟 | 🌟 | 🌟 | 🌟 | 🌟 | 🌟 | 🌟 | 🌟 | 🌟 | 🌟 | 🌟 | 🌟 | 🌟 |
| 2019 |    |    |    |    |    |    |    |    |    |    |    |     |    |    |    |    |    |    |    |    |    |    |    |    |    |
| 2018 |    |    |    |    |    |    |    |    |    |    |    |     |    |    |    |    |    |    |    |    |    |    |    |    |    |
| 2017 |    |    |    |    |    |    |    |    |    |    |    |     |    |    |    |    |    |    |    |    |    |    |    |    |    |
| 2016 | 🌟 | 🌟 | 🌟 | 🌟 | 🌟 | 🌟 | 🌟 | 🌟 | 🌟 | 🌟 |    |     |    |    |    |    |    |    |    |    |    |    |    |    |    |
| 2015 | 🌟 | 🌟 | 🌟 | 🌟 | 🌟 | 🌟 | 🌟 | 🌟 | 🌟 | 🌟 | 🌟 | 🌟 | 🌟 | 🌟 | 🌟 | 🌟 | 🌟 | 🌟 | 🌟 | 🌟 | 🌟 | 🌟 | 🌟 | 🌟 | 🌟 |

## Setup

This project uses `GHC` 9.0.1 and `cabal-install` 3.6.2.0, which can easily be installed using [ghcup](https://www.haskell.org/ghcup/):

```sh
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

ghcup install ghc 9.0.1
ghcup set ghc 9.0.1

ghcup install cabal 3.6.2.0
ghcup set cabal 3.6.2.0
```

For any extra information on `ghcup`, you can check its [gitlab page](https://gitlab.haskell.org/haskell/ghcup-hs#usage).

## Building & Running

To build the entire project

```sh
cabal build all
```

If you prefer to simply build a single executable, you can do so

```sh
cabal build aoc%%%%:day##
```

where `%%%%` is the year and `##` is the day number, left padded with zeroes to two digits.

To run the solution to a given day, just do

```sh
cabal run aoc%%%%:day##
```

Note that you can omit the build steps and simply do `cabal run`, as it will build whatever necessary to run the requested executables.

## Testing

This project uses `doctest` to make sure the solutions are all correct. To run the tests do

```sh
cabal test
```

To run the tests on a single file, do

```sh
cabal test --test-options="<path_to_file>"
```

## To do

Extra things to do apart from completing missing puzzles:

* Go through 2015 for potential improvements
* Improvements on 2016:
  * Day 09: can the call to `parseWrapper` be removed from inside the parser?
* Import and refactor [2019 solutions](https://github.com/jnoliv/AOC19)
* Improvements on 2020:
  * Day 07: would Data.Graph work?
  * Day 14: it looks bad, can it be improved?
  * Day 19: ReadP
  * Day 25: better dicrete logarithm than brute force?
* Improvements on 2021
  * Utils: refactor the distance and manhattan utilities
  * Day 15: refactor (and fix) the expansion of the input into a function, abstract A* into the Grid library
  * Day 16: can this be cleaned up?
