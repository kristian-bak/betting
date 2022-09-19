## Init maximum stake (filters bets larger than max_stake away)
## Updatestate updates the sliders, so not needed to update this number
max_stake <- 500

## Grouping the odds by these breakpoints
breaks_odds <- c(1, 1.1, 1.15, 1.25, 1.50, 1.75, 2, 2.5, 3, 4, 5)

## Grouping the stake by these breakpoints
breaks_stake <- c(0, 25, 50, 75, 100, 200) 

## The value for which odds will be bounded
bound_odds <- 5

## The value for which stake will be bounded
bound_stake <- 200