# Simulation of minimisation

`simulate_mini` repeatedly generates random data and then minimises with
different specified burnin periods and/or minimisation probabilities.
Its purpose is to guide decisions when designing a randomisation
strategy for a randomised controlled trial.

## Usage

``` r
simulate_mini(
  sampsize,
  factors,
  eligibility = FALSE,
  Nsims = 100,
  groups = 3,
  burnin,
  minprob,
  ratio = rep(1, groups)
)
```

## Arguments

- sampsize:

  the desired sample size of the data, i.e., the sample size of your
  prospective trial.

- factors:

  a list of factors, each a list containing two items. The first is
  `levels` which is either a vector of the level names OR the number of
  levels. The second item is either `props`, a vector of proportions
  equal to the number of levels in the factor; OR prop.dist which is a
  named vector containing the mean and sd of the proportions.

- eligibility:

  logical, should the data include group eligibility. If `TRUE` then
  must supply `groups` argument.

- Nsims:

  an integer, the number of simulations to run per scenario, default is
  100.

- groups:

  an integer, the number of groups to randomise, default is 3.

- burnin:

  a vector of integers of possible burnin lengths before minimisation
  kicks in. Individual values must be \> 0 and \< total sample size.

- minprob:

  a vector of the possible minimisation probabilities.

- ratio:

  a numeric vector of randomisation ratios (must be of length equal to
  the number of groups).

## Examples

``` r
simulate_mini(sampsize = 150,
              factors = list(sex = list(levels = c("M", "F"),
                                        props = c(0.5, 0.5)),
                             site = list(levels = 12,
                                         prop.dist = c(mean = 0.1, sd = 0.05))),
              Nsims = 10, groups = 3, burnin = c(10, 15),
              minprob = c(0.7, 0.8, 0.9), ratio = c(1,1,1))
#> Simulation of Multi-arm Minimisation 
#> --------------------------------------------------------------------------------
#> Number of simulations per scenario: 10 
#> Factors: sex, site 
#> Allocation ratio: 1, 1, 1 
#> Burnin options: 10, 15 
#> Minimisation probability options: 0.7, 0.8, 0.9 
#> Average group sizes:
#> | burnin| minprob|    A|    B|    C|
#> |------:|-------:|----:|----:|----:|
#> |     10|     0.7| 49.9| 49.8| 50.3|
#> |     15|     0.7| 50.0| 50.5| 49.5|
#> |     10|     0.8| 49.9| 50.3| 49.8|
#> |     15|     0.8| 50.1| 49.9| 50.0|
#> |     10|     0.9| 50.2| 49.6| 50.2|
#> |     15|     0.9| 49.6| 50.7| 49.7|
#> Average imbalance:
#> | burnin| minprob| imbalance|
#> |------:|-------:|---------:|
#> |     10|     0.7|      11.4|
#> |     15|     0.7|      11.5|
#> |     10|     0.8|       8.6|
#> |     15|     0.8|       9.8|
#> |     10|     0.9|       8.6|
#> |     15|     0.9|       9.5|
```
