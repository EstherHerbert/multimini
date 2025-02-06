
<!-- README.md is generated from README.Rmd. Please edit that file -->

# multimini <img src="man/figures/logo.png" align="right" height="138" alt="" />

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of `multimini` is to use minimisation to randomise a multi-arm
trial with various different techniques for the minimisation
probabilities.

## Installation

You can install the development version of `multimini` from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("EstherHerbert/multimini")
```

## Examples

### Using the default minimisation probabilities

``` r
library(multimini)

(mini <- minimise(patients, groups = 3, factors = c("sex", "stage"), 
                  burnin = 15))
#> Multi-arm Minimisation 
#> --------------------------------------------------------------------------------
#> Groups: 1 2 3 
#> Randomisation ratio: 1:1:1 
#> Factors: sex, stage 
#> Burnin: 15 
#> Minimisation probability: 0.8 
#> Group sizes: 50, 52, 48

balance(mini)
#> Balance of factors (sex, stage) over 123 groups (1:1:1)
#> --------------------------------------------------------------------------------
#> |  F|  M|    |  I| II| III|
#> |--:|--:|    |--:|--:|---:|
#> | 23| 27|    | 25| 16|   9|
#> | 25| 27|    | 27| 15|  10|
#> | 22| 26|    | 25| 15|   8|
#> 
#> Total imbalance: 4.837
```

### Specifying minimisation probability

``` r
(mini <- minimise(patients, groups = 3, factors = c("sex", "stage"),
                  burnin = 15, minprob = 0.8))
#> Multi-arm Minimisation 
#> --------------------------------------------------------------------------------
#> Groups: 1 2 3 
#> Randomisation ratio: 1:1:1 
#> Factors: sex, stage 
#> Burnin: 15 
#> Minimisation probability: 0.8 
#> Group sizes: 50, 49, 51

balance(mini)
#> Balance of factors (sex, stage) over 123 groups (1:1:1)
#> --------------------------------------------------------------------------------
#> |  F|  M|    |  I| II| III|
#> |--:|--:|    |--:|--:|---:|
#> | 22| 28|    | 25| 15|  10|
#> | 23| 26|    | 25| 15|   9|
#> | 25| 26|    | 27| 16|   8|
#> 
#> Total imbalance: 5.414
```

### Updating the miniisation

``` r
new.patients <- data.frame(
 sex = c("F", "F", "M"),
 stage = c("II","I", "I"),
 site = c(9, 9, 1)
)

update(mini, new.patients) 
#> Multi-arm Minimisation 
#> --------------------------------------------------------------------------------
#> Groups: 1 2 3 
#> Randomisation ratio: 1:1:1 
#> Factors: sex, stage 
#> Burnin: 15 
#> Minimisation probability: 0.8 
#> Group sizes: 51, 50, 52
```
