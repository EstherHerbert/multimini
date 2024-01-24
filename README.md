
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

This is a basic example which shows you how to solve a common problem:

``` r
library(multimini)

(mini <- minimise(patients, groups = 3, factors = c("sex", "stage"), 
                  burnin = 15))

balance(mini)
```

### Specifying minimisation probabilities

``` r
(mini <- minimise(patients, groups = 3, factors = c("sex", "stage"),
                  burnin = 15, minprob = c(0.8, 0.2, 0)))

balance(mini)
```

### Updating the miniisation

``` r
new.patients <- data.frame(
 sex = c("F", "F", "M"),
 stage = c("II","I", "I"),
 site = c(9, 9, 1)
)

update(mini, new.patients) 
```
