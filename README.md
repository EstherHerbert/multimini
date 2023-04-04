
# multimini

<!-- badges: start -->
<!-- badges: end -->

The goal of multimini is to use minimisation to randomise a mult-arm trial with various different techniques for the minimisation probabilites.

## Installation

You can install the development version of multimini from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("EstherHerbert/multimini")
```

## Examples

### Using the default minimisation probabilities

This is a basic example which shows you how to solve a common problem:

``` r
library(multimini)
patients <- data.frame(sex = sample(c("M", "F"), 150, replace = T),
                       stage = sample(c("I", "II", "III"), 150, replace = T,
                                      prob = c(0.5, 0.3, 0.2)),
                       site = sample(1:10, 150, replace = T))
minimise(patients, groups = 4, factors = c("sex", "site"), burnin = 15)
```

