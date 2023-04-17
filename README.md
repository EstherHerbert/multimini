
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
### Using stratification

``` r
(mini_s <- minimise(patients, groups = 3, factors = c("sex", "stage"),
                    burnin = 5, stratify = "site"))

balance(mini_s)
```
