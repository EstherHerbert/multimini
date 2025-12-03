# multimini

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
#> Groups: A B C 
#> Randomisation ratio: 1:1:1 
#> Factors: sex, stage 
#> Burnin: 15 
#> Minimisation probability: 0.8 
#> Group sizes: 50, 50, 50

balance(mini)
#> Balance of factors (sex, stage) over ABC groups (1:1:1)
#> --------------------------------------------------------------------------------
#> |   |  F|  M|    |   |  I| II| III|
#> |:--|--:|--:|    |:--|--:|--:|---:|
#> |A  | 24| 26|    |A  | 26| 15|   9|
#> |B  | 23| 27|    |B  | 26| 15|   9|
#> |C  | 23| 27|    |C  | 25| 16|   9|
#> 
#> Total imbalance: 2.309
```

### Specifying minimisation probability

``` r
(mini <- minimise(patients, groups = 3, factors = c("sex", "stage"),
                  burnin = 15, minprob = 0.8))
#> Multi-arm Minimisation 
#> --------------------------------------------------------------------------------
#> Groups: A B C 
#> Randomisation ratio: 1:1:1 
#> Factors: sex, stage 
#> Burnin: 15 
#> Minimisation probability: 0.8 
#> Group sizes: 51, 50, 49

balance(mini)
#> Balance of factors (sex, stage) over ABC groups (1:1:1)
#> --------------------------------------------------------------------------------
#> |   |  F|  M|    |   |  I| II| III|
#> |:--|--:|--:|    |:--|--:|--:|---:|
#> |A  | 24| 27|    |A  | 26| 16|   9|
#> |B  | 23| 27|    |B  | 26| 15|   9|
#> |C  | 23| 26|    |C  | 25| 15|   9|
#> 
#> Total imbalance: 2.309
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
#> Groups: A B C 
#> Randomisation ratio: 1:1:1 
#> Factors: sex, stage 
#> Burnin: 15 
#> Minimisation probability: 0.8 
#> Group sizes: 53, 50, 50
```
