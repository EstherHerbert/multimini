# Get balance of minimisation

Get balance minimisation randomisation

## Usage

``` r
balance(object)
```

## Arguments

- object:

  An object of class `mini`

## Value

An object of class `balance.mini`

## Examples

``` r
# Minimisation to 3 groups, with two factors and a burnin of 15
mini <- minimise(patients, groups = 3, factors = c("sex", "stage"),
                 burnin = 15)

balance(mini)
#> Balance of factors (sex, stage) over ABC groups (1:1:1)
#> --------------------------------------------------------------------------------
#> |   |  F|  M|    |   |  I| II| III|
#> |:--|--:|--:|    |:--|--:|--:|---:|
#> |A  | 25| 25|    |A  | 29| 13|   8|
#> |B  | 25| 25|    |B  | 27| 14|   9|
#> |C  | 26| 24|    |C  | 31| 13|   6|
#> 
#> Total imbalance: 5.26
```
