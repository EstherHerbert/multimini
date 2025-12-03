# Update a minimisation with new data

`update` updates the minimisation with new participant data, using the
existing group allocations and factor information to allocate the new
participants using minimisation.

## Usage

``` r
# S3 method for class 'mini'
update(object, new.data, ...)
```

## Arguments

- object:

  an object of class mini, i.e., resulting from
  [`minimise()`](https://estherherbert.github.io/multimini/reference/minimise.md)

- new.data:

  a `data.frame` object with one line per new participant and columns
  for the minimisation factors. Must be of the same format as the
  `data.frame` in `object` (minus `Group`)

- ...:

  unused arguments passed to
  [`update.default()`](https://rdrr.io/r/stats/update.html)

## Value

(Invisibly) the data.frame of all participants with group existing and
new group allocations

## See also

[`update()`](https://rdrr.io/r/stats/update.html) for the generic
function

## Examples

``` r
mini <- minimise(patients, groups = 3, factors = c("sex", "stage"),
                 burnin = 10)

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
#> Burnin: 10 
#> Minimisation probability: 0.8 
#> Group sizes: 53, 50, 50 
```
