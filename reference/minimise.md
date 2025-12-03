# Minimisation for a dataset

`minimise` randomises patients in a data frame using minimisation

## Usage

``` r
minimise(
  data,
  groups = 3,
  factors,
  burnin = 10,
  minprob = 0.8,
  ratio = rep(1, groups),
  group.names = NULL,
  seed = NULL,
  check.eligibility = FALSE
)
```

## Arguments

- data:

  a `data.frame` object with one line per participant and columns for
  the minimisation factors. It is also possible to pass an empty
  `data.frame` in order to set up minimisation for later use through
  [`update.mini()`](https://estherherbert.github.io/multimini/reference/update.mini.md).

- groups:

  an integer, the number of groups to randomise to, default is 3

- factors:

  a character vector with the factors for minimisation

- burnin:

  an integer, the burnin length before minimisation kicks in, default
  is 10. Must be \> 0.

- minprob:

  the minimisation probability. The default is to give 0.8 probability
  to the group which would lead to the least imbalance.

- ratio:

  a numeric vector of randomisation ratios in ascending order (must be
  of length equal to the number of groups)

- group.names:

  optional, a character vector with the group names, must be the same
  length as `groups`.

- seed:

  optional, an integer that is used with
  [`set.seed()`](https://rdrr.io/r/base/Random.html) for offsetting the
  random number generator.

- check.eligibility:

  logical indicating whether participants have varying eligibility for
  different groups. If `TRUE` then the column `eligible` should exist in
  the data.

## Value

(Invisibly) the data.frame with an additional column `Group` indicating
which group has been allocated.

## Examples

``` r
# Minimisation to 3 groups, with two factors and a burnin of 15, using the
# patients data from the package
(mini <- minimise(patients, groups = 3, factors = c("sex", "stage"),
                  burnin = 15))
#> Multi-arm Minimisation 
#> --------------------------------------------------------------------------------
#> Groups: A B C 
#> Randomisation ratio: 1:1:1 
#> Factors: sex, stage 
#> Burnin: 15 
#> Minimisation probability: 0.8 
#> Group sizes: 52, 48, 50 

# View data with group info
as.data.frame(mini)
#>     sex stage site eligible Group
#> 1     M    II    6  A, B, C     C
#> 2     M   III    6  A, B, C     B
#> 3     M    II    8  A, B, C     C
#> 4     F   III    2  A, B, C     B
#> 5     F   III    4  A, B, C     C
#> 6     F   III    7  A, B, C     A
#> 7     M    II    6  A, B, C     A
#> 8     F    II    4  A, B, C     A
#> 9     M    II    7  A, B, C     A
#> 10    M    II    8  A, B, C     C
#> 11    F     I    7  A, B, C     A
#> 12    M    II    4  A, B, C     A
#> 13    F     I    6  A, B, C     A
#> 14    F     I    1  A, B, C     B
#> 15    F    II    4  A, B, C     B
#> 16    M   III    1  A, B, C     B
#> 17    M     I    1  A, B, C     B
#> 18    F    II    5  A, B, C     B
#> 19    M    II   10  A, B, C     A
#> 20    M     I    4  A, B, C     C
#> 21    M     I    9  A, B, C     B
#> 22    M     I    1  A, B, C     C
#> 23    M     I   10  A, B, C     A
#> 24    F   III    8  A, B, C     C
#> 25    F     I    7  A, B, C     C
#> 26    M    II    4  A, B, C     B
#> 27    F   III    7  A, B, C     C
#> 28    M    II   10  A, B, C     A
#> 29    M     I    5  A, B, C     C
#> 30    F    II    9  A, B, C     A
#> 31    F     I    8  A, B, C     B
#> 32    M    II    3  A, B, C     B
#> 33    F    II    5  A, B, C     C
#> 34    M     I    3  A, B, C     A
#> 35    M   III    7  A, B, C     A
#> 36    M     I    3  A, B, C     B
#> 37    M   III    7  A, B, C     C
#> 38    F     I    9  A, B, C     A
#> 39    F     I    7  A, B, C     C
#> 40    M    II    7  A, B, C     C
#> 41    M     I    1  A, B, C     B
#> 42    M     I   10  A, B, C     A
#> 43    F     I   10  A, B, C     C
#> 44    F     I    5  A, B, C     B
#> 45    F    II    2  A, B, C     B
#> 46    M     I    4  A, B, C     C
#> 47    F     I    5  A, B, C     A
#> 48    F     I    1  A, B, C     C
#> 49    F   III   10  A, B, C     A
#> 50    M    II    3  A, B, C     A
#> 51    F   III    7  A, B, C     B
#> 52    F     I    6  A, B, C     B
#> 53    F   III    6  A, B, C     A
#> 54    M   III    6  A, B, C     B
#> 55    F     I   10  A, B, C     A
#> 56    F   III   10  A, B, C     C
#> 57    F     I    8  A, B, C     B
#> 58    M    II    8  A, B, C     C
#> 59    F     I    6  A, B, C     C
#> 60    F    II    5  A, B, C     B
#> 61    F   III    6  A, B, C     A
#> 62    F     I   10  A, B, C     C
#> 63    M     I    7  A, B, C     B
#> 64    F    II    2  A, B, C     C
#> 65    F    II    2  A, B, C     B
#> 66    F     I    6  A, B, C     A
#> 67    F     I    4  A, B, C     A
#> 68    F   III    6  A, B, C     B
#> 69    F    II    2  A, B, C     A
#> 70    M     I    4  A, B, C     B
#> 71    M   III    9  A, B, C     C
#> 72    F     I    9  A, B, C     C
#> 73    F     I    7  A, B, C     A
#> 74    F    II    8  A, B, C     C
#> 75    F     I    8  A, B, C     B
#> 76    M    II    5  A, B, C     B
#> 77    M     I    4  A, B, C     C
#> 78    M     I    3  A, B, C     A
#> 79    M     I    8  A, B, C     A
#> 80    F     I   10  A, B, C     B
#> 81    F     I    3  A, B, C     A
#> 82    F     I    7  A, B, C     C
#> 83    F     I    2  A, B, C     C
#> 84    M     I    4  A, B, C     B
#> 85    M     I    2  A, B, C     C
#> 86    F    II    3  A, B, C     B
#> 87    F     I    9  A, B, C     C
#> 88    F   III    1  A, B, C     A
#> 89    M   III    4  A, B, C     A
#> 90    F    II    9  A, B, C     B
#> 91    F     I    5  A, B, C     C
#> 92    M    II    6  A, B, C     C
#> 93    M     I    2  A, B, C     B
#> 94    M     I    3  A, B, C     A
#> 95    F     I    9  A, B, C     B
#> 96    F     I    5  A, B, C     A
#> 97    F     I    2  A, B, C     B
#> 98    M     I   10  A, B, C     A
#> 99    M     I    9  A, B, C     C
#> 100   M   III    1  A, B, C     B
#> 101   F     I    8  A, B, C     A
#> 102   M     I    5  A, B, C     C
#> 103   M     I    2  A, B, C     B
#> 104   F     I    1  A, B, C     A
#> 105   M     I    9  A, B, C     B
#> 106   F    II   10  A, B, C     C
#> 107   F     I    4  A, B, C     B
#> 108   M     I    8  A, B, C     C
#> 109   M     I    3  A, B, C     B
#> 110   F     I    6  A, B, C     C
#> 111   F    II    5  A, B, C     A
#> 112   F     I    3  A, B, C     A
#> 113   F     I   10  A, B, C     A
#> 114   M     I    3  A, B, C     A
#> 115   M     I   10  A, B, C     A
#> 116   F     I    7  A, B, C     C
#> 117   M     I    7  A, B, C     C
#> 118   F     I    9  A, B, C     C
#> 119   M     I    4  A, B, C     A
#> 120   M    II    1  A, B, C     C
#> 121   M     I   10  A, B, C     B
#> 122   M    II    9  A, B, C     A
#> 123   M    II    9  A, B, C     B
#> 124   M     I    2  A, B, C     A
#> 125   F     I    3  A, B, C     B
#> 126   F    II    7  A, B, C     B
#> 127   F    II    4  A, B, C     C
#> 128   M   III    6  A, B, C     C
#> 129   F    II    6  A, B, C     A
#> 130   M    II   10  A, B, C     A
#> 131   M     I    7  A, B, C     C
#> 132   M     I    4  A, B, C     B
#> 133   M     I    9  A, B, C     B
#> 134   F     I    6  A, B, C     B
#> 135   F    II   10  A, B, C     C
#> 136   M   III    6  A, B, C     C
#> 137   M     I    8  A, B, C     C
#> 138   F     I    1  A, B, C     A
#> 139   M    II    3  A, B, C     B
#> 140   F     I   10  A, B, C     A
#> 141   M     I    9  A, B, C     A
#> 142   M   III    9  A, B, C     A
#> 143   F     I    3  A, B, C     A
#> 144   M     I    6  A, B, C     B
#> 145   M     I    1  A, B, C     C
#> 146   F     I    5  A, B, C     B
#> 147   M     I    3  A, B, C     A
#> 148   M    II    2  A, B, C     A
#> 149   M     I    3  A, B, C     C
#> 150   F     I    5  A, B, C     C

# Use 1:1:2 ratio
minimise(patients, groups = 3, factors = c("sex", "stage"), burnin = 5,
         ratio = c(1,1,2))
#> Multi-arm Minimisation 
#> --------------------------------------------------------------------------------
#> Groups: A B C 
#> Randomisation ratio: 1:1:2 
#> Factors: sex, stage 
#> Burnin: 5 
#> Minimisation probability: 0.8 
#> Group sizes: 39, 37, 74 

# Account for different group eligibility
minimise(patients, groups = 3, factors = c("sex", "stage"), burnin = 5,
         ratio = c(1,1,2), check.eligibility = TRUE)
#> Multi-arm Minimisation 
#> --------------------------------------------------------------------------------
#> Groups: A B C 
#> Randomisation ratio: 1:1:2 
#> Factors: sex, stage 
#> Burnin: 5 
#> Minimisation probability: 0.8 
#> Group sizes: 42, 37, 71 
```
