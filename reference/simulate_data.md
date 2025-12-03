# Simulate data for minimisation simulations

Given a list of factors with specified levels and proportions, and a
sample size, this function simulates data for use in
[`simulate_mini()`](https://estherherbert.github.io/multimini/reference/simulate_mini.md).

## Usage

``` r
simulate_data(sampsize, factors, eligibility = FALSE, groups = NULL)
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

- groups:

  either a character vector of group names or an integer specifying the
  number of groups.

## Examples

``` r
simulate_data(factors = list(sex = list(levels = c("M", "F"),
                                        props = c(0.5, 0.5)),
                             site = list(levels = 12,
                                         prop.dist = c(mean = 0.1, sd = 0.05))),
              sampsize = 150)
#>      ID sex site
#> 1     1   M    7
#> 2     2   F    8
#> 3     3   M    5
#> 4     4   M    2
#> 5     5   F   11
#> 6     6   M    2
#> 7     7   M   10
#> 8     8   M    8
#> 9     9   M    3
#> 10   10   F   10
#> 11   11   M   11
#> 12   12   M    8
#> 13   13   F    2
#> 14   14   F    8
#> 15   15   F    1
#> 16   16   M    4
#> 17   17   F   12
#> 18   18   M    3
#> 19   19   M    7
#> 20   20   M   10
#> 21   21   F   11
#> 22   22   M    7
#> 23   23   M   10
#> 24   24   F    9
#> 25   25   M   10
#> 26   26   M   10
#> 27   27   M    1
#> 28   28   M    7
#> 29   29   F    2
#> 30   30   F    2
#> 31   31   F    2
#> 32   32   F    2
#> 33   33   M    9
#> 34   34   F    4
#> 35   35   M   11
#> 36   36   M   12
#> 37   37   M   10
#> 38   38   F    3
#> 39   39   M    2
#> 40   40   M    2
#> 41   41   M    3
#> 42   42   F   10
#> 43   43   M   10
#> 44   44   F    6
#> 45   45   F   10
#> 46   46   M    8
#> 47   47   M    7
#> 48   48   F    4
#> 49   49   M    4
#> 50   50   F    2
#> 51   51   M    9
#> 52   52   F    6
#> 53   53   M    9
#> 54   54   M    9
#> 55   55   M    2
#> 56   56   M   12
#> 57   57   F    5
#> 58   58   M    6
#> 59   59   M    6
#> 60   60   M    1
#> 61   61   F    9
#> 62   62   M    4
#> 63   63   M   12
#> 64   64   F    7
#> 65   65   M    5
#> 66   66   M    6
#> 67   67   F   10
#> 68   68   M   12
#> 69   69   F    2
#> 70   70   M   12
#> 71   71   M    5
#> 72   72   F    5
#> 73   73   M   10
#> 74   74   M    9
#> 75   75   F    2
#> 76   76   M   12
#> 77   77   M    3
#> 78   78   F   12
#> 79   79   F    5
#> 80   80   F    4
#> 81   81   F    8
#> 82   82   F    2
#> 83   83   F   10
#> 84   84   M    3
#> 85   85   M    9
#> 86   86   M    3
#> 87   87   F    1
#> 88   88   M    7
#> 89   89   F   10
#> 90   90   M    9
#> 91   91   F    7
#> 92   92   M    3
#> 93   93   M    2
#> 94   94   F    9
#> 95   95   F    7
#> 96   96   M    5
#> 97   97   F    6
#> 98   98   M    4
#> 99   99   M    2
#> 100 100   F    8
#> 101 101   M    2
#> 102 102   F   12
#> 103 103   M    9
#> 104 104   M    7
#> 105 105   M    2
#> 106 106   F    2
#> 107 107   F    7
#> 108 108   F    6
#> 109 109   M    8
#> 110 110   F    9
#> 111 111   F   12
#> 112 112   M    2
#> 113 113   F    2
#> 114 114   M    5
#> 115 115   F    2
#> 116 116   F   12
#> 117 117   M    4
#> 118 118   F    9
#> 119 119   M    8
#> 120 120   F    7
#> 121 121   M   11
#> 122 122   F    2
#> 123 123   F    8
#> 124 124   M   11
#> 125 125   M   10
#> 126 126   F    2
#> 127 127   F    2
#> 128 128   M    6
#> 129 129   F    3
#> 130 130   F    4
#> 131 131   M    2
#> 132 132   F    1
#> 133 133   F    3
#> 134 134   M   10
#> 135 135   M    7
#> 136 136   M    8
#> 137 137   F    1
#> 138 138   M    9
#> 139 139   F   12
#> 140 140   F   10
#> 141 141   F    1
#> 142 142   F    1
#> 143 143   F   12
#> 144 144   M    2
#> 145 145   M    4
#> 146 146   F    5
#> 147 147   M   10
#> 148 148   F    7
#> 149 149   F   10
#> 150 150   M    7

simulate_data(factors = list(sex = list(levels = c("M", "F"),
                                        props = c(0.5, 0.5)),
                             site = list(levels = 12,
                                         prop.dist = c(mean = 0.1, sd = 0.05))),
              sampsize = 100, eligibility = TRUE, groups = 4)
#>      ID sex site eligible
#> 1     1   F    2     ABCD
#> 2     2   M    5      ACD
#> 3     3   F    2      ABC
#> 4     4   F   10       BD
#> 5     5   F    7      ABD
#> 6     6   F    6      BCD
#> 7     7   M    2       BC
#> 8     8   M    1      ABC
#> 9     9   M    3     ABCD
#> 10   10   M    1       BC
#> 11   11   F   10      BCD
#> 12   12   M    5       CD
#> 13   13   F    7       AB
#> 14   14   M    3       CD
#> 15   15   F    7       CD
#> 16   16   M    8     ABCD
#> 17   17   F    2       AC
#> 18   18   M    7       CD
#> 19   19   F    3      ABD
#> 20   20   F    1     ABCD
#> 21   21   F    1       CD
#> 22   22   M    8       AD
#> 23   23   F    4      ABC
#> 24   24   F    9      ABD
#> 25   25   F    9      ABC
#> 26   26   M    5       BD
#> 27   27   M   12       CD
#> 28   28   M    5       BC
#> 29   29   F    2       BC
#> 30   30   M    5      ACD
#> 31   31   F    8       BC
#> 32   32   F    5     ABCD
#> 33   33   M    2     ABCD
#> 34   34   F    3      ACD
#> 35   35   M    8      ACD
#> 36   36   F    7      ABD
#> 37   37   M   10     ABCD
#> 38   38   F    9       BC
#> 39   39   M    7      ABD
#> 40   40   M   12       AC
#> 41   41   F    2      ABC
#> 42   42   F    2       BD
#> 43   43   M    2       BC
#> 44   44   F    5      ABC
#> 45   45   M    2       BC
#> 46   46   F    9      ABC
#> 47   47   M    4       AB
#> 48   48   F    1       AB
#> 49   49   F    3      ABD
#> 50   50   M    8     ABCD
#> 51   51   M    3       AD
#> 52   52   M    7      BCD
#> 53   53   F    2      ACD
#> 54   54   F    3       AD
#> 55   55   F    9      ABC
#> 56   56   M    5      ABC
#> 57   57   F    8       BD
#> 58   58   M   12       CD
#> 59   59   F   12      ABD
#> 60   60   M    9      ABC
#> 61   61   F    2       CD
#> 62   62   M    5      BCD
#> 63   63   F   10      ABD
#> 64   64   F   12       AC
#> 65   65   M    9       AC
#> 66   66   M    9       BD
#> 67   67   F    5       BD
#> 68   68   F   12      BCD
#> 69   69   M   12       AD
#> 70   70   M    3       BD
#> 71   71   F    5     ABCD
#> 72   72   F   10     ABCD
#> 73   73   M    5       AC
#> 74   74   M    2       BC
#> 75   75   M    2      ACD
#> 76   76   F   10     ABCD
#> 77   77   M    2      ACD
#> 78   78   F    5       BC
#> 79   79   F   10       BC
#> 80   80   F    8      ABD
#> 81   81   F    8       AD
#> 82   82   M    6       BD
#> 83   83   M    2      ABC
#> 84   84   M    2       BD
#> 85   85   M    5      BCD
#> 86   86   F    2       CD
#> 87   87   M    2      ABC
#> 88   88   F    6     ABCD
#> 89   89   M    2      BCD
#> 90   90   M   12      BCD
#> 91   91   F    7       AD
#> 92   92   M    6       BC
#> 93   93   M    3     ABCD
#> 94   94   M    9       AB
#> 95   95   F    4       BC
#> 96   96   F    2      ACD
#> 97   97   F    2      ACD
#> 98   98   F    3       AD
#> 99   99   F    9       BC
#> 100 100   M    8      ACD
```
