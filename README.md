
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Dense FTRL-Proximal online learning algorithm for logistic regression

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

The goal of `ftrl` is to implement the FTRL online training algorithm in
R. Really not for production and just for fun. Just works for logisitic
regression. Probably some bugs.

Here is the
[paper](https://www.eecs.tufts.edu/%7Edsculley/papers/ad-click-prediction.pdf).

## Installation

You can install the released version of ftrl from Github:

``` r
remotes::install_github("dirkschumacher/ftrl")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(ftrl)
optimizer <- FTRLDenseOptimizer(
  n_weights = 100, # number of features
  # ... plus some learning rate and regulization parameters
  lambda1 = 20
)
```

You can then stream examples to the optimizer. The optimizer itself will
only keep a copy of the current weights and some intermediate results
but nothing proportional to the number of training examples.

``` r
# fit 100k samples
system.time(
  for (i in seq_len(100000)) {
    # 99 useless features, learn if the first element is negative
    x <- rnorm(100)
    optimizer$fit(x, x[1] < 0)
  }
)
#>    user  system elapsed 
#>   5.060   0.757   6.486
```

``` r
optimizer$predict(c(2, rnorm(99)))
#> [1] 1.464633e-11
optimizer$predict(c(1, rnorm(99)))
#> [1] 3.941826e-06
optimizer$predict(c(0.1, rnorm(99)))
#> [1] 0.2369947
optimizer$predict(c(-0.1, rnorm(99)))
#> [1] 0.7867933
optimizer$predict(c(-1, rnorm(99)))
#> [1] 0.9999964
optimizer$predict(c(-2, rnorm(99)))
#> [1] 1
```

Since we use l1 regularitzation, a lot of these useless weights are
exactly zero. Which makes the resulting model sparse and smaller to
store.

``` r
sum(optimizer$weights() == 0) / length(optimizer$weights())
#> [1] 0.72
```

## References

McMahan, H. Brendan, et al. “Ad click prediction: a view from the
trenches.” Proceedings of the 19th ACM SIGKDD international conference
on Knowledge discovery and data mining. ACM, 2013.
