---
# output:
#   pdf_document: default
#   html_document: default
---

<!-- README.md is generated from README.Rmd. Please edit that file -->

# ImbTree

<!-- badges: start -->

<!-- badges: end -->

Software to build Decision Trees for imballanced data.

## Installation

You can install the released version of ImbTree from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("ImbTree")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(ImbTree)
```

```r

data(iris)

Tree1 <- ImbTree(Y_name = "Species", X_names = colnames(iris)[-ncol(iris)], data = iris, 
                depth = 10, level_positive = "1", min_obs = 10, pct_obs = 0.01, 
                type = "Shannon", qval = 1, cp = 0, n_cores = 1, weights = NULL, 
                AUC_weight = "none", cost = NULL, Class_threshold = "equal")

PrintTree(Tree1)

```