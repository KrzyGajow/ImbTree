
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


```r

library(ImbTree)
library("caret")

data(iris)

# Original dataset, multiclass classification, only numeric attributes
iris

# Predicting 1 as 0 will be penalized 5 times
class_cost_bin <- matrix( c(0,5,1,0), 2, 2, dimnames = list( 0:1, 0:1 ) )

# Predicting Setosa is very easy, Versicolor will have cost 5 for Virginica, Virginica will have cost 10 for Versicolor
class_cost_mult <- matrix( c(0,1,1,1,0,10,1,5,0), 3, 3, dimnames = list( levels(iris$Species), levels(iris$Species) ) )

# Assigning higher weights to those observation which are hard to correctly predict
obs_weights <- c( rep(1, 50), c( rep(1, 20), 5, rep(1, 6), 5, rep(1, 22) ), c( rep(1, 19), 10, rep(1, 9), 10, rep(1, 3), 10, 10, rep(1 ,15) ) )

# Dataset for binary classification, only numeric attributes
iris_2 <- iris
iris_2$Species <- factor( rep(0:1, each = 75) )

# Dataset for binary classification, with one unordered factor attribute
iris_3 <- iris_2
iris_3$Petal.Length <- factor( iris_3$Petal.Length )

# Dataset for binary classification, with one ordered factor attribute
iris_4 <- iris_2
iris_4$Petal.Length <- factor( iris_4$Petal.Length, ordered = T )


# Simulation 1: Default settings
Tree1 <- ImbTree(Y_name = "Species", X_names = colnames(iris)[-ncol(iris)], data = iris, 
                 depth = 5, level_positive = "1", min_obs = 5, pct_obs = 0.1, 
                 type = "Shannon", qval = 1, cp = 0, n_cores = 1, weights = NULL, 
                 AUC_weight = "none", cost = NULL, Class_threshold = "equal")

PrintTree(Tree1)
Tree1_pred <- PredictTree(Tree1, iris)
confusionMatrix( Tree1_pred$Class, iris$Species )
MulticlassAUC( iris$Species, Tree1_pred[,1:3] )


# Simulation 2: Original dataset, adding cost matrix
Tree2 <- ImbTree(Y_name = "Species", X_names = colnames(iris)[-ncol(iris)], data = iris, 
                 depth = 5, level_positive = "1", min_obs = 5, pct_obs = 0.1, 
                 type = "Shannon", qval = 1, cp = 0, n_cores = 1, weights = NULL, 
                 AUC_weight = "none", cost = class_cost_mult, Class_threshold = "equal")

PrintTree(Tree2)
Tree2_pred <- PredictTree(Tree2, iris)
confusionMatrix( Tree2_pred$Class, iris$Species )
MulticlassAUC( iris$Species, Tree2_pred[,1:3] )

# Simulation 3: Original dataset, adding observation weights
Tree3 <- ImbTree(Y_name = "Species", X_names = colnames(iris)[-ncol(iris)], data = iris, 
                 depth = 5, level_positive = "1", min_obs = 5, pct_obs = 0.1, 
                 type = "Shannon", qval = 1, cp = 0, n_cores = 1, weights = obs_weights, 
                 AUC_weight = "none", cost = NULL, Class_threshold = "equal")

PrintTree(Tree3)
Tree3_pred <- PredictTree(Tree3, iris)
confusionMatrix( Tree3_pred$Class, iris$Species )
MulticlassAUC( iris$Species, Tree3_pred[,1:3] )

# Simulation 4: Original dataset, tunned thresholds
Tree4 <- ImbTree(Y_name = "Species", X_names = colnames(iris)[-ncol(iris)], data = iris, 
                 depth = 5, level_positive = "1", min_obs = 5, pct_obs = 0.1, 
                 type = "Shannon", qval = 1, cp = 0, n_cores = 1, weights = NULL, 
                 AUC_weight = "none", cost = NULL, Class_threshold = "tuned")

PrintTree(Tree4)
Tree4_pred <- PredictTree(Tree4, iris)
confusionMatrix( Tree4_pred$Class, iris$Species )
MulticlassAUC( iris$Species, Tree4_pred[,1:3] )

# Simulation 5: Original dataset, Renyi entropy with q = 2
Tree5 <- ImbTree(Y_name = "Species", X_names = colnames(iris)[-ncol(iris)], data = iris, 
                 depth = 5, level_positive = "1", min_obs = 5, pct_obs = 0.1, 
                 type = "Renyi", qval = 2, cp = 0, n_cores = 1, weights = NULL, 
                 AUC_weight = "none", cost = NULL, Class_threshold = "equal")

PrintTree(Tree5)# compare to Tree1
Tree5_pred <- PredictTree(Tree5, iris)
confusionMatrix( Tree5_pred$Class, iris$Species )
MulticlassAUC( iris$Species, Tree5_pred[,1:3] )

# Simulation 6: Original dataset, two parameter Sharma-Mittal entropy with q = 2, r = 1.5
Tree6 <- ImbTree(Y_name = "Species", X_names = colnames(iris)[-ncol(iris)], data = iris, 
                 depth = 5, level_positive = "1", min_obs = 5, pct_obs = 0.1, 
                 type = "Sharma-Mittal", qval = c(2, 1.5), cp = 0, n_cores = 1, weights = NULL, 
                 AUC_weight = "none", cost = NULL, Class_threshold = "equal")

PrintTree(Tree6)
Tree6_pred <- PredictTree(Tree6, iris)
confusionMatrix( Tree6_pred$Class, iris$Species )
MulticlassAUC( iris$Species, Tree6_pred[,1:3] )

# Simulation 7: Original dataset, semi-global AUC
Tree7 <- ImbTree(Y_name = "Species", X_names = colnames(iris)[-ncol(iris)], data = iris, 
                 depth = 5, level_positive = "1", min_obs = 5, pct_obs = 0.1, 
                 type = "AUCg1", qval = 1, cp = 0, n_cores = 1, weights = NULL, 
                 AUC_weight = "none", cost = NULL, Class_threshold = "equal")

PrintTree(Tree7)
Tree7_pred <- PredictTree(Tree7, iris)
confusionMatrix( Tree7_pred$Class, iris$Species )
MulticlassAUC( iris$Species, Tree7_pred[,1:3] )

# Simulation 8: Original dataset, global AUC and weighted by cost matrix, adding cost matrix, tuned thresholds
Tree8 <- ImbTree(Y_name = "Species", X_names = colnames(iris)[-ncol(iris)], data = iris, 
                 depth = 5, level_positive = "1", min_obs = 5, pct_obs = 0.1, 
                 type = "AUCg2", qval = 1, cp = 0, n_cores = 1, weights = NULL, 
                 AUC_weight = "byCost", cost = class_cost_mult, Class_threshold = "tuned")

PrintTree(Tree8)
Tree8_pred <- PredictTree(Tree8, iris)
confusionMatrix( Tree8_pred$Class, iris$Species )
MulticlassAUC( iris$Species, Tree8_pred[,1:3] )

# Simulation 9: Binary classification, only numeric attributes, parallel processing
Tree9 <- ImbTree(Y_name = "Species", X_names = colnames(iris_2)[-ncol(iris_2)], data = iris_2, 
                 depth = 5, level_positive = "1", min_obs = 5, pct_obs = 0.1, 
                 type = "Tsallis", qval = 0.5, cp = 0, n_cores = 10, weights = NULL, 
                 AUC_weight = "none", cost = NULL, Class_threshold = "equal")

PrintTree(Tree9)
Tree9_pred <- PredictTree(Tree9, iris_2)
confusionMatrix( Tree9_pred$Class, iris_2$Species, positive = "1" )
TwoclassAUC( iris_2$Species, Tree9_pred[,2] )

# Simulation 10: Binary classification, semi-global AUC, with one unordered factor attribute, theoretical thresholds, adding cost matrix
Tree10 <- ImbTree(Y_name = "Species", X_names = colnames(iris_3)[-ncol(iris_3)], data = iris_3, 
                 depth = 5, level_positive = "1", min_obs = 5, pct_obs = 0.1, 
                 type = "AUCg1", qval = 1, cp = 0, n_cores = 1, weights = NULL, 
                 AUC_weight = "none", cost = class_cost_bin, Class_threshold = "theoretical")

PrintTree(Tree10)
Tree10_pred <- PredictTree(Tree10, iris_3)
confusionMatrix( Tree10_pred$Class, iris_3$Species, positive = "1" )
TwoclassAUC( iris_3$Species, Tree10_pred[,2] )

# Simulation 11: Binary classification, semi-global AUC, with one ordered factor attribute, tuned thresholds, adding observations weights
Tree11 <- ImbTree(Y_name = "Species", X_names = colnames(iris_4)[-ncol(iris_4)], data = iris_4, 
                  depth = 5, level_positive = "1", min_obs = 5, pct_obs = 0.1, 
                  type = "AUCg2", qval = 1, cp = 0, n_cores = 1, weights = obs_weights, 
                  AUC_weight = "none", cost = NULL, Class_threshold = "tuned")

PrintTree(Tree11)
Tree11_pred <- PredictTree(Tree11, iris_4)
confusionMatrix( Tree11_pred$Class, iris_4$Species, positive = "1" )
TwoclassAUC( iris_4$Species, Tree11_pred[,2] )

```