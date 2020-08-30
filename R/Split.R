# Function for numerical attributes
SplitLocalNum <- function( variable, target, measure_parent, indexes, k_unique, type, qval, n_cores, level_positive, weights, AUC_weight, cost ){

  # Number of observation in parent node
  n_obs <- length(variable)

  # Sequential processing
  if( n_cores == 1 ){

    # Prepare table with results
    results <- data.frame( matrix(0, length(k_unique), 8) )
    colnames(results) <- c( "value", "value_left", "value_right", "split", "split_rest", "n_left", "n_right", "balance" )

    # Loop for each possible split in particular node, it might have less values than in all dataset
    for( i in seq_along(k_unique) ){

      # Split variable into two parts, i.e. less or equal to the split point and greater then the split point
      variable_temp <- ifelse(variable <= k_unique[i], 1, 0)

      # Choose learning type
      if( type %in% c("Shannon", "Renyi", "Tsallis", "Sharma-Mittal") ){

        temp <- InformationGain( variable_temp, target, measure_parent, qval, type, weights, cost )

      }else if(type == "AUCl"){

        temp <- Split_Auc_local( variable, target, k_unique[i], measure_parent, level_positive, weights, AUC_weight, cost )

      }else if(type == "AUCg"){

        temp <- SplitAUC( variable, target, k_unique[i], measure_parent, indexes, level_positive, weights, AUC_weight, cost )
      }

      # Update result matrix for each split point
      results[i, "value"] <- temp["gain"]
      results[i, "value_left"] <- temp["left_value"]
      results[i, "value_right"] <- temp["right_value"]
      results[i, "split"] <- k_unique[i]
      results[i, "n_left"] <- sum(variable_temp)
      results[i, "n_right"] <- n_obs - results[i, "n_left"]
      results[i, "balance"] <- abs( 0.5 - results[i, "n_left"] / n_obs )

    }
    
    # Garbage collector
    # gc()
    
  }else{# Parallel processing

    # Define variables that should be exported into each cluster
    clusterExport(Global_Cluster, c("InformationGain", "SplitAucLocal", "SplitAUC", "Entropy", "CalcProb", "level_positive", "weights", "cost",
                                    "variable", "target", "measure_parent", "indexes", "qval", "type", "Probability_matrix", "k_unique", "n_obs",
                                    "Global_AUC", "AUC_weight", "WeightedROC", "WeightedAUC", "ROCFormula", "AUCMulti", "roc", "auc",
                                    "MultiROC", "PairAUC", "MulticlassAUC", "MulticlassROCFormula", "TwoclassAUC"), envir = environment())

    # Loop for each possible split in particular node, it might have less values than in all dataset
    results <- parallel::parSapply(cl = Global_Cluster, seq_along(k_unique), simplify = F, function(i){

      # Prepare table with results
      out <- data.frame( matrix(0, 1, 8) )
      colnames(out) <- c( "value", "value_left", "value_right", "split", "split_rest", "n_left", "n_right", "balance" )

      # Split variable into two parts, i.e. less or equal to the split point and greater then the split point
      variable_temp <- ifelse( variable <= k_unique[i], 1, 0 )

      # Choose learning type
      if( type %in% c("Shannon", "Renyi", "Tsallis", "Sharma-Mittal") ){

        temp <- InformationGain( variable_temp, target, measure_parent, qval, type, weights, cost )

      }else if( type == "AUCl" ){

        temp <- Split_Auc_local( variable, target, k_unique[i], measure_parent, level_positive, weights, AUC_weight, cost )

      }else if( type == "AUCg" ){

        temp <- SplitAUC( variable, target, k_unique[i], measure_parent, indexes, level_positive, weights, AUC_weight, cost )

      }

      # Update result matrix for each split point
      out[, "value"] <- temp["gain"]
      out[, "value_left"] <- temp["left_value"]
      out[, "value_right"] <- temp["right_value"]
      out[, "split"] <- k_unique[i]
      out[, "n_left"] <- sum(variable_temp)
      out[, "n_right"] <- n_obs - out[,"n_left"]
      out[, "balance"] <- abs( 0.5 - out[,"n_left"] / n_obs )

      return( out )

    })

    # Transform results from listo into table
    results <- do.call("rbind", results)

    # Adjustment when there is no observation in a particular child node
    if(is.null(results)){

      # Create dummy / empty table with results
      results <- data.frame( matrix(0, 0, 8) )
      colnames(results) <- c( "value", "value_left", "value_right", "split", "split_rest", "n_left", "n_right", "balance" )

    }

    # Garbage collector
    # gc()
    
  }

  return( results )

}

# Function for nominal attributes
SplitLocalFac <- function( variable, target, measure_parent, indexes, k_unique, type, qval, n_cores, level_positive, weights, AUC_weight, cost ){

  # Number of observation in parent node
  n_obs <- length(variable)

  # Sequential processing
  if( n_cores == 1 ){

    # Prepare table with results
    results <- data.frame( matrix(0, length(k_unique), 8) )
    colnames(results) <- c( "value", "value_left", "value_right", "split", "split_rest", "n_left", "n_right", "balance" )

    # Loop for each possible split in particular node, it might have less values than in all dataset
    for( i in seq_along(k_unique) ){

      # Split variable into two parts, i.e. less or equal to the split point and greater then the split point
      variable_temp <- ifelse( variable %in% k_unique[1:i], 1, 0 )

      # Choose learning type
      if( type %in% c("Shannon", "Renyi", "Tsallis", "Sharma-Mittal") ){

        temp <- InformationGain( variable_temp, target, measure_parent, qval, type, weights, cost )

      }else if(type == "AUCl"){

        temp <- Split_Auc_local( variable, target, k_unique[1:i], measure_parent, level_positive, weights, AUC_weight, cost )

      }else if(type == "AUCg"){

        temp <- SplitAUC( variable, target, k_unique[1:i], measure_parent, indexes, level_positive, weights, AUC_weight, cost )
      }

      # Update result matrix for each split point
      results[i, "value"] <- temp["gain"]
      results[i, "value_left"] <- temp["left_value"]
      results[i, "value_right"] <- temp["right_value"]
      results[i, "split"] <- paste0( "(", paste0( k_unique[1:i], collapse = "," ) ,")" )
      results[i, "n_left"] <- sum(variable_temp)
      results[i, "n_right"] <- n_obs - results[i, "n_left"]
      results[i, "balance"] <- abs( 0.5 - results[i, "n_left"] / n_obs )

    }
    
    # Garbage collector
    # gc()
    
  }else{# Parallel processing

    # Define variables that should be exported into each cluster
    clusterExport(Global_Cluster, c("InformationGain", "SplitAucLocal", "SplitAUC", "Entropy", "CalcProb", "level_positive", "weights", "cost",
                                    "variable", "target", "measure_parent", "indexes", "qval", "type", "Probability_matrix", "k_unique", "n_obs",
                                    "Global_AUC", "AUC_weight", "WeightedROC", "WeightedAUC", "ROCFormula", "AUCMulti", "roc", "auc",
                                    "MultiROC", "PairAUC", "MulticlassAUC", "MulticlassROCFormula", "TwoclassAUC"), envir = environment())

    # Loop for each possible split in particular node, it might have less values than in all dataset
    results <- parallel::parSapply(cl = Global_Cluster, seq_along(k_unique), simplify = F, function(i){

      # Prepare table with results
      out <- data.frame( matrix(0, 1, 8) )
      colnames(out) <- c( "value", "value_left", "value_right", "split", "split_rest", "n_left", "n_right", "balance" )

      # Split variable into two parts, i.e. less or equal to the split point and greater then the split point
      variable_temp <- ifelse( variable %in% k_unique[1:i], 1, 0 )

      # Choose learning type
      if( type %in% c("Shannon", "Renyi", "Tsallis", "Sharma-Mittal") ){

        temp <- InformationGain( variable_temp, target, measure_parent, qval, type, weights, cost )

      }else if( type == "AUCl" ){

        temp <- Split_Auc_local( variable, target, k_unique[1:i], measure_parent, level_positive, weights, AUC_weight, cost )

      }else if( type == "AUCg" ){

        temp <- SplitAUC( variable, target, k_unique[1:i], measure_parent, indexes, level_positive, weights, AUC_weight, cost )

      }

      # Update result matrix for each split point
      out[, "value"] <- temp["gain"]
      out[, "value_left"] <- temp["left_value"]
      out[, "value_right"] <- temp["right_value"]
      out[, "split"] <- paste0( "(", paste0( k_unique[1:i], collapse = "," ) ,")" )
      out[, "n_left"] <- sum(variable_temp)
      out[, "n_right"] <- n_obs - out[,"n_left"]
      out[, "balance"] <- abs( 0.5 - out[,"n_left"] / n_obs )

      return( out )

    })

    # Transform results from listo into table
    results <- do.call("rbind", results)

    # Adjustment when there is no observation in a particular child node
    if( is.null(results) ){

      # Create dummy / empty table with results
      results <- data.frame( matrix(0, 0, 8) )
      colnames(results) <- c( "value", "value_left", "value_right", "split", "split_rest", "n_left", "n_right", "balance" )

    }
    
    # Garbage collector
    # gc()
    
  }

  return( results )

}

PCAorder <- function( target, variable ){

  # If less than 2 levels there is no sense to do it
  if( nlevels(droplevels(variable) ) < 2 ) {

    return( as.character( levels(droplevels(variable) ) ) )

  }

  ## Create contingency table of the nominal outcome with the nominal covariate
  N <- table( droplevels(variable), droplevels(target) )

  ## PCA of weighted covariance matrix of class probabilites
  # Class probability matrix
  P <- N / rowSums(N)

  # Weighted covariance matrix
  S <- cov.wt( P, wt = rowSums(N) )$cov

  # First principal component
  pc1 <- prcomp( S, rank. = 1 )$rotation

  # Score
  score <- P %*% pc1

  ## Return ordered factor levels
  return( as.character( levels( droplevels(variable) )[ order(score) ] ) )

}
