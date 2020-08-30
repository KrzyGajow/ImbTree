SplitAUC <- function( variable, target, split_point, measure_parent, indexes, level_positive, weights, AUC_weight, cost ){

  # Prepare temporary table with required variables
  matrix_all <- data.frame( variable, target, indexes )

  # Adjustment for nominal attributes, determine indexes of each child
  if( is.character(split_point) ){

    inndx <- matrix_all$variable %in% split_point

  }else{

    inndx <- matrix_all$variable <= split_point
  }

  # Prepare table for each child
  matrix_left <- matrix_all[ inndx,]
  matrix_right <- matrix_all[ !inndx ,]

  # Calculate counts of each child
  n_obs <- nrow(matrix_all)
  l_obs <- nrow(matrix_left)
  r_obs <- nrow(matrix_right)

  # Determine indexes of each child
  ind_left <- matrix_all[ inndx, "indexes"]
  ind_right <- matrix_all[ !inndx, "indexes"]

  # Create temporary copy of the global probability matrix
  Probability_matrix_temp <- Probability_matrix

  # Calculate probability of left child and insert them into temporary probability matrix
  probability <- CalcProb(matrix_left, "target", weights, cost )
  Probability_matrix_temp[ ind_left, -ncol(Probability_matrix_temp)] <- matrix( probability, nrow = length(ind_left), ncol = ncol(Probability_matrix_temp)-1, byrow = T)

  # Calculate probability of rifht child and insert them into temporary probability matrix
  probability <- CalcProb(matrix_right, "target", weights, cost )
  Probability_matrix_temp[ ind_right, -ncol(Probability_matrix_temp)] <- matrix( probability, nrow = length(ind_right), ncol = ncol(Probability_matrix_temp)-1, byrow = T)

  # Calculate AUC of this split,
  if( attr(Probability_matrix, "k") == 2 ){

    # Binary classification
    auc_measure <- TwoclassAUC( Probability_matrix_temp[, "target"], Probability_matrix_temp[, level_positive], weights = weights, cost = cost )

  }else{

    #Multiclass classification
    auc_measure <- MulticlassAUC( Probability_matrix_temp[, "target"], Probability_matrix_temp[, -ncol(Probability_matrix_temp)],
                                  weights = weights, AUC_weight = AUC_weight, cost = cost )

  }

  # Current AUC of the entire tree
  measure_parent <- Global_AUC
  
  # Garbage collector
  # gc()
  
  return( c(gain = auc_measure - measure_parent, left_value = auc_measure, right_value = auc_measure) )

}
