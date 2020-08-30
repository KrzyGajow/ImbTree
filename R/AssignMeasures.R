AssignInitMeasures <- function( tree, data, Y_statistics, qval, type, level_positive, weights, AUC_weight, cost ){
  
  # Assign initial inpiurity measure
  if( type %in% c("AUCl", "AUCg1", "AUCg2") ){
    
    if( attr(Probability_matrix, "k") == 2 ){
      
      # Binary classification
      tree$measure <- TwoclassAUC( Probability_matrix[, "target"], Probability_matrix[, level_positive], weights = weights, cost = cost )
      
    }else{
      
      #Multiclass classification
      tree$measure <- MulticlassAUC( Probability_matrix[, "target"], Probability_matrix[, -ncol(Probability_matrix)],
                                     weights = weights, AUC_weight = AUC_weight, cost = cost )
      
    }
    
    assign( "Global_AUC", tree$measure, envir = .GlobalEnv )
    
  }else{
    
    tree$measure <- Entropy( Y_statistics, qval, type )
    
  }
  
  # Observation indexes
  tree$indexes <- 1:nrow(data)
  
  # Depth of the Tree
  tree$depth <- 0
  
  # Number of observations
  tree$Count <- nrow(data)
  
  # Probabilities
  tree$Probability <- Probability_matrix[1, -ncol(Probability_matrix)]
  
  # gc()
  
}

AssignProbMatrix <- function( data, Y_name, Y_statistics, Y_levels ){
  
  # Duplicate probability vector nrow times
  Probability_matrix <- Y_statistics
  Probability_matrix <- data.frame( Probability_matrix, target = data[,Y_name], row.names = NULL )
  
  # Assigne new names
  colnames(Probability_matrix) <- c(Y_levels, "target")
  
  # Create attribute with number of classes
  attr( Probability_matrix, "k" ) <- length(Y_levels)
  
  # Assigne new names
  colnames(Probability_matrix) <- c(Y_levels, "target")
  
  # Create attribute with number of classes
  attr( Probability_matrix, "k" ) <- length(Y_levels)
  
  # Create probability matrix in Global environment
  assign( "Probability_matrix", Probability_matrix, envir = .GlobalEnv )
  
  # Remove local probability matrix
  rm(Probability_matrix)
  
  # Garbage collector
  # gc()
  
}