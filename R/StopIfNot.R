StopIfNot <- function( Y_name, X_names, data, depth, level_positive, min_obs, pct_obs, type, qval, cp, n_cores, weights, AUC_weight, cost, Class_threshold ){

  if( !type %in% c( "Shannon", "Renyi", "Tsallis", "Sharma-Mittal", "AUCl", "AUCg1", "AUCg2" ) ){

    cat("\n\n********** PROGRAM TERMINATED **********\n\n")
    cat( sprintf( "Type should be in one of the: %s.", paste0( c( "Shannon", "Renyi", "Tsallis", "Sharma-Mittal", "AUCl", "AUCg1", "AUCg2" ) , collapse = ", ")) )
    cat("\n\n********** PROGRAM TERMINATED **********\n\n")
    return( F )

  }

  if( !Y_name %in% colnames(data) ){

    cat("\n\n********** PROGRAM TERMINATED **********\n\n")
    cat( sprintf( "Traget variable %s does not exist in the table", Y_name ) )
    cat("\n\n********** PROGRAM TERMINATED **********\n\n")
    return( F )

  }

  if( depth < 1 ){

    cat("\n\n********** PROGRAM TERMINATED **********\n\n")
    cat( sprintf( "Parameter depth should be equal or greater then 1" ) )
    cat("\n\n********** PROGRAM TERMINATED **********\n\n")
    return( F )

  }

  if( min_obs <= 0 & pct_obs <= 0 ){

    cat("\n\n********** PROGRAM TERMINATED **********\n\n")
    cat( sprintf( "Parameters min_obs, pct_obs should be positive" ) )
    cat("\n\n********** PROGRAM TERMINATED **********\n\n")
    return( F )

  }

  if( cp < 0 ){

    cat("\n\n********** PROGRAM TERMINATED **********\n\n")
    cat( sprintf( "Parameter cp should not be negative" ) )
    cat("\n\n********** PROGRAM TERMINATED **********\n\n")
    return( F )

  }

  if( !is.factor( data[, Y_name] ) ){

    cat("\n\n********** PROGRAM TERMINATED **********\n\n")
    cat( sprintf( "Traget variable %s should be a factor.", Y_name ) )
    cat("\n\n********** PROGRAM TERMINATED **********\n\n")
    return( F )

  }

  if( !Class_threshold %in% c("equal","theoretical","tuned")  ){

    cat("\n\n********** PROGRAM TERMINATED **********\n\n")
    cat( sprintf( "Class_threshold should be in one of the: equal, theoretical, tuned" ) )
    cat("\n\n********** PROGRAM TERMINATED **********\n\n")
    return( F )

  }

  if( !is.null(weights) & !is.null(cost) ){

    cat("\n\n********** PROGRAM TERMINATED **********\n\n")
    cat( sprintf( "Choose only one of the: weights, cost." ) )
    cat("\n\n********** PROGRAM TERMINATED **********\n\n")
    return( F )

  }

  if( is.null(cost) & !Class_threshold %in% c("equal", "tuned" ) ){

    cat("\n\n********** PROGRAM TERMINATED **********\n\n")
    cat( sprintf( "Set up both parameters: Class_threshold in (equal, tuned), cost." ) )
    cat("\n\n********** PROGRAM TERMINATED **********\n\n")
    return( F )

  }

  if( !is.null(cost) ){

    if( is.null( rownames(cost) ) | is.null( colnames(cost) ) ){

      cat("\n\n********** PROGRAM TERMINATED **********\n\n")
      cat( sprintf( "Rows and columns names of the cost matrix should not be NULL" ) )
      cat("\n\n********** PROGRAM TERMINATED **********\n\n")
      return( F )

    }

    if( !all( rownames(cost) %in% levels(data[,Y_name]) ) & !all( rownames(cost) %in% colnames(cost) ) ){

      cat("\n\n********** PROGRAM TERMINATED **********\n\n")
      cat( sprintf( "Rows and columns names of the cost matrix should be in %s.", paste0( levels(data[,Y_name]), collapse = ", " ) ) )
      cat("\n\n********** PROGRAM TERMINATED **********\n\n")
      return( F )

    }

    if( any( cost < 0 ) ){

      cat("\n\n********** PROGRAM TERMINATED **********\n\n")
      cat( "Costs should not be less than 0" )
      cat("\n\n********** PROGRAM TERMINATED **********\n\n")
      return( F )

    }

    if( any( diag(cost) != 0) ){

      cat("\n\n********** PROGRAM TERMINATED **********\n\n")
      cat( sprintf( "Diagonal elements of the cost matrix should be 0" ) )
      cat("\n\n********** PROGRAM TERMINATED **********\n\n")
      return( F )

    }

  }

  if( !AUC_weight %in% c("none","bySize","byCost") ){

    cat("\n\n********** PROGRAM TERMINATED **********\n\n")
    cat( sprintf( "AUC_weight should be in one of the: bySize, byCost, none" ) )
    cat("\n\n********** PROGRAM TERMINATED **********\n\n")
    return( F )

  }

  if( !is.null(weights) & AUC_weight == "byCost" ){

    cat("\n\n********** PROGRAM TERMINATED **********\n\n")
    cat( sprintf( "Choose only one of the: weights, AUC_weight = byCost" ) )
    cat("\n\n********** PROGRAM TERMINATED **********\n\n")
    return( F )

  }

  if( ( AUC_weight == "byCost" & is.null(cost) == T & type %in% c("AUCl", "AUCg1", "AUCg2") ) ){

    cat("\n\n********** PROGRAM TERMINATED **********\n\n")
    cat( sprintf( "Set up both parameters: AUC_weight == byCost, cost." ) )
    cat("\n\n********** PROGRAM TERMINATED **********\n\n")
    return( F )

  }

  if( !( level_positive %in% levels(data[,Y_name]) ) & length( levels( data[,Y_name] ) ) == 2 ){

    cat("\n\n********** PROGRAM TERMINATED **********\n\n")
    cat( sprintf( "Level_positive for binary classification should be in %s levels.", Y_name ) )
    cat("\n\n********** PROGRAM TERMINATED **********\n\n")
    return( F )

  }

  if( !is.null(weights) ){

    if( any( weights < 1 ) ){

      cat("\n\n********** PROGRAM TERMINATED **********\n\n")
      cat( sprintf( "Weights should not be less than 1" ) )
      cat("\n\n********** PROGRAM TERMINATED **********\n\n")
      return( F )

    }

  }

  isNA <- sapply( data, anyNA )
  if( any( isNA ) ){

    cat("\n\n********** PROGRAM TERMINATED **********\n\n")
    cat( sprintf( "The following attributes have missing values: %s.", paste0( names(isNA)[isNA], collapse = ", ") ) )
    cat("\n\n********** PROGRAM TERMINATED **********\n\n")
    return( F )

  }

  return( T )

}
