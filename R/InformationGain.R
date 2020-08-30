InformationGain <- function( variable, target, entropy_parent, qval = 1, type = "Shannon", weights = NULL, cost = NULL ){

  # Create temporary table, if weights and cost are null then then computations are based on counts not sums
  dat <- data.frame(variable, target, weights = ifelse( is.null(weights), 1, weights ) )

  if( !is.null(cost) ){

    # Prepare sums of each row
    costCases <- rowSums( cost )

    # Assign a particular cost to the corresponding class
    for( i in names(costCases) ){

      dat[ dat[, "target"] == i, "weights"] <- costCases[i]

    }

  }

  # Calculate counts/sums of each node (only right or left nodes) and class
  temp <- tapply(dat[, "weights"], list( dat[, "variable"], dat[, "target"] ), sum )
  temp <- ifelse( is.na(temp), 0, temp )

  # Calculate proportions required for the weighted entropy
  split_prop <- rowSums(temp)

  # Calculate entropy of each node
  entropy <-  apply( temp, MARGIN = 1, FUN = function(x, qval, type){ Entropy(x, qval, type) }, qval = qval, type = type )

  # Entropy of each node
  left <- unname( entropy["1"] )
  right <- unname( entropy["0"] )

  # Calculate weighted entropy
  entropy_after <- sum(split_prop / sum(split_prop) * entropy)

  information_gain <- entropy_parent - entropy_after
  
  # Garbage collector
  # gc()
  
  return( c(gain = information_gain, left_value = left, right_value = right) )

}

Entropy <- function( target, qval = 1, type = "Shannon" ){

  if( qval[1] == 1 | type == "Shannon" ){

    # Calculate Shannon entropy
    res <- target / sum(target) * log2(target / sum(target))

    # If there is NA or NaN replace with 0
    res[target == 0] <- 0
    res <- -sum(res)

  }else if( type == "Renyi" ){

    # Calculate Renyi entropy
    res <- ( 1 / (1-qval) ) * log2( sum( ( target/sum(target) ) ^ qval) )

  }else if( type == "Tsallis" ){

    # Calculate Tsallis entropy
    res <- ( 1 / ( qval-1 ) ) * (1 - sum( ( target/sum(target) ) ^ qval ) )

  }else if( type == "Sharma-Mittal" ){

    rval <- ifelse( length(qval) == 1, 1, qval[2] )
    qval <- qval[1]

    # Calculate Sharma-Mittal entropy
    res <- ( 1 / (1-rval) ) * ( sum( ( target/sum(target) ) ^ qval ) ^ ( ( 1 - rval ) / ( 1 - qval ) ) )

  }

  return( res )

}
