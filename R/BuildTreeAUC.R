BuildTreeAUC <- function( tree, Y_name, X_names, data, depth, level_positive, min_obs, pct_obs, type, qval, cp, n_cores, weights, AUC_weight, cost ){

  # Main loop until there is no possible split
  repeat{

    leaves_results <- TraversTree( tree, Y_name, X_names, data, depth, level_positive, min_obs, pct_obs, type, qval, n_cores, weights, AUC_weight, cost )

    leaves_results <- PossibleSplitTravers( leaves_results, depth, cp )
    # print(leaves_results)
    # Check if algorithm finished
    if( nrow(leaves_results) == 0 ) break

    #Choose best split
    split_rule <- BestSplitTravers( leaves_results )

    #Choose node to split
    temp <- NodetoSplit( tree, split_rule )

    leave_path <- temp$leave_path

    # Prepare indexes of observations for each child, name is different for numerical or nominal attribute
    if_fac <- length( grep("\\(", split_rule[, "split"]) ) > 0
    if( if_fac ){

      split_indexes <- data[ leave_path$indexes, temp$split_var] %in% ( eval( parse( text = paste0("c", split_rule[, "split"]) ) ) )

    }else{

      split_indexes <- data[ leave_path$indexes, temp$split_var] <= ( eval( parse( text = split_rule[,"split"]) ) )

    }

    # Update Global_AUC value
    Global_AUC <<- split_rule[, "value_left"]

    # Prepare node name for each child, name is different for numerical or nominal attribute
    if( if_fac ){

      l_name <- sprintf("%s = %s", temp$split_var, split_rule[, "split"])
      r_name <- sprintf("%s = %s", temp$split_var, split_rule[, "split_rest"])

    }else{

      l_name <- sprintf("%s <= %s", temp$split_var, split_rule[, "split"])
      r_name <- sprintf("%s >  %s", temp$split_var, split_rule[, "split"])

    }
    
    # Garbage collector
    # gc()
    
    #Create left child
    CreateLeaf( leave_path, l_name, temp$split_var, split_rule, split_indexes, data, Y_name, weights, cost )

    # Create right child
    CreateLeaf( leave_path, r_name, temp$split_var, split_rule, !split_indexes, data, Y_name, weights, cost )
    
    # Garbage collector
    # gc()
    
  }

}

TraversTree <- function( tree, Y_name, X_names, data, depth, level_positive, min_obs, pct_obs, type, qval, n_cores, weights, AUC_weight, cost ){

  # Create global object (temporary) with results
  assign( "AUC_Traversed_Tree", NULL, envir = .GlobalEnv )

  # Function traversing the tree and reaching all posibble leaves
  fun <- function( tree, Y_name, X_names, data, depth, level_positive, min_obs, pct_obs, type, qval, n_cores, weights, AUC_weight, cost ){

    # If leaf calculate best split
    if( tree$isLeaf ){

      # Check if split is possible in terms of control parameters
      if( {tree$Count / 2 > min_obs} & {tree$Count / 2 > pct_obs} & !all( tree$Probability %in% c(0,1) ) ){

        # Calculate various statistics of all possible best local splits, choose the best one
        split_rule <- BestSplitGlobal(data = data[tree$indexes,c(X_names, Y_name)], Y_name = Y_name, measure_parent = tree$measure,
                                      level_positive = level_positive, min_obs = min_obs, pct_obs = pct_obs, type = "AUCg", qval = qval,
                                      indexes = tree$indexes, n_cores = n_cores, weights = weights,
                                      AUC_weight = AUC_weight, cost = cost)

        # Add depth of the tree to the results
        split_rule <- cbind( split_rule, depth = tree$depth + 1 )

        # Update object with results
        AUC_Traversed_Tree <<- rbind( AUC_Traversed_Tree,split_rule )

        # Assign path to the node
        rownames(AUC_Traversed_Tree)[length(rownames(AUC_Traversed_Tree))] <<- paste0(tree$pathString,"/",rownames(split_rule))

      }

      return( invisible(NULL) )

    }

    # Garbage collector
    # gc()
    
    # Travers left child of the node
    child_left <- tree$children[[ 1 ]]
    fun( tree = child_left, Y_name = Y_name, X_names = X_names, data = data[,c(X_names, Y_name)], depth = depth, level_positive = level_positive,
         min_obs = min_obs, pct_obs = pct_obs, type = type, qval = qval, n_cores = n_cores, weights = weights,
         AUC_weight = AUC_weight, cost = cost)

    # Travers right child of the node
    child_right <- tree$children[[ 2 ]]
    fun( tree = child_right, Y_name = Y_name, X_names = X_names, data = data[,c(X_names, Y_name)], depth = depth, level_positive = level_positive,
         min_obs = min_obs, pct_obs = pct_obs, type = type, qval = qval, n_cores = n_cores, weights = weights,
         AUC_weight = AUC_weight, cost = cost)
    
    # Garbage collector
    # gc()
    
  }

  # Run above function
  fun( tree, Y_name, X_names, data, depth, level_positive, min_obs, pct_obs, type, qval, n_cores, weights, AUC_weight, cost )

  # Create output
  out <- AUC_Traversed_Tree

  # Remove global object (temporary) with results
  rm("AUC_Traversed_Tree", envir = .GlobalEnv)

  # Garbage collector
  # gc()
  
  return( out )

}

CreateLeaf <- function( leave_path, name, split_var, split_rule, split_indexes, data, Y_name, weights, cost ){

  #Create and update leaf info
  child <- leave_path$AddChild(name)

  # Assign splitting attribute
  child$feature <- split_var

  # Assign splitting value
  child$value <- split_rule[, "split"]

  # Assign measure of this node
  child$measure <- split_rule[, "value_left"]

  # Store observation indexes
  child$indexes <- leave_path$indexes[split_indexes]

  # Assign depth of the tree
  child$depth <- leave_path$depth + 1

  # Calculate number of observations
  child$Count <- length(child$indexes)

  # Assign leaf flag
  child$Leaf <- "*"

  # Probability of the node
  probability <- CalcProb( data[child$indexes,], Y_name, weights, cost )

  # Assign probability to the node
  child$Probability <- probability

  # Update Global Probability matrix
  Probability_matrix[child$indexes, -ncol(Probability_matrix)] <<- matrix(probability, nrow = length(child$indexes), ncol = ncol(Probability_matrix)-1, byrow = T )

}

NodetoSplit <- function( tree, split_rule ){

  # Determine path to the node
  temp <- strsplit( rownames(split_rule)[1], "/" )[[1]]
  split_var <- temp[length(temp)]

  # Adjustment for root
  if( length(temp) == 2 ){

    leave_path <- eval( parse(text = deparse( substitute(tree) ) ) )

  }else{

    leave_path <- eval( parse( text = paste( deparse( substitute(tree) ), paste0( paste0( "'", temp[-c(1, length(temp))] ), "'", collapse = "$" ), sep = "$") ) )

  }

  #Remove leaf flag
  leave_path$Leaf <- ""

  return( list( leave_path = leave_path, split_var = split_var ) )

}

PossibleSplitTravers <- function( leaves_results, depth, cp ){

  # Check which splits are possible in terms of control parameters, if none create dummy table with results
  if( is.null(leaves_results) ){

    leaves_results <- matrix(0, 0, 8)

  }else{

    which_possible <- leaves_results[, "value"] > cp & leaves_results[, "depth"] <= depth
    leaves_results <- leaves_results[ which_possible, , drop = F]

  }

  return( leaves_results )

}

BestSplitTravers <- function( leaves_results ){

  # Choose the best split
  index_best <- which( max(leaves_results[, "value"]) == leaves_results[, "value"] )

  # If there are more than one best split, choose the one producing the best balanced split
  index_best <- index_best[ which.min(leaves_results[index_best, "balance"]) ]
  split_rule <- leaves_results[ index_best, , drop = F]

  return( split_rule )

}
