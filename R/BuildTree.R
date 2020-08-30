BuildTree <- function( node, Y_name, X_names, data, depth, level_positive, min_obs, pct_obs, type, qval, cp, n_cores, weights, AUC_weight, cost ){

  # Number of observations in the node
  node$Count <- nrow(data)

  # Probability of the node
  probability <- CalcProb( data, Y_name, weights, cost )

  # Assign probability to the node
  node$Probability <- probability

  # Calculate various statistics of all possible best local splits, choose the best one
  split_rule <- BestSplitGlobal(data = data[, c(X_names, Y_name)], Y_name = Y_name, measure_parent = node$measure, level_positive = level_positive,
                                min_obs = min_obs, pct_obs = pct_obs, type = type, qval = qval, indexes = node$indexes, n_cores = n_cores,
                                weights = weights, AUC_weight = AUC_weight, cost = cost)

  # Check if the improvement is greater than the threshold
  no_improvement <- split_rule[, "value"] <= cp

  # If any is TRUE then node is considered as a leaf
  if( all(probability %in% c(0,1)) | {depth <= 0} | {no_improvement} | {node$Count / 2 <= min_obs} | {node$Count / 2 <= pct_obs} ){

    # Assign leaf flag
    node$Leaf <- "*"

    # Update Global Probability matrix
    Probability_matrix[node$indexes, -ncol(Probability_matrix)] <<- matrix(probability, nrow = length(node$indexes), ncol = ncol(Probability_matrix)-1, byrow = T )
    
    # Garbage collector
    # gc()
    
    return(node)

  }else{

    # Prepare node name and indexes of observations for each child, name is different for numerical or nominal attribute
    if_fac <- length(grep("\\(", split_rule[,"split"])) > 0
    if( if_fac ){

      split_indexes <- data[,rownames(split_rule)] %in% ( eval(parse(text = paste0("c",split_rule[,"split"]))) )
      l_name <- sprintf("%s = %s",rownames(split_rule), split_rule[,"split"])
      r_name <- sprintf("%s = %s",rownames(split_rule), split_rule[,"split_rest"])

    }else{

      split_indexes <- data[,rownames(split_rule)] <= ( eval(parse(text = split_rule[,"split"])) )
      l_name <- sprintf("%s <= %s",rownames(split_rule), split_rule[,"split"])
      r_name <- sprintf("%s >  %s",rownames(split_rule), split_rule[,"split"])

    }

    # Update Global_AUC value
    Global_AUC <<- split_rule[, "value_left"]

    # Split data for each child
    child_frame <- split(data, split_indexes)

    # Create left child
    child <- node$AddChild(l_name)
    child$feature <- rownames(split_rule)
    child$value <- split_rule[,"split"]
    child$measure <- split_rule[,"value_left"]
    child$indexes <- node$indexes[split_indexes]
    child$depth <- node$depth + 1

    # Recursive call of the building function (BuildTree) for the left child
    Recall(node = child, Y_name = Y_name, X_names = X_names, data = child_frame[[2]], depth = depth - 1, level_positive = level_positive,
           min_obs = min_obs, pct_obs = pct_obs, type = type, qval = qval, cp = cp, n_cores = n_cores, weights = weights,
           AUC_weight = AUC_weight, cost = cost)

    # Garbage collector
    # gc()
    
    # Create right child
    child <- node$AddChild(r_name)
    child$feature <- rownames(split_rule)
    child$value <- split_rule[,"split"]
    child$measure <- split_rule[,"value_right"]
    child$indexes <- node$indexes[!split_indexes]
    child$depth <- node$depth + 1

    # Recursive call of the building function (BuildTree) for the right child
    Recall(node = child, Y_name = Y_name, X_names = X_names, data = child_frame[[1]], depth = depth - 1, level_positive = level_positive,
           min_obs = min_obs, pct_obs = pct_obs, type = type, qval = qval, cp = cp, n_cores = n_cores, weights = weights,
           AUC_weight = AUC_weight, cost = cost)
    
    # Garbage collector
    # gc()
    
  }

}
