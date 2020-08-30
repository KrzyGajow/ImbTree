#' Title
#'
#' @param Y_name Name of the target variable
#' @param X_names a
#' @param data a
#' @param depth a
#' @param level_positive a
#' @param min_obs a
#' @param pct_obs a
#' @param type a
#' @param qval a
#' @param cp a
#' @param n_cores a
#' @param weights a
#' @param AUC_weight a
#' @param cost a
#' @param Class_threshold a
#'
#' @return
#' @export ImbTree
#'
#' @examples
#'
#' \dontrun{
#'   ImbTree()
#' }
ImbTree <- function(Y_name, X_names, data, depth = 5, level_positive = "1", min_obs = 5, pct_obs = 0.1, type = "AUCg2", qval = 1, cp = 0, n_cores = 1,
                    weights = NULL, AUC_weight = "none", cost = NULL, Class_threshold = "equal"){

  # Check if all parameters are correctly specified, if no terminate the program
  Stop <- StopIfNot( Y_name, X_names, data, depth, level_positive, min_obs, pct_obs, type, qval, cp, n_cores, weights, AUC_weight, cost, Class_threshold )
  if( !Stop ){

    return( invisible() )

  }

  # Create probability matrix, initially all probabilities are equal
  Y_statistics <- t( as.data.frame.array( prop.table( table(data[, Y_name]) ) ) )
  
  # Determine class labels
  Y_levels <- levels( data[,Y_name] )
  
  # Assign global probability matrix and auc
  AssignProbMatrix( data, Y_name, Y_statistics, Y_levels )

  # Create the root of the Tree
  Tree <- Node$new("Root")

  # Assign various initial measures
  AssignInitMeasures( Tree, data, Y_statistics, qval, type, level_positive, weights, AUC_weight, cost )

  # If needed start cluster for parallel processing
  if( n_cores > 1 ){

    assign( "Global_Cluster", parallel::makeCluster( n_cores ), envir = .GlobalEnv )
    
  }

  # Call of the main Building function
  if( !type == "AUCg2" ){

    # Standard recursive partitioning
    type <- ifelse( type == "AUCg1", "AUCg", type)
    BuildTree(node = Tree, Y_name = Y_name, X_names = X_names, data = data, depth = depth, level_positive = level_positive,
               min_obs = min_obs, pct_obs = ceiling(nrow(data) * pct_obs), type = type, qval = qval, cp = cp, n_cores = n_cores,
               weights = weights, AUC_weight = AUC_weight, cost = cost)

  }else{

    # Repeated recursive partitioning for all existing leaves
    type <- "AUCg"
    BuildTreeAUC(tree = Tree, Y_name = Y_name, X_names = X_names, data = data, depth = depth, level_positive = level_positive,
                          min_obs = min_obs, pct_obs = ceiling(nrow(data) * pct_obs), type = type, qval = qval, cp = cp, n_cores = n_cores,
                          weights = weights, AUC_weight = AUC_weight, cost = cost)

  }

  # Determine class of each observation based on various approaches setting up thresholds
  thresholds <- AssignClass( Class_threshold, cost )

  # Assign final class
  UpdateTree( Tree )

  # Create various info for latter use
  AddAttr( Tree, data, Y_levels, pct_obs, qval, type, cp, weights, AUC_weight, cost, Class_threshold, thresholds )

  # Remove no more required objects
  #rm(list = c("Probability_matrix", "Global_AUC"), envir = .GlobalEnv)

  # If needed stop cluster for parallel processing
  if( n_cores > 1 ){

    parallel::stopCluster( Global_Cluster )

  }

  # Return Final Tree
  return( Tree )

}
