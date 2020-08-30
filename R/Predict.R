PredictObservation <- function( tree, observation ){

  # If leaf is reasched return its probability
  if( tree$isLeaf ){

    return( data.frame( t(tree$Probability), tree$Class, stringsAsFactors = F) )

  }

  # Adjustment for nominal attributes
  if( length(grep("\\(", tree$children[[1]]$value)) > 0 ){

    child <- tree$children[[ ifelse( observation[, tree$children[[1]]$feature] %in% ( eval( parse(text = paste0("c", tree$children[[1]]$value)) ) ), 1, 2) ]]

  }else{

    child <- tree$children[[ ifelse( observation[, tree$children[[1]]$feature] > ( eval( parse(text = tree$children[[1]]$value)) ), 2, 1) ]]

  }

  # Recursive call of the function to traverse the tree
  return( PredictObservation( child, observation ) )

}

#' Title
#'
#' @param tree a
#' @param data a
#'
#' @return
#' @export PredictTree
#'
#' @examples
#' 
PredictTree <- function( tree, data ){

  # Check if all required attributes exist in dataset
  required_features <- names( attr(tree,"Required_features") )

  if ( !all( required_features %in% colnames(data)) ) {

    col <- !required_features %in% colnames(data)
    col <- required_features[col]
    stop( sprintf("The following features are required: %s.", paste0(col,collapse = ", ")) )

  }

  # Check if all required levels of a particular nominal attribute exist in dataset
  required_features <- attr(tree,"Required_features")
  required_features <- required_features[!unlist(lapply(required_features,is.null))]

  col <- unlist( sapply( names(required_features), function(i, lev, dat){ all( levels( dat[,i] ) %in% lev[[i]] ) },
                       lev = required_features, dat = data, simplify = F) )
  if ( !all( col ) ) {

    col <- names( required_features )[!col]
    stop( sprintf("The following features have to many levels: %s.", paste0(col,collapse = ", ")) )

  }

  # Create table with final predictions
  n_observations <- nrow(data)
  results <- as.data.frame( matrix( 0, n_observations, length( attr(tree, "Y_levels") ) + 1 ) )
  colnames(results) <- c( attr(tree, "Y_levels"), "Class" )

  # Main loop predicting each observation
  for( i in 1:n_observations ){

    results[i, ] <- PredictObservation( tree, data[i, , drop = F] )

  }

  results[, "Class"] <- factor( results[, "Class"], levels = attr(tree, "Y_levels") )

  return( results )

}
