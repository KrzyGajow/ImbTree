AddAttr <- function( tree, data, Y_levels, pct_obs, qval, type, cp, weights, AUC_weight, cost, Class_threshold, thresholds ) {

  # Assing Global AUC
  attr(tree, "AUC") <- Global_AUC

  # Create list of required attributes and their levels (only for factors)
  req_feat <- unique( tree$Get("feature") )[-1]
  required_features <- vector( "list", length(req_feat) )
  names( required_features ) <- req_feat
  req_feat_lev <- lapply( data, levels )
  required_features <- sapply( req_feat, function(i, dat, lev){ dat[[i]] <- lev[[i]] }, dat = required_features, lev = req_feat_lev, simplify = F )

  attr(tree, "Required_features") <- required_features

  # Levels of the target variable
  attr(tree, "Y_levels") <- Y_levels

  # Which learning algorithm and measure was used: "Shannon","Renyi","Tsallis","AUCl","AUCg1","AUCg2"
  attr(tree, "Learning_type") <- type

  # Q value for Renyi or Tsallis entropies
  attr(tree, "Qval") <- qval

  # Complexity parameter i.e. how much a particular measure of the parent should be decreased to perform a split
  attr(tree, "Cp") <- cp

  # Thresholds for class determining
  attr(tree, "Thresholds") <- thresholds

  # Type of thresholds for class determining
  attr(tree, "Thresholds_type") <- Class_threshold

  # Type od AUC weightning
  attr(tree, "AUC_weight_type") <- AUC_weight
  
  # Minimal number (%) of observation in each leaf 
  attr(tree, "Min_obs_percentage") <- pct_obs
  
  # Weights of each observation
  attr(tree, "Weights_observation") <- weights
  
  # Cost classification matrix
  attr(tree, "Cost_matrix") <- cost
  
}
