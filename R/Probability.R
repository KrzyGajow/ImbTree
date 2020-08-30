CalcProb <- function( data, Y_name, weights = NULL, cost = NULL ){
  
  # Create temporary table, if weights and cost are null then then computations are based on counts not sums
  dat <- data.frame( target = data[,Y_name], weights = ifelse( is.null(weights), 1, weights ) )
  
  if( !is.null(cost) ){
    
    # Prepare sums of each row
    costCases <- rowSums( cost )
    
    # Assign a particular cost to the corresponding class
    for( i in names(costCases) ){
      
      dat[ dat[, "target"] == i, "weights"] <- costCases[i]
      
    }
    
  }
  
  # Calculate numerator as a counts/sums of each class
  nom <- tapply( dat[, "weights"], list( dat[, "target"] ), sum )
  nom <- ifelse( is.na(nom), 0, nom )
  
  # Calculate denominator
  denom <- sum(nom)
  
  # Calculate probability of each class
  prob <- nom / denom
  
  # It is possible that class has no observations
  prob <- ifelse( is.finite(prob), prob, 0 )
  
  return( prob )
  
}
