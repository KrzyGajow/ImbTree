ROCFormula <- function(formula, data, data.missing, call, ...){

  # Get predictors (easy)
  if (data.missing) {

    predictors <- attr(terms(formula), "term.labels")

  }else {

    predictors <- attr(terms(formula, data = data), "term.labels")

  }

  indx <- match(c("formula", "data", "weights", "subset", "na.action"), names(call), nomatch=0)
  if (indx[1] == 0) {
    stop("A formula argument is required")
  }

  # Keep the standard arguments and run them in model.frame
  temp <- call[c(1,indx)]
  temp[[1]] <- as.name("model.frame")

  # Only na.pass and na.fail should be used

  if (indx[5] != 0) {
    na.action.value = as.character(call[indx[5]])
    if (! na.action.value %in% c("na.pass", "na.fail")) {
      warning(paste0(sprintf("Value %s of na.action is not supported ", na.action.value), "and will break pairing in roc.test and are.paired. ",
                     "Please use 'na.rm = TRUE' instead."))
    }
  } else {
    temp$na.action = "na.pass"
  }

  # Adjust call with data from caller
  if (data.missing) {

    temp$data <- NULL

  }

  # Run model.frame in the parent
  m <- eval.parent(temp, n = 2)

  if (!is.null(model.weights(m))) stop("weights are not supported")

  return( list( response.name = names(m)[1], response = model.response(m), predictor.names = predictors, predictors = m[predictors]) )

}

AUCMulti <- function(mv.multiclass.roc, weights = F, cost = F, AUC_weight = F, ...){

  if( is.null(weights) & is.null(cost) ){

    aucs <- lapply( mv.multiclass.roc$rocs, function(x) list(auc(x[[1]], ...), auc(x[[2]], ...)) )
    numCases <- unlist( sapply( aucs, function(x){ length(attr(x[[1]], "roc")$predictor) }) )
    A.ij.total <- sapply( aucs, function(x){ mean( unlist( x ) ) }  )


  }else{

    aucs <- mv.multiclass.roc$rocs
    numCases <- sapply( aucs, function(x){ mean( unlist( sapply( x, function(y){ y$nobs } ) ) ) }  )
    A.ij.total <- sapply( aucs, function(x){ mean( unlist( sapply( x, function(y){ y$auc } ) ) ) }  )

  }

  k <- length( mv.multiclass.roc$levels )

  if( AUC_weight %in% c("bySize", "byCost") ){

    auc <- sum(A.ij.total * numCases) / sum(numCases)

  }else{

    auc <- 2 / (k * (k-1)) * sum(A.ij.total)

  }

  # Prepare auc object
  auc <- as.vector(auc) # remove potential pre-existing attributes
  attr(auc, "percent") <- mv.multiclass.roc$percent
  attr(auc, "roc") <- mv.multiclass.roc

  # Get partial auc details from first computed auc
  attr(auc, "partial.auc") <- attr(aucs[[1]][[1]], "partial.auc")

  if ( !identical(attr(aucs[[1]], "partial.auc"), FALSE) ) {

    attr(auc, "partial.auc.focus") <- attr(aucs[[1]][[1]], "partial.auc.focus")
    attr(auc, "partial.auc.correct") <- attr(aucs[[1]][[1]], "partial.auc.correct")

  }

  class(auc) <- c("mv.multiclass.auc", "numeric")

  return(auc)

}

MultiROC <- function(response, predictor, levels, percent, direction, weights = NULL, AUC_weight = F, cost = NULL, ...){

  if (!methods::is(predictor, "matrix") && !methods::is(predictor, "data.frame")) {
    stop("Please provide a matrix or data frame via 'predictor'.")
  }
  if (nrow(predictor) != length(response)) {
    stop("Number of rows in 'predictor' does not agree with 'response'");
  }
  if (direction == "auto") {
    stop("'direction=\"auto\"' not available for multivariate multiclass.roc")
  }

  if (is.factor(response) && any(names(table(response))[table(response) == 0] %in% levels)) {

    missing.levels <- names(table(response))[table(response) == 0]
    missing.levels.requested <- missing.levels[missing.levels %in% levels]
    warning(paste("No observation for response level(s):", paste(missing.levels.requested, collapse=", ")))
    levels <- levels[!(levels %in% missing.levels.requested)]

  }

  # check whether the columns of the prediction matrix agree with the factors in 'response'
  m <- match(colnames(predictor), levels)
  missing.classes <- levels[setdiff(seq_along(levels), m)]
  levels <- colnames(predictor)[!is.na(m)]

  if (length(levels) == 1) {
    stop("For a single decision value, please provide 'predictor' as a vector.")
  } else if (length(levels) == 0) {
    stop("The column names of 'predictor' could not be matched to the levels of 'response'.")
  }
  if (length(missing.classes) != 0) {
    out.classes <- paste0(missing.classes, collapse = ",")
    if (length(missing.classes) == length(levels)) {
      # no decision values found
      stop(paste0("Could not find any decision values in 'predictor' matching the 'response' levels.",
                  " Could not find the following classes: ", out.classes, ". Check your column names!"))
    } else {
      # some decision values not found
      warning("You did not provide decision values for the following classes: ", out.classes, ".")
    }
  }

  additional.classes <- colnames(predictor)[which(is.na(m))]
  if (length(additional.classes) != 0) {
    out.classes <- paste0(additional.classes, collapse = ",")
    warning("The following classes were not found in 'response': ", out.classes, ".")
  }
  multiclass.roc <- list( response = response, predictor = predictor, percent = percent )
  class(multiclass.roc) <- "mv.multiclass.roc"
  multiclass.roc$levels <- levels

  rocs <- utils::combn(levels, 2, function(x, predictor, response, levels, percent, direction, weights = NULL, cost = NULL, ...) {
    A1 <- PairAUC(predictor, x[1], x[2], response, levels, percent, direction, weights, cost, ...)
    A2 <- PairAUC(predictor, x[2], x[1], response, levels, percent, direction, weights, cost, ...)
    # merging A1 and A2 is infeasible as auc() would not be well-defined
    A <- list(A1, A2)
    return(A)
  }, simplify = FALSE, predictor = predictor, response = response, levels = levels, percent = percent, direction, weights = weights, cost = cost, ...)

  pairs <- unlist(lapply(utils::combn(levels, 2, simplify = FALSE), function(x) paste(x, collapse = "/")))

  names(rocs) <- pairs
  multiclass.roc$rocs <- rocs

  multiclass.roc$auc <- AUCMulti(multiclass.roc, weights, cost, AUC_weight, ...)

  return(multiclass.roc)

}

PairAUC <- function(pred.matrix, i, j, ref.outcome, levels, percent, direction, weights = NULL, cost = NULL, ... ){

  pred.i <- pred.matrix[which(ref.outcome == i), i] # p(G = i) assigned to class i observations
  pred.j <- pred.matrix[which(ref.outcome == j), i] # p(G = i) assigned to class j observations

  classes <- factor(c(rep(i, length(pred.i)), rep(j, length(pred.j))))

  # override levels argument by new levels
  levels <- unique(classes)
  predictor <- c(pred.i, pred.j)

  if( is.null(weights) & is.null(cost) ){

    auc <- roc(classes, predictor, levels = levels, percent = percent, auc = FALSE, ci = FALSE, direction = direction, ...)

  }else{

    if( is.null(weights) ){

      cost.i <- rep( cost[i,j], length(pred.i) )
      cost.j <- rep( cost[j,i], length(pred.j) )
      cost.ij <- c(cost.i, cost.j)

      auc <- WeightedAUC( WeightedROC(predictor, classes, cost.ij ) )
      if( auc < 0.4 ){ auc <- 1 - auc }
      
      auc <- list( auc = auc, nobs = sum(cost.i) + sum(cost.j) )

    }else{

      weights.i <- weights[which(ref.outcome == i)]
      weights.j <- weights[which(ref.outcome == j)]
      weights.ij <- c(weights.i, weights.j)
      
      auc <- WeightedAUC( WeightedROC(predictor, classes, weights.ij ) )
      if( auc < 0.4 ){ auc <- 1 - auc }
      
      auc <- list( auc = auc, nobs = sum(weights.i) + sum(weights.j) )
      
    }

  }

  return(auc)

}


#' Title
#'
#' @param response a
#' @param predictor a
#' @param levels a
#' @param percent a
#' @param direction a
#' @param weights a
#' @param AUC_weight a
#' @param cost a
#' @param ... a
#'
#' @return
#' @export MulticlassAUC
#'
#' @examples
#' 
MulticlassAUC <- function(response, predictor, levels = base::levels(as.factor(response)), percent = FALSE, direction = c("auto", "<", ">"),
                           weights = NULL, AUC_weight = F, cost = NULL, ...){

  if (methods::is(predictor, "matrix") || methods::is(predictor, "data.frame")) {
    if (missing("direction")) {

      direction <- ">"

    }
    else {

      direction <- match.arg(direction)

    }

    mc.roc <- MultiROC(response, predictor, levels, percent, direction, weights, AUC_weight, cost, ...)

  } else {

    direction <- match.arg(direction)
    mc.roc <- MultiROC(response, predictor, levels, percent, direction, weights, AUC_weight, cost, ...)

  }

  mc.roc$call <- match.call()
  return( as.numeric( mc.roc$auc ) )

}

MulticlassROCFormula <- function(formula, data, ...){
  data.missing <- missing(data)
  call <- match.call()
  roc.data <- ROCFormula(formula, data, ..., data.missing = data.missing, call = call)
  response <- roc.data$response
  predictors <- roc.data$predictors
  if (ncol(predictors) == 1) {
    predictors <- predictors[, 1]
  }
  multiclass.roc <- MulticlassROCDefault(response, predictors, ...)
  multiclass.roc$call <- call
  if (! data.missing) {
    multiclass.roc$data <- data
  }
  return(multiclass.roc)
}



#' Title
#'
#' @param response a
#' @param predictor a
#' @param weights a
#' @param cost a
#'
#' @return
#' @export TwoclassAUC
#'
#' @examples
#' 
TwoclassAUC <- function( response, predictor, weights = NULL, cost = NULL ){

  if( is.null(weights) & is.null(cost) ){

    weights <- rep( 1, length(response) )

  }else if( is.null(weights) & !is.null(cost) ){
    
    # Prepare sums of each row
    costCases <- rowSums( cost )
    
    # Prepare temporary vector
    weights <- double( length(response) )
    
    # Assign a particular cost to the corresponding class
    for( i in names(costCases) ){
      
      weights[ response == i ] <- costCases[i]
      
    }
    
  }else if( is.null(weights) ){
    
    weights <- rep( 1, length(response) )
    
  }
  
  out <- WeightedAUC( WeightedROC( predictor, response, weights ) )

  return( out  )

}

WeightedROC <- function( guess, label, weight = rep(1, length(label)) ){
  if (is.factor(label)) {
    label <- as.integer(label)
  }
  stopifnot(is.numeric(label))
  label.tab <- table(label)
  if (length(label.tab) == 1) {
    print(label.tab)
    stop("only one label value")
  }
  if (all(label %in% c(0, 1))) {
    label[label == 0] <- -1
  }
  if (all(label %in% c(1, 2))) {
    label[label == 1] <- -1
    label[label == 2] <- 1
  }
  stopifnot(label %in% c(-1, 1))
  stopifnot(is.numeric(guess))
  stopifnot(length(label) == length(guess))
  if (any(is.na(guess))) {
    stop("ROC curve undefined for NA guess")
  }
  stopifnot(is.numeric(weight))
  stopifnot(length(label) == length(weight))
  stopifnot(weight > 0)
  ord <- order(guess)
  y <- label[ord]
  w <- weight[ord]
  y.hat <- guess[ord]
  is.positive <- y == 1
  is.negative <- y == -1
  w.positive <- w.negative <- w
  w.positive[is.negative] <- 0
  w.negative[is.positive] <- 0
  cum.positive <- cumsum(w.positive)
  cum.negative <- cumsum(w.negative)
  is.end <- c(diff(y.hat) != 0, TRUE)
  n <- length(y)
  threshold <- c(y.hat[is.end], Inf)
  total.positive <- cum.positive[n]
  total.negative <- cum.negative[n]
  FN <- c(0, cum.positive[is.end])
  FNR <- FN/total.positive
  TPR <- 1 - FNR
  TN <- c(0, cum.negative[is.end])
  FP <- total.negative - TN
  FPR <- FP/total.negative
  d <- data.frame(TPR, FPR, threshold, FN, FP)
  d
}

WeightedAUC <- function( tpr.fpr ){

  stopifnot(is.data.frame(tpr.fpr))
  stopifnot(nrow(tpr.fpr) > 1)
  for (var.name in c("TPR", "FPR")) {
    stopifnot(diff(tpr.fpr[[var.name]]) <= 0)
  }
  right <- tpr.fpr[-nrow(tpr.fpr), ]
  left <- tpr.fpr[-1, ]
  width <- right$FPR - left$FPR
  rect.area <- left$TPR * width
  triangle.h <- right$TPR - left$TPR
  triangle.area <- triangle.h * width/2
  my.auc <- sum(rect.area, triangle.area)
  return( my.auc )

}
