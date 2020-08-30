#' Title
#'
#' @param tree a
#'
#' @return
#' @export PrintTree
#'
#' @examples
#' 
PrintTree <- function( tree ){
  
  # Set displaying format for probability vector
  SetFormat(tree, "Probability", formatFun = function(x){ paste0("(",paste0(format(round(x,2), nsmall = 2),collapse = ", "),")") })
  
  # Print final tree
  print( tree, "Count", "Probability", "Leaf", "Class" )
  
}