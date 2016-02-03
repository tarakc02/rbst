#' Delete the maximum from a tree
#' 
#' @param tree A \code{bst}
#' 
#' @export
delete_max <- function(tree) UseMethod("delete_max")

#' @export
delete_max.bst <- function(tree) {
    if(is_empty(tree)) stop("tree is empty")
    
    maxkey <- max_key(tree)
    delete(tree, maxkey)
}