#' Return a subsetted tree
#' 
#' Prune returns a new \code{bst} with all keys (and associated values) between
#' low and high.
#' 
#' @param tree The \code{bst}
#' @param low Low end of the range to return
#' @param high high end of the range to return
#' 
#' @details Returns a new \code{bst}
#' @export
prune <- function(tree, low, high) {
    keys <- keys_between(tree, low, high)
    vals <- lapply(keys, function(x) retrieve(tree, x))
    bst(keys, vals, tree$compare)
}