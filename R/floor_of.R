#' Get the largest key less than or equal to a given key
#' 
#' @param tree A \code{bst}
#' @param key The key to use in comparisons
#' 
#' @details If the key exists in the tree, then it is returned. If the key is 
#' smaller than the smallest key in the tree, then \code{NULL} is returned. 
#' Otherwise, the largest key that is smaller than the given key is returned.
#' 
#' @examples
#' mytree <- bst()
#' mytree <- insert(mytree, 1, "val1")
#' mytree <- insert(mytree, 3, "val2")
#' retrieve(mytree, floor_of(mytree, 2))
#' retrieve(mytree, floor_of(mytree, 4))
#' @export
floor_of <- function(tree, key) UseMethod("floor_of")

#' @importFrom assertthat assert_that
#' @importFrom assertthat is.scalar
#' @export
floor_of.bst <- function(tree, key) {
    assertthat::assert_that(is.scalar(key) & !is.na(key))
    if (is_empty(tree)) stop("Tree is empty")
    res = floor_of(tree$root, key)
    if (is.null(res)) return(NULL)
    else return(res$key)
}

floor_of.bstnode <- function(node, key) {
    comp = compare(key, node$key)
    if (comp == 0) return(node)
    if (comp < 0) return(floor_of(node$left, key))
    temp = floor_of(node$right, key)
    if (!is.null(temp)) return(temp)
    else return(node)
}

floor_of.NULL <- function(node, key) NULL