#' Delete a key-value pair from a tree
#' 
#' @param tree A \code{bst}
#' @param key The key to delete
#' 
#' @details If the tree contains the key, then a new tree with the key removed
#' is returned. Otherwise, the original tree is returned, with a warning
#' @export
delete <- function(tree, key) UseMethod("delete")

#' @importFrom assertthat assert_that
#' @importFrom assertthat is.scalar
#' @export
delete.bst <- function(tree, key) {
    assert_that(is.scalar(key))
    assert_that(!is.na(key))    
    
    if (!contains(tree, key)) {
        warning("key ", key, " not found")
        return(tree)
    }
    
    newtree <- copy_tree(tree)
    newroot <- delete_black(newtree$root, key, 
                            newtree$compare)
    newtree$root <- newroot$res
    newtree
}