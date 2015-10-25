#' Insert a key-value pair into a bst
#' 
#' If the given key is already present in the tree, the associated value will 
#' be overwritten, with a warning.
#' 
#' @param tree A \code{bst}
#' @param key The key to insert
#' @param value The value to insert
#' 
#' @examples
#' mytree <- bst()
#' mytree <- insert(mytree, "a", 7)
#' 
#' # there is no type-checking, so you can do, eg:
#' mytree <- insert(mytree, 3.14, 9)
#' 
#' # insert has no  side-effects
#' mytree2 <- insert(mytree, "k", 10)
#' retrieve(mytree, "k") # NULL
#' retrieve(mytree2, "k") # 10
#' # or use [
#' mytree["q"] <- 15
#' @export
insert <- function(tree, key, value) UseMethod("insert")

#' @importFrom assertthat assert_that
#' @importFrom assertthat is.scalar
#' @export
insert.bst <- function(tree, key, value) {
    assert_that(is.scalar(key))
    assert_that(!is.null(value))
    assert_that(!is.na(key))
    newtree <- bst()
    newtree$root <- tree$root
    newtree$root <- insert(newtree$root, key, value)
    newtree
}

insert.bstnode <- function(node, key, value) {
    # make sure that newnode is a copy and not reference to old node
    newnode <- bstnode(node$key, node$value, node$n)
    newnode$left <- node$left
    newnode$right <- node$right
    
    comp <- compare(key, newnode$key)
    if (comp < 0L)      newnode$left  <- insert(newnode$left,  key, value)
    else if (comp > 0L) newnode$right <- insert(newnode$right, key, value)
    else {
        warning("Overwriting previous value")
        newnode$value <- value
    }
    
    newnode$n <- 1L + size(newnode$left) + size(newnode$right)
    newnode    
}

insert.NULL <- function(node, key, value) {
    bstnode(key, value, 1L)
}

#' @rdname insert
#' @export
`[<-.bst` <- function(tree, key, value) insert(tree, key, value)
