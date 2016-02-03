#' Insert a key-value pair into a bst
#' 
#' If the given key is already present in the tree, the associated value will 
#' be overwritten, with a warning.
#' 
#' @param tree A \code{bst}
#' @param key The key to insert
#' @param value The value to insert
#' @param ... Other arguments (not used)
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
insert <- function(tree, key, value, ...) UseMethod("insert")

#' @importFrom assertthat assert_that
#' @importFrom assertthat is.scalar
#' @export
insert.bst <- function(tree, key, value) {
    assert_that(is.scalar(key))
    assert_that(!is.null(value))
    assert_that(!is.na(key))
    newtree <- copy_tree(tree)
    newtree$root <- insert(newtree$root, key, value, newtree$compare)
    newtree$root$red <- FALSE
    newtree
}

insert.bstnode <- function(node, key, value, compare) {
    # make sure that newnode is a copy and not reference to old node
    newnode <- copy_node(node)

    comp <- compare(key, newnode$key)
    if (comp < 0L)      newnode$left  <- insert(newnode$left,  key, value, compare)
    else if (comp > 0L) newnode$right <- insert(newnode$right, key, value, compare)
    else {
        warning("Overwriting previous value")
        newnode$value <- value
    }
    
    newnode <- balance(newnode)
    #newnode$n <- 1L + size(newnode$left) + size(newnode$right)
    update_size(newnode)
    newnode    
}

insert.NULL <- function(node, key, value, compare) {
    # new nodes are always red
    bstnode(key, value, 1L, TRUE)
}

#' @rdname insert
#' @export
`[<-.bst` <- function(tree, key, value) insert(tree, key, value)
