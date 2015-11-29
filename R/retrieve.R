#' Retrieve the value associated with a given key
#' 
#' If the key exists in the tree, \code{retrieve} returns the associated value. 
#' Otherwise, \code{retrieve} returns \code{NULL}.
#' 
#' @param tree A \code{bst}
#' @param key The key to search for
#' @param ... Ignored
#' 
#' @examples
#' mytree <- bst()
#' mytree <- insert(mytree, "a", 7)
#' retrieve(mytree, "a")
#' # can also use subset operator
#' mytree["a"]
#' @export
retrieve <- function(tree, key, ...) UseMethod("retrieve")

#' @importFrom assertthat assert_that
#' @importFrom assertthat is.scalar
#' @export
retrieve.bst <- function(tree, key) {
    assert_that(is.scalar(key))
    assert_that(!is.na(key))
    retrieve(tree$root, key, NULL, tree$compare)
}

retrieve.bstnode <- function(node, key, sentinel, compare) {
    x <- compare(key, node$key)
    if (x < 0)      return(retrieve(node$left, key, sentinel, compare))
    else return(retrieve(node$right, key, node, compare))
}

retrieve.NULL <- function(node, key, sentinel, compare) {
    if (!is.null(sentinel) && compare(sentinel$key, key) == 0L)
        return(sentinel$value)
    else return(NULL)
}

#' @rdname retrieve
#' @export
`[.bst` <- function(tree, key) retrieve(tree, key)