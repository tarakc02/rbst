bstnode <- function(key, value, n) {
    node <- new.env(parent = emptyenv())
    node$key <- key
    node$value <- value
    node$n <- n
    node$left <- NULL
    node$right <- NULL
    class(node) <- "bstnode"
    return(node)
}

#' Create a new binary search tree
#' 
#' Creates a new, empty, binary search tree. A \code{bst} stores keys and 
#' values. You can \link{insert} new key-value pairs in, and you can 
#' \link{retrieve} values out by providing a key. 
#' 
#' @details the key can be any type for which there is a \link{compare} method 
#' defined. By default, \link{compare} will use \code{<}, so is defined for 
#' any types with which you can use \code{<}.
#' 
#' Note that there is no checking for consistency of keys. 
#' 
#' @examples 
#' mytree <- bst()
#' mytree
#' # add ("a": 3) to the tree
#' mytree2 <- insert(mytree, "a", 3)
#' 
#' # no side-effects
#' mytree
#' mytree2
#' retrieve(mytree, "a") ## returns NULL
#' retrieve(mytree2, "a") ## 3
#' @export
bst <- function() {
    bst <- new.env(parent = emptyenv())
    bst$root <- NULL
    class(bst) <- "bst"
    bst
}

#' @export
print.bst <- function(tree) {
    cat("tree of size", size(tree))
}

print.bst <- function(tree) {
    cat("tree of size", size(tree), '\n')
    if (!is_empty(tree)) print(tree$root)
}

print.bstnode <- function(node) {
    if (is.null(node)) return()
    cat(node$key, '-', node$value)
    if (is.null(node$left) && is.null(node$right)) cat(' (leaf)')
    cat('\n')
    if (!is.null(node$left)) print(node$left)
    if (!is.null(node$right)) print(node$right)    
}