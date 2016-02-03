is_red <- function(node) {
    if (is.null(node)) return(FALSE)
    node$red
}

flip <- function(node) {
    # re-color nodes after rotating leads
    # to node with two red children
    node$red <- TRUE
    node$left$red <- FALSE
    node$right$red <- FALSE
}

flipcopy <- function(node) {
    # re-color nodes, but copy the left child
    node$red <- TRUE
    x <- node$left
    newleft <- bstnode(x$key, x$value, x$n, FALSE)
    newleft$left <- x$left
    newleft$right <- x$right
    node$left <- newleft
    node$right$red <- FALSE
    node
}

rotate_left <- function(node) {
    x <- node$right
    node$right <- x$left
    x$left <- node
    x$red <- node$red
    node$red <- TRUE
    x$n <- node$n
    update_size(node)
    x
}

rotate_right <- function(node) {
    x <- node$left
    node$left <- x$right
    x$right <- node
    x$red <- node$red
    node$red <- TRUE
    x$n <- node$n
    update_size(node)
    x
}