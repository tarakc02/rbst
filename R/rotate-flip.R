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

rotate_left <- function(node) {
    x <- node$right
    node$right <- x$left
    x$left <- node
    x$red <- node$red
    node$red <- TRUE
    x$n <- node$n
    node$n <- 1L + size(node$left) + size(node$right)
    x
}

rotate_right <- function(node) {
    x <- node$left
    node$left <- x$right
    x$right <- node
    x$red <- node$red
    node$red <- TRUE
    x$n <- node$n
    node$n <- 1L + size(node$left) + size(node$right)
    x
}