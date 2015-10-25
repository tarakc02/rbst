#' Create a mock bst object for testing and analysis
#' 
#' @param nodes Size of the tree
#' @param data_size Size of each data element
#' @param key_type "string" or "integer"
#' @importFrom assertthat assert_that
#' @importFrom assertthat is.count
#' @export
mockbst <- function(nodes, data_size, key_type = "string") {
    assertthat::assert_that(assertthat::is.count(nodes))
    assertthat::assert_that(assertthat::is.count(data_size))
    assertthat::assert_that(assertthat::is.string(key_type))
    tree <- bst()
    for (node in 1:nodes) {
        if (key_type == "string")
            key <- paste(sample(c(letters, LETTERS), 9), collapse="")
        else stop("key_type not supported")
        value <- rnorm(data_size)
        tree <- insert(tree, key, value)
    }
    tree
}