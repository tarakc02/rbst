#' Create a mock bst object for testing and analysis
#' 
#' @param nodes Size of the tree
#' @param data_size Size of each data element
#' @param key_type "string" or "integer"
#' @importFrom assertthat assert_that
#' @importFrom assertthat is.count
#' @export
mockbst <- function(nodes, data_size = 1, key_type = "string") {
    assertthat::assert_that(assertthat::is.count(nodes))
    assertthat::assert_that(assertthat::is.count(data_size))
    assertthat::assert_that(assertthat::is.string(key_type))
    if (key_type == "string")
        key <- replicate(nodes, paste(sample(c(letters, LETTERS), 9), collapse=""))
    else stop("key_type not supported")
    
    value <- replicate(nodes, rnorm(data_size), simplify = FALSE)
    bst(key, value, compare_default)
}