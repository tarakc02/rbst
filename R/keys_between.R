#' Find all keys between low and high
#' 
#' Returns a list of keys
#' 
#' @param tree A \code{bst}
#' @param low Low end of the range to return
#' @param high high end of the range to return
#' @export
keys_between <- function(tree, low, high) UseMethod("keys_between")

#' @import assertthat
#' @export
keys_between.bst <- function(tree, low, high) {
    assert_that(is.scalar(low))
    assert_that(is.scalar(high))
    assert_that(!is.na(low))
    assert_that(!is.na(high))
    assert_that(compare(high, low) > 0)
    
    # resizing vector code from Gabor Grothendieck
    # https://stat.ethz.ch/pipermail/r-help/2005-January/063530.html
    keys <- vector("list")
    keylen <- 0
    
    kb <- function(n, l, h) {
        if (is.null(n)) return()
        cmplo <- compare(l, n$key)
        cmphi <- compare(h, n$key)
        if (cmplo < 0) kb(n$left, l, h)
        if (cmplo <= 0 && cmphi >=0) {
            if (keylen == length(keys)) length(keys) <<- 2 * length(keys)
            keylen <<- keylen + 1
            keys[keylen] <<- n$key
        }
        if (cmphi > 0) kb(n$right, l, h)
    }
    kb(tree$root, low, high)
    
    # need to remove excess pre-allocated
    return(keys[!sapply(keys, is.null)])
}