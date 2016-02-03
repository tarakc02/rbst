update_size <- function(node) {
    if (!is.null(node)) node$n <- 1L + size(node$left) + size(node$right)
}