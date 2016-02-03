copy_tree <- function(tree) {
    newtree <- bst()
    newtree$root <- tree$root
    newtree$compare <- tree$compare
    newtree
}

copy_node  <- function(node) {
    newnode <- bstnode(node$key, node$value, node$n, node$red)
    newnode$left <- node$left
    newnode$right <- node$right
    newnode
}