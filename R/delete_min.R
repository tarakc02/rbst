#' Delete the minimum from a tree
#' 
#' @param tree A \code{bst}
#' 
#' @export
delete_min <- function(tree) UseMethod("delete_min")

#' @export
delete_min.bst <- function(tree) {
    if(is_empty(tree)) stop("tree is empty")
    
    newtree <- copy_tree(tree)
    
    if (!is_red(newtree$root$left)) {
        newtree$root$red <- TRUE
        newtree$root <- delete_min(newtree$root)
        tree$root$red <- FALSE
    } else {
        newroot <- copy_node(tree$root)
        newtree$root <- newroot
        newtree$root$left <- delete_min(newtree$root$left)
    }
    
    if (!is_empty(newtree)) {
        newtree$root$red <- FALSE
        update_size(newtree$root)
    }
    
    newtree
}

delete_min.bstnode <- function(node) {
    # by invariant, node is red
    
    # base case 1: we're at the min
    # since it's red, we can remove without screwing up black-balance
    if (is.null(node$left))
        return(NULL)
    
    # base case 2: left child is the min and no right grandchild
    if (is.null(node$left$left) && is.null(node$right$left)) {
        d <- copy_node(node$right)
        b <- copy_node(node)
        
        b$right <- NULL
        b$left <- NULL # this is the deletion
        d$left <- b
        update_size(b)
        update_size(d)
        return(d)
    }
    
    # base case 3: left child is min, exists a right red grandchild
    if (is.null(node$left$left) && is_red(node$right$left)) {
        c <- copy_node(node$right$left)
        b <- copy_node(node)
        d <- copy_node(node$right)
        b$red <- FALSE
        b$left <- NULL
        b$right <- NULL
        b$n <- 1L
        d$left <- NULL
        
        c$left <- b
        c$right <- d
        
        d$n <- 1L
        update_size(c)
        return(c)
    }
    
    # otherwise: recurse -- there are 3 situations
    
    # first: left grandchild is red, so just do the deletion from there
    # (note this case actually includes the final base case, 
    #  where left red grandchild is min)
    if (is_red(node$left$left)) {
        c <- copy_node(node)
        b <- copy_node(node$left)
        a <- copy_node(node$left$left)
        
        c$left <- b
        a <- delete_min(a)
        b$left <- a
        update_size(a)
        update_size(b)
        update_size(c)
        return(c)
    }
    
    # second: there is a red grandchild on the right subtree, 
    # just rotate as in base case 2
    if (is_red(node$right$left)) {
        b <- copy_node(node)
        b$red <- FALSE
        a <- copy_node(node$left)
        d <- copy_node(node$right)
        c <- copy_node(node$right$left)
        b$right <- c$left
        d$left <- c$right
        c$left <- b
        c$right <- d
        
        a$red <- TRUE
        a <- delete_min(a)
        b$left <- a
        update_size(a)
        update_size(b)
        update_size(d)
        update_size(c)
        return(c)
    }
    
    # finally: no red grandchildren, need to recolor, rebalance, etc.
    d <- copy_node(node)
    b <- copy_node(node$left)

    # when we recolor b, we are reducing black height
    # will need to resolve in order to keep black balance
    b$red <- TRUE # just for now
    b <- delete_min(b)
    d$left <- b

    if (b$red) {
        # in case we returned a red subtreee, just re-color it black,
        # and boom, black balance
        b$red <- FALSE
        update_size(d)
        return(d)
    } else {
        # need to rebalance by rotating the whole subtree
        f <- copy_node(node$right)
        d$right <- node$right$left
        f$left <- d
        update_size(d)
        update_size(f)
        return(f)
    }
}