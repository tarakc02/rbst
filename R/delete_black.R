delete_black <- function(node, key, compare) {
    ## delete node from a black-rooted tree,
    ## return the new tree (minus node with key) along with
    ## news of whether black-height has been reduced
    
    ## assume that the tree contains the key
    
    if (is.null(node$left) && is.null(node$right)) {
        return(list(res = NULL, decrement = TRUE))
    }
    
    if (!is_red(node$left)) {
        node$red <- TRUE
        newnode <- delete_red(node, key, compare)
        
        if (is_red(newnode)) decrement <- FALSE
        else                 decrement <- TRUE
        
        node$red <- FALSE
        newnode$red <- FALSE
        result <- list(res = newnode,
                       decrement = decrement)
        return(result)
    }
    
    if (compare(key, node$key) < 0L) {
        newnode <- copy_node(node)
        newnode$left <- delete_red(node$left, key, compare)
        update_size(newnode)
        result <- list(res = newnode, decrement = FALSE)
        return(result)
    }
    
    # if left child is red and we have to go right
    if (compare(key, node$key) > 0L) {
        newnode <- copy_node(node)
        new_right_child <- delete_black(node$right, key, compare)
        
        if (new_right_child$decrement) {
            if (!is_red(node$left$right$left)) {
                b <- copy_node(node$left)
                f <- copy_node(node$left$right)
                f$red <- TRUE
                newnode$left <- f
                newnode$right <- new_right_child$res
                b$right <- newnode
                b$red <- FALSE
                update_size(newnode)
                update_size(b)
                return(list(res = b, decrement = FALSE))
            } else {
                newnode$right <- new_right_child$res
                b <- copy_node(node$left)
                f <- copy_node(node$left$right)
                d <- copy_node(f$left)
                d$red <- FALSE
                newnode$left <- f$right
                f$right <- newnode
                b$right <- d
                f$left <- b
                update_size(b)
                update_size(newnode)
                update_size(f)
                return(list(res = f, decrement = FALSE))
            }
        } else {
            newnode$right <- new_right_child$res
            update_size(newnode)
            return(list(res = newnode,
                        decrement = FALSE))
        }
    }
    
    # node is a terminal 3-node
    if (is.null(node$left$left)) {
        result <- copy_node(node$left)
        result$red <- FALSE
        return(list(res = result, decrement = FALSE))
    }
    
    # else, replace node key/value
    # with the result of min(node$right), then replace node$right
    # with delete_min(node$right)
    
    
    # easy if right grandchild is red
    if (is_red(node$right$left)) {
        d <- copy_node(node)
        h <- copy_node(node$right)
        minnode <- min(h)
        d$key <- minnode$key
        d$value <- minnode$value
        f <- delete_min(node$right$left)
        h$left <- f
        d$right <- h
        update_size(h)
        update_size(d)
        return(list(res = d,
                    decrement = FALSE))
    }
    
    # otherwise:
    d <- copy_node(node)
    minnode <- min(node$right)
    node$right$red <- TRUE
    h <- delete_min(node$right)
    node$right$red <- FALSE
    d$key <- minnode$key
    d$value <- minnode$value
    
    if (is_red(node$left$right$left)) {
        if (is_red(h)) {
            h$red <- FALSE
            d$right <- h
            update_size(d)
            return(list(res = d,
                        decrement = FALSE))
        } else {
            c <- copy_node(node$left$right)
            b <- copy_node(node$left)
            cl <- copy_node(c$left)
            cl$red <- FALSE
            b$right <- cl
            d$right <- h
            d$left <- c$right
            c$left <- b
            c$right <- d
            update_size(b)
            update_size(d)
            update_size(c)
            return(list(res = c,
                        decrement = FALSE))
        }
    }

    if (is_red(h)) {
        h$red <- FALSE
        d$right <- h
        update_size(d)
        return(list(res = d,
                    decrement = FALSE))
    } else {
        d$right <- h
        c <- copy_node(node$left$right)
        b <- copy_node(node$left)
        c$red <- TRUE
        d$left <- c
        b$right <- d
        update_size(d)
        update_size(b)
        b$red <- FALSE
        return(list(res = b, decrement = FALSE))
    }
}