delete_red <- function(node, key, compare) {
    # delete from a red node
    # can assume that the tree contains the key
    
    # base case: black height = 0
    if (is.null(node$left) && is.null(node$right)) return(NULL)
    
    # base case: black height = 1, no red grandchildren
    if (is.null(node$left$left) && is.null(node$right$left)) {
        # key to remove is on the left
        if (compare(key, node$key) < 0L) {
            b <- copy_node(node)
            c <- copy_node(node$right)
            b$left <- NULL
            b$right <- NULL
            c$left <- b
            c$right <- NULL
            update_size(b)
            update_size(c)
            return(c)
        }
        
        # key to remove is on the right
        if (compare(key, node$key) > 0L) {
            b <- copy_node(node)
            a <- copy_node(node$left)            
            b$red <- FALSE
            a$red <- TRUE
            b$left <- a
            b$right <- NULL
            update_size(a)
            update_size(b)
            return(b)
        }
        
        # key to remove is the node itself
        a <- copy_node(node$left)
        c <- copy_node(node$right)
        a$red <- TRUE
        c$left <- a
        c$right <- NULL
        update_size(a)
        update_size(c)
        return(c)
    }
    
    # base case: black height = 1, 1 red grandchild in the right subtree
    if (is_red(node$right$left) && is.null(node$left$left)) {
        # key to remove is on the left
        if (compare(key, node$key) < 0L) {
            c <- copy_node(node$right$left)
            d <- copy_node(node$right)
            b <- copy_node(node)
            b$red <- FALSE
            b$left <- NULL
            b$right <- NULL
            d$left <- NULL
            c$left <- b
            c$right <- d
            update_size(d)
            update_size(b)
            update_size(c)
            return(c)
        }
        
        # key to remove is on the right
        if (compare(key, node$key) > 0L) {
            b <- copy_node(node)
            if (compare(key, node$right$key) == 0L) {
                c <- copy_node(node$right$left)
                c$red <- FALSE
                b$right <- c
            } else {
                d <- copy_node(node$right)
                d$left <- NULL
                update_size(d)
                b$right <- d
            }
            update_size(b)
            return(b)
        }
        
        # remove node
        c <- copy_node(node$right$left)
        d <- copy_node(node$right)
        d$left <- NULL
        c$left <- node$left
        c$right <- d
        update_size(d)
        update_size(c)
        return(c)
    }
    
    # base case: black height = 1, 1 red grandchild in the left subtree
    if (is_red(node$left$left) && is.null(node$right$left)) {
        # key to delete is on the right
        if (compare(key, node$key) > 0L) {
            b <- copy_node(node$left)
            a <- copy_node(b$left)
            c <- copy_node(node)
            c$right <- NULL
            c$left <- NULL
            b$red <- TRUE
            a$red <- FALSE
            c$red <- FALSE
            b$left <- a
            b$right <- c
            update_size(c)
            update_size(b)
            return(b)
        }
        
        # key to delete is on the left
        if (compare(key, node$key) < 0L) {
            c <- copy_node(node)
            # it's the grandchild
            if (compare(key, node$left$key) < 0L) {
                b <- copy_node(node$left)
                b$left <- NULL
                c$left <- b
                update_size(b)
            } else {
                # it's the child
                a <- copy_node(node$left$left)
                a$red <- FALSE
                c$left <- a
            }
            update_size(c)
            return(c)
        }
        
        # delete node
        b <- copy_node(node$left)
        a <- copy_node(node$left$left)
        a$red <- FALSE
        b$red <- TRUE
        b$left <- a
        b$right <- node$right
        update_size(b)
        return(b)
    }
    
    # base case: black height = 1, red grandchildren on both sides
    if (is_red(node$left$left) && is_red(node$right$left) && is.null(node$left$left$left)) {
        # delete node is on the left
        if (compare(key, node$key) < 0L) {
            c <- copy_node(node)
            #it's the left grandchild
            if (compare(key, node$left$key) < 0L) {
                b <- copy_node(node$left)
                b$left <- NULL
                c$left <- b
                update_size(b)
            } else {
                # it's the left child
                a <- copy_node(node$left$left)
                a$red <- FALSE
                c$left <- a
            }
            update_size(c)
            return(c)
        }
        
        # it's on the right
        if (compare(key, node$key) > 0L) {
            c <- copy_node(node)
            b <- copy_node(node$left)
            a <- copy_node(node$left$left)
            a$red <- FALSE
            b$red <- TRUE
            b$left <- a
            c$left <- NULL
            c$right <- NULL
            update_size(c)
            if (compare(key, node$right$key) < 0L) {
                # it's the grandchild
                e <- copy_node(node$right)
                e$left <- c
                b$right <- e
            } else {
                # it's the child
                d <- copy_node(node$right$left)
                d$red <- FALSE
                d$left <- c
                b$right <- d
                update_size(d)
            }
            update_size(b)
            return(b)
        }
        
        # delete the node
        b <- copy_node(node$left)
        b$red <- TRUE
        a <- copy_node(node$left$left)
        a$red <- FALSE
        b$left <- a
        b$right <- node$right
        update_size(b)
        return(b)
    }
    
    # recursive cases!
    # no red grandchildren
    if (!is_red(node$left$left) && !is_red(node$right$left)) {
        # delete node is on the left
        if (compare(key, node$key) < 0L) {
            d <- copy_node(node)
            b <- copy_node(node$left)
            b$red <- TRUE
            b <- delete_red(b, key, compare)
            d$left <- b
            
            if (b$red) {
                b$red <- FALSE
                update_size(d)
                return(d)
            } else {
                f <- copy_node(node$right)
                d$right <- node$right$left
                f$left <- d
                update_size(d)
                update_size(f)
                return(f)
            }
        }
        
        # delete node is on the right
        if (compare(key, node$key) > 0L) {
            d <- copy_node(node)
            f <- copy_node(node$right)
            
            # temporarily making a right child red
            f$red <- TRUE
            f <- delete_red(f, key, compare)
            d$right <- f
            
            if (f$red) {
                f$red <- FALSE
            } else {
                # we've reduced black height on right, so 
                # flip left child to red to reduce left black height
                b <- copy_node(node$left)
                d$red <- FALSE
                b$red <- TRUE
                d$left <- b
            }
            update_size(d)
            return(d)
        }
        
        # if the node to delete is node itself:
        d <- copy_node(node)
        d$left <- node$left$right
        d$right <- node$right$left
        d <- delete_red(d, key, compare)
        if (is_red(d)) {
            b <- copy_node(node$left)
            f <- copy_node(node$right)
            b$right <- d$left
            f$left <- d$right
            d$left <- b
            d$right <- f
            update_size(f)
            update_size(b)
            update_size(d)
            return(d)
        } else {
            b <- copy_node(node$left)
            b$red <- TRUE
            f <- copy_node(node$right)
            b$right <- d
            f$left <- b
            update_size(b)
            update_size(f)
            return(f)
        }
    }
    
    # two red grandchildren
    if (is_red(node$left$left) && is_red(node$right$left)) {
        if (compare(key, node$key) < 0L) {
            if (compare(key, node$left$key) < 0L) {
                f <- copy_node(node)
                d <- copy_node(node$left)
                b <- delete_red(node$left$left, key, compare)
                d$left <- b
                f$left <- d
                update_size(d)
            } else {
                f <- copy_node(node)
                d <- copy_node(node$left)
                b <- copy_node(node$left$left)
                d$left <- b$right
                d$red <- TRUE
                d <- delete_red(d, key, compare)
                if (d$red) {
                    d$red <- FALSE
                    b$right <- d$left
                    d$left <- b
                    f$left <- d
                    update_size(b)
                    update_size(d)
                } else {
                    b$red <- FALSE
                    b$right <- d
                    f$left <- b
                    update_size(b)

                }
            }
            update_size(f)
            return(f)
        }
        
        if (compare(key, node$key) > 0L) {
            if (compare(key, node$right$key) < 0L) {
                f <- copy_node(node)
                j <- copy_node(node$right)
                h <- delete_red(node$right$left, key, compare)
                j$left <- h
                f$right <- j
                update_size(j)
            } else {
                f <- copy_node(node)
                j <- copy_node(node$right)
                h <- copy_node(node$right$left)
                j$left <- h$right
                j$red <- TRUE
                j <- delete_red(j, key, compare)
                if (j$red) {
                    j$red <- FALSE
                    h$right <- j$left
                    j$left <- h
                    f$right <- j
                    update_size(h)
                    update_size(j)
                } else {
                    h$right <- j
                    h$red <- FALSE
                    f$right <- h
                    update_size(h)
                }
            }
            update_size(f)
            return(f)
        }
        
        f <- copy_node(node)
        f$left <- node$left$right
        f$right <- node$right$left$left
        f <- delete_red(f, key, compare)
        
        if (f$red) {
            d <- copy_node(node$left)
            j <- copy_node(node$right)
            h <- copy_node(node$right$left)
            d$right <- f$left
            h$left <- f$right
            f$left <- d
            j$left <- h
            f$right <- j
            update_size(d)
            update_size(h)
            update_size(j)
            update_size(f)
            return(f)
        } else {
            d <- copy_node(node$left)
            d$red <- TRUE
            j <- copy_node(node$right)
            h <- copy_node(node$right$left)
            h$left <- f
            j$left <- h
            d$right <- j
            b <- copy_node(d$left)
            b$red <- FALSE
            d$left <- b
            update_size(h)
            update_size(j)
            update_size(d)
            return(d)
        }
    }
    
    # one red grandchild, in the right subtree
    if (is_red(node$right$left) && !is_red(node$left$left)) {
        if (compare(key, node$key) < 0L) {
            d <- copy_node(node)
            b <- copy_node(node$left)
            b$red <- TRUE
            b <- delete_red(b, key, compare)
            if (b$red) {
                b$red <- FALSE
                d$left <- b
                update_size(d)
                return(d)
            } else {
                f <- copy_node(node$right$left)
                h <- copy_node(node$right)
                h$left <- f$right
                f$right <- h
                d$red <- FALSE
                d$left <- b
                d$right <- f$left
                f$left <- d
                update_size(d)
                update_size(h)
                update_size(f)
                return(f)
            }
        }
        
        if (compare(key, node$key) > 0L) {
            if (compare(key, node$right$key) < 0L) {
                d <- copy_node(node)
                h <- copy_node(node$right)
                f <- delete_red(h$left, key, compare)
                h$left <- f
                d$right <- h
                update_size(h)
                update_size(d)
                return(d)
            } else {
                d <- copy_node(node)
                h <- copy_node(node$right)
                f <- copy_node(node$right$left)
                h$left <- f$right
                h$red <- TRUE
                h <- delete_red(h, key, compare)
                if (h$red) {
                    h$red <- FALSE
                    f$right <- h$left
                    h$left <- f
                    d$right <- h
                    update_size(f)
                    update_size(h)
                } else {
                    f$red <- FALSE
                    f$right <- h
                    d$right <- f
                    update_size(f)
                }
                update_size(d)
                return(d)
            }
        }
        
        d <- copy_node(node)
        d$left <- node$left$right
        d$right <- node$right$left$left
        d <- delete_red(d, key, compare)
        
        if (d$red) {
            d$red <- FALSE
            h <- copy_node(node$right)
            b <- copy_node(node$left)
            b$red <- TRUE
            b$right <- d$left
            d$left <- b
            f <- copy_node(node$right$left)
            f$left <- d
            h$left <- f$right
            f$right <- h
            update_size(h)
            update_size(b)
            update_size(d)
            update_size(f)
            return(f)
        } else {
            f <- copy_node(node$right$left)
            b <- copy_node(node$left)
            h <- copy_node(node$right)
            b$right <- d
            h$left <- f$right
            f$right <- h
            f$left <- b
            update_size(h)
            update_size(b)
            update_size(f)
            return(f)
        }
    }
    
    # one red grandchild on the left
    if (is_red(node$left$left) && !is_red(node$right$left)) {
        if (compare(key, node$key) < 0L) {
            f <- copy_node(node)
            d <- copy_node(node$left)
            if (compare(key, d$key) < 0L) {
                b <- delete_red(d$left, key, compare)
                d$left <- b
                f$left <- d
                update_size(d)
            } else {
                b <- copy_node(d$left)
                d$left <- b$right
                d$red <- TRUE
                d <- delete_red(d, key, compare)
                if (d$red) {
                    d$red <- FALSE
                    b$right <- d$left
                    d$left <- b
                    f$left <- d
                    update_size(b)
                    update_size(d)
                } else {
                    b$right <- d
                    b$red <- FALSE
                    f$left <- b
                    update_size(b)
                }
            }
            update_size(f)
            return(f)
        }
        
        if (compare(key, node$key) > 0L) {
            f <- copy_node(node)
            h <- copy_node(node$right)
            h$red <- TRUE
            h <- delete_red(h, key, compare)
            if (h$red) {
                d <- copy_node(node$left)
                b <- copy_node(d$left)
                b$red <- FALSE
                d$left <- b
                f$left <- d$right
                f$right <- h$left
                h$left <- f
                d$right <- h
                d$red <- TRUE
                h$red <- FALSE
                update_size(f)
                update_size(h)
                update_size(d)
                return(d)
            } else {
                d <- copy_node(node$left)
                b <- copy_node(node$left$left)
                b$red <- FALSE
                f$left <- d$right
                f$right <- h
                d$right <- f
                d$left <- b
                d$red <- TRUE
                f$red <- FALSE
                update_size(f)
                update_size(d)
                return(d)
            }
        }
        
        f <- copy_node(node)
        h <- copy_node(node$right)
        d <- copy_node(node$left)
        b <- copy_node(node$left$left)
        b$red <- FALSE
        f$left <- node$left$right
        f$right <- node$right$left
        f <- delete_red(f, key, compare)
        d$red <- TRUE
        h$left <- f
        d$right <- h
        d$left <- b
        update_size(h)
        update_size(d)
        return(d)
    }
}