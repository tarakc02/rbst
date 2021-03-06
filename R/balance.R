balance <- function(newnode) {
    # if two red children occured not as the result of a rotation,
    # (which violates left-leaning invariant) then 
    # need to flip colors, but left child was not on search path,
    # so need to make a copy of it before modifying 
    if (is_red(newnode$left) && is_red(newnode$right))
        newnode <- flipcopy(newnode)
    
    # ensure red links always lean left
    if (is_red(newnode$right) && !is_red(newnode$left)) 
        newnode <- rotate_left(newnode)
    
    # then fix cases where two red links in a row on the left
    # note don't have to check for red-red on the right
    if (is_red(newnode$left) && is_red(newnode$left$left))
        newnode <- rotate_right(newnode)
    
    # right rotation can leave cases where both left and right children are red
    # to keep left-leaning, just flip both to black (and parent to red)
    if (is_red(newnode$left) && is_red(newnode$right)) flip(newnode)
    
    newnode
}
