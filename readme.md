rbst
===============================================================================
Inspired by a similar [project by Shawn T. Oneil](https://github.com/oneilsh/rstackdeque), `rbst` is a side-effect free implementation of a binary search tree. That is, you can store and retrieve keys and values relatively quickly (for randomly ordered keys -- currently the implementation is not a balanced tree, so certain inputs will result in worst-case (linear) insert and lookup).

Keys can be of any scalar type that has a meaningful `<` operation defined. For custom key classes, define a `compare` method (see the documentation). Values can be of any type.

The implementation here is heavily based on the one in [Algorithms, 4th Edition by Robert Sedgwick and Kevin Wayne](http://algs4.cs.princeton.edu/home/), but with the appropriate modifications to allow for full persistence. 
```R
> mytree <- bst()
> mytree <- insert(mytree, "key", "value")

## can also use subset operators:
> mytree["another key"] <- "another value"
> mytree["yet another"] <- "values galore"

## no side-effects
> mytree2 <- insert(mytree, "special key", "special value")
> mytree
tree of size 3 
key - value
another key - another value (leaf)
yet another - values galore (leaf)
> mytree2
tree of size 4 
key - value
another key - another value (leaf)
yet another - values galore
special key - special value (leaf)

## retrieval methods:
retrieve(mytree, "key")
[1] "value"
mytree["another key"]
[1] "another value"

## range-based search:
# find keys between letter "k" and "t"
keys_between(mytree2, "k", "t")
[[1]]
[1] "key"

[[2]]
[1] "special key"

## ceiling_of and floor_of
mytree3 <- bst()
random_keys <- sample(1000, 25)
random_values <- sample(letters, 25)
for (k in 1:25) mytree3 <- insert(mytree3, random_keys[k], random_values[k])

# return the smallest key >= 900:
ceiling_of(mytree3, 900)
[1] 919
mytree3[ceiling_of(mytree3, 900)]
[1] "t"

# same with floor
floor_of(mytree3, 123)
[1] 27
```

Installation
------------
```R
# install.packages("devtools")
devtools::install_github("tarakc02/rbst")
```

For the future
--------------
This project has a lot of room for improvement. For starters, the unit tests do not cover all types of inputs, so bug reports would be appreciated. I haven't yet done performance testing. I'd also like to write a vignette. Also need to add `min` and `max` type functions to return the smallest or largest key. Beyond that, the underlying implementation should really be a balanced (red-black) search tree, which might  make it easier to include delete operations. If you have any ideas please let me know. 