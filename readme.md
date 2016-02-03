rbst
===============================================================================
**Now includes delete() function**

Inspired by a similar [project by Shawn T. Oneil](https://github.com/oneilsh/rstackdeque), `rbst` is a side-effect free implementation of a binary search tree. That is, you can store and retrieve keys and values quickly (guaranteed logarithmic time for insert, retrieve, and delete).

Keys can be of any scalar type that has a meaningful `<` operation defined. For custom key classes, define a `compare` method (see the documentation). Values can be of any type.

`rbst` achieves perfect balance (and the resulting speed guarantees) by implementing a left-leaning red-black tree. 

The implementation here is heavily based on the one in [Algorithms, 4th Edition by Robert Sedgwick and Kevin Wayne](http://algs4.cs.princeton.edu/home/), but with the appropriate modifications to allow for full persistence. The `delete` function owes a huge debt to [a paper by Julien Oster](http://www.reinference.net/llrb-delete-julien-oster.pdf).

```R
> mytree <- bst(keys = rev(letters), values = 1:26)
> mytree
tree of size 26 
|  s: int 8
|  k: int 16
|  w: int 4
|  ...

> retrieve(mytree, "z")
[1] 1
> mytree["a"]
[1] 26

> min_key(mytree); max_key(mytree)
[1] "a"
[1] "z"

## no side-effects
> mytree2 <- insert(mytree, "special key", "special value")
> contains(mytree, "special key")
[1] FALSE

> contains(mytree2, "special key")
[1] TRUE

## range-search:
> keys_between(mytree, "f", "i")
[[1]]
[1] "f"

[[2]]
[1] "g"

[[3]]
[1] "h"

[[4]]
[1] "i"

## delete
> mytree2 <- delete(mytree, "q")
> contains(mytree, "q")
[1] TRUE
> contains(mytree2, "q")
[1] FALSE

> mytree3 <- delete_min(mytree)
> contains(mytree3, "a")
[1] FALSE

> setdiff(keys(mytree), keys(mytree3))
[[1]]
[1] "a"


## floor_of and ceiling_of
> example <- bst(sample(1000, 26), letters)
> floor_of(example, 15)
[1] 12
> ceiling_of(example, 178)
[1] 179
> ceiling_of(example, 171)
[1] 179

```

Installation
------------
```R
# install.packages("devtools")
devtools::install_github("tarakc02/rbst")
```

For the future
--------------
This project has a lot of room for improvement. For starters, the unit tests do not cover all types of inputs, so bug reports would be appreciated. Un-implemented features include 