library(rbst)
context("constructors")

test_that("blank tree is empty", {
    tree <- bst()
    expect_true(is_empty(tree))
    expect_equal(size(tree), 0)
})

test_that("new trees use memory as expected", {
    tree1 <- bst()
    tree2 <- bst()
    tree3 <- tree1
    expect_identical(tree1, tree3)
    expect_false(identical(tree1, tree2))
})

test_that("as.bst does the same thing as manual insertion", {
    dummy <- structure(c(1,2,3), names = c("a", "b", "c"))
    tree1 <- bst()
    mapply(function(k, v) tree1 <<- insert(tree1, k, v), names(dummy), dummy)
    tree2 <- as.bst(dummy)
    expect_identical(tree1["a"], tree2["a"])
    expect_identical(tree1["b"], tree2["b"])
    expect_identical(tree1["c"], tree2["c"])
})