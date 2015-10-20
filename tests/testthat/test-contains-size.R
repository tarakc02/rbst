library(rbst)
context("size and contains")

test_that("size works as expected", {
    tree <- bst()
    expect_true(is_empty(tree))
    tree <- insert(tree, 1234, 1)
    tree <- insert(tree, 12, 8)
    tree <- insert(tree, 15, 9)
    expect_false(is_empty(tree))
    expect_equal(size(tree), 3)
    tree[1888] <- 997
    expect_equal(size(tree), 4)
})

test_that("contains works", {
    tree <- bst()
    expect_false(contains(tree, "a"))
    tree["a"] <- 5
    expect_true(contains(tree, "a"))
})

