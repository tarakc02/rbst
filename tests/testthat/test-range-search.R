library(rbst)
context("range search")

test_that("keys_between", {
    tree <- bst()
    l <- keys_between(tree, 10, 30)
    expect_equal(length(l), 0)
    tree[15] <- ""
    tree[5] <- ""
    tree[10] <- 30
    l <- keys_between(tree, 10, 30)
    expect_equal(length(l), 2)
    tree[99] <- "abc"
    l <- keys_between(tree, 10, 30)
    expect_equal(length(l), 2)
    tree[25] <- 25
    l <- keys_between(tree, 10, 30)
    expect_equal(length(l), 3)    
})

test_that("floor and ceiling", {
    tree <- bst()
    tree[1] <- "a"
    expect_equal(floor_of(tree, 10), 1)
    expect_null(ceiling_of(tree, 10))
    tree[15] <- "b"
    expect_equal(ceiling_of(tree, 10), 15)
})