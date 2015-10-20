library(rbst)
context("updating")

test_that("no side effects on insertions", {
    tree1 <- bst()
    tree2 <- tree1
    expect_true(identical(tree1, tree2))
    tree2 <- insert(tree2, "a", 4)
    expect_false(identical(tree1, tree2))
})

test_that("insertions are inserted", {
    tree1 <- bst()
    tree1["a"] <- "b"
    tree1["c"] <- 3
    tree1["4"] <- c(1, 2, 3)
    expect_equal(size(tree1), 3)
    tree1 <- insert(tree1, "abcdefg", "abc")
    expect_equal(size(tree1), 4)
})
