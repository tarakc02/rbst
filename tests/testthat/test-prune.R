library(rbst)
context("prune")

test_that("prune does not modify key-value pairs", {
    tree <- mockbst(100)
    tree2 <- prune(tree, "d", "i")
    for (key in keys(tree2)) {
        expect_equal(tree[key], tree2[key])
    }
})
