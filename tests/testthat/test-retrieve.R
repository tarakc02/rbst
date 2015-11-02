library(rbst)
context("retrieve")

test_that("retrieve works as expected", {
    tree <- bst()
    expect_null(retrieve(tree, "k"))
    tree["k"] <- 4
    expect_equal(tree["k"], 4)
    expect_null(retrieve(tree, "a"))
    expect_null(retrieve(tree, "n"))
})

test_that("retrieve gives correct results on hits and misses", {
    x <- structure(
        rnorm(1000),
        names = replicate(1000, paste(sample(letters, 9), collapse =""))
    )
    tree <- as.bst(x)
    searchstring <- "cdwgh" # can't be a key in x
    expect_null(retrieve(tree, searchstring))
    results <- vapply(names(x), function(key) retrieve(tree, key), FUN.VALUE = numeric(1))
    diff <- length(setdiff(results, x))
    expect_equal(diff, 0) 
    tree2 <- tree
    y <- structure(
        rnorm(100),
        names = replicate(100, paste(sample(letters, 9), collapse =""))
    )
    abc <- Map(function(key, value) tree2 <<- insert(tree2, key, value),
        names(y), y)
    results2 <- lapply(names(y), function(key) retrieve(tree2, key))
    results3 <- lapply(names(y), function(key) retrieve(tree, key))
    expect_true(all(sapply(results3, is.null)))
    expect_false(any(sapply(results2, is.null)))
    results_tree2 <- vapply(names(x), function(key) retrieve(tree2, key), FUN.VALUE = numeric(1))
    results_tree <- vapply(names(x), function(key) retrieve(tree, key), FUN.VALUE = numeric(1))
    expect_true(all(results_tree == results_tree2))
})

test_that("retrieve works as expected when multiple trees share nodes", {
    tree1 <- bst()
    tree1["B"] <- 4
    tree1["b"] <- 8
    tree1["a"] <- 5
    tree1["xyz"] <- 17
    tree1["cdef"] <- 55
    tree2 <- tree1
    expect_equal(tree1["b"], 8)
    expect_equal(tree2["b"], 8)
    tree2["b"] <- 85
    expect_equal(tree1["b"], 8)
    expect_equal(tree2["b"], 85)
    expect_true(tree1["B"] == tree2["B"])
    expect_true(tree1["a"] == tree2["a"])
    expect_true(tree1["xyz"] == tree2["xyz"])
    tree2["newkey"] <- 99
    expect_null(retrieve(tree1, "newkey"))
    expect_equal(retrieve(tree2, "newkey"), 99)
})

