library(rbst)
context("delete")

del_tester <- function(tree) {
    keys <- unlist(keys(tree))
    results <- logical(length(keys))
    for (i in 1:length(keys)) {
        newtree <- delete(tree, keys[i])
        results[i] <- check(newtree) && 
            (setdiff(unlist(keys(tree)), unlist(keys(newtree))) == keys[i]) &&
            (length(setdiff(unlist(keys(tree)), keys)) == 0) &&
            !contains(newtree, keys[i])
    }
    all(results)
}


test_that("delete returns the correct objects", {
    a0 <- bst("a", "a")
    expect_true(is_empty(delete(a0, "a")))
    a1 <- bst(letters, letters)
    expect_true(del_tester(a1))
    a2 <- bst(rev(letters), rev(letters))
    expect_true(del_tester(a2))
    x1 <- mockbst(25)
    x2 <- mockbst(50)
    x3 <- mockbst(100)
    x4 <- mockbst(200)
    expect_true(del_tester(x1))
    expect_true(del_tester(x2))
    expect_true(del_tester(x3))
    expect_true(del_tester(x4))
})

test_that("delete_min deletes the min without affecting original tree", {
    x <- mockbst(25)
    ks <- unlist(keys(x))
    for (k in ks) {
        x1 <- delete_min(x)
        expect_false(contains(x1, k))
        expect_true(contains(x, k))
        expect_true(check(x1))
        expect_true(check(x))
        x <- x1
    }
})
