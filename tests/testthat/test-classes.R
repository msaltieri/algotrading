context("test-classes.R")

test_that("Classes are implemented correctly", {
    expect_is(Deal$new(100, "buy"), "Deal")
})
