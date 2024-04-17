library(testdat)

test_that("aid is unique", {
    data <- fread("../output/interm_data/data.csv")
    expect_unique(data = data, c(aid))
})

test_that("year has correct range", {
    data <- fread("../output/interm_data/data.csv")
    expect_range(data = data, year, 1991, 1996)
})

test_that("gpa has correct range", {
    data <- fread("../output/interm_data/data.csv")
    expect_range(data = data, gpa, 0, 4)
})

test_that("grad has correct values", {
    data <- fread("../output/interm_data/data.csv")
    expect_values(data = data, grad, c(0, 1))
})