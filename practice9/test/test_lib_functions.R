if ("testthat" %in% rownames(installed.packages()) == FALSE) {
  install.packages("testthat")
}


library(testthat)


source("../src/lib_functions.R", chdir=TRUE)


test_that("Simple test", {
  expect_equal(out_of_trend(c(1, 2, 3, 4, 5)), c(0, 0, 0))
})
