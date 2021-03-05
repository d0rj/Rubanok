if ("testthat" %in% rownames(installed.packages()) == FALSE) {
  install.packages("testthat")
}


library(testthat)


source("../src/lib_functions.R", chdir=TRUE)


test_that("out_of_trend: Simple test for arifm proportion", {
  expect_equal(out_of_trend(c(1, 2, 3, 4, 5)), c(0, 0, 0))
  expect_equal(out_of_trend(c(1, 2, 1, 2, 1)), c(-0.2876821, 0.2876821, -0.2876821), tolerance=1e-7)
  expect_equal(out_of_trend(c(10, 20, 40, 80, 160)), c(0.1495317, 0.1790482, 0.1986707), tolerance=1e-7)
})


test_that("out_of_trend: Simple test for geom proportion", {
  expect_equal(out_of_trend(c(1, 2, 3, 4, 5), method = "Geom"), c(-0.06453852, -0.04082199, -0.02817088), tolerance=1e-7)
  expect_equal(out_of_trend(c(1, 2, 1, 2, 1), method = "Geom"), c(-0.5753641, 0.5753641, -0.5753641), tolerance=1e-7)
  expect_equal(out_of_trend(c(1, 2, 4, 8, 16), method = "Geom"), c(0.11778304, 0.10536052, 0.07696104), tolerance=1e-7)
})


test_that("out_of_trend: Simple test for garm proportion", {
  expect_equal(out_of_trend(c(1, 2, 3, 4, 5), method = "Garm"), c(-0.06453852, -0.04082199, -0.02817088), tolerance=1e-7)
  expect_equal(out_of_trend(c(100, 500, 300, 10, 5), method = "Garm"), c(-1.1673886, -2.2889801, 0.2831371), tolerance=1e-7)
  expect_equal(out_of_trend(c(1, 20, 400, 8000, 160000), method = "Garm"), c(-1.306718, -2.215008, -2.300357), tolerance=1e-7)
})
