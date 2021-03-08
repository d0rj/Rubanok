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


test_that("out_of_trend: equality to proportions", {
  expect_equal(out_of_trend(c(1, 2, 3, 4, 5)), arifmProportion(c(1, 2, 3, 4, 5)))
  expect_equal(out_of_trend(c(1, 2, 3, 4, 5), dt = 2), arifmProportion(c(1, 2, 3, 4, 5), dt = 2))
  
  
  expect_equal(out_of_trend(c(1, 2, 3, 4, 5), method = "Geom"), geomProportion(c(1, 2, 3, 4, 5)))
  expect_equal(out_of_trend(c(1, 2, 3, 4, 5), dt = 2, method = "Geom"), geomProportion(c(1, 2, 3, 4, 5), dt = 2))
  
  expect_equal(out_of_trend(c(1, 2, 3, 4, 5), method = "Garm"), garmProportion(c(1, 2, 3, 4, 5)))
  expect_equal(out_of_trend(c(1, 2, 3, 4, 5), dt = 2, method = "Garm"), garmProportion(c(1, 2, 3, 4, 5), dt = 2))
})


test_that("out_of_trend: arguments boundary cases", {
  expect_error(out_of_trend(10))
  expect_error(out_of_trend(c()))
  expect_error(out_of_trend(c(1)))
  expect_error(out_of_trend(c(1, 2)))
  
  expect_error(out_of_trend(c(1, 2, 3, 4, 5), dt = 3))
  
  expect_error(out_of_trend(c(10, 20, 10, 20, 10), method = "Some beleberda"))
})


test_that("solve: Simple tests", {
  expect_equal(solve(rbind(c(1, 0), c(0, 1)), c(2, 3), c(1, 1)), c(2, 3), tolerance = 1e-7)
  expect_equal(solve(rbind(c(1, 0), c(0, 1)), c(2, 3), rnorm(2)), c(2, 3), tolerance = 1e-7)
  
  expect_equal(solve(rbind(c(2, 0), c(0, 2)), c(4, 6), c(1, 1)), c(2, 3), tolerance = 1e-7)
  expect_equal(solve(rbind(c(2, 0), c(0, 2)), c(4, 6), rnorm(2)), c(2, 3), tolerance = 1e-7)
})


test_that("solve: arguments boundary cases", {
  expect_error(solve(c(1, 2, 3), c(1, 2, 3), c(1, 2, 3)))
  expect_error(solve(c(1, 2, 3), 1, c(1, 2, 3)))
  expect_error(solve(c(1, 2, 3), c(1, 2, 3), 1))
  
  expect_error(solve(rbind(c(1, 0), c(0, 1)), c(1, 2, 3), c(1, 1)))
  expect_error(solve(rbind(c(1, 0), c(0, 1)), c(2, 3), c(1, 1, 1)))
  
  expect_error(solve(rbind(c(1, 0), c(0, 1)), c(2, 3), c(1, 1), count = 0))
  expect_error(solve(rbind(c(1, 0), c(0, 1)), c(2, 3), c(1, 1), eps = -1))
  
  expect_error(solve(rbind(c(2, 1), c(1, -2)), c(2, 3), c(1, 1)))
  expect_error(solve(rbind(c(10, 2, -1), c(-2, -6, -1), c(1, -3, 12)), c(5, 24.42, 36), rnorm(3)))
})


test_that("Alter-Johns: arguments boundary cases", {
  expect_error(AlterJohns("HELP"))
  expect_error(AlterJohns(c("H", "E", "L", "P", "!!!")))
})
