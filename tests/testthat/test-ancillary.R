library(joineR)
context("Ancillary functions work")

test_that("simjoint works", {
  # simulate data
  d <- simjoint(10, sepassoc = TRUE)
  # tests
  expect_output(str(d), "List of 2")
  expect_equal(dim(d$longitudinal), c(27, 7))
  expect_equal(dim(d$survival), c(10, 5))
})

test_that("simjoint works", {
  # load data + fit model
  data(mental)
  mental.unbalanced <- to.unbalanced(
    mental, id.col = 1, 
    times = c(0, 1, 2, 4, 6, 8),
    Y.col = 2:7, 
    other.col = c(8, 10, 11))
  names(mental.unbalanced)[3] <- "Y"
  vgm <- variogram(
    indv = tail(mental.unbalanced[, 1], 30),
    time = tail(mental.unbalanced[, 2], 30),
    Y = tail(mental.unbalanced[, 3], 30))
  # tests
  expect_output(str(vgm), "List of 2")
  expect_equal(dim(vgm$svar), c(75, 2))
  expect_equal(vgm$sigma2, 104, tol = 1e-03)
})
