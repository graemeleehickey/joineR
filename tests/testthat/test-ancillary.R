library(joineR)
context("Ancillary functions work")

test_that("simjoint works", {
  # simulate data
  set.seed(1)
  d <- simjoint(10, sepassoc = TRUE)
  # tests
  expect_output(str(d), "List of 2")
  expect_equal(dim(d$longitudinal)[2], 7)
  expect_equal(dim(d$survival), c(10, 5))
})

test_that("variogram works", {
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

test_that("to.unbalanced works", {
  # convert
  simul0 <- data.frame(
    num = 1:10,
    Y1.1 = rnorm(10), Y1.2 = rnorm(10),
    Y2.1 = rnorm(10), Y2.2 = rnorm(10),
    age = rnorm(10))
  simul1 <- to.unbalanced(
    simul0, 
    id.col = 1, 
    times = c(1, 2), 
    Y.col = 2:5, 
    other.col = 6)
  simul2 <- to.balanced(
    simul1, 
    id.col = "num", 
    time.col = "time",
    Y.col = c("Y1.1", "Y2.1"), 
    other.col = "age")
  # tests
  expect_equal(simul0, simul2, check.attributes = FALSE)
  expect_equal(dim(simul0), c(10, 6))
  expect_equal(dim(simul1), c(20, 5))
  expect_output(str(simul1), "data.frame")
  expect_output(str(simul2), "data.frame")
})


