test_that("simulation", {
  # simulate data
  set.seed(1)
  d1 <- simjoint(10, sepassoc = TRUE)
  d2 <- simjoint(10, model = "int", gamma = 1)
  d3 <- simjoint(10, model = "quad", sepassoc = TRUE, gamma = 1:3)
  # tests
  expect_output(str(d1), "List of 2")
  expect_output(str(d2), "List of 2")
  expect_output(str(d3), "List of 2")
  expect_equal(dim(d1$longitudinal)[2], 7)
  expect_equal(dim(d2$longitudinal)[2], 7)
  expect_equal(dim(d3$longitudinal)[2], 7)
  expect_equal(dim(d1$survival), c(10, 5))
  expect_equal(dim(d2$survival), c(10, 5))
  expect_equal(dim(d3$survival), c(10, 5))
  expect_warning(simjoint(10, model = "intslop", sigu = 1))
})


test_that("variogram estimation", {
  # load data + fit model
  data(mental)
  mental.unbalanced <- to.unbalanced(
    mental,
    id.col = 1,
    times = c(0, 1, 2, 4, 6, 8),
    Y.col = 2:7,
    other.col = c(8, 10, 11)
  )
  names(mental.unbalanced)[3] <- "Y"
  vgm <- variogram(
    indv = tail(mental.unbalanced[, 1], 30),
    time = tail(mental.unbalanced[, 2], 30),
    Y = tail(mental.unbalanced[, 3], 30)
  )
  # tests
  expect_output(str(vgm), "List of 2")
  expect_equal(dim(vgm$svar), c(75, 2))
  expect_equal(vgm$sigma2, 104, tol = 1e-03)
})


test_that("conversion between balanced + unbalanced works", {
  simul0 <- data.frame(
    num = 1:10,
    Y1.1 = rnorm(10),
    Y1.2 = rnorm(10),
    Y2.1 = rnorm(10),
    Y2.2 = rnorm(10),
    age = rnorm(10)
  )
  simul1 <- to.unbalanced(
    simul0,
    id.col = 1,
    times = c(1, 2),
    Y.col = 2:5,
    other.col = 6
  )
  simul2 <- to.balanced(
    simul1,
    id.col = "num",
    time.col = "time",
    Y.col = c("Y1.1", "Y2.1"),
    other.col = "age"
  )
  expect_equal(simul0, simul2, check.attributes = FALSE)
  expect_equal(dim(simul0), c(10, 6))
  expect_equal(dim(simul1), c(20, 5))
  expect_output(str(simul1), "data.frame")
  expect_output(str(simul2), "data.frame")
  # character id.col exercises the else branch in to.unbalanced()
  simul1b <- to.unbalanced(
    simul0,
    id.col = "num",
    times = c(1, 2),
    Y.col = 2:5,
    other.col = 6
  )
  expect_equal(dim(simul1b), c(20, 5))
  # character Y.col exercises the !is.numeric branch
  simul1c <- to.unbalanced(
    simul0,
    id.col = 1,
    times = c(1, 2),
    Y.col = c("Y1.1", "Y1.2", "Y2.1", "Y2.2")
  )
  expect_equal(dim(simul1c), c(20, 4))
  # mismatched Y.col length triggers stop()
  expect_error(
    to.unbalanced(simul0, id.col = 1, times = c(1, 2), Y.col = 2:4),
    class = "simpleError"
  )
})


test_that("subset of jointdata", {
  data(heart.valve)
  heart.surv <- UniqueVariables(
    heart.valve,
    var.col = c("fuyrs", "status"),
    id.col = "num"
  )
  heart.long <- heart.valve[, c(1, 4, 5, 7, 8, 9, 10, 11)]
  heart.jd <- jointdata(
    longitudinal = heart.long,
    survival = heart.surv,
    id.col = "num",
    time.col = "time"
  )
  take <- heart.jd$survival$num[heart.jd$survival$status == 0]
  heart.jd.cens <- subset(heart.jd, take)
  expect_output(str(heart.jd.cens), "List of 6")
  expect_equal(dim(heart.jd.cens$longitudinal), c(827, 8))
  expect_equal(dim(heart.jd.cens$survival), c(202, 3))
  # wrong class triggers stop()
  expect_error(subset.jointdata(list(), subj.subset = 1), class = "simpleError")
})


test_that("jointdata without survival or baseline data", {
  data(heart.valve)
  heart.long <- heart.valve[, c(1, 4, 5, 7, 8, 9, 10, 11)]
  heart.jd <- jointdata(
    longitudinal = heart.long,
    id.col = "num",
    time.col = "time"
  )
  expect_output(str(heart.jd), "List of 6")
  expect_identical(heart.jd$survival, NA)
  expect_identical(heart.jd$baseline, NA)
})


test_that("sample.jointdata with and without replacement", {
  data(heart.valve)
  heart.surv <- UniqueVariables(
    heart.valve,
    var.col = c("fuyrs", "status"),
    id.col = "num"
  )
  heart.long <- heart.valve[, c("num", "time", "log.lvmi")]
  heart.cov <- UniqueVariables(heart.valve, c("age", "hs"), id.col = "num")
  heart.valve.jd <- jointdata(
    longitudinal = heart.long,
    baseline = heart.cov,
    survival = heart.surv,
    id.col = "num",
    time.col = "time"
  )
  # without replacement (existing test path)
  set.seed(4823)
  s1 <- sample.jointdata(heart.valve.jd, size = 20)
  expect_s3_class(s1, "jointdata")
  expect_equal(length(s1$subject), 20)
  # with replacement (exercises the replace = TRUE branch)
  set.seed(4823)
  s2 <- sample.jointdata(heart.valve.jd, size = 20, replace = TRUE)
  expect_s3_class(s2, "jointdata")
  expect_equal(length(s2$subject), 20)
  expect_equal(nrow(s2$survival), 20)
  expect_equal(nrow(s2$baseline), 20)
  # wrong class triggers stop()
  expect_error(sample.jointdata(list(), size = 5), class = "simpleError")
})
