library(joineR)
context("Print + summaries")


test_that("summary of joint objects", {
  # load data + fit model
  data(heart.valve)
  heart.surv <- UniqueVariables(heart.valve, 
                                var.col = c("fuyrs", "status"),
                                id.col = "num")
  heart.long <- heart.valve[, c("num", "time", "log.lvmi")]
  heart.cov <- UniqueVariables(heart.valve, 
                               c("age", "hs", "sex"), 
                               id.col = "num")
  heart.valve.jd <- jointdata(longitudinal = heart.long, 
                              baseline = heart.cov, 
                              survival = heart.surv, 
                              id.col = "num", 
                              time.col = "time")
  fit <- joint(data = heart.valve.jd, 
               long.formula = log.lvmi ~ 1 + time + hs, 
               surv.formula = Surv(fuyrs, status) ~ hs, 
               model = "int",
               tol = 1e-05)
  # tests
  expect_output(str(summary(fit)), "List of 17")
})


test_that("bootstrap SE estimation print", {
  # load data + fit model
  data(heart.valve)
  heart.surv <- UniqueVariables(heart.valve, 
                                var.col = c("fuyrs", "status"),
                                id.col = "num")
  heart.long <- heart.valve[, c("num", "time", "log.lvmi")]
  heart.cov <- UniqueVariables(heart.valve, 
                               c("age", "hs", "sex"), 
                               id.col = "num")
  heart.valve.jd <- jointdata(longitudinal = heart.long, 
                              baseline = heart.cov, 
                              survival = heart.surv, 
                              id.col = "num", 
                              time.col = "time")
  fit <- joint(data = heart.valve.jd, 
               long.formula = log.lvmi ~ 1 + time + hs, 
               surv.formula = Surv(fuyrs, status) ~ hs, 
               model = "intslope",
               tol = 1e-04)
  fit.boot <- jointSE(fitted = fit, n.boot = 3)
  # tests
  expect_s3_class(fit.boot, "data.frame")
})


test_that("summary of balanced data", {
  # load data + run summarybal
  data(mental)
  out <- summarybal(mental, Y.col = 2:7, times = c(0, 1, 2, 4, 6, 8), 
                    na.rm = TRUE)
  # tests
  expect_output(str(out), "List of 3")
  expect_equal(names(out), c("mean.vect", "variance", "cor.mtx"))
  expect_equal(dim(out$mean.vect), c(6, 2))
  expect_equal(dim(out$cor.mtx), c(6, 6))
  expect_equal(length(out$variance), 6)
})


test_that("summary of jointdata", {
  # make a jointdata object
  data(heart.valve)
  heart.surv <- UniqueVariables(heart.valve, 
                                var.col = c("fuyrs", "status"),
                                id.col = "num")
  heart.long <- heart.valve[, c("num", "time", "log.lvmi")]
  heart.cov <- UniqueVariables(heart.valve, 
                               c("age", "hs", "sex"), 
                               id.col = "num")
  heart.valve.jd <- jointdata(longitudinal = heart.long, 
                              baseline = heart.cov, 
                              survival = heart.surv, 
                              id.col = "num", 
                              time.col = "time")
  out <- summary(heart.valve.jd)
  # tests
  expect_output(str(out), "List of 5")
  expect_equal(out$subjects, "Number of subjects: 256")
  expect_output(str(out$longitudinal), "data.frame")
  expect_output(str(out$baseline), "data.frame")
  expect_identical(class(out$subjects), class(out$times))
})
