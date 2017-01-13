library(joineR)
context("Print + summaries work")

test_that("summary of joint object works", {
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
  expect_output(str(summary(fit)), "List of 16")
})

test_that("bootstrap SE prints", {
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


