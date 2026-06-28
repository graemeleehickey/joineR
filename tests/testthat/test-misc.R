test_that("summary of joint objects", {
  data(heart.valve)
  heart.surv <- UniqueVariables(
    heart.valve,
    var.col = c("fuyrs", "status"),
    id.col = "num"
  )
  heart.long <- heart.valve[, c("num", "time", "log.lvmi")]
  heart.cov <- UniqueVariables(
    heart.valve,
    c("age", "hs", "sex"),
    id.col = "num"
  )
  heart.valve.jd <- jointdata(
    longitudinal = heart.long,
    baseline = heart.cov,
    survival = heart.surv,
    id.col = "num",
    time.col = "time"
  )
  fit <- joint(
    data = heart.valve.jd,
    long.formula = log.lvmi ~ 1 + time + hs,
    surv.formula = Surv(fuyrs, status) ~ hs,
    model = "int",
    tol = 1e-05
  )
  expect_output(str(summary(fit)), "List of 17")
  # variance = FALSE exercises the sqrt() + note branch
  expect_output(summary(fit, variance = FALSE), "standard deviations")
  # wrong class
  expect_error(summary.joint(list()), class = "simpleError")
})


test_that("bootstrap SE estimation print", {
  # load data + fit model
  data(heart.valve)
  heart.surv <- UniqueVariables(
    heart.valve,
    var.col = c("fuyrs", "status"),
    id.col = "num"
  )
  heart.long <- heart.valve[, c("num", "time", "log.lvmi")]
  heart.cov <- UniqueVariables(
    heart.valve,
    c("age", "hs", "sex"),
    id.col = "num"
  )
  heart.valve.jd <- jointdata(
    longitudinal = heart.long,
    baseline = heart.cov,
    survival = heart.surv,
    id.col = "num",
    time.col = "time"
  )
  fit <- joint(
    data = heart.valve.jd,
    long.formula = log.lvmi ~ 1 + time + hs,
    surv.formula = Surv(fuyrs, status) ~ hs,
    model = "intslope",
    tol = 1e-04
  )
  fit.boot <- jointSE(fitted = fit, n.boot = 3)
  # tests
  expect_s3_class(fit.boot, "data.frame")
})


test_that("tidy() and glance() for joint objects", {
  data(heart.valve)
  heart.surv <- UniqueVariables(
    heart.valve,
    var.col = c("fuyrs", "status"),
    id.col = "num"
  )
  heart.long <- heart.valve[, c("num", "time", "log.lvmi")]
  heart.cov <- UniqueVariables(
    heart.valve,
    c("age", "hs", "sex"),
    id.col = "num"
  )
  heart.valve.jd <- jointdata(
    longitudinal = heart.long,
    baseline = heart.cov,
    survival = heart.surv,
    id.col = "num",
    time.col = "time"
  )
  fit <- joint(
    data = heart.valve.jd,
    long.formula = log.lvmi ~ 1 + time + hs,
    surv.formula = Surv(fuyrs, status) ~ hs,
    model = "intslope",
    tol = 1e-04
  )

  # tidy() without SE
  td <- tidy(fit)
  expect_s3_class(td, "data.frame")
  expect_named(
    td,
    c("component", "term", "estimate", "std.error", "statistic", "p.value")
  )
  expect_equal(td$component[1], "longitudinal")
  expect_true(all(is.na(td$std.error)))
  expect_true(all(is.na(td$p.value)))

  # correct number of rows: 3 long + 1 surv + 1 assoc + 3 var (U_0, U_1, Resid)
  expect_equal(nrow(td), 8)

  # tidy() with SE (n.boot = 3 to keep fast; warns about < 100)
  suppressWarnings(
    se <- jointSE(fitted = fit, n.boot = 3)
  )
  td_se <- tidy(fit, se = se)
  expect_false(all(is.na(td_se$std.error)))
  # variance rows should still have NA p.value
  expect_true(all(is.na(td_se$p.value[td_se$component == "variance"])))

  # tidy() with conf.int
  td_ci <- tidy(fit, se = se, conf.int = TRUE)
  expect_named(
    td_ci,
    c(
      "component",
      "term",
      "estimate",
      "std.error",
      "statistic",
      "p.value",
      "conf.low",
      "conf.high"
    )
  )

  # glance()
  gl <- glance(fit)
  expect_s3_class(gl, "data.frame")
  expect_equal(nrow(gl), 1)
  expect_named(
    gl,
    c("logLik", "AIC", "BIC", "nobs", "nsubj", "convergence", "iter")
  )
  expect_equal(gl$nobs, 988)
  expect_equal(gl$nsubj, 256)
  expect_true(gl$convergence)
  expect_true(is.finite(gl$AIC))
  expect_true(gl$AIC > gl$logLik) # AIC = -2*ll + 2k, always > ll for positive k

  # wrong class
  expect_error(tidy.joint(list()), class = "simpleError")
  expect_error(glance.joint(list()), class = "simpleError")
})


test_that("summary of balanced data", {
  # load data + run summarybal
  data(mental)
  out <- summarybal(
    mental,
    Y.col = 2:7,
    times = c(0, 1, 2, 4, 6, 8),
    na.rm = TRUE
  )
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
  heart.surv <- UniqueVariables(
    heart.valve,
    var.col = c("fuyrs", "status"),
    id.col = "num"
  )
  heart.long <- heart.valve[, c("num", "time", "log.lvmi")]
  heart.cov <- UniqueVariables(
    heart.valve,
    c("age", "hs", "sex"),
    id.col = "num"
  )
  heart.valve.jd <- jointdata(
    longitudinal = heart.long,
    baseline = heart.cov,
    survival = heart.surv,
    id.col = "num",
    time.col = "time"
  )
  out <- summary(heart.valve.jd)
  # tests
  expect_output(str(out), "List of 5")
  expect_equal(out$subjects, "Number of subjects: 256")
  expect_output(str(out$longitudinal), "data.frame")
  expect_output(str(out$baseline), "data.frame")
  expect_identical(class(out$subjects), class(out$times))
})
