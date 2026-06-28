test_that("random-intercept and random-slope models", {
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
    tol = 1e-05
  )
  # tests
  expect_s3_class(fit, "joint")
  expect_true(fit$convergence)
  expect_equal(
    unlist(fit$coefficients$fixed$longitudinal),
    c(4.993674622, -0.006744282, 0.055565458),
    tol = 1e-03,
    check.attributes = FALSE
  )
  expect_equal(
    unlist(fit$coefficients$fixed$survival),
    0.8097966,
    tol = 1e-03,
    check.attributes = FALSE
  )
  expect_equal(
    unlist(fit$coefficients$latent),
    0.8544206,
    tol = 1e-03,
    check.attributes = FALSE
  )
  expect_output(str(fit$sigma.u), "random.effects")
  expect_equal(dim(fit$sigma.u), c(2, 2))
  expect_output(str(fit$data), "List of 6")
})


test_that("no survival model covariates", {
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
    surv.formula = Surv(fuyrs, status) ~ 1,
    model = "intslope",
    tol = 1e-05
  )
  # tests
  expect_s3_class(fit, "joint")
  expect_null(fit$coefficients$fixed$survival)
})


test_that("random-intercept only models", {
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
    model = "int",
    tol = 1e-05
  )
  # tests
  expect_s3_class(fit, "joint")
  expect_true(fit$convergence)
  expect_equal(
    unlist(fit$coefficients$fixed$longitudinal),
    c(4.984628937, 0.000112953, 0.051343555),
    tol = 1e-03,
    check.attributes = FALSE
  )
  expect_equal(
    unlist(fit$coefficients$fixed$survival),
    0.8092167,
    tol = 1e-03,
    check.attributes = FALSE
  )
  expect_equal(
    unlist(fit$coefficients$latent),
    1.134799,
    tol = 1e-03,
    check.attributes = FALSE
  )
  expect_output(str(fit$sigma.u), "random.effects")
  expect_equal(dim(fit$sigma.u), c(1, 1))
  expect_output(str(fit$data), "List of 6")
})


test_that("quadratic models", {
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
    model = "quad",
    tol = 1e-04
  )
  # tests
  expect_s3_class(fit, "joint")
  expect_true(fit$convergence)
  expect_equal(
    unlist(fit$coefficients$fixed$longitudinal),
    c(4.9870206, -0.001215202, 0.059136863),
    tol = 1e-03,
    check.attributes = FALSE
  )
  expect_equal(
    unlist(fit$coefficients$fixed$survival),
    0.8195572,
    tol = 1e-03,
    check.attributes = FALSE
  )
  expect_equal(
    unlist(fit$coefficients$latent),
    1.001866,
    tol = 1e-03,
    check.attributes = FALSE
  )
  expect_output(str(fit$sigma.u), "random.effects")
  expect_equal(dim(fit$sigma.u), c(3, 3))
  expect_output(str(fit$data), "List of 6")
})


test_that("separate association models", {
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
    sepassoc = TRUE,
    tol = 1e-04
  )
  # tests
  expect_s3_class(fit, "joint")
  expect_true(fit$convergence)
  expect_equal(
    unlist(fit$coefficients$latent),
    c(0.7926382, -2.0466059),
    tol = 1e-03,
    check.attributes = FALSE
  )
})


test_that("bootstrap SE", {
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
  expect_output(str(fit.boot), "data.frame")
  expect_equal(dim(fit.boot), c(8, 7))
})


test_that("jointdata throws errors for incorrect data", {
  # data
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
  # tests
  expect_error(
    jointdata(
      longitudinal = 1,
      baseline = heart.cov,
      survival = heart.surv,
      id.col = "num",
      time.col = "time"
    ),
    "Longitudinal object must be a matrix or a data.frame"
  )
  expect_error(
    jointdata(
      longitudinal = heart.long,
      baseline = 1,
      survival = heart.surv,
      id.col = "num",
      time.col = "time"
    ),
    "Baseline object must be a matrix or a data.frame or a vector of names of baseline covariates"
  )
  expect_error(
    jointdata(
      longitudinal = heart.long,
      baseline = heart.cov,
      survival = 1,
      id.col = "num",
      time.col = "time"
    ),
    "Survival object must be a matrix or a data.frame"
  )
  expect_error(
    jointdata(
      longitudinal = heart.long,
      baseline = heart.cov,
      survival = heart.surv,
      id.col = NA,
      time.col = "time"
    ),
    "It is necessary to specify a subject identification column name"
  )
  names(heart.cov)[1] <- "Num" # case change
  expect_error(
    jointdata(
      longitudinal = heart.long,
      baseline = heart.cov,
      survival = heart.surv,
      id.col = "num",
      time.col = "time"
    ),
    "ID column does not exist in baseline object"
  )
  names(heart.cov)[1] <- "num"
  names(heart.long)[1] <- "Num"
  expect_error(
    jointdata(
      longitudinal = heart.long,
      baseline = heart.cov,
      survival = heart.surv,
      id.col = "num",
      time.col = "time"
    ),
    "ID column does not exist in longitudinal object"
  )
  names(heart.long)[1] <- "num"
  names(heart.surv)[1] <- "Num"
  expect_error(
    jointdata(
      longitudinal = heart.long,
      baseline = heart.cov,
      survival = heart.surv,
      id.col = "num",
      time.col = "time"
    ),
    "ID column does not exist in survival object"
  )
})
