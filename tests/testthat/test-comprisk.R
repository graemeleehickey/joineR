library(joineR)
context("Competing risks")


test_that("competing risks models", {
  # load data + fit model
  data(epileptic)
  epileptic$interaction <- with(epileptic, time * (treat == "LTG"))
  longitudinal <- epileptic[, c(1:3, 13)]
  survival <- UniqueVariables(epileptic, c(4, 6), "id")
  baseline <- UniqueVariables(epileptic, "treat", "id")
  data <- jointdata(longitudinal = longitudinal,
                    survival = survival,
                    baseline = baseline,
                    id.col = "id",
                    time.col = "time")
  fit <- joint(data = data, long.formula = dose ~ time + treat + interaction,
               surv.formula = Surv(with.time, with.status2) ~ treat,
               longsep = TRUE, survsep = TRUE,
               gpt = 3)
  # tests
  expect_is(fit, "joint")
  expect_true(fit$convergence)
  expect_equal(unlist(fit$coefficients$fixed$longitudinal), 
               c(1.9730161778, 0.0003545542, -0.1380857344, 0.0006185851),
               tol = 1e-03, check.attributes = FALSE)
  expect_equal(unlist(fit$coefficients$fixed$survival1), 
               0.02721022,
               tol = 1e-03, check.attributes = FALSE)
  expect_equal(unlist(fit$coefficients$fixed$survival2), 
               -0.6601709,
               tol = 1e-03, check.attributes = FALSE)  
  expect_equal(unlist(fit$coefficients$latent), 
               c(0.5894194, -0.9258503),
               tol = 1e-03, check.attributes = FALSE)
  expect_output(str(fit$sigma.u), "random.effects")
  expect_equal(dim(fit$sigma.u), c(2, 2))
  expect_output(str(fit$data), "List of 6")
})


test_that("bootstrap SEs with competing risks", {
  # load data + fit model
  data(epileptic)
  epileptic$interaction <- with(epileptic, time * (treat == "LTG"))
  longitudinal <- epileptic[, c(1:3, 13)]
  survival <- UniqueVariables(epileptic, c(4, 6), "id")
  baseline <- UniqueVariables(epileptic, "treat", "id")
  data <- jointdata(longitudinal = longitudinal,
                    survival = survival,
                    baseline = baseline,
                    id.col = "id",
                    time.col = "time")
  fit <- joint(data = data, long.formula = dose ~ time + treat + interaction,
               surv.formula = Surv(with.time, with.status2) ~ treat,
               longsep = FALSE, survsep = FALSE,
               gpt = 3)
  fit.boot <- jointSE(fit, n.boot = 2)
  # tests
  expect_output(str(fit.boot), "data.frame")
  expect_equal(dim(fit.boot), c(11, 6))
})


test_that("ordering of subjects with competing risks", {
  # load data + fit model (original data)
  data(epileptic)
  epileptic$interaction <- with(epileptic, time * (treat == "LTG"))
  longitudinal <- epileptic[, c(1:3, 13)]
  survival <- UniqueVariables(epileptic, c(4, 6), "id")
  baseline <- UniqueVariables(epileptic, "treat", "id")
  data <- jointdata(longitudinal = longitudinal,
                    survival = survival,
                    baseline = baseline,
                    id.col = "id",
                    time.col = "time")
  fit <- joint(data = data, long.formula = dose ~ time + treat + interaction,
               surv.formula = Surv(with.time, with.status2) ~ treat,
               longsep = FALSE, survsep = FALSE,
               gpt = 3)
  # load data + fit model (unordered data)
  longitudinal2 <- longitudinal[sample(nrow(longitudinal)), ]
  survival2 <- survival[sample(nrow(survival)), ]
  baseline2 <- baseline[sample(nrow(baseline)), ]
  data2 <- jointdata(longitudinal = longitudinal2,
                     survival = survival2,
                     baseline = baseline2,
                     id.col = "id",
                     time.col = "time")
  fit2 <- joint(data = data2, long.formula = dose ~ time + treat + interaction,
                surv.formula = Surv(with.time, with.status2) ~ treat,
                longsep = FALSE, survsep = FALSE,
                gpt = 3)
  # tests
  expect_identical(summary(data), summary(data2))
  expect_identical(fit$coefficients, fit2$coefficients)
  expect_identical(fit$sigma.z, fit2$sigma.z)
  expect_identical(fit$sigma.u, fit2$sigma.u)
  expect_identical(fit$haz.a, fit2$haz.a)
  expect_identical(fit$haz.b, fit2$haz.b)
})
