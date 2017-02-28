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
               longsep = FALSE, survsep = FALSE,
               gpt = 3)
  # tests
  expect_is(fit, "joint")
  expect_true(fit$convergence)
  expect_equal(unlist(fit$coefficients$fixed$longitudinal), 
               c(1.9730161778, 0.0003545542, -0.1380857344, 0.0006185851),
               tol = 1e-03, check.attributes = FALSE)
  expect_equal(unlist(fit$coefficients$fixed$survival1), 
               0.02723834,
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

