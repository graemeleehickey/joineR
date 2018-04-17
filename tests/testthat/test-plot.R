library(joineR)
context("Plotting")


test_that("plot jointdata objects (longitudinal)", {
  # load data
  data(heart.valve)
  heart.surv <- UniqueVariables(heart.valve,
                                var.col = c("fuyrs", "status"),
                                id.col = "num")
  heart.long <- heart.valve[, c(1, 4, 5, 7, 8, 9, 10, 11)]
  heart.jd <- jointdata(longitudinal = heart.long, 
                        survival = heart.surv,
                        id.col = "num",
                        time.col = "time")
  # tests
  expect_silent(plot(heart.jd, Y.col = "grad", col = "grey"))
  expect_silent(plot(heart.jd))
})


test_that("plot jointdata objects (joint plot)", {
  # load data
  data(heart.valve)
  heart.surv <- UniqueVariables(heart.valve, 
                                var.col = c("fuyrs", "status"),
                                id.col = "num")
  heart.long <- heart.valve[, c("num", "time", "log.lvmi")]
  heart.cov <- UniqueVariables(heart.valve, 
                               c("age", "sex"), 
                               id.col = "num")
  heart.valve.jd <- jointdata(longitudinal = heart.long, 
                              baseline = heart.cov, 
                              survival = heart.surv,
                              id.col = "num",
                              time.col = "time")
  # tests
  expect_output(str(jointplot(heart.valve.jd, Y.col = "log.lvmi", 
                              Cens.col = "status", lag = 5)),
                "trellis")
  expect_output(str(jointplot(heart.valve.jd, Y.col = "log.lvmi", 
                              Cens.col = "status", lag = 5,
                              mean.profile = TRUE, split = FALSE)),
                "trellis")
  expect_output(str(jointplot(heart.valve.jd, Y.col = "log.lvmi", 
                              Cens.col = "status", lag = 5,
                              mean.profile = FALSE, split = FALSE)),
                "trellis")
})


test_that("points + lines added to a plot of jointdata", {
  # data
  data(heart.valve)
  heart.surv <- UniqueVariables(heart.valve,
                                var.col = c("fuyrs", "status"),
                                id.col = "num")
  heart.long <- heart.valve[, c(1, 4, 5, 7, 8, 9, 10, 11)]
  heart.jd <- jointdata(longitudinal = heart.long,
                        survival = heart.surv,
                        id.col = "num",
                        time.col = "time")
  # randomly select a pair of subjects to plot profiles of
  set.seed(1)
  take <- sample(1 : max(heart.jd$survival$num), 2)
  heart.jd.1 <- subset(heart.jd, take[1])
  heart.jd.2 <- subset(heart.jd, take[2])
  ylim1 <- min(min(heart.jd.1$longitudinal$grad, na.rm = TRUE),
               min(heart.jd.2$longitudinal$grad, na.rm = TRUE))
  ylim2 <- max(max(heart.jd.1$longitudinal$grad, na.rm = TRUE),
               max(heart.jd.2$longitudinal$grad, na.rm = TRUE))  
  # main plot
  plot(heart.jd.1, Y.col = "grad", type = "p", ylim = c(ylim1, ylim2))
  # tests
  expect_silent(points(heart.jd.2, Y.col = "grad", col = "blue", pch = 20))
  expect_silent(lines(heart.jd.2, Y.col = "grad", col = "blue", pch = 20))
})


test_that("variogram plot", {
  # data + variogram
  data(mental)
  mental.unbalanced <- to.unbalanced(mental, id.col = 1, 
                                     times = c(0, 1, 2, 4, 6, 8),
                                     Y.col = 2:7, 
                                     other.col = c(8, 10, 11))
  names(mental.unbalanced)[3] <- "Y"
  vgm <- variogram(indv = tail(mental.unbalanced[, 1], 30),
                   time = tail(mental.unbalanced[, 2], 30),
                   Y = tail(mental.unbalanced[, 3], 30))
  # tests
  expect_silent(plot(vgm))
})
  