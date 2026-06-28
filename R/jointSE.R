#' Standard errors via bootstrap for a joint model fit
#'
#' @description This function takes a model fit from a joint model and
#'   calculates standard errors, with optional confidence intervals, for the
#'   main longitudinal and survival covariates.
#'
#' @param fitted a list containing as components the parameter estimates
#'   obtained by fitting a joint model along with the respective formulae for
#'   the longitudinal and survival sub-models and the model chosen, see
#'   \code{joint} for further details.
#' @param n.boot the number of bootstrap samples used to compute standard errors
#'   and confidence intervals. A minimum of 100 is recommended for reliable
#'   confidence intervals; fewer samples will trigger a warning.
#' @inheritParams joint
#' @param print.detail This argument determines the level of printing that is
#'   done during the bootstrapping. If \code{TRUE} then the parameter estimates
#'   from each bootstrap sample are output.
#'
#' @details Standard errors and confidence intervals are obtained by repeated
#'   fitting of the requisite joint model to bootstrap samples of the original
#'   longitudinal and survival data. It is rare that more than 200 bootstrap
#'   samples are needed for estimating a standard error. Confidence intervals
#'   use the percentile method and are computed for all \code{n.boot} values,
#'   though fewer than 100 samples will trigger a warning about reliability.
#'   Two-sided Wald p-values are computed as \eqn{2\Phi(-|\hat\theta /
#'   \widehat{SE}|)} for all fixed-effect and association parameters. P-values
#'   are \code{NA} for variance components (\code{U_*} and \code{Residual})
#'   because Wald tests are not appropriate for variance parameters constrained
#'   to be positive.
#'
#' @author Ruwanthi Kolamunnage-Dona and Pete Philipson
#' @keywords models survival htest
#' @seealso \code{\link[nlme]{lme}}, \code{\link[survival]{coxph}},
#'   \code{\link{joint}}, \code{\link{jointdata}}.
#'
#' @return An object of class \code{data.frame} with columns \code{Component},
#'   \code{Parameter}, \code{Estimate}, \code{SE}, \code{p-value},
#'   \code{95\%Lower}, and \code{95\%Upper}.
#' @export
#'
#' @references
#'
#' Wulfsohn MS, Tsiatis AA. A joint model for survival and longitudinal data
#' measured with error. \emph{Biometrics.} 1997; \strong{53(1)}: 330-339.
#'
#' Efron B, Tibshirani R. \emph{An Introduction to the Bootstrap.} 2000; Boca
#' Raton, FL: Chapman & Hall/CRC.
#'
#' @examples
#' data(heart.valve)
#' heart.surv <- UniqueVariables(heart.valve,
#'                               var.col = c("fuyrs", "status"),
#'                               id.col = "num")
#' heart.long <- heart.valve[, c("num", "time", "log.lvmi")]
#' heart.cov <- UniqueVariables(heart.valve,
#'                              c("age", "hs", "sex"),
#'                              id.col = "num")
#' heart.valve.jd <- jointdata(longitudinal = heart.long,
#'                             baseline = heart.cov,
#'                             survival = heart.surv,
#'                             id.col = "num",
#'                             time.col = "time")
#' fit <- joint(heart.valve.jd,
#'              long.formula = log.lvmi ~ 1 + time + hs,
#'              surv.formula = Surv(fuyrs, status) ~ hs,
#'              model = "int")
#' jointSE(fitted = fit, n.boot = 1)
jointSE <- function(
  fitted,
  n.boot,
  gpt = 3,
  lgpt = 10,
  max.it = 200,
  tol = 0.001,
  print.detail = FALSE
) {
  if (!inherits(fitted, "joint")) {
    stop("Fitted model must be of class 'joint'\n")
  }

  data <- fitted$data
  id <- fitted$data$subj.col
  time.long <- fitted$data$time.col
  q <- length(diag(fitted$sigma.u))
  b2.names <- NULL
  if (fitted$compRisk) {
    b2.est <- c(
      fitted$coefficients$fixed$survival1,
      fitted$coefficients$fixed$survival2
    )
    b2.names <- c(
      names(fitted$coefficients$fixed$survival1),
      names(fitted$coefficients$fixed$survival2)
    )
  } else {
    b2.est <- fitted$coefficients$fixed$survival
    b2.names <- names(fitted$coefficients$fixed$survival)
  }

  paranames <- c(
    row.names(fitted$coefficients$fixed$longitudinal),
    b2.names,
    names(fitted$coefficients$latent),
    paste("U_", 0:(q - 1), sep = ""),
    "Residual"
  )

  mles <- c(
    fitted$coefficients$fixed$longitudinal[, 1],
    b2.est,
    fitted$coefficients$latent,
    diag(fitted$sigma.u),
    fitted$sigma.z
  )

  compnames <- rep("", length(paranames))
  compnames[1] <- "Longitudinal"
  lb1 <- length(fitted$coefficients$fixed$longitudinal[, 1])
  lb2.a <- ifelse(
    fitted$compRisk,
    length(fitted$coefficients$fixed$survival1),
    length(fitted$coefficients$fixed$survival)
  )
  lb2.b <- ifelse(
    fitted$compRisk,
    length(fitted$coefficients$fixed$survival2),
    0
  )
  lg <- length(fitted$coefficients$latent)
  if (fitted$compRisk) {
    compnames[lb1 + 1] <- "Failure (cause 1)"
    compnames[lb1 + lb2.a + 1] <- "Failure (cause 2)"
  } else {
    compnames[lb1 + 1] <- "Failure"
  }
  compnames[lb1 + lb2.a + lb2.b + 1] <- "Association"
  compnames[lb1 + lb2.a + lb2.b + lg + 1] <- "Variance"

  model <- fitted$model
  surv.formula <- fitted$formulae$sformula
  long.formula <- fitted$formulae$lformula
  sepassoc <- fitted$sepassoc

  n.est <- length(paranames)
  nsubj <- length(fitted$data$subject)
  out <- matrix(0, n.boot, n.est)

  for (i in 1:n.boot) {
    # start bootstrap loop here

    s.new <- sample.jointdata(data, nsubj, replace = TRUE)
    suppressMessages(
      fitb <- joint(
        data = s.new,
        long.formula = long.formula,
        surv.formula = surv.formula,
        model = model,
        sepassoc = sepassoc,
        gpt = gpt,
        max.it = max.it,
        tol = tol,
        lgpt = lgpt
      )
    )

    b1 <- as.numeric(as.vector(as.matrix(fitb$coefficients$fixed$longitudinal[,
      1
    ])))
    b3 <- as.numeric(as.vector(as.matrix(fitb$coefficients$latent)))
    b4 <- as.numeric(as.vector(as.matrix(diag(fitb$sigma.u))))
    b5 <- as.numeric(as.vector(as.matrix(fitb$sigma.z)))

    if (lb2.a > 0) {
      if (fitted$compRisk) {
        b2 <- c(
          as.numeric(as.vector(as.matrix(fitb$coefficients$fixed$survival1))),
          as.numeric(as.vector(as.matrix(fitb$coefficients$fixed$survival2)))
        )
      } else {
        b2 <- as.numeric(as.vector(as.matrix(fitb$coefficients$fixed$survival)))
      }
      out[i, ] <- c(b1, b2, b3, b4, b5)
      ests <- out[i, ]
    } else {
      out[i, ] <- c(b1, b3, b4, b5)
      ests <- out[i, ]
    }

    if (print.detail) {
      detail <- data.frame(iteration = i, t(ests))
      names(detail) <- c("Iteration", paranames)
      print(detail)
    }
  } # end of bootstrap loop

  if (n.boot == 1) {
    out <- matrix(out, nrow = 1)
  }
  se <- apply(out, 2, function(x) sqrt(var(as.numeric(x))))
  if (n.boot < 100) {
    warning(
      "Fewer than 100 bootstrap samples: confidence intervals may be unreliable"
    )
  }
  ci1 <- apply(out, 2, function(x) quantile(as.numeric(x), probs = 0.025))
  ci2 <- apply(out, 2, function(x) quantile(as.numeric(x), probs = 0.975))

  b1 <- data.frame(compnames, paranames)

  # Two-sided Wald p-values for fixed effects and association parameters.
  # Variance component rows (U_* and Residual) get NA: a Wald test is not
  # meaningful for variance parameters constrained to be positive.
  n_var <- q + 1 # number of variance rows (random effects + residual)
  n_fixed <- length(paranames) - n_var
  pval <- c(
    2 * pnorm(-abs(mles[seq_len(n_fixed)] / se[seq_len(n_fixed)])),
    rep(NA_real_, n_var)
  )

  b1 <- cbind(
    b1,
    round(mles, 4),
    round(cbind(se), 4),
    round(pval, 4),
    round(ci1, 4),
    round(ci2, 4)
  )

  colnames(b1)[1:7] <- c(
    "Component",
    "Parameter",
    "Estimate",
    "SE",
    "p-value",
    "95%Lower",
    "95%Upper"
  )
  return(b1)
}
