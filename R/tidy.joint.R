#' Tidy a joint model fit
#'
#' @description Summarises parameter estimates from a fitted \code{joint} model
#'   into a tidy \code{data.frame}, one row per term. Follows the conventions
#'   of the \pkg{broom} package.
#'
#' @param x an object of class \code{joint}.
#' @param se a \code{data.frame} returned by \code{\link{jointSE}},
#'   optionally supplied to include standard errors, Wald p-values, and
#'   confidence intervals in the output. If \code{NULL} (the default), these
#'   columns are included but set to \code{NA}.
#' @param conf.int logical; if \code{TRUE} and \code{se} is provided, include
#'   \code{conf.low} and \code{conf.high} columns. Defaults to \code{FALSE}.
#' @param conf.level the confidence level for the intervals, matching the
#'   level used in \code{se}. Defaults to \code{0.95}.
#' @param ... additional arguments (currently unused).
#'
#' @return A \code{data.frame} with columns:
#'   \item{\code{component}}{sub-model: \code{"longitudinal"},
#'     \code{"survival"}, \code{"association"}, or \code{"variance"}.}
#'   \item{\code{term}}{parameter name.}
#'   \item{\code{estimate}}{point estimate.}
#'   \item{\code{std.error}}{bootstrap standard error (\code{NA} if \code{se}
#'     not supplied).}
#'   \item{\code{statistic}}{Wald z-statistic (\code{NA} if \code{se} not
#'     supplied or for variance components).}
#'   \item{\code{p.value}}{two-sided Wald p-value (\code{NA} if \code{se} not
#'     supplied or for variance components).}
#'   \item{\code{conf.low}, \code{conf.high}}{bootstrap percentile confidence
#'     bounds, included only when \code{conf.int = TRUE} and \code{se} is
#'     provided.}
#'
#' @importFrom generics tidy
#' @export
#' @seealso \code{\link{glance.joint}}, \code{\link{joint}},
#'   \code{\link{jointSE}}
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
#'              model = "intslope")
#' tidy(fit)
tidy.joint <- function(x, se = NULL, conf.int = FALSE, conf.level = 0.95, ...) {
  if (!inherits(x, "joint")) {
    stop("'x' must be of class 'joint'")
  }

  # --- build parameter table from the joint object ---

  # longitudinal fixed effects (single-column data.frame, row names = terms)
  long_coef <- x$coefficients$fixed$longitudinal[, 1]
  long_df <- data.frame(
    component = "longitudinal",
    term = row.names(x$coefficients$fixed$longitudinal),
    estimate = unname(long_coef),
    stringsAsFactors = FALSE
  )

  # survival fixed effects (standard or competing risks)
  if (x$compRisk) {
    surv1 <- x$coefficients$fixed$survival1
    surv2 <- x$coefficients$fixed$survival2
    surv_df <- data.frame(
      component = c(
        rep("survival (cause 1)", length(surv1)),
        rep("survival (cause 2)", length(surv2))
      ),
      term = c(names(surv1), names(surv2)),
      estimate = c(unname(surv1), unname(surv2)),
      stringsAsFactors = FALSE
    )
  } else {
    surv_coef <- x$coefficients$fixed$survival
    surv_df <- data.frame(
      component = "survival",
      term = names(surv_coef),
      estimate = unname(surv_coef),
      stringsAsFactors = FALSE
    )
  }

  # latent association(s)
  lat <- x$coefficients$latent
  lat_df <- data.frame(
    component = "association",
    term = names(lat),
    estimate = unname(lat),
    stringsAsFactors = FALSE
  )

  # variance components (diagonal of sigma.u + residual)
  q <- nrow(x$sigma.u)
  var_names <- c(paste0("U_", seq(0, q - 1)), "Residual")
  var_vals <- c(diag(x$sigma.u), x$sigma.z)
  var_df <- data.frame(
    component = "variance",
    term = var_names,
    estimate = unname(var_vals),
    stringsAsFactors = FALSE
  )

  out <- rbind(long_df, surv_df, lat_df, var_df)

  # --- merge SE / p-value info from jointSE output if supplied ---
  out$std.error <- NA_real_
  out$statistic <- NA_real_
  out$p.value <- NA_real_

  if (!is.null(se)) {
    if (
      !is.data.frame(se) || !all(c("Parameter", "SE", "p-value") %in% names(se))
    ) {
      stop("'se' must be a data.frame returned by jointSE()")
    }
    # match on term name; variance components have NA p-value by design
    idx <- match(out$term, se$Parameter)
    out$std.error <- se$SE[idx]
    out$statistic <- ifelse(
      is.na(se[["p-value"]][idx]),
      NA_real_,
      out$estimate / se$SE[idx]
    )
    out$p.value <- se[["p-value"]][idx]

    if (conf.int) {
      out$conf.low <- se[["95%Lower"]][idx]
      out$conf.high <- se[["95%Upper"]][idx]
    }
  }

  row.names(out) <- NULL
  out
}


#' Glance at a joint model fit
#'
#' @description Returns a single-row \code{data.frame} of model-level
#'   statistics for a fitted \code{joint} model. Follows the conventions of
#'   the \pkg{broom} package.
#'
#' @param x an object of class \code{joint}.
#' @param ... additional arguments (currently unused).
#'
#' @return A one-row \code{data.frame} with columns:
#'   \item{\code{logLik}}{joint log-likelihood at convergence.}
#'   \item{\code{AIC}}{Akaike information criterion.}
#'   \item{\code{BIC}}{Bayesian information criterion.}
#'   \item{\code{nobs}}{total number of longitudinal observations.}
#'   \item{\code{nsubj}}{number of subjects.}
#'   \item{\code{convergence}}{logical; whether the EM algorithm converged.}
#'   \item{\code{iter}}{number of EM iterations taken.}
#'
#' @importFrom generics glance
#' @export
#' @seealso \code{\link{tidy.joint}}, \code{\link{joint}}
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
#'              model = "intslope")
#' glance(fit)
glance.joint <- function(x, ...) {
  if (!inherits(x, "joint")) {
    stop("'x' must be of class 'joint'")
  }

  ll <- x$loglik$jointlhood

  # count estimable parameters: longitudinal fixed + survival fixed +
  # latent associations + variance components (diag sigma.u + sigma.z)
  n_long <- length(x$coefficients$fixed$longitudinal[, 1])
  if (x$compRisk) {
    n_surv <- length(x$coefficients$fixed$survival1) +
      length(x$coefficients$fixed$survival2)
  } else {
    n_surv <- length(x$coefficients$fixed$survival)
  }
  n_lat <- length(x$coefficients$latent)
  n_var <- nrow(x$sigma.u) + 1L # diag(sigma.u) + sigma.z
  k <- n_long + n_surv + n_lat + n_var

  nobs <- nrow(x$data$longitudinal)
  nsubj <- nrow(x$data$survival)

  data.frame(
    logLik = ll,
    AIC = -2 * ll + 2 * k,
    BIC = -2 * ll + k * log(nsubj),
    nobs = nobs,
    nsubj = nsubj,
    convergence = x$convergence,
    iter = x$numIter,
    stringsAsFactors = FALSE
  )
}
