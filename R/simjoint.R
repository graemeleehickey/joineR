#' Simulate data from a joint model
#' 
#' This function simulates longitudinal and time-to-event data from a joint
#' model.
#' 
#' @param n the number of subjects to simulate data for.
#' @inheritParams joint
#' @param ntms the maximum number of (discrete) time points to simulate repeated
#'   longitudinal measurements at.
#' @param b1 a vector specifying the coefficients of the fixed effects in the
#'   longitudinal sub-model. The order in each row is intercept, a continuous 
#'   covariate, covariate, a binary covariate, the measurement time.
#' @param b2 a vector of \code{length = 2} specifying the coefficients for the
#'   time-to-event baseline covariates, in the order of a continuous covariate
#'   and a binary covariate.
#' @param gamma a vector of specifying the latent association parameter(s) for
#'   the longitudinal outcome. It must be of length 1 if \code{sepassoc =
#'   FALSE}.
#' @param sigu a positive-definite matrix specifying the variance-covariance 
#'   matrix. If \code{model = "int"}, the matrix has dimension \code{dim = c(1,
#'   1)}; if \code{model = "intslope"}, the matrix has dimension \code{dim = 
#'   c(2, 2)}; else if \code{model = "quad"}, the matrix has dimension \code{dim
#'   = c(3, 3)}. If \code{D = NULL} (default), an identity matrix is assumed.
#' @param vare a numeric value specifying the residual standard error.
#' @param theta0 the log-scale parameter for a Gompertz distribution used to 
#'   simulate the time-to-event outcome. The scale is calculated as 
#'   \eqn{\exp(\theta_0)} to ensure it is positive.
#' @param theta1 the shape parameter for a Gompertz distribution used to 
#'   simulate the time-to-event outcome.
#' @param censoring logical: if \code{TRUE}, includes an independent censoring 
#'   time.
#' @param censlam a scale (\eqn{>0}) parameter for an exponential distribution 
#'   used to simulate random censoring times for when \code{censoring = TRUE}.
#' @param truncation logical: if \code{TRUE}, adds a truncation time for a 
#'   maximum event time.
#' @param trunctime a truncation time for use when \code{truncation = TRUE}.
#' @param gridstep the step-size for the grid used to simulate event times when
#'   \code{model = "quad"}. Default is \code{gridstep = 0.01}.
#'   
#' @details The function \code{simData} simulates data from a joint model, 
#'   similar to that performed in Henderson et al. (2000). It works by first 
#'   simulating longitudinal data for all possible follow-up times using random
#'   draws for the multivariate Gaussian random effects and residual error
#'   terms. Data can be simulated assuming either random-intercepts only in each
#'   of the longitudinal sub-models; random-intercepts and random-slopes; or
#'   quadratic random effects structures. The failure times are simulated from
#'   proportional hazards time-to-event models; either an exponential
#'   distribution (in the case of random-intercepts models) or Gompertz
#'   distribution (random-intercept and random-slopes models) conditional on
#'   either known baseline effects (\code{model = "int"}), or a predictable
#'   time-varying process (\code{model = "intslope"}). In the case of the
#'   former, the methodology of Bender et al. (2005) is used to simulate the
#'   time, whilst in the case of latter, the approach of Austin (2012) is used.
#'   
#'   TODO: add something on the \code{model = "quad"}. Also, check
#'   \code{grid.step} definition.
#'   
#' @author Pete Philipson (\email{pete.philipson@northumbria.ac.uk})
#' @keywords datagen survival
#'   
#' @references
#' 
#' Austin PC. Generating survival times to simulate Cox proportional hazards 
#' models with time-varying covariates. \emph{Stat Med.} 2012; \strong{31(29)}: 
#' 3946-3958.
#' 
#' Bender R, Augustin T, Blettner M. Generating survival times to simulate Cox 
#' proportional hazards models. \emph{Stat Med.} 2005; \strong{24}: 1713-1723.
#' 
#' Henderson R, Diggle PJ, Dobson A. Joint modelling of longitudinal 
#' measurements and event time data. \emph{Biostatistics.} 2000; \strong{1(4)}: 
#' 465-480.
#' 
#' @return A list of 2 data.frames: one recording the requisite longitudinal 
#'   outcomes data, and one recording the time-to-event data.
#' @export
#' 
#' @examples
#' simjoint(10, sepassoc = TRUE)
simjoint <- function(n = 500, model = c("intslope", "int", "quad"), sepassoc = FALSE,
                     ntms = 5, b1 = c(1, 1, 1, 1), b2 = c(1, 1), gamma = c(1, 0.1), 
                     sigu, vare = 0.01, theta0 = -3, theta1 = 1, censoring = TRUE, 
                     censlam = exp(-3), truncation = FALSE, trunctime = max(ntms), 
                     gridstep = 0.01) {
  
  model <- match.arg(model)
  if (model != "intslope" && model != "int" && model != "quad") {
    stop(paste("Unknown model:", model))
  }
  ran <- 2 # default: model = "intslope"
  if (model == "int") {
    ran <- 1
  } else if (model == "quad") {
    ran <- 3
  }
  lat <- ran
  if (!sepassoc) {
    lat <- 1
  }
  if (length(gamma) != lat) {
    warning("Number of association parameters do not match model choice\n")
  }
  gamma <- rep(gamma, length = ran)
  if (missing(sigu)) {
    sigu <- diag(ran)
  }
  if (length(sigu) != ran^2) {
    warning("Dimension of covariance matrix does not match chosen model\n")
    if(length(sigu) > ran^2) {
      sigu <- sigu[1:ran, 1:ran]
    } else {
      sigu <- diag(ran) * sigu[1]
    }
  }
  if (model == "int") {
    if(sigu < 0) {
      stop("Variance must be positive")
    }
  } else {
    if (!isSymmetric(sigu)) {
      stop("Covariance matrix is not symmetric")
    }
    if (any(eigen(sigu)$values < 0) || (det(sigu) <= 0)) {
      stop("Covariance matrix must be positive semi-definite")
    }
  }

  sim <- simdat(n, model, sepassoc, ntms, ran, b1, b2, gamma, sigu, vare, 
                theta0, theta1,
                censoring, censlam, truncation, trunctime, gridstep)
  
  list(longitudinal = sim$longdat, survival = sim$survdat)
  
}
