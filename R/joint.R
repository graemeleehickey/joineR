#' Fit joint model for survival and longitudinal data measured with error
#'
#' @description This generic function fits a joint model with random latent
#'   association, building on the formulation described in Wulfsohn and Tsiatis
#'   (1997) while allowing for the presence of longitudinal and survival
#'   covariates, and three choices for the latent process. The link between the
#'   longitudinal and survival processes can be proportional or separate. When
#'   failure is attributable to 2 separate causes, a competing risks joint model
#'   is fitted as per Williamson et al. (2008).
#'
#' @param data an object of class \code{jointdata} containing the variables
#'   named in the formulae arguments.
#' @param long.formula a formula object with the response variable, and the
#'   covariates to include in the longitudinal sub-model.
#' @param surv.formula a formula object with the survival time, censoring
#'   indicator and the covariates to include in the survival sub-model. The
#'   response must be a survival object as returned by the
#'   \code{\link[survival]{Surv}} function.
#' @param model a character string specifying the type of latent association.
#'   This defaults to the intercept and slope version as seen in Wulfsohn and
#'   Tsiatis (1997). For association via the random intercept only, choose
#'   \code{model = "int"}, whereas for a quadratic association, use \code{model
#'   = "quad"}. Computing times are commensurate with the type of association
#'   structure chosen.
#' @param sepassoc logical value: if \code{TRUE} then the joint model is fitted
#'   with separate association, see \code{Details}.
#' @param longsep logical value: if \code{TRUE}, parameter estimates and
#'   log-likelihood from a separate linear mixed model analysis of the
#'   longitudinal data (see the \code{\link[nlme]{lme}} function for details)
#'   are returned.
#' @param survsep if \code{TRUE}, parameter estimates and log-likelihood from a
#'   separate analysis of the survival data using the Cox proportional hazards
#'   model are returned (see \code{\link[survival]{coxph}} function for
#'   details).
#' @param gpt the number of quadrature points across which the integration with
#'   respect to the random effects will be performed. Defaults to \code{gpt = 3}
#'   which produces stable estimates in most datasets.
#' @param lgpt the number of quadrature points which the log-likelihood is
#'   evaluated over following a model fit. This defaults to \code{lgpt = 10},
#'   though \code{lgpt = 3} is often sufficient.
#' @param max.it the maximum number of iterations of the EM algorithm that the
#'   function will perform. Defaults to \code{max.it = 200}, though more
#'   iterations may be necessary for large, complex data.
#' @param tol the tolerance level before convergence of the algorithm is deemed
#'   to have occurred. Default value is \code{tol = 0.001}.
#' @param verbose if \code{TRUE}, the parameter estimates at each iteration of
#'   the EM algorithm are printed. Default is \code{verbose = FALSE}.
#'
#' @details The \code{joint} function fits a joint model to survival and 
#'   longitudinal data. The formulation is similar to Wulfsohn and Tsiatis 
#'   (1997). A linear mixed effects model is assumed for the longitudinal data,
#'   namely
#'   
#'   \deqn{Y_i = X_{i1}(t_i)^T\beta_1 + D_i(t_i)^T U_i + \epsilon_i,}
#'   
#'   where \eqn{U_i} is a vector of random effects, \eqn{(U_{0i}, \ldots 
#'   U_{qi})} whose length depends on the model chosen, i.e. \eqn{q = 1} for the
#'   random intercept model. \eqn{D_i} is the random effects covariate matrix, 
#'   which will be time-dependent for all but the random intercept model. 
#'   \eqn{X_{i1}} is the longitudinal design matrix for unit \eqn{i}, and 
#'   \eqn{t_i} is the vector of measurement times for subject \eqn{i}. 
#'   Measurement error is represented by \eqn{\epsilon_i}.
#'   
#'   The Cox proportional hazards model is adopted for the survival data, namely
#'   
#'   \deqn{\lambda(t) = \lambda_0(t) \exp\{{X_{i2}(t)^T \beta_2 + 
#'   D_i(t)(\gamma^T U_i)}\}.}
#'   
#'   The parameter \eqn{\gamma} determines the level of association between the 
#'   two processes. For the intercept and slope model with separate association 
#'   we have
#'   
#'   \deqn{D_i(t) (\gamma^T U_i) = \gamma_0 U_{0i} + \gamma_1 U_{1i} t,}
#'   
#'   whereas under proportional association
#'   
#'   \deqn{D_i(t)(\gamma^T U_i) = \gamma (U_{0i} + U_{1i} t).}
#'   
#'   \eqn{X_{i2}} is the vector of survival covariates for unit \eqn{i}. The 
#'   baseline hazard function is \eqn{\lambda_0(t)}.
#'   
#'   The function uses an EM algorithm to estimate parameters in the joint
#'   model. Starting values are provided by calls to standard R functions
#'   \code{\link[nlme]{lme}} and \code{\link[survival]{coxph}} for the
#'   longitudinal and survival components, respectively.
#'   
#' @section Competing risks: If failure can be attributed to 2 causes, i.e. 
#'   so-called competing risks events data, then a cause-specific hazards model
#'   is adopted, namely
#'   
#'   \deqn{\lambda_g(t) = \lambda_{0g}(t) \exp\{{X_{i2}(t)^T \beta_2^{(g)} + 
#'   D_i(t)(\gamma^T U_i)}\},}
#'   
#'   where \eqn{g = 1,2} denotes the failure type, \eqn{\beta_2^{(g)}} (\eqn{g =
#'   1,2}) are cause-specific hazard parameters corresponding to the same 
#'   covariates, and \eqn{\lambda_{0g}(t)} are cause-specific baseline hazard 
#'   functions. For this data, a proportional association structure is assumed 
#'   (i.e. \code{sepassoc = FALSE}) and a random-intercepts and random-slopes 
#'   model must be used (i.e. \code{model = "intslope"}). Note that the function
#'   only permits 2 failure types. The model is specified in full by Williamson
#'   et al. (2008). The function \code{joint()} automatically detects whether
#'   competing risks are present by counting the number of unique components in
#'   the event column on the event time data.
#' 
#' @section Separate models: Both \code{longsep} and \code{survsep} ignore any
#'   latent association (i.e. \eqn{\gamma = 0}) between the longitudinal and
#'   survival processes but their output can be used to compare with the results
#'   from the joint model. If interest is solely in the individual processes
#'   then the user should instead make use of the functions
#'   \code{\link[nlme]{lme}} and \code{\link[survival]{coxph}} mentioned above.
#'   Furthermore, if interest is in the separate effect of each random effect
#'   (this is for intercept and slope or quadratic models only) upon the
#'   survival data, the user should set \code{sepassoc = TRUE}.
#'   
#' @note Since numerical integration is required, it is advisable to check the
#'   stability of the maximum likelihood estimates with an increasing number of
#'   Gauss-Hermite quadrature points. \code{joint()} uses \code{gpt = 3} by
#'   default, as this has been adequate for many datasets. However, for certain
#'   datasets and models, this might be too small.
#'
#' @author Pete Philipson (\email{pete.philipson@@northumbria.ac.uk})
#' @keywords models survival
#' @seealso \code{\link[nlme]{lme}}, \code{\link[survival]{coxph}},
#'   \code{\link{jointdata}}, \code{\link{jointplot}}.
#'
#' @references
#'
#' Wulfsohn MS, Tsiatis AA. A joint model for survival and longitudinal data
#' measured with error. \emph{Biometrics.} 1997; \strong{53(1)}: 330-339.
#'
#' Henderson R, Diggle PJ, Dobson A. Joint modelling of longitudinal
#' measurements and event time data. \emph{Biostatistics.} 2000; \strong{1(4)}:
#' 465-480.
#' 
#' Williamson PR, Kolamunnage-Dona R, Philipson P, Marson AG. Joint modelling of
#' longitudinal and competing risks data. \emph{Stat Med.} 2008; \strong{27}:
#' 6426-6438.
#' 
#' @return A list containing the parameter estimates from the joint model and,
#'   if required, from either or both of the separate analyses. The combined
#'   log-likelihood from a separate analysis and the log-likelihood from the
#'   joint model are also produced as part of the fit.
#' @import graphics stats survival utils
#' @export
#'
#' @examples
#' ## Standard joint model
#' 
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
#' fit <- joint(data = heart.valve.jd,
#'              long.formula = log.lvmi ~ 1 + time + hs,
#'              surv.formula = Surv(fuyrs, status) ~ hs,
#'              model = "intslope")
#'              
#' ## Competing risks joint model (same data as Williamson et al. 2008)
#' 
#' \dontrun{
#' data(epileptic)
#' epileptic$interaction <- with(epileptic, time * (treat == "LTG"))
#' longitudinal <- epileptic[, c(1:3, 13)]
#' survival <- UniqueVariables(epileptic, c(4, 6), "id")
#' baseline <- UniqueVariables(epileptic, "treat", "id")
#' data <- jointdata(longitudinal = longitudinal,
#'                   survival = survival,
#'                   baseline = baseline,
#'                   id.col = "id",
#'                   time.col = "time")
#'                   
#' fit2 <- joint(data = data,
#'               long.formula = dose ~ time + treat + interaction,
#'               surv.formula = Surv(with.time, with.status2) ~ treat,
#'               longsep = FALSE, survsep = FALSE,
#'               gpt = 3)
#' summary(fit2)
#' }
joint <- function(data, long.formula, surv.formula,
                  model = c("intslope", "int", "quad"),
                  sepassoc = FALSE, longsep = FALSE, survsep = FALSE,
                  gpt, lgpt, max.it, tol, verbose = FALSE) {
  
  if (!inherits(data, "jointdata")) {
    stop("Data must be of class 'jointdata'\n")
  }
  
  id <- data$subj.col
  time.long <- data$time.col
  
  #**********************************************************
  # Control parameters
  #**********************************************************
  
  if (missing(gpt)) {
    gpt <- 3
  }
  if (missing(lgpt)) {
    lgpt <- 10
  }
  if (missing(max.it)) {
    max.it <- 200
  }
  if (missing(tol)) {
    tol <- 0.001
  }
  
  Call <- match.call()
  
  if (any(sapply(data$baseline, "class") == "factor")) {
    data$baseline <- droplevels(data$baseline)
  }
  
  #**********************************************************
  # Data
  #**********************************************************
  
  # Longitudinal data
  ldatList <- prepLongData(long.formula, data, id, time.long)
  longdat <- ldatList$longdat
  long.data <- ldatList$long.data
  
  # Survival data
  sdatList <- prepSurvData(surv.formula, data, id, time.long)
  survdat <- sdatList$survdat
  survdat2 <- sdatList$survdat2
  p2 <- sdatList$p2
  compRisk <- sdatList$compRisk
  
  # Sort the data
  sort.dat <- function(longdat, survdat) {
    longid <- longdat[, 1]
    nn <- diff(match(unique(longid), longid))
    nn[length(nn) + 1] <- length(longid) - sum(nn)
    svec <- rep(survdat[, 2], nn)
    sort.long <- longdat[order(svec), ]
    os <- order(survdat[, 2])
    sort.surv <- survdat[os, ]
    list(long.s = data.frame(sort.long),
         surv.s = data.frame(sort.surv))
  }
  sort <- sort.dat(longdat, survdat)
  longdat <- sort$long.s
  survdat <- sort$surv.s
 
  #**********************************************************
  # Model structure
  #**********************************************************
  
  model <- match.arg(model)
  if ((model == "int" || model == "quad") && compRisk) {
    warning("Competing risks models are only fitted with model = 'intslope'")
    model <- "intslope"
  }
  if (model != "intslope" && model != "int" && model != "quad") {
    stop(paste("Unknown model:", model))
  }
  ran <- 2
  if (model == "int") {
    ran <- 1
  }
  if (model == "quad") {
    ran <- 3
  }
  lat <- ran
  if (sepassoc && compRisk) {
    warning("Competing risks models are only fitted with sepassoc = FALSE")
    sepassoc <- FALSE
  }
  if (!sepassoc) {
    lat <- 1
  }
  
  #**********************************************************
  # Inits
  #**********************************************************
  
  # Longitudinal submodel
  ldaests <- longst(longdat,
                    long.formula,
                    model,
                    long.data)
  
  # Time-to-event submodel
  if (!compRisk) {
    # single event time
    survests <- survst(survdat,
                       surv.formula,
                       survdat2)
    paraests <- c(ldaests, survests)
  } else {
    # competing risks
    survests.a <- survstCR(survdat,
                           surv.formula,
                           survdat2,
                           event = 1)
    survests.b <- survstCR(survdat,
                           surv.formula,
                           survdat2,
                           event = 2)
    paraests <- c(ldaests, survests.a, survests.b)
  } 
  
  # Initial log-likelihoods
  if (!compRisk) {
    # single event time
    sep.ll <- ldaests$log.like + survests$log.like[2]
    sep.loglik <- list(seplhood = sep.ll,
                       sepy = ldaests$log.like,
                       sepn = survests$log.like[2])    
  } else {
    # competing risks
    sep.ll <- ldaests$log.like + survests.a$log.like[2] + survests.b$log.like[2]
    sep.loglik <- list(seplhood = sep.ll,
                       sepy = ldaests$log.like,
                       sepn = survests.a$log.like[2] + survests.b$log.like[2])
  }
  
  #**********************************************************
  # EM algorithm
  #**********************************************************
  
  if (!compRisk) {
    # single event time
    jointfit <- emUpdate(longdat = longdat,
                         survdat = survdat,
                         model = model,
                         ran = ran,
                         lat = lat,
                         sepassoc = sepassoc,
                         paraests = paraests,
                         gpt = gpt,
                         max.it = max.it,
                         tol = tol,
                         loglik = FALSE,
                         verbose = verbose)
  } else {
    # competing risks
    jointfit <- emUpdateCR(longdat = longdat,
                           survdat = survdat,
                           paraests = paraests,
                           gpt = gpt,
                           max.it = max.it,
                           tol = tol,
                           loglik = FALSE,
                           verbose = verbose)
  }
  
  #**********************************************************
  # Extract results
  #**********************************************************
  
  # Extract MLEs (all except survival parameters)
  b1 <- jointfit$b1
  rownames(b1) <- rownames(paraests$b1)
  random <- jointfit$random
  colnames(random) <- paste0("U_", 0:(ran - 1))
  rownames(random) <- survdat[, 1]
  sigma.u <- jointfit$sigma.u
  rownames(sigma.u) <- colnames(sigma.u) <- rownames(ldaests$sigma.u)
  
  # Extract MLEs (survival parameters)
  if (!compRisk) {
    # single event time
    hazard <- jointfit$haz
    likeests <- c(jointfit, list(rs = survests$rs,
                                 sf = survests$sf))
    if (p2 > 0) {
      b2 <- jointfit$b2[1:p2, ]
      names(b2) <- names(paraests$b2)
    } else {
      b2 <- NULL
    }
    fixed <- list(longitudinal = b1,
                  survival = b2)
    latent <- jointfit$b2[(p2 + 1):(p2 + lat), ]
    names(latent) <- paste0("gamma_", 0:(lat - 1))
  } else {
    # competing risks
    hazard <- list(
      "haz.a" = jointfit$haz.a,
      "haz.b" = jointfit$haz.b)
    likeests <- c(jointfit, list(s.dist.a = survests.a$s.dist.a,
                                 id.a = survests.a$id.a,
                                 s.dist.b = survests.b$s.dist.b,
                                 id.b = survests.b$id.b))
    if (p2 > 0) {
      b2.a <- jointfit$b2.a[1:p2, ]
      b2.b <- jointfit$b2.b[1:p2, ]
      names(b2.a) <- names(b2.b) <- names(paraests$b2.a[1:p2])
    } else {
      b2.a <- b2.b <- NULL
    }
    fixed <- list(longitudinal = b1,
                  survival1 = b2.a,
                  survival2 = b2.b)
    latent <- with(jointfit, c(b2.a[(p2 + 1), ],  b2.b[(p2 + 1), ]))
    names(latent) <- paste0("gamma_", 1:2)
  }
  coefficients <- list(fixed = fixed,
                       random = random,
                       latent = latent)
  
  # Log-likelihood at MLE
  if (!compRisk) {
    # single event time
    jointll <- emUpdate(longdat = longdat,
                        survdat = survdat,
                        model = model,
                        ran = ran,
                        lat = lat,
                        sepassoc = sepassoc,
                        paraests = likeests,
                        gpt = lgpt,
                        max.it = 1,
                        tol = tol,
                        loglik = TRUE,
                        verbose = FALSE)
  } else {
    # competing risks
    jointll <- emUpdateCR(longdat = longdat,
                          survdat = survdat,
                          paraests = likeests,
                          gpt = lgpt,
                          max.it = 1,
                          tol = tol,
                          loglik = TRUE,
                          verbose = FALSE)
  }
  
  loglik <- list(jointlhood = jointll$log.like,
                 jointy = jointll$longlog.like,
                 jointn = jointll$survlog.like)
  
  # Separate model estimates
  if (!compRisk) {
    sepests <- list(longests = sep(ldaests, longsep),
                    survests = sep(survests, survsep))
  } else {
    sepests <- list(longests = sep(ldaests, longsep),
                    survests1 = sep(survests.a, survsep),
                    survests2 = sep(survests.b, survsep))
  }
  
  #**********************************************************
  # Return joint object
  #**********************************************************
  
  results <- list(coefficients = coefficients,
                  sigma.z = jointfit$sigma.z,
                  sigma.u = sigma.u,
                  hazard <- hazard,
                  loglik = loglik,
                  numIter = jointfit$iters,
                  convergence = jointfit$conv,
                  model = model,
                  sepassoc = sepassoc,
                  sepests = sepests,
                  compRisk = compRisk,
                  sep.loglik = sep.loglik,
                  formulae = list(lformula = long.formula,
                                  sformula = surv.formula),
                  data = data,
                  call = Call)
  
  class(results) <- "joint"
  return(results)
  
}
