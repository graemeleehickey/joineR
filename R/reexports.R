# Re-export Surv so users do not need to load survival explicitly
# when writing surv.formula arguments (e.g. Surv(time, status) ~ x).

#' @importFrom survival Surv
#' @export
survival::Surv
