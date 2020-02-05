#' Convert Between Stress and Effect
#'
#' Functions to convert effect to general stress or vice versa using the beta
#' distribution.
#'
#' These are simple wrappers around the beta distribution function
#' \code{\link[stats:Beta]{pbeta}} and the beta quantile function
#' \code{\link[stats:Beta]{qbeta}}. \code{stress_to_effect} returns
#' \code{1 - pbeta(stress, p, q)}. \code{effect_to_stress} first clips the
#' effect to the interval [0, 1] and then returns
#' \code{qbeta(1 - effect, p, q)}.
#'
#' "Effect" is a response which is negatively correlated with stress. For
#' example increasing toxicant concentration (stress) reduces survival (effect).
#'
#' @name Stressconversion
#'
#' @param effect One or more effect values to convert to general stress.
#'   Should be a value between 0 and 1. Smaller or bigger values are treated as
#'   0 or 1 respectively.
#' @param stress One or more stress values to convert to effect.
#' @param p,q The shape parameters of the beta distribution. Default is 3.2.
#'
#' @examples
#' stress <- 0.3
#' effect <- stress_to_effect(stress)
#' effect_to_stress(effect)
#'
NULL


#' @rdname Stressconversion
#' @export
effect_to_stress <- function(effect, p = 3.2, q = 3.2) {
    effect <- pmin(pmax(effect, 0), 1)
    qbeta(1 - effect, p, q)
}


#' @rdname Stressconversion
#' @export
stress_to_effect <- function(stress, p = 3.2, q = 3.2) {
    1 - pbeta(stress, p, q)
}
