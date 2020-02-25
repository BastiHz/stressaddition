#' Predict effects and stresses
#'
#' Calculate the effects and stresses of an ECx-SyS model at arbitrary
#' concentrations.
#'
#' @param model An ECx-SyS model as returned by \code{\link{ecxsys}}.
#' @param concentration A numeric vector of concentrations.
#'
#' @return A data frame (of class "ecxsys_predicted") with the following
#'   columns:
#'   \describe{
#'     \item{concentration}{The supplied concentrations.}
#'     \item{effect_tox_LL5}{The effect predicted by the five-parameter
#'     log-logistic model derived from the observations under toxicant stress
#'     but without environmental stress.}
#'     \item{effect_tox}{Modeled effect resulting from toxicant stress.}
#'     \item{effect_tox_sys}{Modeled effect resulting from toxicant and system
#'     stress.}
#'     \item{stress_tox}{The toxicant stress.}
#'     \item{sys_tox}{System stress under toxicant stress conditions
#'     without environmental stress.}
#'     \item{stress_tox_sys}{The sum of \code{stress_tox} and
#'     \code{sys_tox}.}
#'     \item{effect_tox_env_LL5}{The effect predicted by the five-parameter
#'     log-logistic model derived from the observations under toxicant stress
#'     with environmental stress.}
#'     \item{effect_tox_env}{Modeled effect resulting from toxicant and
#'     environmental stress.}
#'     \item{effect_tox_env_sys}{Modeled effect resulting from toxicant,
#'     environmental and system stress.}
#'     \item{stress_env}{Environmental stress.}
#'     \item{stress_tox_env}{The sum of toxicant and environmental stress.}
#'     \item{sys_tox_env}{System stress under toxicant and
#'     environmental stress conditions.}
#'     \item{stress_tox_env_sys}{The sum of \code{stress_tox_env} and
#'     \code{sys_tox_env}.}
#'   }
#'
#' @examples model <- ecxsys(
#'     concentration = c(0, 0.05, 0.5, 5, 30),
#'     hormesis_concentration = 0.5,
#'     effect_tox_observed = c(90, 81, 92, 28, 0),
#'     effect_tox_env_observed = c(29, 27, 33, 5, 0)
#' )
#' p <- predict_ecxsys(model, c(0.001, 0.01, 0.1, 1, 10))
#'
#' @export
predict_ecxsys <- function(model, concentration) {
    # This function returns all modeled values at the provided
    # concentrations.
    stopifnot(
        inherits(model, "ecxsys"),
        is.numeric(concentration)
    )
    p <- model$args$p
    q <- model$args$q
    effect_max <- model$args$effect_max
    out_df <- data.frame(
        concentration = concentration
    )

    out_df$effect_tox_LL5 <- predict(
        model$effect_tox_LL5_mod,
        data.frame(concentration = concentration)
    ) * effect_max

    if (model$with_env) {
        out_df$effect_tox_env_LL5 <- predict(
            model$effect_tox_env_LL5_mod,
            data.frame(concentration = concentration)
        ) * effect_max
    }

    effect_tox <- predict(
        model$effect_tox_mod,
        data.frame(concentration = concentration)
    )
    out_df$effect_tox <- effect_tox * effect_max

    stress_tox <- effect_to_stress(effect_tox, p, q)
    out_df$stress_tox <- stress_tox

    sys_tox <- predict(
        model$sys_tox_mod,
        data.frame(stress_tox = stress_tox)
    )
    out_df$sys_tox <- sys_tox

    stress_tox_sys <- stress_tox + sys_tox
    out_df$stress_tox_sys <- stress_tox_sys

    effect_tox_sys <- stress_to_effect(stress_tox_sys, p, q)
    out_df$effect_tox_sys <- effect_tox_sys * effect_max

    if (model$with_env) {
        out_df$stress_env <- model$stress_env

        stress_tox_env <- stress_tox + model$stress_env
        out_df$stress_tox_env <- stress_tox_env

        out_df$effect_tox_env <- stress_to_effect(
            stress_tox_env, p, q
        ) * effect_max

        sys_tox_env <- predict(
            model$sys_tox_env_mod,
            data.frame(stress_tox = stress_tox)
        )
        out_df$sys_tox_env <- sys_tox_env

        stress_tox_env_sys <- stress_tox_env +
            sys_tox_env
        out_df$stress_tox_env_sys <- stress_tox_env_sys

        effect_tox_env_sys <- stress_to_effect(
            stress_tox_env_sys, p, q
        )
        out_df$effect_tox_env_sys <- effect_tox_env_sys * effect_max
    }

    class(out_df) <- c("ecxsys_predicted", class(out_df))
    out_df
}
