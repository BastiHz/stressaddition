#' Effect Concentrations
#'
#' Estimate the concentration to reach a certain effect relative to the control.
#'
#' If the value occurs multiple times because of hormesis, which may happen for
#' low values of \code{target_effect}, then the first occurrence corresponding
#' to the smaller concentration is returned by default (i.e. the lowest
#' concentration)..
#'
#' @param model The object returned from \code{ecxsys()}.
#' @param effect_name The name of the effect for which you want to calculate the
#'   EC.
#' @param target_effect The desired effect percentage between 0 and 100. For
#'   example with the value 10 the function will return the EC10, i.e. the
#'   concentration where the response falls below 90 % of the maximum possible
#'   response.
#'
#' @return A list with the elements \code{concentration} and \code{effect}
#'   giving the effect concentration and the corresponding effect.
#'
#' @examples result <- ecxsys(
#'     concentration = c(0, 0.03, 0.3, 3, 10),
#'     effect_tox_observed = c(85, 76, 94, 35, 0),
#'     effect_tox_env_observed = c(24, 23, 32, 0, 0),
#'     hormesis_index = 3
#' )
#' # Calculate the EC_10
#' EC_10 <- ec(result, "effect_tox_sys", 10)
#'
#' @export
ec <- function(model, effect_name, target_effect) {
    stopifnot(
        is.character(effect_name),
        length(effect_name) == 1,
        effect_name %in% names(model$curves),
        effect_name != "concentration",
        target_effect > 0,
        target_effect < 100
    )
    effect <- model$curves[, effect_name]
    concentration <- model$curves$concentration
    control_effect <- effect[1]
    if (control_effect == 0) {
        stop("Reference effect is zero, calculation of EC not possible.")
    }
    target_effect <- (1 - target_effect / 100) * control_effect
    output <- list(effect = target_effect)
    # Get the index of where the effect changes from above to below
    # target_effect:
    below <- which(effect < target_effect)[1]
    above <- below - 1
    # linear interpolation
    dist <- (target_effect - effect[below]) / (effect[above] - effect[below])
    output$concentration <- dist *
        (concentration[above] - concentration[below]) + concentration[below]
    return(output)
}
