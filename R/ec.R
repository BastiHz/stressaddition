#' Effect Concentrations
#'
#' Estimate the concentration to reach a certain effect or stress level relative
#' to the control.
#'
#' If the response level occurs multiple times because of hormesis, which may
#' happen for low values of \code{response_level}, then the occurrence with the
#' smallest concentration is returned.
#'
#' @param model This can be one of three types of objects: Either the output of
#'   \code{\link{ecxsys}} or the output of \code{\link{predict_ecxsys}} or a
#'   data frame with a "concentration" column and a \code{response_name} column.
#'   See the examples.
#' @param response_name The name of the effect or stress for which you want to
#'   calculate the EC. Must be one of \code{colnames(model$curves)}.
#' @param response_level The desired response level as a percentage between 0
#'   and 100. For example with the value 10 the function will return the EC10,
#'   i.e. the concentration where the response falls below 90 \% of the maximum
#'   response.
#'
#' @return A list containing the response concentration and the corresponding
#'   response value.
#'
#' @examples # Calculate the EC_10, the concentration where the effect falls
#' # below 90 % of the effect in the control.
#'
#' model <- ecxsys(
#'     concentration = c(0, 0.05, 0.5, 5, 30),
#'     hormesis_concentration = 0.5,
#'     effect_tox_observed = c(90, 81, 92, 28, 0)
#' )
#'
#' # using the ecxsys() output or the curves therein directly:
#' ec(model, "effect_tox_sys", 10)
#' ec(model$curves, "effect_tox_sys", 10)
#'
#' # using the output of predict_ecxsys() with custom concentrations:
#' conc <- 10^seq(-9, 1, length.out = 1000)
#' curves <- predict_ecxsys(model, conc)
#' ec(curves, "effect_tox_sys", 10)
#'
#' # using a custom data frame:
#' df_custom <- data.frame(
#'     concentration = curves$concentration,
#'     foo = curves$effect_tox_sys
#' )
#' ec(df_custom, "foo", 10)
#'
#' @export
ec <- function(model, response_name, response_level) {
    if (inherits(model, "drc")) {
        stop("Please use drc::ED for drc objects.")
    }

    stopifnot(
        is.character(response_name),
        length(response_name) == 1,
        response_name != "concentration",
        response_level > 0,
        response_level < 100
    )

    if (!inherits(model, c("ecxsys", "ecxsys_predicted"))) {
        if (is.data.frame(model)) {
            concentration <- model$concentration
            response <- model[, response_name]
        } else {
            stop("Invalid first argument.")
        }
    } else if (inherits(model, "ecxsys")) {
        concentration <- model$curves$concentration
        response <- model$curves[, response_name]
    } else if (inherits(model, "ecxsys_predicted")) {
        concentration <- model$concentration
        response <- model[, response_name]
    }

    reference <- response[1]
    if (reference == 0) {
        stop("Reference value is zero, calculation of EC not possible.")
    }
    response_level <- (1 - response_level / 100) * reference
    output <- list(response_value = response_level)

    # Get the index of where the response changes from above to below
    # response_level:
    below <- which(response < response_level)[1]
    above <- below - 1

    # linear interpolation
    dist <- (response_level - response[below]) / (response[above] - response[below])
    output$concentration <- dist *
        (concentration[above] - concentration[below]) + concentration[below]
    output
}
