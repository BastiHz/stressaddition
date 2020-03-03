# Copyright (C) 2020  Helmholtz-Zentrum fuer Umweltforschung GmbH - UFZ
# See file inst/COPYRIGHTS for details.
#
# This file is part of the R package stressaddition.
#
# stressaddition is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.


#' Effect Concentrations
#'
#' Estimate the concentration to reach a certain effect or stress level relative
#' to the control.
#'
#' If the response level occurs multiple times because of hormesis, which may
#' happen for low values of \code{response_level}, then the occurrence with the
#' smallest concentration is returned.
#'
#' This function only makes sense for curves which generally go down with
#' increasing concentration, i.e. all \code{effect_*} curves and also
#' \code{sys_tox} and \code{sys_tox_env}. Others are untested and may give
#' unexpected results, if any.
#'
#' @param model This can be one of three types of objects: Either the output of
#'   \code{\link{ecxsys}} or the output of \code{\link{predict_ecxsys}} or a
#'   data frame with a "concentration" column and a \code{response_name} column.
#'   See the examples.
#' @param response_name The name of the effect or stress for which you want to
#'   calculate the EC. Must be one of \code{colnames(model$curves)}.
#' @param response_level The desired response level as a percentage between 0
#'   and 100. For example with the value 10 the function will return the EC10,
#'   i.e. the concentration where the response falls below 90 \% of the control
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
        stop("Reference value is zero, calculation of EC is impossible.")
    }
    response_value <- (1 - response_level / 100) * reference
    output <- list(response_value = response_value)

    # Get the index of where the response changes from above to below
    # response_value:
    below <- which(response < response_value)[1]
    if (is.na(below)) {
        stop("The curve '", response_name, "' does not fall below ",
             100 - response_level, "% of the control, which makes ",
             "determining the EC", response_level, " impossible.\n  You could ",
             "try using predict_ecxsys() to predict more values in a wider ",
             "concentration range.")
    }
    above <- below - 1

    # linear interpolation
    dist <- (response_value - response[below]) / (response[above] - response[below])
    output$concentration <- dist *
        (concentration[above] - concentration[below]) + concentration[below]
    output
}
