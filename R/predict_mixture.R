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


#' Predict the effect of a mixture of two toxicants
#'
#' Given the ecxsys models of two toxicants this method predicts the effects of
#' different mixtures of both.
#'
#' The prediction happens at one or multiple concentrations of toxicant 1 and
#' one concentration of toxicant 2. This allows for easy plotting over the
#' concentrations of toxicant 1.
#'
#' The predictions are symmetric, i.e. it does not matter which of the toxicant
#' models is 1 or 2 as long as the concentration arguments are supplied
#' in the right order. See the example below.
#'
#' This method is only suitable for experiments without environmental stress.
#' Any environmental stress in \code{model_1} or \code{model_2} is ignored.
#'
#' @param model_1 The ecxsys model of the toxicant that varies in concentration.
#' @param model_2 The ecxsys model of the toxicant with a fixed concentration.
#' @param concentration_1 The concentration of toxicant 1 in the mixture.
#' @param concentration_2 The concentration of toxicant 2 in the mixture.
#' @param proportion_ca How much of the combined toxicant stress is determined
#'   by concentration addition. Must be between 0 and 1.
#'
#' @return A vector of the effects of the mixture, scaled to the effect_max
#'   value of model_1.
#'
#' @examples toxicant_1  <- ecxsys(
#'     concentration = c(0, 0.05, 0.5, 5, 30),
#'     hormesis_concentration = 0.5,
#'     effect_tox_observed = c(90, 81, 92, 28, 0),
#' )
#' toxicant_2  <- ecxsys(
#'     concentration = c(0, 0.1, 1, 10, 100, 1000),
#'     hormesis_concentration = 10,
#'     effect_tox_observed = c(26, 25, 24, 27, 5, 0),
#'     effect_max = 30
#' )
#' predict_mixture(
#'     toxicant_1 ,
#'     toxicant_2 ,
#'     c(0, 0.02, 0.2, 2, 20),
#'     3
#' )
#'
#' # Example of symmetric prediction:
#' conc_1 <- c(0, 0.03, 0.3, 3)
#' conc_2 <- 5.5
#' prop_ca <- 0.75
#' effect_a <- predict_mixture(toxicant_1 , toxicant_2 , conc_1, conc_2, prop_ca)
#' effect_b <- sapply(
#'     conc_1,
#'     function(x) predict_mixture(toxicant_2 , toxicant_1 , conc_2, x, prop_ca)
#' )
#' # The effect_max values of the models are different. effect_b is scaled to
#' # the one from toxicant 2 but to compare the results effect_b must be scaled
#' # to the effect_max of toxicant 1:
#' effect_b <- effect_b / toxicant_2 $args$effect_max * toxicant_1$args$effect_max
#' identical(effect_a, effect_b)
#'
#' @export
predict_mixture <- function(model_1,
                            model_2,
                            concentration_1,
                            concentration_2,
                            proportion_ca = 0.5) {
    stopifnot(
        inherits(model_1, "ecxsys"),
        inherits(model_2, "ecxsys"),
        is.numeric(concentration_1),
        is.numeric(concentration_2),
        length(concentration_2) == 1,
        !is.na(concentration_2),
        proportion_ca >= 0,
        proportion_ca <= 1,
        model_1$args$p == model_2$args$p,
        model_1$args$q == model_2$args$q
    )

    predicted_model_1 <- predict_ecxsys(model_1, concentration_1)
    predicted_model_2 <- predict_ecxsys(model_2, concentration_2)

    # tox stress ----------------------------------------------------------
    stress_tox_sam <- predicted_model_1$stress_tox + predicted_model_2$stress_tox

    # Convert the model_2 concentration into an equivalent model_1 concentration
    # anc vice versa. Clamp the response levels because drc::ED can't deal with
    # 0 and 100.
    response_level_2 <- 100 - predicted_model_2$effect_tox / model_2$args$effect_max * 100
    response_level_2 <- clamp(response_level_2, 1e-10, 100 - 1e-10)
    concentration_2_equivalent <- drc::ED(
        model_1$effect_tox_mod,
        response_level_2,
        display = FALSE
    )[, "Estimate"]
    effect_tox_ca_1 <- predict(
        model_1$effect_tox_mod,
        data.frame(concentration = concentration_1 + concentration_2_equivalent)
    )
    stress_tox_ca_1 <- effect_to_stress(effect_tox_ca_1)

    response_level_1 <- 100 - predicted_model_1$effect_tox / model_1$args$effect_max * 100
    response_level_1 <- clamp(response_level_1, 1e-10, 100 - 1e-10)
    concentration_1_equivalent <- drc::ED(
        model_2$effect_tox_mod,
        response_level_1,
        display = FALSE
    )[, "Estimate"]
    effect_tox_ca_2 <- predict(
        model_2$effect_tox_mod,
        data.frame(concentration = concentration_2 + concentration_1_equivalent)
    )
    stress_tox_ca_2 <- effect_to_stress(effect_tox_ca_2)

    stress_tox_ca <- (stress_tox_ca_1 + stress_tox_ca_2) / 2

    # sys -----------------------------------------------------------------
    sys_1 <- predict(model_1$sys_tox_mod, data.frame(stress_tox = stress_tox_ca))
    sys_2 <- predict(model_2$sys_tox_mod, data.frame(stress_tox = stress_tox_ca))
    sys_total <- sys_1 * 0.5 + sys_2 * 0.5

    # combined stress and result ------------------------------------------
    proportion_sam <- 1 - proportion_ca
    stress_tox_total <- stress_tox_ca * proportion_ca + stress_tox_sam * proportion_sam
    stress_total <- stress_tox_total + sys_total
    effect_total <- stress_to_effect(stress_total) * model_1$args$effect_max

    # unname() to remove the name when concentration_1 is a single number.
    unname(effect_total)
}
