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
#' The predictions are symmetric, i.e. it does not matter which of the toxicant
#' models is 1 or 2 as long as the concentration arguments are supplied in the
#' right order. See the example below.
#'
#' This method is only suitable for experiments without environmental stress.
#' Any environmental stress in \code{model_a} or \code{model_b} is ignored.
#'
#' @param model_a,model_b The ecxsys models of the toxicants.
#' @param concentration_a,concentration_b The concentrations of the toxicants in
#'   the mixture. Both vectors must be the same length.
#' @param proportion_ca The proportion of concentration addition in the
#'   calculation of the toxicant stress of the mixture. Must be between 0 and 1.
#' @param effect_max Controls the scaling of the result. This represents the
#'   maximum value the effect could possibly reach. For survival data in percent
#'   this should be 100 (the default).
#'
#' @return A data frame with columns of the supplied concentrations and the
#'   corresponding mixture effects.
#'
#' @examples toxicant_a  <- ecxsys(
#'     concentration = c(0, 0.05, 0.5, 5, 30),
#'     hormesis_concentration = 0.5,
#'     effect_tox_observed = c(90, 81, 92, 28, 0),
#' )
#' toxicant_b  <- ecxsys(
#'     concentration = c(0, 0.1, 1, 10, 100, 1000),
#'     hormesis_concentration = 10,
#'     effect_tox_observed = c(26, 25, 24, 27, 5, 0),
#'     effect_max = 30
#' )
#' predict_mixture(
#'     toxicant_a ,
#'     toxicant_b ,
#'     c(0, 0.02, 0.2, 2, 20),
#'     c(0, 0.03, 0.4, 5, 10)
#' )
#'
#' # Example of symmetric prediction:
#' conc_a <- c(0, 0.03, 0.3, 3)
#' conc_b <- rep(5.5, 4)
#' prop_ca <- 0.75
#' mix_a <- predict_mixture(toxicant_a , toxicant_b , conc_a, conc_b, prop_ca)
#' mix_b <- predict_mixture(toxicant_b , toxicant_a , conc_b, conc_a, prop_ca)
#' identical(mix_a$effect, mix_b$effect)
#'
#' @export
predict_mixture <- function(model_a,
                            model_b,
                            concentration_a,
                            concentration_b,
                            proportion_ca = 0.5,
                            effect_max = 100) {
    stopifnot(
        inherits(model_a, "ecxsys"),
        inherits(model_b, "ecxsys"),
        is.numeric(concentration_a),
        is.numeric(concentration_b),
        length(concentration_a) > 0,
        length(concentration_b) > 0,
        length(concentration_a) == length(concentration_b),
        all(!is.na(concentration_a)),
        all(!is.na(concentration_b)),
        proportion_ca >= 0,
        proportion_ca <= 1,
        model_a$args$p == model_b$args$p,
        model_a$args$q == model_b$args$q
    )

    predicted_model_a <- predict_ecxsys(model_a, concentration_a)
    predicted_model_b <- predict_ecxsys(model_b, concentration_b)

    # tox stress ----------------------------------------------------------
    stress_tox_sam <- predicted_model_a$stress_tox + predicted_model_b$stress_tox

    # Convert the model_b concentration into an equivalent model_a concentration
    # and vice versa.
    concentration_b_equivalent <- W1.2_inverse(
        model_a$effect_tox_mod,
        predicted_model_b$effect_tox / model_b$args$effect_max
    )
    effect_tox_ca_a <- predict(
        model_a$effect_tox_mod,
        data.frame(concentration = concentration_a + concentration_b_equivalent)
    )
    stress_tox_ca_a <- effect_to_stress(effect_tox_ca_a)

    concentration_a_equivalent <- W1.2_inverse(
        model_b$effect_tox_mod,
        predicted_model_a$effect_tox / model_a$args$effect_max
    )
    effect_tox_ca_b <- predict(
        model_b$effect_tox_mod,
        data.frame(concentration = concentration_b + concentration_a_equivalent)
    )
    stress_tox_ca_b <- effect_to_stress(effect_tox_ca_b)

    stress_tox_ca <- (stress_tox_ca_a + stress_tox_ca_b) / 2

    # sys -----------------------------------------------------------------
    sys_a <- predict(model_a$sys_tox_mod, data.frame(stress_tox = stress_tox_ca))
    sys_b <- predict(model_b$sys_tox_mod, data.frame(stress_tox = stress_tox_ca))
    sys_total <- (sys_a + sys_b) / 2

    # combined stress and result ------------------------------------------
    proportion_sam <- 1 - proportion_ca
    stress_tox_total <- stress_tox_ca * proportion_ca + stress_tox_sam * proportion_sam
    stress_total <- stress_tox_total + sys_total
    effect <- stress_to_effect(stress_total) * effect_max

    # Setting row.names to NULL to prevent row names when one of the
    # concentrations is a single number.
    data.frame(concentration_a, concentration_b, effect, row.names = NULL)
}


W1.2_inverse <- function(mod, x) {
    # Using drc::ED() with respLev = 0 does not work, which is why I use
    # this function instead.
    # x = 1 - respLev / 100
    b <- mod$fit$par[1]
    e <- mod$fit$par[2]
    exp(log(-log(x)) / b + log(e))
}
