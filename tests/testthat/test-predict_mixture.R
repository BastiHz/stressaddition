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


model_a <- ecxsys(
    concentration = c(0, 0.05, 0.5, 5, 30),
    hormesis_concentration = 0.5,
    effect_tox_observed = c(90, 81, 92, 28, 0),
    effect_tox_env_observed = c(29, 27, 33, 5, 0),
    effect_max = 101
)
model_b <- ecxsys(
    concentration = c(0, 0.1, 1, 10, 100, 1000),
    hormesis_concentration = 10,
    effect_tox_observed = c(26, 25, 24, 27, 5, 0),
    effect_max = 30
)


test_that("results have not changed", {
    # one concentration_b
    new <- predict_mixture(
        model_a,
        model_b,
        c(0, 0.01, 0.1, 1, 7, 15),
        rep(5, 6),
        0.3
    )$effect
    reference <- c(88.574578, 84.361552, 80.633762, 56.730550, 2.882718, 0)
    expect_equal(new, reference, tolerance = 1e-5)

    # diverse concentration_b
    new <- predict_mixture(
        model_a,
        model_b,
        c(0, 0.01, 0.1, 1, 7, 15),
        c(0, 0.02, 0.2, 2, 14, 30),
        0.3
    )$effect
    reference <- c(88.2698383, 79.9617127, 78.1574808, 65.7999834, 0.3861678, 0)
    expect_equal(new, reference, tolerance = 1e-5)

    # diverse concentration_b and custom effect_max
    new <- predict_mixture(
        model_a,
        model_b,
        c(0, 0.01, 0.1, 1, 7, 15),
        c(0, 0.02, 0.2, 2, 14, 30),
        0.3,
        42
    )$effect
    reference <- c(88.2698383, 79.9617127, 78.1574808, 65.7999834, 0.3861678, 0) * 0.42
    expect_equal(new, reference, tolerance = 1e-5)
})


test_that("predictions are symmetric", {
    conc_a <- c(0, 10^seq(log10(0.001), log10(40), length.out = 50))
    conc_b <- rep(3.5, length(conc_a))
    prop_ca <- 0.8
    effect_12 <- predict_mixture(model_a, model_b, conc_a, conc_b, prop_ca)$effect
    effect_21 <- predict_mixture(model_b, model_a, conc_b, conc_a, prop_ca)$effect
    expect_equal(effect_12, effect_21)
})


test_that("effect_tox_mod is a W1.2 model", {
    # This is a safeguard for if I decide to use something else than drc::W1.2
    # for effect_tox_mod. It is extremely unlikely that this ever happens but
    # better safe than sorry. Currently, the inverse only works for W1.2 models.
    # If this test throws errors then I probably forget to adjust the inverse
    # function accordingly.
    expect_s3_class(model_a$effect_tox_mod$fct, "Weibull-1")
    expect_equal(model_a$effect_tox_mod$fct$noParm, 2)
})
