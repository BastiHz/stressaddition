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


model_1 <- ecxsys(
    concentration = c(0, 0.05, 0.5, 5, 30),
    hormesis_concentration = 0.5,
    effect_tox_observed = c(90, 81, 92, 28, 0),
    effect_tox_env_observed = c(29, 27, 33, 5, 0),
    effect_max = 101
)
model_2 <- ecxsys(
    concentration = c(0, 0.1, 1, 10, 100, 1000),
    hormesis_concentration = 10,
    effect_tox_observed = c(26, 25, 24, 27, 5, 0),
    effect_max = 30
)


test_that("results have not changed", {
    new <- predict_mixture(model_1, model_2, c(0, 0.01, 0.1, 1, 7, 15), 5, 0.3)
    reference <- c(89.460324, 85.205168, 81.440100, 57.297855, 2.911545, 0)
    expect_equal(new, reference, tolerance = 1e-5)
})


test_that("predictions are symmetric", {
    conc_1 <- c(0, 10^seq(log10(0.001), log10(40), length.out = 50))
    conc_2 <- 3.5
    prop_ca <- 0.8
    effect_12 <- predict_mixture(model_1, model_2, conc_1, conc_2, prop_ca)
    effect_21 <- sapply(
        conc_1,
        function(x) predict_mixture(model_2, model_1, conc_2, x, prop_ca)
    )
    effect_21 <- effect_21 / model_2$args$effect_max * model_1$args$effect_max
    expect_equal(effect_12, effect_21)
})
