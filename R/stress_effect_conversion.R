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


#' Convert Between Stress and Effect
#'
#' Functions to convert effect to general stress or vice versa using the beta
#' distribution.
#'
#' These are simple wrappers around the beta distribution function
#' \code{\link[stats:Beta]{pbeta}} and the beta quantile function
#' \code{\link[stats:Beta]{qbeta}}. \code{stress_to_effect} returns
#' \code{1 - pbeta(stress, p, q)}. \code{effect_to_stress} first clamps the
#' effect to the interval [0, 1] and then returns
#' \code{qbeta(1 - effect, p, q)}.
#'
#' "Effect" is a response which is negatively correlated with stress. For
#' example increasing toxicant concentration (stress) reduces survival (effect).
#'
#' @name Stressconversion
#'
#' @param effect One or more effect values to convert to general stress. Should
#'   be a value between 0 and 1. Smaller or bigger values are treated as 0 or 1
#'   respectively.
#' @param stress One or more stress values to convert to effect.
#' @param p,q The shape parameters of the \code{\link[stats:Beta]{beta}}
#'   distribution. Default is 3.2.
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
    stopifnot(p >= 0, q >= 0)
    effect <- clamp(effect)
    qbeta(1 - effect, p, q)
}


#' @rdname Stressconversion
#' @export
stress_to_effect <- function(stress, p = 3.2, q = 3.2) {
    stopifnot(p >= 0, q >= 0)
    1 - pbeta(stress, p, q)
}
