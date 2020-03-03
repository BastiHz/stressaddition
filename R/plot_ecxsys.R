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


# This is the shared documentation for the plotting functions.


#' Plot the results of the ECx-SyS model
#'
#' Plot the observed and modeled effects and stresses.
#'
#' @name plot_ecxsys
#'
#' @param model The list returned from \code{\link{ecxsys}}.
#' @param which A vector of curve names to plot. Allowed values are the column
#'   names of the \code{model$curves} data frame. The default \code{NULL} only
#'   plots the most important curves. Use \code{which = "all"} to display all
#'   curves.
#' @param show_legend Should the plot include a legend? Defaults to \code{FALSE}
#'   because it may cover some parts of the plot depending on the plot size and
#'   the number of elements shown.
#' @param xlab,ylab,main Axis labels and title.
#'
#' @examples model <- ecxsys(
#'     concentration = c(0, 0.05, 0.5, 5, 30),
#'     hormesis_concentration = 0.5,
#'     effect_tox_observed = c(90, 81, 92, 28, 0),
#'     effect_tox_env_observed = c(29, 27, 33, 5, 0)
#' )
#' plot_effect(model)
#' plot_stress(model)
#'
#' # Plot all curves:
#' plot_effect(model, which = "all")
#' plot_stress(model, which = "all")
#'
#' # Plot only some selected curves:
#' plot_effect(model, which = c("effect_tox", "effect_tox_env"))
#' plot_stress(model, which = c("stress_tox", "stress_tox_env"))
NULL
