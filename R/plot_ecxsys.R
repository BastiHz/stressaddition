# This is the shared documentation for the plotting functions


#' Plot the results of the ECx-SyS model
#'
#' Convenience functions to plot the observed and modeled effect and the
#' system stress with and without environmental stress.
#'
#' @name plot_ecxsys
#'
#' @param model The list returned from \code{\link{ecxsys}}.
#' @param show_LL5_model Should the log-logistic models be plotted for
#'   comparison? Defaults to \code{FALSE}.
#' @param show_legend Should the plot include a legend? Defaults to FALSE
#'   because it may cover some points or lines depending on the plot size.
#' @param xlab,ylab Axis labels.
#'
#' @examples model <- ecxsys(
#'     concentration = c(0, 0.03, 0.3, 3, 10),
#'     hormesis_concentration = 0.3,
#'     effect_tox_observed = c(85, 76, 94, 35, 0),
#'     effect_tox_env_observed = c(24, 23, 32, 0, 0)
#' )
#' plot_effect(model)
#' plot_stress(model)
NULL
