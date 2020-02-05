# This is the shared documentation for the plotting functions


#' Plot the results of the ECx-SyS model
#'
#' Convenience functions to plot the observed and modeled effect and the
#' system stress with and without environmental stress.
#'
#' @name plot_ecxsys
#'
#' @param model The list returned from ecxsys().
#' @param show_simple_model Should the log-logistic models be plotted for
#'   comparison? Defaults to \code{FALSE}.
#' @param show_legend Should the plot include a legend? Defaults to FALSE
#'   because it may cover some points or lines depending on the plot size.
#' @param xlab,ylab Axis labels.
#'
#' @examples model <- ecxsys(
#'     effect_tox_observed = c(85, 76, 94, 35, 0),
#'     effect_tox_env_observed = c(24, 23, 32, 0, 0),
#'     concentration = c(0, 0.03, 0.3, 3, 10),
#'     hormesis_concentration = 0.3
#' )
#' plot_effect(model)
#' plot_system_stress(model)
NULL



# internal helper functions for plotting ----------------------------------

get_log_ticks <- function(x) {
    # Calculate the positions of major and minor tick marks on a base 10
    # logarithmic axis.
    stopifnot(min(x, na.rm = TRUE) > 0)
    x <- log10(x)
    major <- 10 ^ seq(
        floor(min(x, na.rm = TRUE)),
        ceiling(max(x, na.rm = TRUE))
    )
    n_between <- length(major) - 1
    minor <- integer(n_between * 8)
    for (i in 1:n_between) {
        a <- major[i]
        b <- major[i+1]
        minor[seq(i * 8 - 7, i * 8)] <- seq(a + a, b - a, a)
    }
    major_tick_labels <- formatC(major, format = "fg")
    major_tick_labels[1] <- "0"
    list(
        major = major,
        minor = minor,
        major_labels = major_tick_labels
    )
}


adjust_smooth_concentrations <- function(curves, conc_adjust_factor) {
    # Helper for the plot functions, not exported.
    # Deals with the concentrations which are unnecessary for plotting.
    # This means it removes the concentrations in the gap and scales the
    # concentrations left of the gap up.

    gap_idx <- min(which(!curves$use_for_plotting))

    # Keep only the values to the left and right of the axis break:
    curves <- curves[curves$use_for_plotting, ]

    # Add NAs to force breaks in the lines:
    axis_break_conc <- curves$concentration[gap_idx]
    curves[gap_idx, ] <- NA

    # Shift the small concentrations upwards so the plot has a nice x-axis:
    curves$concentration[1:gap_idx] <-
        curves$concentration[1:gap_idx] / conc_adjust_factor

    list(
        curves = curves,
        axis_break_conc = axis_break_conc
    )
}
