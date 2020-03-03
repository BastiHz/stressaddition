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


#' @rdname plot_ecxsys
#' @export
plot_stress <- function(model,
                        which = NULL,
                        show_legend = FALSE,
                        xlab = "concentration",
                        ylab = "stress",
                        main = NULL) {
    stopifnot(inherits(model, "ecxsys"))

    curve_names <- names(model$curves)
    valid_names <- curve_names[
        startsWith(curve_names, "stress") | startsWith(curve_names, "sys")
    ]
    if (is.null(which)) {
        which <- c("sys_tox")
        if (model$with_env) {
            which <- c(which, "sys_tox_env")
        }
    } else if ("all" %in% which) {
        if (length(which) == 1) {
            which <- valid_names
        } else {
            stop("'all' must not be combined with other curve names.")
        }
    } else if (!all(which %in% valid_names)) {
        warning("Argument 'which' contains invalid names.")
        if (!model$with_env && any(grepl("env", which, fixed = TRUE))) {
            warning("'which' contains names with 'env' but the model was",
                    " built without environmental effects.")
        }
        which <- which[which %in% valid_names]
        if (length(which) == 0) {
            stop("No curves to display.")
        }
    }

    temp <- adjust_plot_concentrations(model)
    curves <- temp$curves
    log_ticks <- get_log_ticks(curves$concentration)
    concentration <- c(curves$concentration[1], model$args$concentration[-1])

    legend_df <- data.frame(
        text = character(),
        pch = numeric(),
        lty = numeric(),
        col = character(),
        order = numeric(),  # controls sorting of legend elements
        stringsAsFactors = FALSE
    )

    plot(
        NA,
        NA,
        xlim = range(curves$concentration, na.rm = TRUE),
        ylim = c(0, max(curves[, which], 1, na.rm = TRUE)),
        log = "x",
        xlab = xlab,
        ylab = ylab,
        main = main,
        xaxt = "n",
        yaxt = "n",
        bty = "L"
    )

    # The lines are drawn in this order to ensure that dotted and dashed lines
    # are on top of solid lines for better visibility.
    if ("stress_tox_sys" %in% which) {
        lines(
            curves$concentration,
            curves$stress_tox_sys,
            col = "blue"
        )
        legend_df[nrow(legend_df) + 1, ] <- list("tox + sys", NA, 1, "blue", 3)
    }
    if ("stress_tox" %in% which) {
        lines(
            curves$concentration,
            curves$stress_tox,
            col = "deepskyblue",
            lty = 2
        )
        legend_df[nrow(legend_df) + 1, ] <- list("tox", NA, 2, "deepskyblue", 1)
    }
    if ("sys_tox" %in% which) {
        points(
            concentration,
            model$sys_tox_not_fitted,
            pch = 16,
            col = "steelblue3"
        )
        lines(
            curves$concentration,
            curves$sys_tox,
            col = "steelblue3"
        )
        legend_df[nrow(legend_df) + 1, ] <- list("sys (tox)", 16, 1, "steelblue3", 2)
    }
    if (model$with_env) {
        if ("stress_tox_env_sys" %in% which) {
            lines(
                curves$concentration,
                curves$stress_tox_env_sys,
                col = "red"
            )
            legend_df[nrow(legend_df) + 1, ] <- list("tox + env + sys", NA, 1, "red", 7)
        }
        if ("stress_env" %in% which) {
            lines(
                curves$concentration,
                curves$stress_env,
                col = "forestgreen",
                lty = 3
            )
            legend_df[nrow(legend_df) + 1, ] <- list("env", NA, 3, "forestgreen", 4)
        }
        if ("stress_tox_env" %in% which) {
            lines(
                curves$concentration,
                curves$stress_tox_env,
                col = "orange",
                lty = 2
            )
            legend_df[nrow(legend_df) + 1, ] <- list("tox + env", NA, 2, "orange", 5)
        }
        if ("sys_tox_env" %in% which) {
            points(
                concentration,
                model$sys_tox_env_not_fitted,
                pch = 16,
                col = "violetred"
            )
            lines(
                curves$concentration,
                curves$sys_tox_env,
                col = "violetred"
            )
            legend_df[nrow(legend_df) + 1, ] <- list("sys (tox + env)", 16, 1, "violetred", 6)
        }
    }

    # The setting of col = NA and col.ticks = par("fg") is to prevent ugly line
    # thickness issues when plotting as a png with type = "cairo" and at a low
    # resolution.
    axis(1, at = log_ticks$major, labels = log_ticks$major_labels,
         col = NA, col.ticks = par("fg"))
    axis(1, at = log_ticks$minor, labels = FALSE, tcl = -0.25,
         col = NA, col.ticks = par("fg"))
    plotrix::axis.break(1, breakpos = temp$axis_break_conc)
    axis(2, col = NA, col.ticks = par("fg"), las = 1)

    if (show_legend) {
        legend_df <- legend_df[order(legend_df$order), ]
        legend(
            "topright",
            legend = legend_df$text,
            pch = legend_df$pch,
            lty = legend_df$lty,
            col = legend_df$col
        )
    }
}
