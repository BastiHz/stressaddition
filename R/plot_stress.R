#' @rdname plot_ecxsys
#' @export
plot_stress <- function(model, show_legend = FALSE) {
    stopifnot(inherits(model, "ecxsys"))
    temp <- adjust_smooth_concentrations(
        model$curves,
        model$conc_adjust_factor
    )
    curves <- temp$curves
    axis_break_conc <- temp$axis_break_conc
    log_ticks <- get_log_ticks(curves$concentration)
    concentration <- c(curves$concentration[1], model$args$concentration[-1])

    plot(
        NA,
        NA,
        xlim = range(curves$concentration, na.rm = TRUE),
        ylim = c(0, 1),
        log = "x",
        xlab = "concentration",
        ylab = "system stress",
        xaxt = "n",
        las = 1,
        bty = "L"
    )

    lines(
        curves$concentration,
        curves$sys_tox,
        col = "blue"
    )
    points(
        concentration,
        model$sys_tox_not_fitted,
        pch = 16,
        col = "blue"
    )

    if (model$with_env) {
        lines(
            curves$concentration,
            curves$sys_tox_env,
            col = "red"
        )
        points(
            concentration,
            model$sys_tox_env_not_fitted,
            pch = 16,
            col = "red"
        )
    }

    axis(1, at = log_ticks$major, labels = log_ticks$major_labels)
    axis(1, at = log_ticks$minor, labels = FALSE, tcl = -0.25)
    plotrix::axis.break(1, breakpos = axis_break_conc)

    if (show_legend) {
        legend_text <- c("tox")
        legend_col <- c("blue")
        if (model$with_env) {
            legend_text <- c(legend_text, "tox + env")
            legend_col <- c(legend_col, "red")
        }
        legend(
            "topright",
            legend = legend_text,
            col = legend_col,
            pch = 16,
            lty = 1
        )
    }
}
