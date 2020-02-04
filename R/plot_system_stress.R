#' @rdname plot_ecxsys
#' @export
plot_system_stress <- function(model, show_legend = FALSE) {
    temp <- adjust_smooth_concentrations(
        model$curves,
        model$conc_adjust_factor
    )
    curves <- temp$curves
    axis_break_conc <- temp$axis_break_conc
    log_ticks <- get_log_ticks(curves$concentration)
    # FIXME: Don't overwrite this arg. Make a new variable instead.
    model$args$concentration[1] <- curves$concentration[1]

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
        curves$sys_stress_tox,
        col = "blue"
    )
    points(
        model$args$concentration,
        model$sys_stress_tox,
        pch = 16,
        col = "blue"
    )
    if (model$with_env) {
        lines(
            curves$concentration,
            curves$sys_stress_tox_env,
            col = "red"
        )
        points(
            model$args$concentration,
            model$sys_stress_tox_env,
            pch = 16,
            col = "red"
        )
    }
    axis(1, at = log_ticks$major, labels = log_ticks$major_labels)
    axis(1, at = log_ticks$minor, labels = FALSE, tcl = -0.25)
    plotrix::axis.break(1, breakpos = axis_break_conc)
    if (show_legend) {
        legend_text <- c(
            # TODO: Maybe do proper subscript in legend
            "system stress (tox)",
            "system stress (tox+env)"
        )
        legend_col <- c("blue", "red")
        if (!model$with_env) {
            legend_text <- legend_text[1]
            legend_col <- legend_col[1]
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
