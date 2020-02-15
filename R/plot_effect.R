#' @rdname plot_ecxsys
#' @export
plot_effect <- function(model,
                        show_LL5_model = FALSE,
                        show_legend = FALSE,
                        xlab = "concentration",
                        ylab = "effect") {
    stopifnot(inherits(model, "ecxsys"))
    temp <- adjust_smooth_concentrations(
        model$curves,
        model$conc_adjust_factor
    )
    curves <- temp$curves
    log_ticks <- get_log_ticks(curves$concentration)
    concentration <- c(curves$concentration[1], model$args$concentration[-1])

    plot(
        NA,
        NA,
        xlim = range(curves$concentration, na.rm = TRUE),
        ylim = c(0, model$args$effect_max),
        log = "x",
        xlab = xlab,
        ylab = ylab,
        xaxt = "n",
        bty = "L",
        las = 1
    )

    lines(
        curves$concentration,
        curves$effect_tox_sys,
        col = "blue"
    )
    lines(
        curves$concentration,
        curves$effect_tox,
        col = "deepskyblue",
        lty = 2
    )
    points(
        concentration,
        model$args$effect_tox_observed,
        pch = 16,
        col = "blue"
    )

    if (model$with_env) {
        lines(
            curves$concentration,
            curves$effect_tox_env_sys,
            col = "red"
        )
        lines(
            curves$concentration,
            curves$effect_tox_env,
            col = "orange",
            lty = 2
        )
        points(
            concentration,
            model$args$effect_tox_env_observed,
            pch = 16,
            col = "red"
        )
    }

    if (show_LL5_model) {
        lines(
            curves$concentration,
            curves$effect_tox_LL5,
            col = "darkblue",
            lty = 4
        )
        if (model$with_env) {
            lines(
                curves$concentration,
                curves$effect_tox_env_LL5,
                col = "darkred",
                lty = 4
            )
        }
    }

    axis(1, at = log_ticks$major, labels = log_ticks$major_labels)
    axis(1, at = log_ticks$minor, labels = FALSE, tcl = -0.25)
    plotrix::axis.break(1, breakpos = temp$axis_break_conc)

    if (show_legend) {
        legend_text <- c("tox", "tox + sys")
        legend_pch <- c(NA, 16)
        legend_lty <- c(2, 1)
        legend_col <- c("deepskyblue", "blue")
        if (model$with_env) {
            legend_text <- c(legend_text, "tox + env", "tox + env + sys")
            legend_pch <- c(legend_pch, NA, 16)
            legend_lty <- c(legend_lty, 2, 1)
            legend_col <- c(legend_col, "orange", "red")
        }
        if (show_LL5_model) {
            legend_text <- c(legend_text, "tox (LL5)", "tox + env (LL5)")
            legend_pch <- c(legend_pch, NA, NA)
            legend_lty <- c(legend_lty, 4, 4)
            legend_col <- c(legend_col, "darkblue", "darkred")
        }
        legend(
            "topright",
            legend = legend_text,
            pch = legend_pch,
            lty = legend_lty,
            col = legend_col
        )
    }
}
