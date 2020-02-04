#' @rdname plot_ecxsys
#' @export
plot_effect <- function(model,
                        show_simple_model = FALSE,
                        show_legend = FALSE,
                        xlab = "concentration",
                        ylab = "effect") {
    temp <- adjust_smooth_concentrations(
        model$curves,
        model$conc_adjust_factor
    )
    curves <- temp$curves
    log_ticks <- get_log_ticks(curves$concentration)
    model$args$concentration[1] <- curves$concentration[1]

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
    if (show_simple_model) {
        lines(
            curves$concentration,
            curves$effect_tox_simple,
            col = rgb(112, 112, 207, maxColorValue = 255),
            lty = 4
        )
        if (model$with_env) {
            lines(
                curves$concentration,
                curves$effect_tox_env_simple,
                col = rgb(207, 112, 112, maxColorValue = 255),
                lty = 4
            )
        }
    }

    lines(
        curves$concentration,
        curves$effect_tox_sys,
        col = "blue"
    )
    if (model$with_env) {
        lines(
            curves$concentration,
            curves$effect_tox_env_sys,
            col = "red"
        )
    }
    lines(
        curves$concentration,
        curves$effect_tox,
        col = "deepskyblue",
        lty = 2
    )
    if (model$with_env) {
        lines(
            curves$concentration,
            curves$effect_tox_env,
            col = "orange",
            lty = 2
        )
    }
    points(
        model$args$concentration,
        model$args$effect_tox_observed,
        pch = 16,
        col = "blue"
    )
    if (model$with_env) {
        # TODO: merge this with the same condition above.
        points(
            model$args$concentration,
            model$args$effect_tox_env_observed,
            pch = 16,
            col = "red"
        )
    }
    axis(1, at = log_ticks$major, labels = log_ticks$major_labels)
    axis(1, at = log_ticks$minor, labels = FALSE, tcl = -0.25)
    plotrix::axis.break(1, breakpos = temp$axis_break_conc)
    if (show_legend) {
        # TODO: Maybe do proper subscript in legend
        # TODO: Add simple models to the legend
        legend_text <- c(
            "effect (tox+sys)",
            "effect (tox+env+sys)",
            "toxicant effect",
            "tox_env effect"
        )
        legend_pch <- c(16, 16, NA, NA)
        legend_lty <- c(1, 1, 2, 2)
        legend_col <- c("blue", "red", "deepskyblue", "orange")
        if (!model$with_env) {
            keep <- c(1, 3)
            legend_text <- legend_text[keep]
            legend_pch <- legend_pch[keep]
            legend_lty <- legend_lty[keep]
            legend_col <- legend_col[keep]
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
