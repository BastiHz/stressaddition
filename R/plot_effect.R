#' @rdname plot_ecxsys
#' @export
plot_effect <- function(model,
                        which = NULL,
                        show_legend = FALSE,
                        xlab = "concentration",
                        ylab = "effect",
                        main = NULL) {
    stopifnot(inherits(model, "ecxsys"))

    curve_names <- names(model$curves)
    valid_names <- curve_names[startsWith(curve_names, "effect")]
    if (is.null(which)) {
        which <- c("effect_tox_sys")
        if (model$with_env) {
            which <- c(which, "effect_tox_env_sys")
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
    }

    temp <- adjust_smooth_concentrations(model)
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
        ylim = c(0, model$args$effect_max),
        log = "x",
        xlab = xlab,
        ylab = ylab,
        main = main,
        xaxt = "n",
        yaxt = "n",
        bty = "L"
    )

    points(
        concentration,
        model$args$effect_tox_observed,
        pch = 16,
        col = "blue"
    )
    legend_df[nrow(legend_df) + 1, ] <- list("tox (observed)", 16, 0, "blue", 1)
    if (model$with_env) {
        points(
            concentration,
            model$args$effect_tox_env_observed,
            pch = 16,
            col = "red"
        )
        legend_df[nrow(legend_df) + 1, ] <- list("tox + env (observed)", 16, 0, "red", 2)
    }
    # The lines are drawn in this order to ensure that dotted and dashed lines
    # are on top of solid lines for better visibility.
    if ("effect_tox_sys" %in% which) {
        lines(
            curves$concentration,
            curves$effect_tox_sys,
            col = "blue"
        )
        legend_df[nrow(legend_df) + 1, ] <- list("tox + sys", NA, 1, "blue", 5)
    }
    if ("effect_tox" %in% which) {
        lines(
            curves$concentration,
            curves$effect_tox,
            col = "deepskyblue",
            lty = 2
        )
        legend_df[nrow(legend_df) + 1, ] <- list("tox", NA, 2, "deepskyblue", 4)
    }
    if ("effect_tox_LL5" %in% which) {
        lines(
            curves$concentration,
            curves$effect_tox_LL5,
            col = "darkblue",
            lty = 3
        )
        legend_df[nrow(legend_df) + 1, ] <- list("tox (LL5)", NA, 3, "darkblue", 3)
    }
    if (model$with_env) {
        if ("effect_tox_env_sys" %in% which) {
            lines(
                curves$concentration,
                curves$effect_tox_env_sys,
                col = "red"
            )
            legend_df[nrow(legend_df) + 1, ] <- list("tox + env + sys", NA, 1, "red", 8)
        }
        if ("effect_tox_env" %in% which) {
            lines(
                curves$concentration,
                curves$effect_tox_env,
                col = "orange",
                lty = 2
            )
            legend_df[nrow(legend_df) + 1, ] <- list("tox + env", NA, 2, "orange", 7)
        }
        if ("effect_tox_env_LL5" %in% which) {
            lines(
                curves$concentration,
                curves$effect_tox_env_LL5,
                col = "darkred",
                lty = 3
            )
            legend_df[nrow(legend_df) + 1, ] <- list("tox + env (LL5)", NA, 3, "darkred", 6)
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
