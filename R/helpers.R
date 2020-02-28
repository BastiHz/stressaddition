# A collection of internal helper functions which get used in more than one place.


clamp <- function(x, lower = 0, upper = 1) {
    # Returns lower if x < lower, returns upper if x > upper, else returns x
    pmin(pmax(x, lower), upper)
}


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
        b <- major[i + 1]
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


adjust_plot_concentrations <- function(model) {
    # Helper for the plot functions, not exported.
    # Deals with the concentrations which are unnecessary for plotting. This
    # means it removes the concentrations in the gap and increases the
    # concentrations left of the gap.
    curves <- model$curves
    gap_idx <- min(which(!curves$use_for_plotting))

    # Keep only the values to the left and right of the axis break:
    curves <- curves[curves$use_for_plotting, ]

    # Add NAs to force breaks in the lines:
    axis_break_conc <- curves$concentration[gap_idx]
    curves[gap_idx, ] <- NA

    # Shift the small concentrations upwards so the plot has a nice x-axis:
    curves$concentration[1:gap_idx] <-
        curves$concentration[1:gap_idx] / model$conc_adjust_factor

    list(
        curves = curves,
        axis_break_conc = axis_break_conc
    )
}
