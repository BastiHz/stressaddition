# Note about the setting and resetting of options:
# The drc package changes some of the options which some users may have
# modified. Of particular interest is options("warn") because it is important
# that the ecxsys function can generate some warnings. So every time drc::drm is
# called this option is cached beforehand and reset afterwards. Additionally,
# all options are cached at the beginning of ecxsys and reset on exit.


#' ECx-SyS
#'
#' The ECx-SyS model for modeling concentration-effect relationships which
#' indicate signs of hormesis.
#'
#' It is advised to complete the curve down to zero for optimal prediction.
#' Therefore \code{effect_tox_observed} in the highest concentration should be
#' at or close to zero. If the model does not fit properly try adding an effect
#' of 0 at ten times the maximum observed concentration.
#'
#' The vectors \code{concentration}, \code{effect_tox_observed} and
#' \code{effect_tox_env_observed} (if provided) must be of equal length and
#' sorted by increasing concentration.
#'
#' @param concentration A vector of concentrations, one of which must be 0 to
#'   indicate the control. Should be sorted in ascending order, otherwise it
#'   will be sorted automatically.
#' @param hormesis_concentration The concentration where the hormesis occurs.
#'   This is usually the concentration of the highest effect after the control.
#' @param effect_tox_observed A vector of effect values observed at the given
#'   concentrations and in absence of environmental stress. Values must be
#'   between 0 and \code{effect_max}.
#' @param effect_tox_env_observed Effect values observed in the presence of
#'   environmental stress. Must be between 0 and \code{effect_max}.
#' @param effect_max The maximum value the effect could possibly reach. For
#'   survival data in percent this should be 100 (the default).
#' @param p,q The shape parameters of the beta distribution. Default is 3.2.
#'
#' @return A list (of class ecxsys) containing many different objects with the
#'   most important being \code{curves}, a data frame containing effect and
#'   stress values at different concentrations. See \code{\link{predict_ecxsys}}
#'   for details.
#'
#' @examples model <- ecxsys(
#'     concentration = c(0, 0.03, 0.3, 3, 10),
#'     hormesis_concentration = 0.3,
#'     effect_tox_observed = c(85, 76, 94, 35, 0),
#'     effect_tox_env_observed = c(24, 23, 32, 0, 0)
#' )
#'
#' # Use effect_max if for example the effect is given as the number of
#' # surviving animals and the initial number of animals is 20:
#' model <- ecxsys(
#'     concentration = c(0, 0.03, 0.3, 3, 10),
#'     hormesis_concentration = 0.3,
#'     effect_tox_observed = c(17, 15.2, 18.8, 7, 0),
#'     effect_tox_env_observed = c(4.8, 4.6, 6.4, 0, 0),
#'     effect_max = 20
#' )
#'
#' @export
ecxsys <- function(concentration,
                   hormesis_concentration,
                   effect_tox_observed,
                   effect_tox_env_observed = NULL,
                   effect_max = 100,
                   p = 3.2,
                   q = 3.2) {
    output <- list(args = as.list(environment()))
    class(output) <- c("ecxsys", class(output))

    original_options <- options()
    on.exit(reset_options(original_options))


    # input validation ----------------------------------------------------
    if (effect_max <= 0) {
        stop("effect_max must be >= 0")
    }
    if (length(concentration) != length(effect_tox_observed)) {
        stop("concentration and effect_tox_observed must have the same length.")
    }
    if (length(concentration) > length(unique(concentration))) {
        stop("Concentrations must be unique.")
    }
    if (length(hormesis_concentration) != 1) {
        stop("hormesis_concentration must be a single number.")
    }
    if (!hormesis_concentration %in% concentration) {
        stop("hormesis_concentration must equal one of the concentration values.")
    }
    hormesis_index = which(hormesis_concentration == concentration)
    if (hormesis_index <= 2 || hormesis_index >= length(concentration)) {
        stop("hormesis_concentration must be greater than the lowest ",
             "non-control concentration and less than the highest concentration")
    }
    if (is.null(effect_tox_env_observed)) {
        with_env <- FALSE
        effect_tox_env_observed <- rep(NA, length(concentration))
    } else {
        with_env <- TRUE
    }
    output$with_env <- with_env
    # Creating all_observations makes it easier to test some assumptions.
    all_observations <- effect_tox_observed
    if (with_env) {
        if (length(effect_tox_observed) != length(effect_tox_env_observed)) {
            stop("effect_tox_observed and effect_tox_env_observed must have ",
                 "the same length.")
        }
        all_observations <- c(all_observations, effect_tox_env_observed)
    }
    if (any(is.na(c(all_observations, concentration)))) {
        stop("Values containing NA are not supported.")
    }
    if (any(all_observations > effect_max) ||
        any(all_observations < 0)) {
        stop("Observed effect must be between 0 and effect_max.")
    }
    conc_shift <- 2  # Powers of ten to shift the control downwards from the
    # second lowest concentration. This is required to approximate 0 because
    # of the logarithmic axis.
    if (is.unsorted(concentration)) {
        stop("The values must be sorted by increasing concentration.")
    }
    if (any(concentration < 0)) {
        stop("Concentrations must be >= 0")
    }
    if (min(concentration) > 0) {
        stop("No control is given. The first concentration must be 0.")
    }
    min_conc <- 10 ^ floor(log10(concentration[2]) - conc_shift)


    # scale observed effect -----------------------------------------------
    # scale the observed effect to [0,1] to make calculations independent of
    # value of the theoretical maximum effect
    effect_tox_observed <- effect_tox_observed / effect_max
    effect_tox_env_observed <- effect_tox_env_observed / effect_max


    # traditional log-logistic model (LL.5) -------------------------------
    LL5_tox <- fit_LL5_model(min_conc, concentration,
                             effect_tox_observed,
                             original_options)
    output$effect_tox_LL5_mod <- LL5_tox$effect_LL5_mod
    output$effect_tox_LL5 <- LL5_tox$effect_LL5 * effect_max
    if (with_env) {
        LL5_tox_env <- fit_LL5_model(min_conc, concentration,
                                     effect_tox_env_observed,
                                     original_options)
        output$effect_tox_env_LL5_mod <- LL5_tox_env$effect_LL5_mod
        output$effect_tox_env_LL5 <- LL5_tox_env$effect_LL5 * effect_max
    }


    # interpolation between subhormesis and hormesis ----------------------
    n_new <- 3  # number of new points
    concentration <- interpolate(concentration, hormesis_index, n_new, TRUE)
    effect_tox_observed <- interpolate(effect_tox_observed, hormesis_index, n_new)
    if (with_env) {
        effect_tox_env_observed <- interpolate(effect_tox_env_observed,
                                               hormesis_index, n_new)
    }
    hormesis_index <- hormesis_index + n_new
    # In the output return only the values at the original concentrations
    # and exclude those at the interpolated concentrations:
    keep <- !seq_along(concentration) %in% seq(hormesis_index - n_new, hormesis_index - 1)


    # effect_tox ----------------------------------------------------------
    effect_tox <- c(1, effect_tox_observed[-1])
    effect_tox[2:(hormesis_index - 1)] <- NA
    effect_tox_mod <- drc::drm(effect_tox ~ concentration, fct = drc::W1.2())
    options(original_options["warn"])
    effect_tox <- predict(effect_tox_mod, data.frame(concentration = concentration))
    output$effect_tox_mod <- effect_tox_mod
    output$effect_tox <- effect_tox[keep] * effect_max

    stress_tox <- effect_to_stress(effect_tox, p, q)
    output$stress_tox <- stress_tox[keep]


    # system stress without environmental stress --------------------------
    fit_sys_output <- fit_sys(
        effect_to_stress(effect_tox_observed, p, q),
        stress_tox,
        stress_tox,
        hormesis_index,
        original_options
    )
    output$sys_tox_not_fitted <- fit_sys_output$sys[keep]
    output$sys_tox_mod <- fit_sys_output$sys_mod
    if (inherits(fit_sys_output$sys_mod, "lm")) {
        warning(
            "Using a horizontal linear model for sys_tox_mod ",
            "because the Weibull model did not converge."
        )
    }
    sys_tox <- fit_sys_output$sys_modeled
    output$sys_tox <- sys_tox


    # modeled effect without environmental stress -------------------------
    stress_tox_sys <- stress_tox + sys_tox
    effect_tox_sys <- stress_to_effect(stress_tox_sys, p, q)
    output$effect_tox_sys <- effect_tox_sys[keep] * effect_max


    if (with_env) {
        # env stress ------------------------------------------------------
        stress_tox_env_observed <- effect_to_stress(effect_tox_env_observed, p, q)
        stress_env <- (stress_tox_env_observed - stress_tox)[hormesis_index]
        stress_env <- clamp(stress_env)
        output$stress_env <- stress_env

        stress_tox_env <- stress_tox + stress_env
        output$stress_tox_env <- stress_tox_env[keep]
        effect_tox_env <- stress_to_effect(stress_tox_env, p, q)
        output$effect_tox_env <- effect_tox_env[keep] * effect_max


        # system stress with environmental stress -------------------------
        fit_sys_output <- fit_sys(
            stress_tox_env_observed,
            stress_tox_env,
            stress_tox,
            hormesis_index,
            original_options
        )
        output$sys_tox_env_not_fitted <- fit_sys_output$sys[keep]
        output$sys_tox_env_mod <- fit_sys_output$sys_mod
        if (inherits(fit_sys_output$sys_mod, "lm")) {
            warning(
                "Using a horizontal linear model for sys_tox_env_mod ",
                "because the Weibull model did not converge."
            )
        }
        sys_tox_env <- fit_sys_output$sys_modeled
        output$sys_tox_env <- sys_tox_env


        # modeled effect with environmental stress ------------------------
        stress_tox_env_sys <- stress_tox_env + sys_tox_env
        effect_tox_env_sys <- stress_to_effect(stress_tox_env_sys, p, q)
        output$effect_tox_env_sys <- effect_tox_env_sys[keep] * effect_max
    }


    # smooth curves -------------------------------------------------------
    # In order to generate a broken x-axis the concentration vector must
    # also be broken in two. The left part of the axis is supposed to be at
    # 0 but because it's a log axis I have to make the values just really
    # small. The concentrations in the gap won't be used for plotting later.
    n_smooth <- 1000  #  number of points to approximate the curves
    conc_adjust_factor <- 10^-5
    output$conc_adjust_factor <- conc_adjust_factor
    concentration_smooth <- 10 ^ seq(
        log10(min_conc * conc_adjust_factor),
        log10(max(concentration)),
        length.out = n_smooth
    )
    output$curves <- predict_ecxsys(output, concentration_smooth)
    output$curves$use_for_plotting <-
        concentration_smooth < min_conc * conc_adjust_factor * 1.5 |
        concentration_smooth > min_conc * 1.5
    output
}


reset_options <- function(original_options) {
    # Reset all the options which have changed.

    # You may ask why I don't just reset the options using
    # options(original_options). The reason is that when I do this and ecxsys
    # generates warnings then those warnings don't show up in the console. I
    # don't know why, but resetting only the options which have changed
    # alleviates that problem.

    changed <- list()
    for (n in names(original_options)) {
        orig_opt <- original_options[[n]]
        if (!identical(orig_opt, getOption(n))) {
            changed[n] <- orig_opt
        }
    }
    options(changed)
}


fit_sys <- function(stress_external_observed,
                    stress_external_modeled,
                    stress_tox,
                    hormesis_index,
                    original_options) {
    sys <- stress_external_observed - stress_external_modeled
    sys <- clamp(sys)
    sys[hormesis_index:length(sys)] <- 0
    # Add sys to the output before it is fitted:
    out <- list(sys = sys)
    sys_mod <- tryCatch(
        {
            # There is no other way to suppress that one error message
            # from optim except by changing the options temporarily.
            warn_error_original <- original_options[c("warn", "show.error.messages")]
            options(show.error.messages = FALSE)
            drc::drm(sys ~ stress_tox, fct = drc::W1.3())
        },
        error = function(e) {
            # Failure to converge often happens when all or almost all sys
            # stress values are zero. Fitting a linear model in this case seems
            # to be the most appropriate remedy.
            stress_tox <- range(stress_tox)
            sys <- c(0, 0)
            lm(sys ~ stress_tox)
        },
        finally = options(warn_error_original)
    )
    out$sys_mod <- sys_mod
    out$sys_modeled <- unname(
        predict(sys_mod, data.frame(stress_tox = stress_tox))
    )
    out
}


moving_weighted_mean <- function(x) {
    # This is used to smooth out points which are jumping up and down.
    count <- rep(1, length(x))
    x_diff <- diff(x) * -1
    while (any(x_diff < 0, na.rm = TRUE)) {
        i <- which(x_diff < 0)[1]
        j <- i + 1
        x[i] <- weighted.mean(x[i:j], count[i:j])
        x <- x[-j]
        count[i] <- count[i] + count[j]
        count <- count[-j]
        x_diff <- diff(x) * -1
    }
    rep(x, count)
}


fit_LL5_model <- function(min_conc,
                          concentration,
                          effect_observed,
                          original_options) {
    # The traditional log-logistic model, here using the five-parameter
    # log-logistic function drc::LL.5.
    conc_with_control_shifted <- c(min_conc, concentration[-1])
    effect_observed_averaged <- moving_weighted_mean(effect_observed)
    interpolated <- approx(
        log10(conc_with_control_shifted),
        effect_observed_averaged,
        n = 10
    )
    conc_interpolated <- 10^interpolated$x
    effect_interpolated <- interpolated$y
    effect_LL5_mod <- drc::drm(
        effect_interpolated ~ conc_interpolated,
        fct = drc::LL.5(fixed = c(NA, 0, effect_observed_averaged[1], NA, NA))
    )
    options(original_options["warn"])
    effect_LL5 <- predict(
        effect_LL5_mod,
        data.frame(conc_interpolated = concentration)
    )
    list(
        effect_LL5_mod = effect_LL5_mod,
        effect_LL5 = effect_LL5
    )
}


interpolate <- function(x, to_index, n_new, conc = FALSE) {
    from_index <- to_index - 1  # subhormesis_index
    len <- n_new + 2  # Add 2 because seq() includes the left and right end.
    if (conc) {
        x_new <- 10^seq(log10(x[from_index]), log10(x[to_index]), length.out = len)
    } else {
        x_new <- seq(x[from_index], x[to_index], length.out = len)
    }
    append(x, x_new[-c(1, len)], from_index)
}
