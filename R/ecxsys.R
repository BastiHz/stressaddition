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
#' \code{effect_tox_env_observed} must be of equal length and should be sorted
#' by increasing concentration.
#'
#' @param concentration A vector of concentrations, one of which must be 0 to
#'   indicate the control. Should be sorted in ascending order, otherwise it
#'   will be sorted automatically.
#' @param effect_tox_observed A vector of effect values observed at the given
#'   concentrations and in absence of environmental stress. Values must be
#'   between 0 and \code{effect_max}.
#' @param effect_tox_env_observed Effect values observed in the presence of
#'   environmental stress. This argument is optional and can be left out to
#'   model without environmental stress. Values must be between 0 and
#'   \code{effect_max}.
#' @param hormesis_concentration The concentration where the hormesis occurs.
#'   This is usually the concentration of the highest effect after the control.
#' @param hormesis_index \strong{Deprecated, will be removed soon.} A single
#'   integer specifying the index of the hormesis concentration in the
#'   concentration vector. This argument exists for compatibility with older
#'   versions of this function.
#' @param effect_max The maximum value the effect could possibly reach. For
#'   survival data in percent this should be 100 (the default).
#' @param p,q The shape parameters of the beta distribution. Default is 3.2.
#'
#' @return The result is a list containing many different objects with the most
#'   important being \code{curves} and \code{fn}. You can use \code{fn()} to
#'   calculate the curves at your concentrations of choice, see examples.
#'   \code{curves} is a data frame with the following columns:
#'   \describe{
#'     \item{concentration}{Concentrations regularly spaced on a logarithmic
#'     scale in the given concentration range. The control is approximated by
#'     the lowest non-control concentration times 1e-7.}
#'     \item{effect_tox_simple}{The five-parameter log-logistic model of the
#'     effect derived from the observations under toxicant stress but without
#'     environmental stress.}
#'     \item{effect_tox}{Modeled effect resulting from toxicant and system
#'     stress.}
#'     \item{effect_tox_sys}{Modeled effect resulting from toxicant and system
#'     stress.}
#'     \item{stress_tox}{The toxicant stress.}
#'     \item{sys_stress_tox}{System stress under toxicant stress conditions
#'     without environmental stress.}
#'     \item{stress_tox_sys}{The sum of \code{stress_tox} and
#'     \code{sys_stress_tox}.}
#'     \item{effect_tox_env_simple}{The five-parameter log-logistic model of the
#'     effect derived from the observations under toxicant stress with
#'     environmental stress.}
#'     \item{effect_tox_env}{Modeled effect resulting from toxicant and
#'     environmental stress.}
#'     \item{effect_tox_env_sys}{Modeled effect resulting from toxicant,
#'     environmental and system stress.}
#'     \item{stress_env}{Environmental stress.}
#'     \item{stress_tox_env}{The sum of toxicant and environmental stress.}
#'     \item{sys_stress_tox_env}{System stress under toxicant and
#'     environmental stress conditions.}
#'     \item{stress_tox_env_sys}{The sum of \code{stress_tox_env} and
#'     \code{sys_stress_tox_env}.}
#'     \item{use_for_plotting}{A boolean vector which is used in the plotting
#'     functions. It controls which parts of the curves are removed for the
#'     broken concentration axis.}
#'   }
#'
#' @examples model <- ecxsys(
#'     concentration = c(0, 0.03, 0.3, 3, 10),
#'     effect_tox_observed = c(85, 76, 94, 35, 0),
#'     effect_tox_env_observed = c(24, 23, 32, 0, 0),
#'     hormesis_concentration = 0.3
#' )
#'
#' # Use effect_max if for example the effect is given as the number of
#' # surviving animals and the initial number of animals is 20:
#' model <- ecxsys(
#'     concentration = c(0, 0.03, 0.3, 3, 10),
#'     effect_tox_observed = c(17, 15.2, 18.8, 7, 0),
#'     effect_tox_env_observed = c(4.8, 4.6, 6.4, 0, 0),
#'     hormesis_concentration = 0.3,
#'     effect_max = 20
#' )
#'
#' # The returned object contains a function which is useful for calculating
#' # effect and stress values at specific concentrations:
#' model$fn(c(0, 0.01, 0.1, 1))
#'
#' @export
ecxsys <- function(concentration,
                   effect_tox_observed,
                   effect_tox_env_observed,
                   hormesis_concentration,
                   hormesis_index,
                   effect_max = 100,
                   #stress_tox_at_hormesis = NULL,
                   p = 3.2,
                   q = 3.2) {
    output <- list(args = as.list(environment()))

    # input validation ----------------------------------------------------
    if (effect_max <= 0) {
        stop("effect_max must be >= 0")
    }
    if (length(concentration) != length(effect_tox_observed)) {
        stop("concentration and effect_tox_observed must have the ",
             "same length.")
    }
    if (length(concentration) > length(unique(concentration))) {
        stop("Concentrations must be unique.")
    }

    m_hc <- missing(hormesis_concentration)
    m_hi <- missing(hormesis_index)
    if (m_hc && m_hi) {
        stop("Pleace specify either hormesis_concentration or hormesis_index.")
    } else if (!m_hi && !m_hc) {
        stop("Use either hormesis_concentration or hormesis_index but not both.")
    } else if (!m_hc) {
        if (!hormesis_concentration %in% concentration) {
            stop("hormesis_concentration must be one of the values in ",
                 "concentration.")
        }
        hormesis_index = which(hormesis_concentration == concentration)
    }

    if (length(hormesis_index) != 1) {
        stop("hormesis_index must be a single integer.")
    } else if (hormesis_index <= 2 || hormesis_index >= length(concentration)) {
        stop("hormesis_index must be greater than 2 and less than ",
             "(length(concentration)).")
    }
    if (missing(effect_tox_env_observed)) {
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
            stop("effect_tox_observed and effect_tox_env_observed must have the ",
                 "same length.")
        }
        all_observations <- c(all_observations, effect_tox_env_observed)
    }
    if (any(is.na(c(all_observations, concentration)))) {
        stop("Values containing NA are not supported.")
    }
    if (any(all_observations > effect_max) ||
        any(all_observations < 0)) {
        stop("Observed effect must be between 0 and ",
             "effect_max.")
    }
    conc_shift <- 2  # Powers of ten to shift the control downwards from the
    # second lowest concentration. This is required to approximate 0 because
    # of the logarithmic axis.
    if (any(concentration < 0)) {
        stop("Concentration must be >= 0")
    } else if (min(concentration) > 0) {
        warning("No control is given and therefore the smallest concentration ",
                "is assumed to be the control.")
        min_conc <- min(concentration)
        concentration[which.min(concentration)] <- 0
    } else {
        min_conc <- 10 ^ floor(log10(concentration[2]) - conc_shift)
    }
    if (is.unsorted(concentration)) {
        warning("The concentrations are not sorted in increasing order. The ",
                "provided effect vectors will be sorted by concentration.")
        od <- order(concentration)
        concentration <- concentration[od]
        effect_tox_env_observed <- effect_tox_env_observed[od]
        effect_tox_observed <- effect_tox_observed[od]
    }
    if (effect_tox_observed[length(effect_tox_observed)] > 0) {
        warning("It is advised to complete the curve down to zero for ",
                "optimal prediction.")
    }


    # scale observed effect -----------------------------------------------
    # scale the observed effect to [0,1] to make calculations independent of
    # value of the theoretical maximum effect
    effect_tox_observed <- effect_tox_observed / effect_max
    effect_tox_env_observed <- effect_tox_env_observed / effect_max


    # prepare adjusted control concentration ------------------------------
    conc_adjust_factor <- 10^-5
    output$conc_adjust_factor <- conc_adjust_factor


    # traditional simple model (LL.5) -------------------------------------
    conc_with_control_shifted <- c(min_conc, concentration[-1])
    effect_tox_observed_averaged <- moving_weighted_mean(effect_tox_observed)
    effect_tox_env_observed_averaged <- moving_weighted_mean(effect_tox_env_observed)
    temp <- approx(
        log10(conc_with_control_shifted),
        effect_tox_observed_averaged,
        n = 10
    )
    conc_interpolated <- 10^temp$x
    effect_tox_observed_interpolated_simple_model <- temp$y
    original_options <- options()
    effect_tox_mod_simple <- drc::drm(
        effect_tox_observed_interpolated_simple_model ~ conc_interpolated,
        fct = drc::LL.5(fixed = c(
            NA, 0, effect_tox_observed_averaged[1], NA, NA
        ))
    )
    options(original_options)  #  because drm modifies options
    effect_tox_simple <- predict(effect_tox_mod_simple, data.frame(concentration))
    output$effect_tox_mod_simple <- effect_tox_mod_simple
    output$effect_tox_simple <- effect_tox_simple * effect_max
    if (with_env) {
        effect_tox_env_observed_interpolated_simple_model <- approx(
            log10(conc_with_control_shifted),
            effect_tox_env_observed_averaged,
            xout = temp$x
        )$y
        original_options <- options()
        effect_tox_env_mod_simple <- drc::drm(
            effect_tox_env_observed_interpolated_simple_model ~ conc_interpolated,
            fct = drc::LL.5(fixed = c(
                NA, 0, effect_tox_env_observed_averaged[1], NA, NA
            ))
        )
        options(original_options)  #  because drm modifies options
        effect_tox_env_simple <- predict(
            effect_tox_env_mod_simple,
            data.frame(concentration)
        )
        output$effect_tox_env_mod_simple <- effect_tox_env_mod_simple
        output$effect_tox_env_simple <- effect_tox_env_simple * effect_max
    }


    # interpolation between subhormesis and hormesis ----------------------
    n_new <- 3  # number of new points
    len <- n_new + 2  # Add 2 because seq() includes the left and right end.
    subhormesis_index <- hormesis_index - 1

    concentration_interpolated <- 10^seq(
        log10(concentration[subhormesis_index]),
        log10(concentration[hormesis_index]),
        length.out = len
    )
    concentration <- append(
        concentration,
        concentration_interpolated[-c(1, len)],
        subhormesis_index
    )

    effect_tox_observed_interpolated <- seq(
        effect_tox_observed[subhormesis_index],
        effect_tox_observed[hormesis_index],
        length.out = len
    )
    effect_tox_observed <- append(
        effect_tox_observed,
        effect_tox_observed_interpolated[-c(1, len)],
        subhormesis_index
    )

    if (with_env) {
        effect_tox_env_observed_interpolated <- seq(
            effect_tox_env_observed[subhormesis_index],
            effect_tox_env_observed[hormesis_index],
            length.out = len
        )
        effect_tox_env_observed <- append(
            effect_tox_env_observed,
            effect_tox_env_observed_interpolated[-c(1, len)],
            subhormesis_index
        )
    }

    hormesis_index <- hormesis_index + n_new

    # In the output return only the values at the original concentrations
    # and exclude those at the interpolated concentrations:
    exclude <- seq(subhormesis_index + 1, hormesis_index - 1)
    keep <- !seq_along(concentration) %in% exclude


    # effect_tox ----------------------------------------------------------
    effect_tox <- effect_tox_observed
    effect_tox[1] <- 1
    effect_to_fit_idx <- 2:(hormesis_index - 1)
    effect_tox[effect_to_fit_idx] <- NA
    original_options <- options()
    effect_tox_mod <- drc::drm(
        effect_tox ~ concentration,
        fct = drc::W1.2()
    )
    options(original_options)  #  because drm modifies options
    effect_tox <- predict(
        effect_tox_mod,
        data.frame(concentration = concentration)
    )
    output$effect_tox_mod <- effect_tox_mod
    output$effect_tox <- effect_tox[keep] * effect_max


    # system stress without environmental stress --------------------------
    stress_tox_observed <- effect_to_stress(
        effect_tox_observed, p, q
    )
    stress_tox <- effect_to_stress(effect_tox, p, q)
    output$stress_tox <- stress_tox[keep]
    sys_stress_tox <- stress_tox_observed - stress_tox
    sys_stress_tox <- pmin(pmax(sys_stress_tox, 0), 1)
    sys_stress_tox[hormesis_index:length(sys_stress_tox)] <- 0
    # Add sys_stress_tox to the output before it is fitted:
    output$sys_stress_tox <- sys_stress_tox[keep]
    sys_stress_tox_mod <- tryCatch(
        {
            # There is no other way to suppress that one error message
            # except by changing the options temporarily.
            original_options <- options()
            options(show.error.messages = FALSE)
            drc::drm(sys_stress_tox ~ stress_tox, fct = drc::W1.3())
        },
        error = function(e) {
            options(original_options)  # because drm() modifies the "warn" option
            warning(
                "Using a horizontal linear model for sys_stress_tox_mod ",
                "because the Weibull model did not converge.",
                call. = FALSE
            )
            # Failure to converge often happens when all or almost all sys
            # stress values are zero. Fitting a linear model in this case seems
            # to be the most appropriate remedy.
            stress_tox <- range(stress_tox)
            sys_stress_tox <- c(0, 0)
            return(lm(sys_stress_tox ~ stress_tox))
        },
        finally = options(original_options)
    )
    output$sys_stress_tox_mod <- sys_stress_tox_mod
    sys_stress_tox <- unname(
        predict(sys_stress_tox_mod, data.frame(stress_tox))
    )


    # modeled effect without environmental stress -------------------------
    stress_tox_sys <- stress_tox + sys_stress_tox
    effect_tox_sys <- stress_to_effect(stress_tox_sys, p, q)
    output$effect_tox_sys <- effect_tox_sys[keep] * effect_max


    if (with_env) {
        # env stress ------------------------------------------------------
        stress_tox_env_observed <- effect_to_stress(
            effect_tox_env_observed, p, q
        )
        stress_env <- (stress_tox_env_observed - stress_tox)[hormesis_index]
        stress_env <- pmax(stress_env, 0)
        output$stress_env <- stress_env


        # system stress with environmental stress -------------------------
        stress_tox_env <- stress_tox + stress_env
        effect_tox_env <- stress_to_effect(stress_tox_env, p, q)
        output$stress_tox_env <- stress_tox_env[keep]
        output$effect_tox_env <- effect_tox_env[keep] * effect_max
        sys_stress_tox_env <- stress_tox_env_observed - stress_tox_env
        sys_stress_tox_env <- pmin(pmax(sys_stress_tox_env, 0), 1)
        sys_stress_tox_env[hormesis_index:length(sys_stress_tox_env)] <- 0
        # Add sys_stress_tox_env to the output before it is fitted:
        output$sys_stress_tox_env <- sys_stress_tox_env[keep]
        sys_stress_tox_env_mod <- tryCatch(
            {
                # There is no other way to suppress that one error message
                # except by changing the options temporarily.
                original_options <- options()
                options(show.error.messages = FALSE)
                drc::drm(sys_stress_tox_env ~ stress_tox, fct = drc::W1.3())
            },
            error = function(e) {
                options(original_options)  # because drm() modifies the "warn" option
                warning(
                    "Using a horizontal linear model for ",
                    "sys_stress_tox_env_mod because the Weibull model did ",
                    "not converge.",
                    call. = FALSE
                )
                # Failure to converge often happens when all or almost all sys
                # stress values are zero. Fitting a linear model in this case
                # seems to be the most appropriate remedy.
                stress_tox <- range(stress_tox)
                sys_stress_tox_env <- c(0, 0)
                return(lm(sys_stress_tox_env ~ stress_tox))
            },
            finally = options(original_options)
        )
        output$sys_stress_tox_env_mod <- sys_stress_tox_env_mod
        sys_stress_tox_env <- unname(
            predict(sys_stress_tox_env_mod, data.frame(stress_tox))
        )


        # modeled effect with environmental stress ------------------------
        stress_tox_env_sys <- stress_tox_env + sys_stress_tox_env
        effect_tox_env_sys <- stress_to_effect(stress_tox_env_sys, p, q)
        output$effect_tox_env_sys <- effect_tox_env_sys[keep] * effect_max
    }


    # building the function -----------------------------------------------
    fn <- function(conc) {
        # This function returns all modeled values at the provided
        # concentrations. conc = a vector of concentrations
        stopifnot(is.numeric(conc))
        effect_tox_simple_fn <- predict(
            effect_tox_mod_simple,
            data.frame(concentration = conc)
        )
        effect_tox_fn <- predict(
            effect_tox_mod,
            data.frame(concentration = conc)
        )
        stress_tox_fn <- effect_to_stress(effect_tox_fn, p, q)
        sys_stress_tox_fn <- predict(
            sys_stress_tox_mod,
            data.frame(stress_tox = stress_tox_fn)
        )
        stress_tox_sys_fn <- stress_tox_fn + sys_stress_tox_fn
        effect_tox_sys_fn <- stress_to_effect(stress_tox_sys_fn, p, q)

        out_df <- data.frame(
            concentration = conc,
            effect_tox_simple = effect_tox_simple_fn * effect_max,
            effect_tox = effect_tox_fn * effect_max,
            effect_tox_sys = effect_tox_sys_fn * effect_max,
            stress_tox = stress_tox_fn,
            sys_stress_tox = sys_stress_tox_fn,
            stress_tox_sys = stress_tox_sys_fn
        )
        if (with_env) {
            effect_tox_env_simple_fn = predict(
                effect_tox_env_mod_simple,
                data.frame(concentration = conc)
            )
            stress_tox_env_fn <- stress_tox_fn + stress_env
            effect_tox_env_fn <- stress_to_effect(
                stress_tox_env_fn, p, q
            )
            sys_stress_tox_env_fn <- predict(
                sys_stress_tox_env_mod,
                data.frame(stress_tox = stress_tox_fn)
            )
            stress_tox_env_sys_fn <- stress_tox_env_fn +
                sys_stress_tox_env_fn
            effect_tox_env_sys_fn <- stress_to_effect(
                stress_tox_env_sys_fn, p, q
            )
            out_df <- cbind(out_df, data.frame(
                effect_tox_env_simple = effect_tox_env_simple_fn * effect_max,
                effect_tox_env = effect_tox_env_fn * effect_max,
                effect_tox_env_sys = effect_tox_env_sys_fn * effect_max,
                stress_env = stress_env,
                stress_tox_env = stress_tox_env_fn,
                sys_stress_tox_env = sys_stress_tox_env_fn,
                stress_tox_env_sys = stress_tox_env_sys_fn
            ))
        }
        out_df
    }

    output$fn <- fn


    # smooth curves -------------------------------------------------------
    # In order to generate a broken x-axis the concentration vector must
    # also be broken in two. The left part of the axis is supposed to be at
    # 0 but because it's a log axis I have to make the values just really
    # small. The concentrations in the gap won't be used for plotting later.

    # TODO: Also return the LL.5-curves with and without env.

    n_smooth <- 1000  #  number of points to approximate the curves
    concentration_smooth <- 10 ^ seq(
        log10(min_conc * conc_adjust_factor),
        log10(max(concentration)),
        length.out = n_smooth
    )
    output$curves <- fn(concentration_smooth)
    output$curves$use_for_plotting <-
        concentration_smooth < min_conc * conc_adjust_factor * 1.5 |
        concentration_smooth > min_conc * 1.5

    return(output)
}
