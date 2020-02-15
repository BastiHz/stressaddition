context("ecxsys")


# Create model here for use in the various tests to save typing:
mod <- ecxsys(
    concentration = c(0, 0.03, 0.3, 3, 10),
    hormesis_concentration = 0.3,
    effect_tox_observed = c(85, 76, 94, 35, 0),
    effect_tox_env_observed = c(24, 23, 32, 0, 0)
)


test_that("error when hormesis_concentration not in concentration", {
    errorm <- "hormesis_concentration must equal one of the concentration values."
    expect_error(
        ecxsys(
            concentration = c(0, 0.03, 0.3, 3, 10),
            hormesis_concentration = 0.4,
            effect_tox_observed = c(85, 76, 94, 35, 0)
        ),
        errorm,
        fixed = TRUE
    )
    expect_error(
        ecxsys(
            concentration = c(0, 0.03, 0.3, 3, 10),
            hormesis_concentration = 30,
            effect_tox_observed = c(85, 76, 94, 35, 0)
        ),
        errorm,
        fixed = TRUE
    )
})


test_that("error when hormesis_index <= 2 or >= (length(concentration))", {
    errorm <- paste(
        "hormesis_concentration must be greater than the lowest",
        "non-control concentration and less than the highest concentration"
    )
    expect_error(
        ecxsys(
            concentration = c(0, 0.03, 0.3, 3, 10),
            hormesis_concentration = 0,
            effect_tox_observed = c(85, 76, 94, 35, 0)
        ),
        errorm,
        fixed = TRUE
    )
    expect_error(
        ecxsys(
            concentration = c(0, 0.03, 0.3, 3, 10),
            hormesis_concentration = 0.03,
            effect_tox_observed = c(85, 76, 94, 35, 0)
        ),
        errorm,
        fixed = TRUE
    )
    expect_error(
        ecxsys(
            concentration = c(0, 0.03, 0.3, 3, 10),
            hormesis_concentration = 10,
            effect_tox_observed = c(85, 76, 94, 35, 0)
        ),
        errorm,
        fixed = TRUE
    )
})


test_that("min(concentration) == 0 is shifted the correct amount", {
    expect_equal(mod$curves$concentration[1] * 10^5, 0.0001)
})


test_that("the discrete results have not changed", {
    expect_equal(
        mod$effect_tox_LL5,
        c(85, 84.5420, 78.9464, 31.9586, 2.6096),
        tolerance = 1e-4
    )
    expect_equal(
        mod$effect_tox,
        c(100, 99.6760, 94.3198, 34.8601, 0.8400),
        tolerance = 1e-4
    )
    expect_equal(
        mod$stress_tox,
        c(0, 0.0784, 0.2067, 0.5794, 0.8927),
        tolerance = 1e-4
    )
    expect_equal(
        mod$sys_tox_not_fitted,
        c(0.2965, 0.2795, 0, 0, 0),
        tolerance = 1e-4
    )
    expect_equal(
        mod$effect_tox_sys,
        c(84.7912, 77.4811, 93.1380, 34.8602, 0.8400),
        tolerance = 1e-4
    )
    expect_equal(mod$stress_env, 0.3885, tolerance = 1e-3)
    expect_equal(
        mod$effect_tox_env_LL5,
        c(26.3333, 26.3333, 26.0620, 0.7104, 0.0374),
        tolerance = 1e-4
    )
    expect_equal(
        mod$stress_tox_env,
        c(0.3884, 0.4669, 0.5952, 0.9679, 1.2812),
        tolerance = 1e-4
    )
    expect_equal(
        mod$effect_tox_env,
        c(70.8786, 56.4138, 32, 0.0202, 0),
        tolerance = 1e-4
    )
    expect_equal(
        mod$sys_tox_env_not_fitted,
        c(0.2537, 0.1815, 0, 0, 0),
        tolerance = 1e-4
    )
    expect_equal(
        mod$effect_tox_env_sys,
        c(24.3254, 22.3232, 29.3860, 0.0202, 0),
        tolerance = 1e-4
    )
})


test_that("the curves have not changed", {
    new_curves <- mod$curves[c(1, 714, 810, 905, 1000), ]  # random indices
    rownames(new_curves) <- NULL
    reference_curves <- data.frame(
        concentration = c(0.0000, 0.0137, 0.1253, 1.1196, 10.0000),
        effect_tox_LL5 = c(85.0000, 84.8116, 82.6996, 61.2882, 2.6096),
        effect_tox_env_LL5 = c(26.3333, 26.3333, 26.3289, 7.5384, 0.0374),
        effect_tox = c(100.0000, 99.8787, 98.0645, 73.6652, 0.8400),
        stress_tox = c(0.0001, 0.0570, 0.1421, 0.3721, 0.8927),
        sys_tox = c(0.2980, 0.2887, 0.1336, 0.0000, 0.0000),
        stress_tox_sys = c(0.2981, 0.3457, 0.2757, 0.3721, 0.8927),
        effect_tox_sys = c(84.7797, 77.9323, 87.5799, 73.6652, 0.8400),
        stress_env = c(0.3885, 0.3885, 0.3885, 0.3885, 0.3885),
        stress_tox_env = c(0.3886, 0.4455, 0.5306, 0.7606, 1.2812),
        effect_tox_env = c(70.8634, 60.4957, 44.0834, 8.5137, 0.0000),
        sys_tox_env = c(0.2516, 0.2175, 0.0762, 0.0000, 0.0000),
        stress_tox_env_sys = c(0.6402, 0.6630, 0.6068, 0.7606, 1.2812),
        effect_tox_env_sys = c(24.3112, 20.7272, 29.9481, 8.5133, 0.0000),
        use_for_plotting = c(TRUE, TRUE, TRUE, TRUE, TRUE)
    )
    class(new_curves) <- class(reference_curves)
    expect_equal(new_curves, reference_curves, tolerance = 1e-3)
})


test_that("the returned fn works the same way as internally", {
    # I don't know why it would fail but it doesn't hurt to test it.
    curves <- mod$curves
    curves$use_for_plotting <- NULL  # remove column "use_for_plotting"
    expect_identical(curves, predict_ecxsys(mod, curves$concentration))
})


test_that("function arguments are returned unchanged", {
    args_reference <- list(
        concentration = c(0, 0.03, 0.3, 3, 10),
        hormesis_concentration = 0.3,
        effect_tox_observed = c(85, 76, 94, 35, 0),
        effect_tox_env_observed = c(24, 23, 32, 0, 0),
        effect_max = 100,
        p = 3.2,
        q = 3.2
    )
    expect_identical(args_reference, mod$args)
})


test_that("results are independent of concentration shift", {
    mod_2 <- ecxsys(
        concentration = c(0, 0.03, 0.3, 3, 10) * 2,
        hormesis_concentration = 0.3 * 2,
        effect_tox_observed = c(85, 76, 94, 35, 0),
        effect_tox_env_observed = c(24, 23, 32, 0, 0)
    )
    expect_equal(mod$effect_tox_sys, mod_2$effect_tox_sys)
    expect_equal(mod$effect_tox_env_sys, mod_2$effect_tox_env_sys)
    mod_10 <- ecxsys(
        concentration = c(0, 0.03, 0.3, 3, 10) * 10,
        hormesis_concentration = 0.3 * 10,
        effect_tox_observed = c(85, 76, 94, 35, 0),
        effect_tox_env_observed = c(24, 23, 32, 0, 0)
    )
    # Concentration shifts by factors other than powers of 10 will affect
    # the result because of the way the zero concentration is "corrected".
    expect_equal(mod$curves$effect_tox,
                 mod_10$curves$effect_tox)
    expect_equal(mod$curves$effect_tox_sys,
                 mod_10$curves$effect_tox_sys)
    expect_equal(mod$curves$effect_tox_env_sys,
                 mod_10$curves$effect_tox_env_sys)
})


test_that("effect_tox_env_observed can be left out", {
    mod_without_env <- ecxsys(
        concentration = c(0, 0.03, 0.3, 3, 10),
        hormesis_concentration = 0.3,
        effect_tox_observed = c(85, 76, 94, 35, 0)
    )
    expect_equal(mod$effect_tox_sys, mod_without_env$effect_tox_sys)
    expect_equal(mod$curves$effect_tox_sys,
                 mod_without_env$curves$effect_tox_sys)
})


test_that("sys model not converging produces a warning, not an error", {
    expect_warning(
        ecxsys(
            concentration = c(0, 0.1, 0.5, 1, 10, 33),
            hormesis_concentration = 0.5,
            effect_tox_observed = c(98, 98, 96, 76, 26, 0)
        ),
        paste(
            "Using a horizontal linear model for sys_tox_mod because the",
            "Weibull model did not converge."
        ),
        fixed = TRUE
    )
})
