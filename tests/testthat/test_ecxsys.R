context("ecxsys")


# Create model here for use in the various tests to save typing:
mod <- ecxsys(
    effect_tox_observed = c(85, 76, 94, 35, 0),
    effect_tox_env_observed = c(24, 23, 32, 0, 0),
    concentration = c(0, 0.03, 0.3, 3, 10),
    hormesis_index = 3
    # hormesis_concentration = 0.3
)


test_that("error when both hormesis arguments passed to ecxsys", {
    expect_error(
        ecxsys(
            effect_tox_observed = c(85, 76, 94, 35, 0),
            concentration = c(0, 0.03, 0.3, 3, 10),
            hormesis_index = 3,
            hormesis_concentration = 0.3
        )
    )
})


test_that("error when hormesis_concentration not in concentration", {
    expect_error(
        ecxsys(
            effect_tox_observed = c(85, 76, 94, 35, 0),
            concentration = c(0, 0.03, 0.3, 3, 10),
            hormesis_concentration = 0.4
        )
    )
})


test_that("error when hormesis_index <= 2 or >= (length(concentration))", {
    expect_error(
        ecxsys(
            effect_tox_observed = c(85, 76, 94, 35, 0),
            concentration = c(0, 0.03, 0.3, 3, 10),
            hormesis_index = 1
        )
    )
    expect_error(
        ecxsys(
            effect_tox_observed = c(85, 76, 94, 35, 0),
            concentration = c(0, 0.03, 0.3, 3, 10),
            hormesis_index = 2
        )
    )
    expect_error(
        ecxsys(
            effect_tox_observed = c(85, 76, 94, 35, 0),
            concentration = c(0, 0.03, 0.3, 3, 10),
            hormesis_index = 5
        )
    )
    expect_error(
        ecxsys(
            effect_tox_observed = c(85, 76, 94, 35, 0),
            concentration = c(0, 0.03, 0.3, 3, 10),
            hormesis_index = 6
        )
    )
})


test_that("min(concentration) == 0 is shifted the correct amount", {
    expect_equal(mod$curves$concentration[1] * 10^5, 0.0001)
})


test_that("min(concentration) > 0 is conserved", {
    expect_warning(
        ecxsys(
            effect_tox_observed = c(85, 76, 94, 35, 0),
            concentration = c(0.0005, 0.03, 0.3, 3, 10),
            hormesis_index = 3
        )
    )
    suppressWarnings({
        mod <- ecxsys(
            effect_tox_observed = c(85, 76, 94, 35, 0),
            effect_tox_env_observed = c(24, 23, 32, 0, 0),
            concentration = c(0.0005, 0.03, 0.3, 3, 10),
            hormesis_index = 3
        )
    })
    expect_equal(mod$curves$concentration[1] * 10^5, 0.0005)
})


test_that("the discrete results have not changed", {
    expect_equal(
        round(mod$effect_tox_simple, 3),
        c(85.000, 84.542, 78.946, 31.959, 2.610)
    )
    expect_equal(
        round(mod$effect_tox, 3),
        c(100.000, 99.676, 94.320, 34.860, 0.840)
    )
    expect_equal(
        round(mod$stress_tox, 3),
        c(0.000, 0.078, 0.207, 0.579, 0.893)
    )
    expect_equal(
        round(mod$sys_stress_tox, 3),
        c(0.296, 0.280, 0.000, 0.000, 0.000)
    )
    expect_equal(
        round(mod$effect_tox_sys, 3),
        c(84.791, 77.481, 93.138, 34.86, 0.84)
    )
    expect_equal(round(mod$stress_env, 3), 0.388)
    expect_equal(
        round(mod$effect_tox_env_simple, 3),
        c(26.333, 26.333, 26.062, 0.710, 0.037)
    )
    expect_equal(
        round(mod$stress_tox_env, 3),
        c(0.388, 0.467, 0.595, 0.968, 1.281)
    )
    expect_equal(
        round(mod$effect_tox_env, 3),
        c(70.879, 56.414, 32.000, 0.020, 0.000)
    )
    expect_equal(
        round(mod$sys_stress_tox_env, 3),
        c(0.254, 0.181, 0.000, 0.000, 0.000)
    )
    expect_equal(
        round(mod$effect_tox_env_sys, 3),
        c(24.325, 22.323, 29.386, 0.020, 0.000)
    )
})


test_that("the smooth curves have not changed", {
    curves <- mod$curves
    i <- c(1, 714, 810, 905, 1000)
    expect_equal(
        round(curves$concentration[i], 3),
        c(0.000, 0.014, 0.125, 1.120, 10.000)
    )
    expect_equal(
        round(curves$effect_tox_simple[i], 3),
        c(85.000, 84.812, 82.700, 61.288, 2.610)
    )
    expect_equal(
        round(curves$effect_tox[i], 3),
        c(100.000, 99.879, 98.064, 73.665, 0.840)
    )
    expect_equal(
        round(curves$effect_tox_sys[i], 3),
        c(84.780, 77.932, 87.580, 73.665, 0.840)
    )
    expect_equal(
        round(curves$stress_tox[i], 3),
        c(0.000, 0.057, 0.142, 0.372, 0.893)
    )
    expect_equal(
        round(curves$sys_stress_tox[i], 3),
        c(0.298, 0.289, 0.134, 0.000, 0.000)
    )
    expect_equal(
        round(curves$stress_tox_sys[i], 3),
        c(0.298, 0.346, 0.276, 0.372, 0.893)
    )
    expect_equal(
        round(curves$effect_tox_env[i], 3),
        c(70.863, 60.496, 44.083, 8.514, 0)
    )
    expect_equal(
        round(curves$effect_tox_env_sys[i], 3),
        c(24.311, 20.727, 29.948, 8.513, 0.000)
    )
    expect_equal(
        round(curves$stress_env[i], 3),
        c(0.388, 0.388, 0.388, 0.388, 0.388)
    )
    expect_equal(
        round(curves$effect_tox_env_simple[i], 3),
        c(26.333, 26.333, 26.329, 7.538, 0.037)
    )
    expect_equal(
        round(curves$stress_tox_env[i], 3),
        c(0.389, 0.445, 0.531, 0.761, 1.281)
    )
    expect_equal(
        round(curves$sys_stress_tox_env[i], 3),
        c(0.252, 0.218, 0.076, 0.000, 0.000)
    )
    expect_equal(
        round(curves$stress_tox_env_sys[i], 3),
        c(0.640, 0.663, 0.607, 0.761, 1.281)
    )
    expect_equal(
        curves$use_for_plotting[i],
        c(TRUE, TRUE, TRUE, TRUE, TRUE)
    )
})


test_that("the returned fn works the same way as internally", {
    # I don't know why it would fail but it doesn't hurt to test it.
    curves <- mod$curves
    curves$use_for_plotting <- NULL  # remove column "use_for_plotting"
    expect_identical(curves, mod$fn(curves$concentration))
})


test_that("function arguments are returned unchanged", {
    expect_equal(mod$args$effect_tox_observed, c(85, 76, 94, 35, 0))
    expect_equal(mod$args$effect_tox_env_observed, c(24, 23, 32, 0, 0))
    expect_equal(mod$args$concentration, c(0, 0.03, 0.3, 3, 10))
    expect_equal(mod$args$hormesis_index, 3)
    # expect_equal(mod$args$hormesis_concentration, 0.3)
    expect_equal(mod$args$effect_max, 100)
    expect_equal(mod$args$p, 3.2)
    expect_equal(mod$args$q, 3.2)
})


test_that("results are independent of concentration shift", {
    mod_2 <- ecxsys(
        effect_tox_observed = c(85, 76, 94, 35, 0),
        effect_tox_env_observed = c(24, 23, 32, 0, 0),
        concentration = c(0, 0.03, 0.3, 3, 10) * 2,
        hormesis_index = 3
    )
    expect_equal(mod$effect_tox_sys, mod_2$effect_tox_sys)
    expect_equal(mod$effect_tox_env_sys, mod_2$effect_tox_env_sys)
    mod_10 <- ecxsys(
        effect_tox_observed = c(85, 76, 94, 35, 0),
        effect_tox_env_observed = c(24, 23, 32, 0, 0),
        concentration = c(0, 0.03, 0.3, 3, 10) * 10,
        hormesis_index = 3
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
        effect_tox_observed = c(85, 76, 94, 35, 0),
        concentration = c(0, 0.03, 0.3, 3, 10),
        hormesis_index = 3
    )
    expect_equal(mod$effect_tox_sys, mod_without_env$effect_tox_sys)
    expect_equal(mod$curves$effect_tox_sys,
                 mod_without_env$curves$effect_tox_sys)
})


test_that("model not converging produces warnings but no errors", {
    expect_warning(
        ecxsys(
            concentration = c(0, 0.1, 0.5, 1, 10, 33),
            effect_tox_observed = c(98, 98, 96, 76, 26, 0),
            hormesis_index = 3
        )
    )
})
