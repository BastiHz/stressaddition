context("effect concentration")


test_that("all input formats produce identical models", {
    model <- ecxsys(
        concentration = c(0, 0.03, 0.3, 3, 10),
        hormesis_concentration = 0.3,
        effect_tox_observed = c(85, 76, 94, 35, 0)
    )

    # using the ecxsys() output or the curves therein directly:
    ec10_a <- ec(model, "effect_tox_sys", 10)
    ec10_b <- ec(model$curves, "effect_tox_sys", 10)

    # using the output of predict_ecxsys() with custom concentrations:
    conc <- 10^seq(-9, 1, length.out = 1000)
    curves <- predict_ecxsys(model, conc)
    ec10_c <- ec(curves, "effect_tox_sys", 10)

    # using a custom data frame:
    df_custom <- data.frame(
        concentration = curves$concentration,
        foo = curves$effect_tox_sys
    )
    ec10_d <- ec(df_custom, "foo", 10)

    expect_identical(ec10_a, ec10_b)
    expect_identical(ec10_b, ec10_c)
    expect_identical(ec10_c, ec10_d)
})


test_that("ec values have not changed", {
    model <- ecxsys(
        concentration = c(0, 0.03, 0.3, 3, 10),
        hormesis_concentration = 0.3,
        effect_tox_observed = c(85, 76, 94, 35, 0),
        effect_tox_env_observed = c(24, 23, 32, 0, 0)
    )
    expect_equal(
        ec(model, "effect_tox_sys", 50),
        list(response_value = 42.389844, concentration = 2.547712),
        tolerance = 1e-4
    )
    expect_equal(
        ec(model, "effect_tox_sys", 10),
        list(response_value = 76.301719, concentration = 1.015731),
        tolerance = 1e-4
    )
    expect_equal(
        ec(model, "effect_tox_sys", 5),
        list(response_value = 80.540705, concentration = 0.003173),
        tolerance = 1e-4
    )
    expect_equal(
        ec(model, "effect_tox_sys", 1),
        list(response_value = 83.931891, concentration = 0.000057),
        tolerance = 1e-4
    )
    expect_equal(
        ec(model, "effect_tox_env_sys", 50),
        list(response_value = 12.155592, concentration = 0.905848),
        tolerance = 1e-4
    )
    expect_equal(
        ec(model, "effect_tox_env_sys", 10),
        list(response_value = 21.880066, concentration = 0.0007604),
        tolerance = 1e-4
    )
    expect_equal(
        ec(model, "effect_tox_env_sys", 5),
        list(response_value = 23.095625, concentration = 0.00010044),
        tolerance = 1e-4
    )
    expect_equal(
        ec(model, "effect_tox_env_sys", 1),
        list(response_value = 24.068073, concentration = 0.000002),
        tolerance = 1e-4
    )
})
