test_that("all input formats produce identical models", {
    model <- ecxsys(
        concentration = c(0, 0.05, 0.5, 5, 30),
        hormesis_concentration = 0.5,
        effect_tox_observed = c(90, 81, 92, 28, 0)
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

    expect_equal(ec10_a, ec10_b, tolerance = 1e-5)
    expect_equal(ec10_b, ec10_c, tolerance = 1e-5)
    expect_equal(ec10_c, ec10_d, tolerance = 1e-5)
})


test_that("ec values have not changed", {
    model <- ecxsys(
        concentration = c(0, 0.05, 0.5, 5, 30),
        hormesis_concentration = 0.5,
        effect_tox_observed = c(90, 81, 92, 28, 0),
        effect_tox_env_observed = c(29, 27, 33, 5, 0)
    )
    expect_equal(
        ec(model, "effect_tox_sys", 50),
        list(response_value = 44.95368, concentration = 3.375735),
        tolerance = 1e-4
    )
    expect_equal(
        ec(model, "effect_tox_sys", 10),
        list(response_value = 80.91662, concentration = 1.098648),
        tolerance = 1e-4
    )
    expect_equal(
        ec(model, "effect_tox_sys", 5),
        list(response_value = 85.41198, concentration = 0.005502182),
        tolerance = 1e-4
    )
    expect_equal(
        ec(model, "effect_tox_sys", 1),
        list(response_value = 89.00828, concentration = 8.53288e-05),
        tolerance = 1e-4
    )
    expect_equal(
        ec(model, "effect_tox_env_sys", 50),
        list(response_value = 14.67725, concentration = 1.299516),
        tolerance = 1e-4
    )
    expect_equal(
        ec(model, "effect_tox_env_sys", 10),
        list(response_value = 26.41904, concentration = 0.0008571244),
        tolerance = 1e-4
    )
    expect_equal(
        ec(model, "effect_tox_env_sys", 5),
        list(response_value = 27.88677, concentration = 0.0001044245),
        tolerance = 1e-4
    )
    expect_equal(
        ec(model, "effect_tox_env_sys", 1),
        list(response_value = 29.06095, concentration = 1.388112e-06),
        tolerance = 1e-4
    )
})
