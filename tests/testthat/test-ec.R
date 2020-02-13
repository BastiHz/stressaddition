context("effect concentration")


test_that("ec values have not changed", {
    result <- ecxsys(
        concentration = c(0, 0.03, 0.3, 3, 10),
        effect_tox_observed = c(85, 76, 94, 35, 0),
        effect_tox_env_observed = c(24, 23, 32, 0, 0),
        hormesis_index = 3
    )
    expect_equal(
        ec(result, "effect_tox_sys",50),
        list(effect = 42.38984413, concentration = 2.547711893)
    )
    expect_equal(
        ec(result, "effect_tox_sys", 10),
        list(effect = 76.30171944, concentration = 1.015730561)
    )
    expect_equal(
        ec(result, "effect_tox_sys", 5),
        list(effect = 80.54070385, concentration = 0.003172831366)
    )
    expect_equal(
        ec(result, "effect_tox_sys", 1),
        list(effect = 83.93189138, concentration = 0.00005727730324)
    )
    expect_equal(
        ec(result, "effect_tox_env_sys", 50),
        list(effect = 12.1555922, concentration = 0.9058480238)
    )
    expect_equal(
        ec(result, "effect_tox_env_sys", 10),
        list(effect = 21.88006596, concentration = 0.0007604409081)
    )
    expect_equal(
        ec(result, "effect_tox_env_sys", 5),
        list(effect = 23.09562518, concentration = 0.0001004406979)
    )
    expect_equal(
        ec(result, "effect_tox_env_sys", 1),
        list(effect = 24.06807255, concentration = 0.000001640834495)
    )
})
