test_that("stress_to_effect handles bad input", {
    expect_equal(stress_to_effect(-3), 1)
    expect_equal(stress_to_effect(10), 0)
    expect_error(stress_to_effect("a"))
    expect_error(stress_to_effect(0.5, -3, 10))
    expect_error(stress_to_effect(0.5, 3, -10))
})

test_that("effect_to_stress handles bad input", {
    expect_equal(effect_to_stress(-15), 1)
    expect_equal(effect_to_stress(20), 0)
    expect_error(effect_to_stress("a"))
    expect_error(effect_to_stress(0.5, -3, 10))
    expect_error(effect_to_stress(0.5, 3, -10))
})

test_that("stress_to_effect is correct", {
    expect_equal(stress_to_effect(0.5), 0.5)
    expect_equal(round(stress_to_effect(0.1), 5), 0.99321)
})

test_that("effect_to_stress is correct", {
    expect_equal(effect_to_stress(0.5), 0.5)
    expect_equal(round(effect_to_stress(0.1), 5), 0.74588)
})
