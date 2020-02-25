test_that("user options are not permanently changed by ecxsys()", {
    # drc::drm() messes with the options and fails to return them to their
    # previous values. And options are temporarily modified in ecxsys(). This
    # test checks if all options are returned to their values from before
    # calling ecxsys().

    # This problem becomes visible only if the user changes the default options:
    options(show.error.messages = FALSE)  #  default is TRUE
    options(warn = 1)  # default is 0

    original_options <- options()
    model <- ecxsys(
        concentration = c(0, 0.05, 0.5, 5, 30),
        hormesis_concentration = 0.5,
        effect_tox_observed = c(90, 81, 92, 28, 0),
        effect_tox_env_observed = c(29, 27, 33, 5, 0)
    )
    new_options <- options()

    # When doing devtools::check() some additional options may get added while
    # running the model. So limit the equality testing to the option names which
    # were there originally.
    expect_identical(original_options, new_options[names(original_options)])
})
