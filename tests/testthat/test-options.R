context("options")


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
        concentration = c(0, 0.03, 0.3, 3, 10),
        effect_tox_observed = c(85, 76, 94, 35, 0),
        effect_tox_env_observed = c(24, 23, 32, 0, 0),
        hormesis_concentration = 0.3
    )
    new_options <- options()

    # When doing devtools::check() some additional options may get added while
    # running the model. So limit the equality testing to the option names which
    # were there originally.
    expect_identical(original_options, new_options[names(original_options)])
})
