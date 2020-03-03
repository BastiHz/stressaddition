# stressaddition 2.2.1

* Improve documentation of `predict_mixture()` and include example of symmetry.

# stressaddition 2.2.0

* `ec()` raises an error if the curve does not cross the desired response level.
* `ecxsys()` gains a new argument `curves_concentration_max` which allows setting the maximum concentration of the predicted curves.

# stressaddition 2.1.1

* Restore the default behaviour of `plot_effect()` to also show `effect_tox` and `effect_tox_env`.

# stressaddition 2.1.0

* The functions `plot_effect()` and `plot_stress()` gain a `which` argument that controls which curves are plotted. Consequently, the `show_LL5_model` argument of `plot_effect()` was removed.
* Added arguments `xlab` and `ylab` to `plot_stress`.
* Added argument `main` to both plot functions.
* Changed some colors of the stress curves so they better match with the colors of related effect curves.
* Added `predict_mixture()` for the prediction of the effects of mixtures of two toxicants.
* Fixed documentation of `ecxsys()` and `predict_ecxsys()`.

# stressaddition 2.0.0

* Changed the order of arguments in `ecxsys()`.
* Removed `hormesis_index` argument from `ecxsys()`. Use `hormesis_concentration` instead.
* New function `predict_ecxsys()` replaces `fn()` from the `ecxsys()` output.
* Renamed the arguments in `ec()`.
* Made `ec()` more flexible. It now also accepts a data.frame with a concentration column and a column of response values.
* Added LL5 curves to the legend of `plot_effect()`.
* Replaced every occurrence of "simple" in variable names with "LL5".
* Replaced every occurrence of "sys_stress" in variable names with "sys" because the extra "stress" was redundant.
* Renamed `plot_system_stress()` to `plot_stress()` because it is planned to plot more stresses with this function in a future update.
* Changed the order of the columns in the output of `predict_ecxsys()`.
* Improved the internal structure of the package.
* Improved the tests.
* Improved the documentation.


# stressaddition 1.11.1

* First public version.
* Added a `NEWS.md` file to track changes to the package.
