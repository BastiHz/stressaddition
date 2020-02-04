# stressaddition
This is the R implementation of the tri-phasic concentration-response model introduced in
[Liess, M., Henz, S. & Knillmann, S. Predicting low-concentration effects of pesticides. Sci Rep 9, 15248 (2019).](https://doi.org/10.1038/s41598-019-51645-4)
It allows modeling of ecotoxicological experiments where the response shows signs of a hormesis effect.

## Installation
Stressaddition is not on CRAN. You can install the newest version from GitLab using the devtools package:
``` r
# install.packages("devtools")
devtools::install_gitlab("oekotox/stressaddition", host = "git.ufz.de")
```
Alternatively there are binary and source builds downloadable from the [releases page](https://git.ufz.de/oekotox/stressaddition/-/releases).

## Updating
RStudio's integrated package updater won't detect updates in packages installed from GitHub or GitLab. I recommend running 
```r
# install.packages("remotes")
remotes::update_packages()
```
in regular intervals to check for updates from those sources.

## Citation
Please cite this package if you use it in your analysis. See `citation("stressaddition")` for details.

## Example
In the paper we describe the model in the context of survival experiments. However, it can also be used to model other concentration dependent responses. For this reason the more general term "effect" instead of "survival" is used throughout the package.
```r
library(stressaddition)
model <- ecxsys(
    concentration = c(0, 0.03, 0.3, 3, 10),
    effect_tox_observed = c(85, 76, 94, 35, 0),
    effect_tox_env_observed = c(24, 23, 32, 0, 0),
    hormesis_concentration = 0.3
)

# Plot the effect and the system stress:
par(mfrow = c(2, 1))
plot_effect(model)
plot_system_stress(model)

# The LC50 of the effect under the influence of toxicant and system tress:
ec(model, "effect_tox_sys", 50)

# The LC10 of the effect under the influence  of toxicant, system and environmental tress:
ec(model, "effect_tox_env_sys", 10)
```

## License
TODO: license info
