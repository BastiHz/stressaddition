
<!-- README.md is generated from README.Rmd. Please edit that file -->

# stressaddition

This R package makes it possible to model tri-phasic
concentration-response relationships using the stress addition approach.
It is useful for the analysis of ecotoxicological data where the
traditional concentration addition or effect addition models are
inadequate. Its main functions are `ecxsys()` and `multi_tox()`.

`ecxsys()` implements **EC<sub>x-SyS</sub>**, the tri-phasic
concentration-response model introduced in [Liess, M., Henz, S. &
Knillmann, S. Predicting low-concentration effects of pesticides. Sci
Rep 9, 15248 (2019)](https://doi.org/10.1038/s41598-019-51645-4). It is
applicable to modelling ecotoxicological experiments with and without
environmental stress where the response contains a hormesis effect.

`multi_tox()` implements **Multi-TOX**, a model for binary mixtures of
toxicants where each toxicant exhibits a tri-phasic
concentration-response relationship. See [Liess, M., Henz, S. & Shahid,
N. Modeling the synergistic effects of toxicant mixtures. Environ Sci
Eur 32, 119 (2020)](https://doi.org/10.1186/s12302-020-00394-7).

The EC<sub>x-SyS</sub> and Multi-TOX models are also available as part
of the [Indicate app](http://www.systemecology.eu/indicate) which offers
an easy to use graphical user interface.

## Installation

This package is available from CRAN:

``` r
install.packages("stressaddition")
```

You can also get the development version (potentially unstable) from
GitLab:

``` r
install.packages("remotes")
remotes::install_gitlab("oekotox/stressaddition", host = "git.ufz.de")
```

Alternatively, there are binary and source builds of the current release
and older versions available for download from the [releases
page](https://git.ufz.de/oekotox/stressaddition/-/releases).

## Examples

### EC<sub>x-SyS</sub>

Model a concentration-response relationship with hormesis:

``` r
library(stressaddition)
model_a <- ecxsys(
    concentration = c(0, 0.05, 0.5, 5, 30),
    survival_tox_observed = c(90, 81, 92, 28, 0),
    survival_tox_env_observed = c(29, 27, 33, 5, 0),
    hormesis_concentration = 0.5
)
```

Calculate the LC<sub>50</sub> under the influence of toxicant and system
stress and the LC<sub>10</sub> under the influence of toxicant,
environmental and system stress:

``` r
lc(model_a, "survival_tox_sys", 50)
#> $response
#> [1] 44.95368
#> 
#> $concentration
#> [1] 3.375735
lc(model_a, "survival_tox_env_sys", 10)
#> $response
#> [1] 26.41904
#> 
#> $concentration
#> [1] 0.0008571244
```

Plot the survival and the system stresses:

``` r
par(mfrow = c(2, 1), mar = c(4, 4, 0.5, 0.1))
plot_survival(model_a)
plot_stress(model_a)
```

<img src="images/README-example_plot-1.png" width="100%" />

### Multi-TOX

Define an additional single toxicant model and calculate the survival
for some binary concentration mixtures:

``` r
model_b <- ecxsys(
    concentration = c(0, 0.01, 0.1, 1, 10, 100),
    survival_tox_observed = c(96, 89, 91, 57, 9, 0),
    hormesis_concentration = 0.1
)
multi_tox(
    model_a,
    model_b,
    concentration_a = c(0.1, 0.3, 2, 15),
    concentration_b = c(0.04, 0.1, 1, 13)
)[, 1:3]
#>   concentration_a concentration_b survival
#> 1             0.1            0.04 84.44956
#> 2             0.3            0.10 73.53734
#> 3             2.0            1.00 13.38661
#> 4            15.0           13.00  0.00000
```

## Citation

Please cite this package if you use it in your analysis. See
`citation("stressaddition")` for details.

## Copyright and License

Copyright (c) 2020,  
Helmholtz-Zentrum fuer Umweltforschung GmbH - UFZ.  
All rights reserved.

The code is a property of:

Helmholtz-Zentrum fuer Umweltforschung GmbH - UFZ  
Registered Office: Leipzig  
Registration Office: Amtsgericht Leipzig  
Trade Register: Nr. B 4703  
Chairman of the Supervisory Board: MinDirig’in Oda Keppler  
Scientific Director: Prof. Dr. Georg Teutsch  
Administrative Director: Dr. Sabine König

stressaddition is free software: you can redistribute it and/or modify  
it under the terms of the GNU General Public License as published by  
the Free Software Foundation, either version 3 of the License, or  
(at your option) any later version.

This program is distributed in the hope that it will be useful,  
but WITHOUT ANY WARRANTY; without even the implied warranty of  
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the  
GNU General Public License for more details.

You should have received a copy of the GNU General Public License  
along with this program. If not, see <https://www.gnu.org/licenses/>.
