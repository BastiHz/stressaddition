# stressaddition
This is the R implementation of the tri-phasic concentration-response model introduced in
[Liess, M., Henz, S. & Knillmann, S. Predicting low-concentration effects of pesticides. Sci Rep 9, 15248 (2019).](https://doi.org/10.1038/s41598-019-51645-4)
It allows modeling of ecotoxicological experiments where the response shows signs of a hormesis effect.

## Installation
Stressaddition is not on CRAN. You can install the most recent stable version from GitLab using the devtools package:
``` r
# install.packages("devtools")
devtools::install_gitlab("oekotox/stressaddition", host = "git.ufz.de")
```
Alternatively there are binary and source builds downloadable from the [releases page](https://git.ufz.de/oekotox/stressaddition/-/releases).

## Updating
RStudio's integrated package updater won't detect updates in packages installed from GitHub or GitLab. I recommend running 
```r
devtools::update_packages()
```
in regular intervals to check for updates from those sources.

## Citation
Please cite this package if you use it in your analysis. See `citation("stressaddition")` for details.

## Example
In the paper we use the model in the context of survival experiments. However, it can also be applicable in modeling other concentration or dose dependent responses. For this reason the more general term "effect" instead of "survival" is used throughout the package.
```r
library(stressaddition)
model <- ecxsys(
    concentration = c(0, 0.05, 0.5, 5, 30),
    hormesis_concentration = 0.5,
    effect_tox_observed = c(90, 81, 92, 28, 0),
    effect_tox_env_observed = c(29, 27, 33, 5, 0)
)

# Plot the effect and the system stress:
par(mfrow = c(2, 1))
plot_effect(model)
plot_stress(model)

# The LC50 of the effect under the influence of toxicant and system tress:
ec(model, "effect_tox_sys", 50)

# The LC10 of the effect under the influence  of toxicant, system and environmental tress:
ec(model, "effect_tox_env_sys", 10)
```

## Copyright and License
Copyright (c) 2020,  
Helmholtz-Zentrum fuer Umweltforschung GmbH - UFZ.  
All rights reserved.

The code is a property of:

Helmholtz-Zentrum fuer Umweltforschung GmbH - UFZ  
Registered Office: Leipzig  
Registration Office: Amtsgericht Leipzig  
Trade Register: Nr. B 4703  
Chairman of the Supervisory Board: MinDirig'in Oda Keppler  
Scientific Director: Prof. Dr. Georg Teutsch  
Administrative Director: Dr. Sabine König  


stressaddition is free software: you can redistribute it and/or modify  
it under the terms of the GNU General Public License as published by  
the Free Software Foundation, either version 3 of the License, or  
(at your option) any later version.

This program is distributed in the hope that it will be useful,  
but WITHOUT ANY WARRANTY; without even the implied warranty of  
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the  
GNU General Public License for more details.

You should have received a copy of the GNU General Public License  
along with this program.  If not, see <https://www.gnu.org/licenses/>.
