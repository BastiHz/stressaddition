# Copyright (C) 2020  Helmholtz-Zentrum fuer Umweltforschung GmbH - UFZ
# See file inst/COPYRIGHTS for details.
#
# This file is part of the R package stressaddition.
#
# stressaddition is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.


# A collection of some internal helper functions.


clamp <- function(x, lower = 0, upper = 1) {
    # Returns lower if x < lower, returns upper if x > upper, else returns x
    pmin(pmax(x, lower), upper)
}


get_log_ticks <- function(x) {
    # Calculate the positions of major and minor tick marks on a base 10
    # logarithmic axis.
    stopifnot(min(x, na.rm = TRUE) > 0)
    x <- log10(x)
    major <- 10 ^ seq(
        floor(min(x, na.rm = TRUE)),
        ceiling(max(x, na.rm = TRUE))
    )
    n_between <- length(major) - 1
    minor <- integer(n_between * 8)
    for (i in 1:n_between) {
        a <- major[i]
        b <- major[i + 1]
        minor[seq(i * 8 - 7, i * 8)] <- seq(a + a, b - a, a)
    }
    major_tick_labels <- formatC(major, format = "fg")
    major_tick_labels[1] <- "0"
    list(
        major = major,
        minor = minor,
        major_labels = major_tick_labels
    )
}
