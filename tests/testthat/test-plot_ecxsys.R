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


test_that("log ticks are correct", {
  x <- c(0.03, 0.3, 3, 30)
  ticks <- get_log_ticks(x)
  expect_equal(ticks$major, c(0.01, 0.10, 1.00, 10.00, 100.00))
  expect_equal(ticks$major_labels, c("0", "0.1", "1", "10", "100"))
  expect_equal(
      ticks$minor,
      c(0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.09, 0.20, 0.30, 0.40, 0.50,
        0.60, 0.70, 0.80, 0.90, 2.00, 3.00, 4.00, 5.00, 6.00, 7.00, 8.00, 9.00,
        20.00, 30.00, 40.00, 50.00, 60.00, 70.00, 80.00, 90.00))
})
