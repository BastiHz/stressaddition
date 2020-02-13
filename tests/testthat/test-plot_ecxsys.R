context("plots")


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
