library(ggplot2)
test_that("Plots", {
  expect_true("gtable" %in% class(plotSim(simulator())))
  expect_true(is.ggplot(plotMaps(simulator())))
  expect_true(is.ggplot(plotTrajectories(simulator())))
})
