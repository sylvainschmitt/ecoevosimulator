library(ggplot2)
test_that("Plot works", {
    expect_true(is.ggplot(plotSim(simulator())))
})
