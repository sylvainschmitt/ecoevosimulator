test_that("Simulator C++", {
  expect_type(simulatorCpp(Topography = sinusoidalTopography(grid = 10)), "list")
})
