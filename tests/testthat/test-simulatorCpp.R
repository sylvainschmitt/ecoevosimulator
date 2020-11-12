test_that("Simulator C++", {
  Topo <- sinusoidalTopography(grid = 10, Elim = 5, amplitude = 0.01)
  NCI <- generateNCI(grid = 10, Nt = 50)
  sim <- simulatorCpp(Topo = Topo, NCI = NCI, grid = 10, Nt = 50)
  expect_type(sim, "list")
})

