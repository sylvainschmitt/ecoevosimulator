test_that("Simulator", {
  sim <- simulator()
  expect_equal(dim(sim), c(120000,7))
})
