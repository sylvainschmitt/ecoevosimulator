test_that("Product environmental matrix C++", {
  expect_equal(dim(product_environmental_matrix(10, 10)), c(10,10))
})
