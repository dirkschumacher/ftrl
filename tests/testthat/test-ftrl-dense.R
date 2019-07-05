context("ftrl")

test_that("it is gradient descent with lambdas = 0", {
  optimizer <- FTRLDenseOptimizer(3, lambda1 = 0, lambda2 = 0)
  expect_equal(optimizer$weights(), c(0, 0, 0))
  optimizer$fit(c(1, 2, 3), 1)
  expect_equivalent(optimizer$weights(), c(0.03333333, 0.05, 0.06))
})