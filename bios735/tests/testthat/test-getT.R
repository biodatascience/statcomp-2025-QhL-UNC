test_that("examine the input of the function", {

  # x and f should not be a zero matrix and vector
  expect_error(getT(matrix(0), c()))

  # x and f should have consistent dimensions
  expect_error(getT(matrix(rnorm(4*50), nrow = 4,ncol = 50), c(1, 1, 1, 2, 2, 2)))

  ## there is equal sample size between the two levels of f
  expect_error(getT(matrix(rnorm(4*50), nrow = 4,ncol = 50), c(1, 2, 2, 2)))
})

test_that("expecting same output as t test with equal variance", {
  set.seed(1)
  x0 <- matrix(rnorm(400*50),nrow=400,ncol=50)
  f0 <- gl(2,25)
  expect_equal(getT(x0, f0),
               unname(sapply(seq_len(400), function(i) t.test(x0[i,] ~ f0, var.equal = TRUE)$statistic))
  )

  x1 <- matrix(rnorm(600*20),nrow=600,ncol=20)
  f1 <- c(1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2)
  expect_equal(getT(x1, f1),
               unname(sapply(seq_len(600), function(i) t.test(x1[i,] ~ f1, var.equal=TRUE)$statistic))
  )

})
