

test_that("fhat_lp returns a 3-dimensional array" , {
  set.seed(42)
  X <- matrix(rnorm(300) , ncol = 3)
  p <- fhat_lp(X , N = 40 , stdz = FALSE)
  expect_equal(length(dim(p)) , 3)
})

test_that("fhat_lp first dimension equals floor(N/2) + 1" , {
  set.seed(42)
  N <- 40
  X <- matrix(rnorm(400) , ncol = 4)
  p <- fhat_lp(X , N = N , stdz = FALSE)
  expect_equal(dim(p)[1] , floor(N / 2) + 1)
})

test_that("fhat_lp second dimension equals R^2 (number of columns squared)" , {
  set.seed(42)
  R <- 3
  X <- matrix(rnorm(300) , ncol = R)
  p <- fhat_lp(X , N = 40 , stdz = FALSE)
  expect_equal(dim(p)[2] , R ^ 2)
})

test_that("fhat_lp third dimension equals the number of rows (time points)" , {
  set.seed(42)
  n_time <- 100
  X <- matrix(rnorm(n_time * 3) , ncol = 3)
  p <- fhat_lp(X , N = 40 , stdz = FALSE)
  expect_equal(dim(p)[3] , n_time)
})

