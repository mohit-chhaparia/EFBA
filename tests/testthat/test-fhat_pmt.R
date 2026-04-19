

test_that("fhat_pmt returns a 3-dimensional array" , {
  set.seed(234)
  X <- fws.sim(nb = 10 , gsz = 5 , Ts = 200 , seed = 234)
  p <- fhat_pmt(X , N = 40 , K = 2 , Rsel = 4 , stdz = FALSE)
  expect_equal(length(dim(p)) , 3)
})

test_that("fhat_pmt first dimension equals floor(N/2) + 1" , {
  set.seed(234)
  N <- 40
  X <- fws.sim(nb = 10 , gsz = 5 , Ts = 200 , seed = 234)
  p <- fhat_pmt(X , N = N , K = 2 , Rsel = 4 , stdz = FALSE)
  expect_equal(dim(p)[1] , floor(N / 2) + 1)
})

test_that("fhat_pmt second dimension equals Rsel^2" , {
  set.seed(234)
  Rsel <- 4
  X <- fws.sim(nb = 10 , gsz = 5 , Ts = 200 , seed = 234)
  p <- fhat_pmt(X , N = 40 , K = 2 , Rsel = Rsel , stdz = FALSE)
  expect_equal(dim(p)[2] , Rsel ^ 2)
})

test_that("fhat_pmt third dimension equals floor(T/N)" , {
  set.seed(234)
  n_time <- 200
  N <- 40
  X <- fws.sim(nb = 10 , gsz = 5 , Ts = n_time , seed = 234)
  p <- fhat_pmt(X , N = N , K = 2 , Rsel = 4 , stdz = FALSE)
  expect_equal(dim(p)[3] , floor(n_time / N))
})

