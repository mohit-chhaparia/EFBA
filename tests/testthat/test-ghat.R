

test_that("ghat returns an array with the same dimensions as its input" , {
  set.seed(234)
  X <- fws.sim(nb = 10 , gsz = 5 , Ts = 200 , seed = 234)
  p <- fhat_pmt(X , N = 40 , K = 2 , Rsel = 4 , stdz = FALSE)
  g <- ghat(p)
  expect_equal(dim(g) , dim(p))
})

test_that("ghat produces near-zero means along the time (third) dimension" , {
  set.seed(234)
  X <- fws.sim(nb = 10 , gsz = 5 , Ts = 200 , seed = 234)
  p <- fhat_pmt(X , N = 40 , K = 2 , Rsel = 4 , stdz = FALSE)
  g <- ghat(p)
  time_means <- apply(Re(g) , c(1 , 2) , mean)
  expect_true(all(abs(time_means) < 1e-10))
})

