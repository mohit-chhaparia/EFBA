

test_that("f3bL.sim returns a numeric matrix" , {
  X <- f3bL.sim(nb = 10 , gsz = 5 , Ts = 100 , seed = 234)
  expect_true(is.matrix(X))
  expect_true(is.numeric(X))
})

test_that("f3bL.sim returns a matrix with Ts rows and gsz columns" , {
  Ts <- 120
  gsz <- 6
  X <- f3bL.sim(nb = 10 , gsz = gsz , Ts = Ts , seed = 234)
  expect_equal(nrow(X) , Ts)
  expect_equal(ncol(X) , gsz)
})

test_that("f3bL.sim output contains only finite values" , {
  X <- f3bL.sim(nb = 10 , gsz = 5 , Ts = 100 , seed = 234)
  expect_true(all(is.finite(X)))
})

test_that("f3bL.sim is reproducible with the same seed" , {
  X1 <- f3bL.sim(nb = 10 , gsz = 5 , Ts = 100 , seed = 42)
  X2 <- f3bL.sim(nb = 10 , gsz = 5 , Ts = 100 , seed = 42)
  expect_identical(X1 , X2)
})

