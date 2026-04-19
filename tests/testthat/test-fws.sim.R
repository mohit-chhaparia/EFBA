

test_that("fws.sim returns a numeric matrix" , {
  X <- fws.sim(nb = 10 , gsz = 5 , Ts = 100 , seed = 234)
  expect_true(is.matrix(X))
  expect_true(is.numeric(X))
})

test_that("fws.sim returns a matrix with Ts rows and gsz columns" , {
  Ts <- 100
  gsz <- 8
  X <- fws.sim(nb = 10 , gsz = gsz , Ts = Ts , seed = 234)
  expect_equal(nrow(X) , Ts)
  expect_equal(ncol(X) , gsz)
})

test_that("fws.sim output contains only finite values" , {
  X <- fws.sim(nb = 10 , gsz = 5 , Ts = 100 , seed = 234)
  expect_true(all(is.finite(X)))
})

test_that("fws.sim is reproducible with the same seed" , {
  X1 <- fws.sim(nb = 10 , gsz = 5 , Ts = 100 , seed = 42)
  X2 <- fws.sim(nb = 10 , gsz = 5 , Ts = 100 , seed = 42)
  expect_identical(X1 , X2)
})

test_that("fws.sim produces different results with different seeds" , {
  X1 <- fws.sim(nb = 10 , gsz = 5 , Ts = 100 , seed = 1)
  X2 <- fws.sim(nb = 10 , gsz = 5 , Ts = 100 , seed = 2)
  expect_false(identical(X1, X2))
})

test_that("fws.sim works with different gsz values" , {
  for (gsz in c(5 , 10 , 20)) {
    X <- fws.sim(nb = 10 , gsz = gsz , Ts = 80 , seed = 1)
    expect_equal(ncol(X) , gsz , label = paste0("fws.sim ncol with gsz=" , gsz))
  }
})

