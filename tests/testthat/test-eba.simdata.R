


test_that("eba.simdata returns a list with three named elements" , {
  sim <- eba.simdata(T = 1000)
  expect_true(is.list(sim))
  expect_named(sim , c("wn" , "bL" , "bS"))
})

test_that("eba.simdata elements have the requested length T" , {
  n_time <- 1000
  sim <- eba.simdata(T = n_time)
  expect_length(sim$wn , n_time)
  expect_length(sim$bL , n_time)
  expect_length(sim$bS , n_time)
})

test_that("eba.simdata elements are numeric vectors" , {
  sim <- eba.simdata(T = 1000)
  expect_true(is.numeric(sim$wn))
  expect_true(is.numeric(sim$bL))
  expect_true(is.numeric(sim$bS))
})

test_that("eba.simdata elements contain only finite values" , {
  sim <- eba.simdata(T = 1000)
  expect_true(all(is.finite(sim$wn)))
  expect_true(all(is.finite(sim$bL)))
  expect_true(all(is.finite(sim$bS)))
})

test_that("eba.simdata works for different values of T" , {
  for (n_time in c(200 , 500 , 2000)) {
    sim <- eba.simdata(T = n_time)
    expect_length(sim$wn , n_time)
  }
})

test_that("eba.simdata produces different results with different seeds" , {
  set.seed(734947)
  sim1 <- eba.simdata(T = 500)
  set.seed(385939)
  sim2 <- eba.simdata(T = 500)
  expect_false(identical(sim1$wn , sim2$wn))
})



