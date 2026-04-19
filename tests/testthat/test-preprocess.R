

test_that("preprocess returns a list with expected elements" , {
  set.seed(1)
  x <- matrix(rnorm(5120) , ncol = 4)
  out <- preprocess(x , dsfrq = 64 , channels = paste0("C" , 1:4))
  expect_true(is.list(out))
  expect_named(out , c("signal" , "Ts" , "N" , "ff"))
})

test_that("preprocess signal has the correct number of columns (channels)" , {
  set.seed(1)
  n_channels <- 4
  x <- matrix(rnorm(5120) , ncol = n_channels)
  out <- preprocess(x , dsfrq = 64 , channels = paste0("C" , 1:n_channels))
  expect_equal(ncol(out$signal) , n_channels)
})

test_that("preprocess assigns provided channel names to the signal columns" , {
  set.seed(1)
  cnames <- paste0("Ch" , 1:3)
  x <- matrix(rnorm(5120 * 3 / 4) , ncol = 3)
  out <- preprocess(x , dsfrq = 64 , channels = cnames)
  expect_equal(colnames(out$signal) , cnames)
})

test_that("preprocess Ts matches the number of rows after downsampling" , {
  set.seed(1)
  x <- matrix(rnorm(5120) , ncol = 4)
  dsfrq <- 64
  out <- preprocess(x , dsfrq = dsfrq , channels = paste0("C" , 1:4))
  expected_Ts <- nrow(x) / (512 / dsfrq)
  expect_equal(out$Ts , expected_Ts)
  expect_equal(nrow(out$signal) , expected_Ts)
})

test_that("preprocess ff contains only values in [0, 0.5]" , {
  set.seed(1)
  x <- matrix(rnorm(5120) , ncol = 4)
  out <- preprocess(x , dsfrq = 64 , channels = paste0("C" , 1:4))
  expect_true(all(out$ff >= 0 & out$ff <= 0.5))
})

test_that("preprocess N is a positive even integer" , {
  set.seed(1)
  x <- matrix(rnorm(5120) , ncol = 4)
  out <- preprocess(x , dsfrq = 64 , channels = paste0("C" , 1:4))
  expect_true(is.numeric(out$N))
  expect_true(out$N > 0)
  expect_equal(out$N %% 2 , 0)
})

test_that("preprocess works for a different downsample frequency" , {
  set.seed(2)
  x <- matrix(rnorm(5120) , ncol = 2)
  dsfrq <- 128
  out <- preprocess(x , dsfrq = dsfrq , channels = c("A" , "B"))
  expected_Ts <- nrow(x) / (512 / dsfrq)
  expect_equal(out$Ts , expected_Ts)
})
