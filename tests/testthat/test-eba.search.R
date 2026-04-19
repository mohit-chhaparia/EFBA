# EBA tests for error handling
test_that("errors if X is not a numeric Vector" , {
  T <- 50000
  X <- c("a" , "b" , "c")
  N <- 500
  K <- 15
  alpha <- 0.05
  std <- FALSE
  expect_error(eba.search(X = X , N = N , K = K , std = std , alpha = alpha))
})

test_that("errors if X has no missing or non-finite values" , {
  T <- 50000
  X <- eba.simdata(T = T)
  X$wn[2] <- NA
  N <- 500
  K <- 15
  alpha <- 0.05
  std <- FALSE
  expect_error(eba.search(X = X$wn , N = N , K = K , std = std , alpha = alpha))
})

test_that("errors if N is missing" , {
  T <- 50000
  X <- eba.simdata(T = T)
  N <- NA
  K <- 15
  alpha <- 0.05
  std <- FALSE
  expect_error(eba.search(X = X$wn , N = N , K = K , std = std , alpha = alpha))
})

test_that("errors if N < 0" , {
  T <- 50000
  X <- eba.simdata(T = T)
  N <- -5000
  K <- 15
  alpha <- 0.05
  std <- FALSE
  expect_error(eba.search(X = X$wn , N = N , K = K , std = std , alpha = alpha))
})

test_that("errors if N is greater than the length of X" , {
  T <- 50000
  X <- eba.simdata(T = T)
  N <- 500000
  K <- 15
  alpha <- 0.05
  std <- FALSE
  expect_error(eba.search(X = X$wn , N = N , K = K , std = std , alpha = alpha))
})

test_that("error if N is less than 30" , {
  T <- 50000
  X <- eba.simdata(T = T)
  N <- 25
  K <- 15
  alpha <- 0.05
  std <- FALSE
  expect_error(eba.search(X = X$wn , N = N , K = K , std = std , alpha = alpha))
})

test_that("error if K is non-finite or missing" , {
  T <- 50000
  X <- eba.simdata(T = T)
  N <- 500
  K <- NA
  alpha <- 0.05
  std <- FALSE
  expect_error(eba.search(X = X$wn , N = N , K = K , std = std , alpha = alpha))
})

test_that("error if K is too large, greater than floor(2 * N * 0.15 - 1)" , {
  T <- 50000
  X <- eba.simdata(T = T)
  N <- 500
  K <- 500
  alpha <- 0.05
  std <- FALSE
  expect_error(eba.search(X = X$wn , N = N , K = K , std = std , alpha = alpha))
})

# eba.search tests for FRESH statistic calculations

test_that("error in FRESH statistic calculations linear" , {
  set.seed(823819) #if you change the seed, you will get different results
  T <- 50000
  X <- eba.simdata(T = T)
  N <- 500
  K <- 15
  alpha <- 0.05
  std <- FALSE
  ebaout.bl <- eba.search(X = X$bL , N = N , K = K , std = std , alpha = alpha)
  expect_equal(ebaout.bl$part.final , c(0.000 , 0.150 , 0.344 , 0.500))
})

test_that("error in FRESH statistic calculations white noise" , {
  set.seed(823819) #if you change the seed, you will get different results
  T <- 50000
  X <- eba.simdata(T=T)
  N <- 500
  K <- 15
  alpha <- 0.05
  std <- FALSE
  ebaout.wn <- eba.search(X = X$wn , N = N , K = K , std = std , alpha = alpha)
  expect_equal(ebaout.wn$part.final , c(0.000 , 0.500))
})

test_that("error in FRESH statistic calculations Sinusoidal" , {
  set.seed(823819) #if you change the seed, you will get different results
  T <- 50000
  X <- eba.simdata(T = T)
  N <- 500
  K <- 15
  alpha <- 0.05
  std <- FALSE
  ebaout.bs <- eba.search(X = X$bS , N = N , K = K , std = std , alpha = alpha)
  expect_equal(ebaout.bs$part.final , c(0.000 , 0.144 , 0.338 , 0.500))
})

test_that("errors if K is zero" , {
  T <- 2000
  X <- eba.simdata(T = T)
  expect_error(eba.search(X = X$wn , N = 100 , K = 0 , std = FALSE , alpha = 0.05))
})

test_that("errors if K is negative" , {
  T <- 2000
  X <- eba.simdata(T = T)
  expect_error(eba.search(X = X$wn , N = 100 , K = -3 , std = FALSE , alpha = 0.05))
})

test_that("errors if K is non-integer" , {
  T <- 2000
  X <- eba.simdata(T = T)
  expect_error(eba.search(X = X$wn , N = 100 , K = 2.5 , std = FALSE , alpha = 0.05))
})

test_that("errors if N is zero" , {
  T <- 2000
  X <- eba.simdata(T = T)
  expect_error(eba.search(X = X$wn , N = 0 , K = 5 , std = FALSE , alpha = 0.05))
})

test_that("errors if N is a non-integer positive value" , {
  T <- 2000
  X <- eba.simdata(T = T)
  expect_error(eba.search(X = X$wn , N = 100.5 , K = 5 , std = FALSE , alpha = 0.05))
})

test_that("errors if X is empty" , {
  expect_error(eba.search(X = numeric(0) , N = 100 , K = 5 , std = FALSE , alpha = 0.05))
})

test_that("eba.search returns a named list with all expected elements" , {
  set.seed(42)
  T <- 2000
  X <- eba.simdata(T = T)
  out <- eba.search(X = X$wn , N = 100 , K = 5 , std = FALSE , alpha = 0.05)
  expect_true(is.list(out))
  expect_true(all(c("part.final" , "part.list" , "log" , "mtspec" , "flat" , "pvals") %in% names(out)))
})

test_that("part.final starts at 0 and ends at the Nyquist frequency 0.5" , {
  set.seed(42)
  T <- 2000
  X <- eba.simdata(T = T)
  out <- eba.search(X = X$wn , N = 100 , K = 5 , std = FALSE , alpha = 0.05)
  expect_equal(out$part.final[1] , 0)
  expect_equal(out$part.final[length(out$part.final)] , 0.5)
})

test_that("part.final is a non-decreasing numeric vector of length >= 2" , {
  set.seed(42)
  T <- 2000
  X <- eba.simdata(T = T)
  out <- eba.search(X = X$wn , N = 100 , K = 5 , std = FALSE , alpha = 0.05)
  expect_true(is.numeric(out$part.final))
  expect_gte(length(out$part.final) , 2)
  expect_true(all(diff(out$part.final) > 0))
})

test_that("part.list is a list whose first element matches the starting partition" , {
  set.seed(42)
  T <- 2000
  X <- eba.simdata(T = T)
  out <- eba.search(X = X$wn , N = 100 , K = 5 , std = FALSE , alpha = 0.05)
  expect_true(is.list(out$part.list))
  expect_gte(length(out$part.list) , 1)
  expect_equal(out$part.list[[1]] , c(0 , 0.5))
})

test_that("pvals is a data.frame with columns 'Frequency' and 'P.Values'" , {
  set.seed(42)
  T <- 2000
  X <- eba.simdata(T = T)
  out <- eba.search(X = X$wn , N = 100 , K = 5 , std = FALSE , alpha = 0.05)
  expect_s3_class(out$pvals , "data.frame")
  expect_named(out$pvals , c("Frequency" , "P.Values"))
})

test_that("pvals Frequency values are between 0 and 0.5" , {
  set.seed(42)
  T <- 2000
  X <- eba.simdata(T = T)
  out <- eba.search(X = X$wn , N = 100 , K = 5 , std = FALSE , alpha = 0.05)
  expect_true(all(out$pvals$Frequency >= 0 & out$pvals$Frequency <= 0.5))
})

test_that("pvals p-values are between 0 and 1" , {
  set.seed(42)
  T <- 2000
  X <- eba.simdata(T = T)
  out <- eba.search(X = X$wn , N = 100 , K = 5 , std = FALSE , alpha = 0.05)
  expect_true(all(out$pvals$P.Values >= 0 & out$pvals$P.Values <= 1))
})

test_that("flat has columns 'partfinal' and 'pval.flat'" , {
  set.seed(42)
  T <- 2000
  X <- eba.simdata(T = T)
  out <- eba.search(X = X$wn , N = 100 , K = 5 , std = FALSE , alpha = 0.05)
  expect_true(is.matrix(out$flat))
  expect_equal(colnames(out$flat) , c("partfinal" , "pval.flat"))
})

test_that("mtspec has elements 'mtspec', 'tapers', 'f', and 't'" , {
  set.seed(42)
  T <- 2000
  X <- eba.simdata(T = T)
  out <- eba.search(X = X$wn , N = 100 , K = 5 , std = FALSE , alpha = 0.05)
  expect_true(all(c("mtspec" , "tapers" , "f" , "t") %in% names(out$mtspec)))
})

test_that("eba.search runs without error with std = TRUE" , {
  set.seed(42)
  T <- 2000
  X <- eba.simdata(T = T)
  expect_no_error(eba.search(X = X$wn , N = 100 , K = 5 , std = TRUE , alpha = 0.05))
})

test_that("eba.search output structure is the same when std = TRUE" , {
  set.seed(42)
  T <- 2000
  X <- eba.simdata(T = T)
  out <- eba.search(X = X$wn , N = 100 , K = 5 , std = TRUE , alpha = 0.05)
  expect_true(is.list(out))
  expect_true(all(c("part.final" , "part.list" , "log" , "mtspec" , "flat" , "pvals") %in% names(out)))
  expect_equal(out$part.final[1] , 0)
  expect_equal(out$part.final[length(out$part.final)] , 0.5)
})

test_that("eba.search with stricter alpha returns fewer or equal partition points" , {
  set.seed(42)
  T <- 2000
  X <- eba.simdata(T = T)
  out_loose <- eba.search(X = X$bL , N = 100 , K = 5 , std = FALSE , alpha = 0.10)
  out_strict <- eba.search(X = X$bL , N = 100 , K = 5 , std = FALSE , alpha = 0.001)
  expect_lte(length(out_strict$part.final) , length(out_loose$part.final))
})


