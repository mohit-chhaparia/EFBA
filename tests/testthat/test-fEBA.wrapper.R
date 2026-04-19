

make_feba_alpha <- function(K , N , alpha_base = 0.05) {
  bw <- (K + 1) / (N + 1)
  alpha_base / ceiling((1 - 2 * bw / 0.5) * (floor(N / 2) + 1) / 20)
}


test_that("fEBA.wrapper returns a named list with all expected elements" , {
  set.seed(47)
  X <- fws.sim(nb = 10 , gsz = 5 , Ts = 200 , seed = 234)
  N <- 40
  K <- 2
  Rsel <- 4
  out <- fEBA.wrapper(X = X , Rsel = Rsel , K = K , N = N , ndraw = 50 ,
                      alpha = make_feba_alpha(K , N) , std = FALSE ,
                      blockdiag = TRUE , dcap = 20)
  expect_true(is.list(out))
  expect_true(all(c("part.final" , "part.list" , "summary" , "fhat_pmt" ,
                    "ghat" , "log") %in% names(out)))
})

test_that("fEBA.wrapper part.final starts at 0" , {
  set.seed(47)
  X <- fws.sim(nb = 10 , gsz = 5 , Ts = 200 , seed = 234)
  N <- 40
  K <- 2
  Rsel <- 4
  out <- fEBA.wrapper(X = X , Rsel = Rsel , K = K , N = N , ndraw = 50 ,
                      alpha = make_feba_alpha(K , N) , std = FALSE ,
                      blockdiag = TRUE , dcap = 20)
  expect_equal(out$part.final[1] , 0)
})

test_that("fEBA.wrapper part.final ends at the Nyquist frequency" , {
  set.seed(47)
  N <- 40
  K <- 2
  Rsel <- 4
  X <- fws.sim(nb = 10 , gsz = 5 , Ts = 200 , seed = 234)
  out <- fEBA.wrapper(X = X , Rsel = Rsel , K = K , N = N , ndraw = 50 ,
                      alpha = make_feba_alpha(K , N) , std = FALSE ,
                      blockdiag = TRUE , dcap = 20)
  nyquist <- floor(N / 2) / N
  expect_equal(out$part.final[length(out$part.final)] , nyquist)
})

test_that("fEBA.wrapper part.final is a non-decreasing numeric vector" , {
  set.seed(47)
  N <- 40
  K <- 2
  Rsel <- 4
  X <- fws.sim(nb = 10 , gsz = 5 , Ts = 200 , seed = 234)
  out <- fEBA.wrapper(X = X , Rsel = Rsel , K = K , N = N , ndraw = 50 ,
                      alpha = make_feba_alpha(K , N) , std = FALSE ,
                      blockdiag = TRUE , dcap = 20)
  expect_true(is.numeric(out$part.final))
  expect_gte(length(out$part.final) , 2)
  expect_true(all(diff(out$part.final) > 0))
})

test_that("fEBA.wrapper part.list first element equals starting partition" , {
  set.seed(47)
  N <- 40
  K <- 2
  Rsel <- 4
  X <- fws.sim(nb = 10 , gsz = 5 , Ts = 200 , seed = 234)
  out <- fEBA.wrapper(X = X , Rsel = Rsel , K = K , N = N , ndraw = 50 ,
                      alpha = make_feba_alpha(K , N) , std = FALSE ,
                      blockdiag = TRUE , dcap = 20)
  expect_true(is.list(out$part.list))
  expect_gte(length(out$part.list) , 1)
  expect_equal(out$part.list[[1]] , c(0 , floor(N / 2) / N))
})

test_that("fEBA.wrapper on functional white noise finds no extra partition points" , {
  set.seed(123)
  N <- 40
  K <- 2
  Rsel <- 3
  X <- fws.sim(nb = 10 , gsz = 5 , Ts = 200 , seed = 999)
  out <- fEBA.wrapper(X = X , Rsel = Rsel , K = K , N = N , ndraw = 50 ,
                      alpha = make_feba_alpha(K , N , alpha_base = 1e-6) ,
                      std = FALSE , blockdiag = TRUE , dcap = 20)
  expect_length(out$part.final , 2)
})
