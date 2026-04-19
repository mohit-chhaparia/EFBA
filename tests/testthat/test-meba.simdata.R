

test_that("meba.simdata returns a list with seven named elements" , {
  sim <- meba.simdata(400)
  expect_true(is.list(sim))
  expect_named(sim , c("wn" , "bL" , "bS" , "bL2f15" , "bL2f35" , "bS2f15" , "bS2f35"))
})

test_that("meba.simdata elements have the requested length t" , {
  n_time <- 400
  sim <- meba.simdata(n_time)
  for (nm in names(sim)) {
    expect_equal(length(sim[[nm]]) , n_time)
  }
})

test_that("meba.simdata elements are numeric vectors" , {
  sim <- meba.simdata(400)
  for (nm in names(sim)) {
    expect_true(is.numeric(sim[[nm]]) ,
                label = paste0("meba.simdata element '" , nm , "' is numeric"))
  }
})

test_that("meba.simdata elements contain only finite values" , {
  sim <- meba.simdata(400)
  for (nm in names(sim)) {
    expect_true(all(is.finite(sim[[nm]])) ,
                label = paste0("meba.simdata element '" , nm , "' is finite"))
  }
})

test_that("meba.simdata works for different values of t" , {
  for (n_time in c(200 , 600)) {
    sim <- meba.simdata(n_time)
    expect_length(sim$wn , n_time)
  }
})
