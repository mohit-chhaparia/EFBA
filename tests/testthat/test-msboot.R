

test_that("msboot returns a list of length 7" , {
  set.seed(123)
  x <- cbind(meba.simdata(120)$bL , meba.simdata(120)$bS)
  out <- msboot(nrep = 5 , x = x , Wsel = 2 , stdz = FALSE , ncore = 1)
  expect_true(is.list(out))
  expect_length(out , 7)
})

test_that("msboot significance indicator (element 4) is a two-column matrix" , {
  set.seed(123)
  x <- cbind(meba.simdata(120)$bL , meba.simdata(120)$bS)
  out <- msboot(nrep = 5 , x = x , Wsel = 2 , stdz = FALSE , ncore = 1)
  expect_true(is.matrix(out[[4]]))
  expect_equal(ncol(out[[4]]) , 2)
})
