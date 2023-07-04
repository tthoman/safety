test_that("likelihood list is generated correctly ", {
  expect_equal(rep(c(1:5),5), c(1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5))
})

test_that("likelihood_score list is generated correctly ", {
  expect_equal(rep(c(1,2,4,6,12),5), c(1,2,4,6,12,1,2,4,6,12,1,2,4,6,12,1,2,4,6,12,1,2,4,6,12))
})

test_that("consequence list is generated correctly ", {
  expect_equal(rep(c(1:5),each=5), c(1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,4,4,4,4,4,5,5,5,5,5))
})

test_that("consequence_score list is generated correctly ", {
  expect_equal(rep(c(1,2,4,6,12),each=5), c(1,1,1,1,1,2,2,2,2,2,4,4,4,4,4,6,6,6,6,6,12,12,12,12,12))
})


