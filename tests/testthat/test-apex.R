mat <- matrix(1:36, nrow = 6)
test_that("apex works", {
  expect_no_error(apex(mat))
  expect_message(apex(mat, default = c(3, 3)))
 expect_equal(
   dim(apex(mat)),
       c(3, 3))
 expect_equal(
   dim(apex(mat, 2)),
   c(2, 2)
 )
 expect_equal(
   dim(apex(mat, c(2, 4))),
   c(2, 4)
 )
  arr <- array(mat, dim = c(6, 6, 3))
  expect_equal(
    dim(apex(arr, 2)),
    c(2, 2, 3)
  )
  expect_equal(
    dim(apex(arr, c(2, 4))),
    c(2, 4, 3)
  )
  expect_equal(
    dim(apex(arr, c(2, 4, 1))),
    c(2, 4, 1)
  )
  options(asr.np = NULL)
  expect_error(apex(x = NULL, np = NULL, default = NULL))
  options(asr.np = NULL)
  expect_error(apex(x = mat, np = 2, default = 2))
})
