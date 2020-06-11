test_that("sigma works", {
  x=blblm(mpg~wt*hp,data=mtcars,m=3,B=100)
  expect_equal(length(sigma(x)),1)
  expect_equal(length(sigma(x, confidence = TRUE)),3)
})
