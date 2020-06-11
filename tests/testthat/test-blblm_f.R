test_that("blblm_f works", {
  planf(4)
  x=blblm_f(mpg~wt*hp,data=mtcars,m=3,B=100)
  expect_equal(class(x), "blblm")
  expect_equal(class(coef(x)), "numeric")
  expect_equal(length(coef(x)),4)
})
