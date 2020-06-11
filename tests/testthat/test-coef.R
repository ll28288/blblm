test_that("coef works", {
  x=blblm(mpg~wt*hp,data=mtcars,m=3,B=100)
  expect_equal(class(coef(x)), "numeric")
  expect_equal(length(coef(x)),4)
})
