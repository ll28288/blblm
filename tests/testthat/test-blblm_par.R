test_that("blblm_par works", {
  cl=makec(2)
  x=blblm_par(mpg~wt*hp,data=mtcars,m=3,B=100,cl=cl)
  stopc(cl)
  expect_equal(class(x), "blblm")
  expect_equal(class(coef(x)), "numeric")
  expect_equal(length(coef(x)),4)
})
