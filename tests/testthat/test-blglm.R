test_that("blbglm works", {
  y=suppressWarnings(blbglm(vs~wt,data=mtcars,m=3,B=100))
  expect_equal(class(y), "blblm")
  expect_equal(class(coef(y)), "numeric")
  expect_equal(length(coef(y)),2)
})
