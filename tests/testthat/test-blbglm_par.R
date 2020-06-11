test_that("blbglm_par works", {
  cl=makec(2)
  y=suppressWarnings(blbglm_par(vs~wt,data=mtcars,m=3,B=100,cl=cl))
  stopc(cl)
  expect_equal(class(y), "blblm")
  expect_equal(class(coef(y)), "numeric")
  expect_equal(length(coef(y)),2)
})
