test_that("predict works", {
  x=blblm(mpg~wt*hp,data=mtcars,m=3,B=100)
  expect_equal(class(predict(x, data.frame(wt = c(2.5, 3), hp = c(150, 170)))),"numeric")
  expect_equal(class(predict(x, data.frame(wt = c(2.5, 3), hp = c(150, 170)), confidence = TRUE)),"matrix")
})

