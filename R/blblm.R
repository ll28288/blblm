#' @import purrr
#' @import stats
#' @import furrr
#' @import future
#' @import parallel
#' @importFrom magrittr %>%
#' @importFrom utils capture.output
#' @details
#' @aliases blblm-package
#' Linear Regression with Little Bag of Bootstraps
"_PACKAGE"


## quiets concerns of R CMD check re: the .'s that appear in pipelines
# from https://github.com/jennybc/googlesheets/blob/master/R/googlesheets.R
utils::globalVariables(c("."))



#' Create cluster
#'
#' Create a new cluster using makeCluster in parallel
#'
#' @param n a number
#'
#' @return cluster
#' @export
makec <- function(n){
  makeCluster(n)
}

#' stop cluster
#'
#' stop a cluster that already begin
#'
#' @param cll cluster
#'
#' @return none
#' @export
stopc <-function(cll){
  stopCluster(cll)
}

#' Generate linear regression with boostrap
#'
#' create a linear regression with the method of bootstrips, with people selected feature
#'
#' @param formula regression formula
#' @param data the data you want to regression
#' @param m number of subdata
#' @param B number of bootstrips
#'
#'
#' @return class blblm
#' @export
#' @examples
#' x=blblm(mpg~wt,data=mtcars,m=3,B=100)
#' coef(x)
blblm <- function(formula, data, m = 10, B = 5000) {
  data_list <- split_data(data, m)
    estimates <- map(
     data_list,
      ~ lm_each_subsample(formula = formula, data = ., n = nrow(data), B = B))
  res <- list(estimates = estimates, formula = formula)
  class(res) <- "blblm"
  invisible(res)
}
#' Generate a plan
#'
#' create a plan for thr blblm_f function
#'
#' @param n number of workers you want
#'
#' @export
planf <- function(n){
  suppressWarnings(plan(multiprocess, workers = n))
}
#' Generate linear regression with boostrap
#'
#' create a linear regression with the method of bootstrips, with people selected feature, and this function is using future map so you need to use it with planf
#' to specified the number of workers you want
#'
#' @param formula regression formula
#' @param data the data you want to regression
#' @param m number of subdata
#' @param B number of bootstrips
#'
#'
#' @return class blblm
#' @examples \dontrun{
#' planf(4)
#' x=blblm_f(mpg~wt,data=mtcars,m=3,B=100)
#' coef(x)}
#' @export
blblm_f <- function(formula, data, m = 10, B = 5000) {
  data_list <- split_data(data, m)
  estimates <- future_map(
    data_list,
    ~ lm_each_subsample(formula = formula, data = ., n = nrow(data), B = B))
  res <- list(estimates = estimates, formula = formula)
  class(res) <- "blblm"
  invisible(res)
}
#' Generate linear regression with boostrap
#'
#' create a linear regression with the method of bootstrips, with people selected feature. Now, support parallel with user specified cluster
#'
#' @param formula regression formula
#' @param data the data you want to regression
#' @param m number of subdata
#' @param B number of bootstrips
#' @param cl cluster specified
#'
#' @return class blblm
#' @export
#' @examples
#' \dontrun{
#' cl <- makec(2)
#' x=blblm_par(mpg~wt,data=mtcars,m=3,B=100,cl=cl)
#' stopCluster(cl)
#' coef(x)
#' }
blblm_par <- function(formula, data, m = 10, B = 5000,cl) {
  data_list <- split_data(data, m)
#  estimates <- map(
#   data_list,
#    ~ lm_each_subsample(formula = formula, data = ., n = nrow(data), B = B))
  estimates <- parLapply(cl,data_list,fun=lm_each_subsample,formula = formula, n = nrow(data), B = B)
  res <- list(estimates = estimates, formula = formula)
  class(res) <- "blblm"
  invisible(res)
}

#' split the data
#' split data into m subdata
#'
#'
#' @param data the data you want to regression
#' @param m number of subdata
#'
#'
#' @return data.frame
#' @export
split_data <- function(data, m) {
  idx <- sample.int(m, nrow(data), replace = TRUE)
  data %>% split(idx)
}

#' Generate linear regression with boostrap
#'
#' compute the estimates
#'
#' @param formula regression formula
#' @param data the data you want to regression
#' @param n number of row of the data
#' @param B number of bootstrips
#'
#'
#' @return vector
#' compute the estimates
#' @export
lm_each_subsample <- function(formula, data, n, B) {
  replicate(B, lm_each_boot(formula, data, n), simplify = FALSE)
}

#' Generate linear regression with boostrap
#'
#' compute the regression estimates for a blb dataset
#'
#' @param formula regression formula
#' @param data the data you want to regression
#' @param n number of row of the data
#'
#' @return regression estimates
#' compute the regression estimates for a blb dataset
#' @export
lm_each_boot <- function(formula, data, n) {
  freqs <- rmultinom(1, n, rep(1, nrow(data)))
  lm1(formula, data, freqs)
}

#' estimate the regression estimates
#'
#' estimate the regression estimates based on given the number of repetitions
#'
#' @param formula regression formula
#' @param data the data you want to regression
#' @param freqs frequency
#'
#' @return estimate the regression estimates based on given the number of repetitions
#' estimate the regression estimates based on given the number of repetitions
#' @export
lm1 <- function(formula, data, freqs) {
  # drop the original closure of formula,
  # otherwise the formula will pick a wront variable from the global scope.
  environment(formula) <- environment()
  fit <- lm(formula, data, weights = freqs)
  list(coef = blbcoef(fit), sigma = blbsigma(fit))
}

#' Generate logistic regression with boostrap
#'
#' create logistic linear regression with the method of bootstrips, with people selected feature.
#'
#' @param formula regression formula
#' @param data the data you want to regression
#' @param m number of subdata
#' @param B number of bootstrips
#'
#' @return class blblm
#' @export
#' @examples
#' \dontrun{
#' cl=makec(2)
#' x=blbglm(mpg~wt,data=mtcars,m=3,B=100)
#' stopCluster(cl)
#' }
blbglm <- function(formula, data, m = 10, B = 5000) {
  data_list <- split_data(data, m)
  estimates <- map(
    data_list,
    ~ glm_each_subsample(formula = formula, data = ., n = nrow(data), B = B))
  res <- list(estimates = estimates, formula = formula)
  class(res) <- "blblm"
  invisible(res)
}


#' get coefficient
#'
#' compute the coefficients from fit
#'
#' @param fit regression model
#'
#' @return vector of coefficient
#' compute the coefficients from fit
#' @export
blbcoef <- function(fit) {
  coef(fit)
}

#' estimate sigma
#'
#' compute sigma from fit
#'
#' @param fit regression model
#'
#' @return double
#' compute sigma from fit
blbsigma <- function(fit) {
  p <- fit$rank
  y <- model.extract(fit$model, "response")
  e <- fitted(fit) - y
  w <- fit$weights
  sqrt(sum(w * (e^2)) / (sum(w) - p))
}

#' view model
#'
#' view the model fomula
#'
#' @param x a model
#' @param ... ...
#'
#' @return print
#' @method print blblm
print.blblm <- function(x, ...) {
  cat("blblm model:", capture.output(x$formula))
  cat("\n")
}

#' compute sigma
#'
#' computer sigma from  estimated values
#'
#' @param object blblm model
#' @param confidence logic value
#' @param level confidence level
#' @param ... ...
#' @return double or a vector of interval
#' @method sigma blblm
sigma.blblm <- function(object, confidence = FALSE, level = 0.95, ...) {
  est <- object$estimates
  sigma <- mean(map_dbl(est, ~ mean(map_dbl(., "sigma"))))
  if (confidence) {
    alpha <- 1 - 0.95
    limits <- est %>%
      map_mean(~ quantile(map_dbl(., "sigma"), c(alpha / 2, 1 - alpha / 2))) %>%
      set_names(NULL)
    return(c(sigma = sigma, lwr = limits[1], upr = limits[2]))
  } else {
    return(sigma)
  }
}

#' computer coeficient
#'
#' computer coeficient from the estimate
#'
#' @param object blblm model
#' @param ... ...
#'
#' @return vector
#' @method coef blblm
coef.blblm <- function(object, ...) {
  est <- object$estimates
  map_mean(est, ~ map_cbind(., "coef") %>% rowMeans())
}

#' computer coeficient
#'
#' computer coeficient from the estimate
#'
#' @param object blblm model
#' @param parm parameter
#' @param level confidence level
#' @param ... ...
#'
#' @return vector of interval
#' @method confint blblm
confint.blblm <- function(object, parm = NULL, level = 0.95, ...) {
  if (is.null(parm)) {
    parm <- attr(terms(object$formula), "term.labels")
  }
  alpha <- 1 - level
  est <- object$estimates
  out <- map_rbind(parm, function(p) {
    map_mean(est, ~ map_dbl(., list("coef", p)) %>% quantile(c(alpha / 2, 1 - alpha / 2)))
  })
  if (is.vector(out)) {
    out <- as.matrix(t(out))
  }
  dimnames(out)[[1]] <- parm
  out
}

#' prediction
#'
#' predict value base on our model
#'
#' @param object blblm model
#' @param new_data new data to predict
#' @param confidence logic value if we want an interval
#' @param level confidence level
#' @param ... ...
#'
#' @return vector of interval
#'
#' @export
#' @method predict blblm
predict.blblm <- function(object, new_data, confidence = FALSE, level = 0.95, ...) {
  est <- object$estimates
  X <- model.matrix(reformulate(attr(terms(object$formula), "term.labels")), new_data)
  if (confidence) {
    map_mean(est, ~ map_cbind(., ~ X %*% .$coef) %>%
      apply(1, mean_lwr_upr, level = level) %>%
      t())
  } else {
    map_mean(est, ~ map_cbind(., ~ X %*% .$coef) %>% rowMeans())
  }
}


mean_lwr_upr <- function(x, level = 0.95) {
  alpha <- 1 - level
  c(fit = mean(x), quantile(x, c(alpha / 2, 1 - alpha / 2)) %>% set_names(c("lwr", "upr")))
}

map_mean <- function(.x, .f, ...) {
  (map(.x, .f, ...) %>% reduce(`+`)) / length(.x)
}

map_cbind <- function(.x, .f, ...) {
  map(.x, .f, ...) %>% reduce(cbind)
}

map_rbind <- function(.x, .f, ...) {
  map(.x, .f, ...) %>% reduce(rbind)
}

#' Generate logistic regression with boostrap
#'
#' create logistic linear regression with the method of bootstrips, with people selected feature. Now, support parallel with user specified cluster
#'
#' @param formula regression formula
#' @param data the data you want to regression
#' @param m number of subdata
#' @param B number of bootstrips
#' @param cl cluster specified
#'
#' @return class blblm
#' @export
#' @examples
#' \dontrun{
#' cl <- makec(2)
#' x=blbglm_par(mpg~wt,data=mtcars,m=3,B=100,cl=cl)
#' stopCluster(cl)
#' coef(x)
#' }
blbglm_par <- function(formula, data, m = 10, B = 5000,cl) {
  clusterExport(cl,"glm_each_subsample")
  clusterExport(cl,"glm_each_boot")
  clusterExport(cl,"glm1")
  data_list <- split_data(data, m)
  estimates <- parLapply(cl,data_list,fun=glm_each_subsample,formula = formula, n = nrow(data), B = B)
  res <- list(estimates = estimates, formula = formula)
  class(res) <- "blblm"
  invisible(res)
}


#' Generate logistic regression with boostrap
#'
#' compute the estimates
#'
#' @param formula regression formula
#' @param data the data you want to regression
#' @param n number of row of the data
#' @param B number of bootstrips
#'
#'
#' @return vector
#' compute the estimates
#' @export
glm_each_subsample <- function(formula, data, n, B) {
  replicate(B, glm_each_boot(formula, data, n), simplify = FALSE)
}

#' Generate logistic regression with boostrap
#'
#' compute the regression estimates for a blb dataset
#'
#' @param formula regression formula
#' @param data the data you want to regression
#' @param n number of row of the data
#'
#' @return regression estimates
#' compute the regression estimates for a blb dataset
#' @export
glm_each_boot <- function(formula, data, n) {
  freqs <- rmultinom(1, n, rep(1, nrow(data)))
  glm1(formula, data, freqs)
}

#' estimate the regression estimates
#'
#' estimate the regression estimates based on given the number of repetitions
#'
#' @param formula regression formula
#' @param data the data you want to regression
#' @param freqs frequency
#'
#' @return estimate the regression estimates based on given the number of repetitions
#' estimate the regression estimates based on given the number of repetitions
#' @export
glm1 <- function(formula, data, freqs) {
  # drop the original closure of formula,
  # otherwise the formula will pick a wront variable from the global scope.
  environment(formula) <- environment()
  fit <- glm(formula, data, weights = freqs,family = "binomial")
  list(coef = blbcoef(fit), sigma = blbsigma(fit))
}


#' Generate logistic regression with boostrap(without sigma)
#'
#' create a logistic regression with the method of bootstrips, with people selected feature. Now, support parallel with user specified cluster
#'
#' @param formula regression formula
#' @param data the data you want to regression
#' @param m number of subdata
#' @param B number of bootstrips
#' @param cl cluster specified
#'
#' @return class blblm
#' @export
#' @examples
#' \dontrun{
#' cl <- makec(2)
#' x=blblm_par(mpg~wt,data=mtcars,m=3,B=100,cl=cl)
#' stopCluster(cl)
#' coef(x)
#' }
blbglm_par_c <- function(formula, data, m = 10, B = 5000,cl) {
  clusterExport(cl,"glm_each_subsample_c")
  clusterExport(cl,"glm_each_boot_c")
  clusterExport(cl,"glm1_c")
  data_list <- split_data(data, m)
  estimates <- parLapply(cl,data_list,fun=glm_each_subsample_c,formula = formula, n = nrow(data), B = B)
  res <- list(estimates = estimates, formula = formula)
  class(res) <- "blblm"
  invisible(res)
}


#' Generate logistic regression with boostrap(without sigma)
#'
#' compute the estimates
#'
#' @param formula regression formula
#' @param data the data you want to regression
#' @param n number of row of the data
#' @param B number of bootstrips
#'
#'
#' @return vector
#' compute the estimates
#' @export
glm_each_subsample_c <- function(formula, data, n, B) {
  replicate(B, glm_each_boot_c(formula, data, n), simplify = FALSE)
}

#' Generate logistic regression with boostrap(without sigma)
#'
#' compute the regression estimates for a blb dataset
#'
#' @param formula regression formula
#' @param data the data you want to regression
#' @param n number of row of the data
#'
#' @return regression estimates
#' compute the regression estimates for a blb dataset
#' @export
glm_each_boot_c <- function(formula, data, n) {
  freqs <- rmultinom(1, n, rep(1, nrow(data)))
  glm1_c(formula, data, freqs)
}

#' Generate logistic regression with boostrap(without sigma)
#'
#' estimate the regression estimates based on given the number of repetitions
#'
#' @param formula regression formula
#' @param data the data you want to regression
#' @param freqs frequency
#'
#' @return estimate the regression estimates based on given the number of repetitions
#' estimate the regression estimates based on given the number of repetitions
#' @export
glm1_c <- function(formula, data, freqs) {
  # drop the original closure of formula,
  # otherwise the formula will pick a wront variable from the global scope.
  environment(formula) <- environment()
  fit <- glm(formula, data, weights = freqs,family = "binomial")
  list(coef = blbcoef(fit))
}