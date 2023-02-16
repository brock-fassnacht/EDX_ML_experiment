## Linear Regression

library(tidyverse)
library(caret)

set.seed(1)
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

#set.seed(1)
################## Q1
res <- replicate(n,{
  test_index <- createDataPartition(dat$y, times = 1, p=.5, list=FALSE)
  train_set <- dat |> slice(-test_index) 
  test_set <- dat |> slice(test_index) 

  fit <- train_set %>%
    lm(y ~ x, data = .)

  p_hat <- predict(fit, newdata = test_set, type = "response")

  RMSE = sqrt(sum((p_hat - test_set$y)^2)/length(p_hat))

  return(RMSE)})



################## Q2

RSME_CALC <- function(n){
  dat <- MASS::mvrnorm(n = n, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))
  
  res <- replicate(n,{
    test_index <- createDataPartition(dat$y, times = 1, p=.5, list=FALSE)
    train_set <- dat |> slice(-test_index) 
    test_set <- dat |> slice(test_index) 
    
    fit <- train_set %>%
      lm(y ~ x, data = .)
    
    p_hat <- predict(fit, newdata = test_set, type = "response")
    
    RMSE = sqrt(sum((p_hat - test_set$y)^2)/length(p_hat))
    
    return(RMSE)})
  
  print(n)
  print(mean(res))
  print(sd(res))
}
set <- c(100, 500, 1000, 5000, 10000)

sapply(set, RSME_CALC)


##### Q4

set.seed(1)
n <- 100
Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))


##### Q6 (might be wrong. Do no know why)

set.seed(1)
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

test_index <- createDataPartition(dat$y, times = 1, p=.5, list=FALSE)
train_set <- dat |> slice(-test_index) 
test_set <- dat |> slice(test_index) 

fit <- train_set %>%
  lm(y ~ x_1 + x_2, data = .)

p_hat <- predict(fit, newdata = test_set, type = "response")

RMSE = sqrt(sum((p_hat - test_set$y)^2)/length(p_hat))

RMSE




## Q8 Same as Q6 but with highly correlated variables


set.seed(1)
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))
  

  
  