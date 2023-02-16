## 5.1 Cross Validation and Random Trees/Forest

library(tidyverse)
library(caret)

set.seed(1996)
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()

x_subset <- x[ ,sample(p, 100)]


# Q1 

set.seed(1)
fit <- train(x_subset, y, method = "glm")
fit$results

# Q2

pvals <- rep(0, ncol(x))
for (i in 1:ncol(x)) {
  pvals[i] <- t.test(x[,i][y==0], x[,i][y==1], var.equal=TRUE)$p.value
}

length(pvals[pvals<=.01])

# Q3
set.seed(1)

filter <- pvals <= .01
x_subset <- x[ ,filter]

fit <- train(x_subset, y, method = "glm")
fit$results



#Q4
set.seed(1)

fit <- train(x_subset, y, method = "knn")

train_knn <- train(x_subset, y,  method = "knn",
                   tuneGrid = data.frame(k = seq(101, 301, 25)))

ggplot(train_knn)



## Random Trees Check


library(rpart)
n <- 1000
sigma <- 0.25
set.seed(1)
x <- rnorm(n, 0, 1)
y <- 0.75 * x + rnorm(n, 0, sigma)
dat <- data.frame(x = x, y = y)


# Q1

fit <- rpart(y ~ ., data = dat) 


# Q2

plot(fit, margin = 0.1)
text(fit, cex = 0.75)

# Q3

dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col=2)


# Q4

library(randomForest)
fit <- randomForest(y ~ x, data = dat)
  dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = "red")
  
# Q5
  
plot(fit)

# Q6

library(randomForest)
fit <- randomForest(y ~ x, data = dat, nodesize = 50, maxnodes = 25)
  dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = "red")
  
plot(fit)
