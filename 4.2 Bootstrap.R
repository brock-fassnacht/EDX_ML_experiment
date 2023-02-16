### 4.2 Check Bootstrap

#Q1

library(dslabs)
library(caret)

data(mnist_27)

set.seed(1995)
indexes <- createResample(mnist_27$train$y, 10)

# Q2 number of times 3 is in all indexes
total = 0
for (list in indexes){
  counter = 0
  for (i in list){
    if (i==3){
      counter = counter + 1
    }
  }
  print(counter)
  total = total + counter
}
total



#Q3 finding mean and sd of 75th quantile using 10000 samples (monte carlo)

y <- rnorm(100, 0, 1)
quantile(y, 0.75)
set.seed(1)

est_list <- list()

for (i in 1:10000){
  y <- rnorm(100, 0, 1)
  quant <- as.numeric(quantile(y, 0.75))
  est_list <- append(est_list, quant)
}

sd(as.matrix(est_list))
mean(unlist(est_list))


#Q4 same as q3 but with bootstrap samples (Even though we are taking a sample o)
#(Even though we are taking a sample of size 100 from data of size 100, sometimes multiples are listed and there are some missing)

set.seed(1)
y <- rnorm(100, 0, 1)
quantile(y, 0.75)

B <- 10000
M_star <- replicate(B, {
  X_star <- sample(y, 100, replace = TRUE)
  quantile(X_star, 0.75)
})
sd(M_star)
mean(M_star)








