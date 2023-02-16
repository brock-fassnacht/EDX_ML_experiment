# 4.1 Check


set.seed(1)

library(tidyverse)
library(caret)
library(dslabs)
data(heights)

#Q1 Parsing data and running knn model with 33 different values of k. Finding f1 stat for each and finding the max

test_index <- createDataPartition(heights$height, times = 1, p = 0.5, list = FALSE)

train <- heights[-test_index,]
test <- heights[test_index,]

kns <- sapply(seq(1,101,3), function(i){
        fit <- knn3(sex ~ height, data = train, k=i) 
        y_hat <- predict(fit, test, type="class") %>%
          factor(levels = levels(train$sex))
        F_meas(data=y_hat, reference = test$sex)
        })
plot(seq(1,101,3), kns)

max(kns)

which.max(kns)*3 +1


#Q2
library(dslabs)
library(caret)

data("tissue_gene_expression")
set.seed(1)

y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
test_index <- createDataPartition(y, list = FALSE)


Medata <- sapply(seq(1, 11, 2), function(i){
  fit <- knn3(x[-test_index,], y[-test_index], k=i)
  y_hat <- predict(fit, newdata = data.frame(x=x[test_index,]), type="class") %>%
    factor(levels = levels(y))
  mean(y_hat == y[test_index])
})




