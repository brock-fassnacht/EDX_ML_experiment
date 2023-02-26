#6.1 Check Ensembles


#Q1 Run Models

models <- c("glm", "lda", "naive_bayes", "knn", "gamLoess", "qda", "rf")

library(caret)
library(dslabs)
library(tidyverse)
set.seed(1)
data("mnist_27")

fits <- lapply(models, function(model){ 
  print(model)
  train(y ~ ., method = model, data = mnist_27$train)
}) 

names(fits) <- models

# Q2 Create matrix for predictions of test set

prediction_matrix <- sapply(fits, function(i){
                          predict(i, mnist_27$test)
                            })


# Q3 check accuracy of each model in test_set, calculate mean accuracy

compute_accuracy <- sapply(fits, function(i){
                      y_hat <- predict(i, mnist_27$test)
                      mean(y_hat == mnist_27$test$y)
                  })

# Q4 Next, build an ensemble prediction by majority vote and compute the accuracy of the ensemble. 
# Vote 7 if more than 50% of the models are predicting a 7, and 2 otherwise.


ensemble <- list()


for (i in 1:nrow(prediction_matrix)){
  prop <- sum(prediction_matrix[i,]==7) / 7
  ensemble <- append(ensemble, ifelse(prop >.5, 7, 2))
}


mean(ensemble == mnist_27$test$y)

# Q5 which models perform better than ensemble on the test set?
compute_accuracy


# Q6 
accuracy <- list()
for (i in fits){
  accuracy <- append(accuracy, i$results$Accuracy)
}

mean(as.numeric(accuracy))

# Q7 Adjust ensemble for models with min accuracy > .8


# putting models in with min accuracy requirements
models_adj <- c("naive_bayes", "gamLoess", "qda", "rf")

fits_adj <- lapply(models_adj, function(model){ 
  print(model)
  train(y ~ ., method = model, data = mnist_27$train)
}) 

#creating new prediction matrix
prediction_matrix_adj <- sapply(fits_adj, function(i){
  predict(i, mnist_27$test)
})

#creating new ensemble
ensemble_adj <- list()

for (i in 1:nrow(prediction_matrix_adj)){
  prop <- sum(prediction_matrix[i,]==7) / 4
  ensemble_adj <- append(ensemble, ifelse(prop >.5, 7, 2))
}


mean(ensemble_adj == mnist_27$test$y)
