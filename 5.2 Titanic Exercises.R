## 5.2 Titanic Exercises

library(titanic)    # loads titanic_train data frame
library(caret)
library(tidyverse)
library(rpart)

# 3 significant digits
options(digits = 3)

# clean the data - `titanic_train` is loaded with the titanic package
titanic_clean <- titanic_train %>%
  mutate(Survived = factor(Survived),
         Embarked = factor(Embarked),
         Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age), # NA age to median age
         FamilySize = SibSp + Parch + 1) %>%    # count family members
  select(Survived,  Sex, Pclass, Age, Fare, SibSp, Parch, FamilySize, Embarked)


# Q1
set.seed(42)

test_index <- createDataPartition(titanic_clean$Survived, times = 1, p = 0.2, list = FALSE)

clean_train <- titanic_clean[-test_index,]
clean_test <- titanic_clean[test_index,]

dim(clean_test)



total <- ifelse(as.numeric(clean_train$Survived) == 2, 1, 0)

sum(total) / dim(clean_train)[1]



# Q2 setting a baseline of randomly guessing
set.seed(3)

guess <- sample(c(0,1), dim(clean_test)[1], replace = TRUE) #prob = c(.617, .383)

confusionMatrix(as.factor(guess), clean_test$Survived)


# Q3A What percentage of males and females survived in train set

train_female <- clean_train[clean_train$Sex == "female",]
train_male <- clean_train[clean_train$Sex == "male",]

sum(ifelse(as.numeric(train_female$Survived) == 2,1,0)) / dim(train_female[1])
sum(ifelse(as.numeric(train_male$Survived) == 2,1,0)) / dim(train_male[1])

# Q3b predict death of males and survival of females


guess_onsex <- ifelse(clean_test$Sex == "female", 1, 0)
confusionMatrix(as.factor(guess_onsex), clean_test$Survived)



# Q4 which pclass were more likely to survive than die on in train set

head(clean_train)

for (i in 1:3){
  set <- clean_train[clean_train$Pclass == i,]
  tot <- sum(ifelse(as.numeric(set$Survived) == 2,1,0))
  print(tot / dim(set)[1])
}

# Q4B compute accuracy of survival where we are guessing pclass 1 survive and other pclass die

pclass_guess <- ifelse(clean_test$Pclass == 1, 1 , 0)
confusionMatrix(as.factor(pclass_guess), clean_test$Survived)


# Q4C which sex and class combos were likely to survive (> 50%)

for (i in 1:3){
  for (j in c("female", "male")){
    set <- clean_train[clean_train$Pclass == i & clean_train$Sex == j,]
    tot <- sum(ifelse(as.numeric(set$Survived) == 2,1,0))
    cat(tot / dim(set)[1], i , j, "\n")
  }
}

# Q4D predict survive for pclass 1&2 females, death otherwise

pcl_sex_guess <- ifelse(clean_test$Sex == "female" & (clean_test$Pclass == 1 | clean_test$Pclass == 2), 1, 0)
confusionMatrix(as.factor(pcl_sex_guess), clean_test$Survived)


# Q6 F1 scores for each model

F_meas(data = as.factor(guess_onsex),reference = clean_test$Survived)
F_meas(data = as.factor(pclass_guess),reference = clean_test$Survived)
F_meas(data = as.factor(pcl_sex_guess),reference = clean_test$Survived)


#########################################################################
################################# Part 2 ################################

# Q7 train loess model using only fare as predictor
set.seed(1)

titan_loess <- train(Survived ~ Fare, 
                     method = "gamLoess",
                     data = clean_train)

confusionMatrix(data = predict(titan_loess, clean_test), 
                reference = clean_test$Survived)$overall["Accuracy"]



# Q8 train model using glm using age as the only predictor, then 4 predictors than all
set.seed(1)


titan_glm <- train(Survived ~ Age, 
                     method = "glm",
                     data = clean_train)
#.615 accuracy
confusionMatrix(data = predict(titan_glm, clean_test), 
                reference = clean_test$Survived)$overall["Accuracy"]

titan_glm <- train(Survived ~ Sex + Pclass + Fare + Age, 
                   method = "glm",
                   data = clean_train)
#.821 accuracy
titan_glm <- train(Survived ~ ., 
                   method = "glm",
                   data = clean_train)
#.827 accuracy


#Q9A Training a knn model and seeing which K has highest accuracy

set.seed(6)

titanic_knn <- train(Survived ~ . ,  method = "knn", 
                     data = clean_train,
                   tuneGrid = data.frame(k = seq(3, 51, 2)))

ggplot(titanic_knn)

titanic_knn$bestTune
titanic_knn$finalModel
#titanic_knn$results


#Q9C Accuracy of optimal knn model on the test set

titan_phat <- predict(titanic_knn, clean_test)

mean(titan_phat == clean_test$Survived)


# Q10 cross - validation train same model with tr control
set.seed(8)


titanic_knn_cross <- train(Survived ~ . ,  method = "knn", 
                     data = clean_train,
                     tuneGrid = data.frame(k = seq(3, 51, 2)),
                     trControl = trainControl(method = "cv", number=10, p=0.9))

titanic_knn_cross$bestTune
titanic_knn_cross$finalModel

#Accuracy

titan_phat2 <- predict(titanic_knn_cross, clean_test)

mean(titan_phat2 == clean_test$Survived)


# Q11

set.seed(10)

train_rpart <- train(Survived ~ .,
                     method = "rpart",
                     tuneGrid = data.frame(cp = seq(0, 0.05, 0.002)),
                     data = clean_train)

train_rpart$bestTune
train_rpart$finalModel

titan_phat3 <- predict(train_rpart, clean_test)
mean(titan_phat3 == clean_test$Survived)


plot(train_rpart$finalModel, margin = .01)
text(train_rpart$finalModel)

# Q12

set.seed(14)


tit_rf_fit <-  train(Survived ~., method = "rf",
                    data = clean_train,
                    ntree = 100,
                    tuneGrid = data.frame(mtry = seq(1:7)))

titan_phat4 <- predict(tit_rf_fit, clean_test)
mean(titan_phat4 == clean_test$Survived)



