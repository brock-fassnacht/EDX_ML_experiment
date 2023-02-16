library(dslabs)
library(dplyr)
library(lubridate)
data(reported_heights)

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type

# Question 1: determine proportion of females in online/ females in person
table(dat)

# Question 2: Reporting accuracy if we assume all in class are female and all online are male

y_hat <- ifelse(x == "online", "Male", "Female") %>% factor(levels = levels(y))
mean(y == y_hat)

# Question 3: One line of code for the confusion Matrix for y and y_hat
table(y_hat, y)
confusionMatrix(data = y_hat, reference = y)

# Question 4: What is the sensitivity of this prediction?
sensitivity(data = y_hat, reference = y)

# Question 5: What is the specificity of this prediction?
specificity(data = y_hat, reference = y)

# Question 6: What is the prevalence(% females) in the dat data set
mean(y == "Female")


## Part 2
library(caret)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

# Question 7: What line of code is necessary to create data partition

set.seed(76)
# line of code
test_index <- createDataPartition(y, times = 1, p = .5, list = FALSE)

test <- iris[test_index,]
train <- iris[-test_index,]

# Question 8: Finding which cutoff gets highest accuracy
values <- list(iris$Sepal.Length, iris$Sepal.Width, iris$Petal.Length, iris$Petal.Width)


cut_sep_len <- seq(min(iris$Petal.Width), max(iris$Petal.Width), .1)
x1 <- iris$Petal.Width

accuracy1 <- map_dbl(cut_sep_len, function(x1){
  y_hat1 <- ifelse(train$Petal.Width > x1, "virginica", "versicolor") %>% 
    factor(levels = levels(test$Species))
  mean(y_hat1 == train$Species)
})

max(accuracy1)

## question 8 simple answer (Above had to enter each item in values individually but came to same result)

foo <- function(x){
  rangedValues <- seq(range(x)[1], range(x)[2], by=0.1)
  sapply(rangedValues, function(i){
    y_hat <- ifelse(x>i, 'virginica', 'versicolor')
    mean(y_hat==train$Species)
  })
}
predictions <- apply(train[,-5], 2, foo)
sapply(predictions, max)

## Question 9: Applying best cutoff from train data to the test data and testing accuracy

predictions <- foo(train[,4])
rangedValues <- seq(range(train[,4])[1], range(train[,4])[2], by=0.1)
cutoffs <-rangedValues[which(predictions==max(predictions))]

y_hat <- ifelse(test[,4]>cutoffs[1], 'virginica', 'versicolor')
mean(y_hat==test$Species)


## Question 11: See how petal length and petal width can work together to optimize accuracy

plot(iris, pch=21, bg=iris$Species)

# finding cuttoff for petal 

prediction1 <- foo(train[,3])
rangedValue1 <- seq(range(train[,3])[1], range(train[,3])[2], by=0.1)
cutoff1 <-rangedValue1[which(prediction1==max(prediction1))]
cutoff1

y_hat1 <- ifelse(test[,4]>cutoffs[1] & test[,3]>cutoff1[1], 'virginica', 'versicolor')
mean(y_hat1==test$Species)
