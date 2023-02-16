## Comprehension check 3.2

library(tidyverse)
library(lubridate)
library(purrr)
library(pdftools)

fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")
dat <- map_df(str_split(pdf_text(fn), "\n"), function(s){
  s <- str_trim(s)
  header_index <- str_which(s, "2015")[1]
  tmp <- str_split(s[header_index], "\\s+", simplify = TRUE)
  month <- tmp[1]
  header <- tmp[-1]
  tail_index  <- str_which(s, "Total")
  n <- str_count(s, "\\d+")
  out <- c(1:header_index, which(n==1), which(n>=28), tail_index:length(s))
  s[-out] %>%
    str_remove_all("[^\\d\\s]") %>%
    str_trim() %>%
    str_split_fixed("\\s+", n = 6) %>%
    .[,1:5] %>%
    as_tibble() %>% 
    setNames(c("day", header)) %>%
    mutate(month = month,
           day = as.numeric(day)) %>%
    gather(year, deaths, -c(day, month)) %>%
    mutate(deaths = as.numeric(deaths))
}) %>%
  mutate(month = recode(month, "JAN" = 1, "FEB" = 2, "MAR" = 3, "APR" = 4, "MAY" = 5, "JUN" = 6, 
                        "JUL" = 7, "AGO" = 8, "SEP" = 9, "OCT" = 10, "NOV" = 11, "DEC" = 12)) %>%
  mutate(date = make_date(year, month, day)) %>%
  dplyr::filter(date <= "2018-05-01")



## Q1 use the loess function to obtain a smooth estimate of the expected number of deaths
##    as a function of date


span = 60 / diff(range(as.numeric(dat$date)))



dat %>% ggplot(aes(date, deaths)) +
  geom_point() +
  geom_smooth(color="red",span = span, method = "loess", method.args = list(degree=1))


## Q2 
fit <- with(dat, 
            ksmooth(date, deaths, kernel = "box", bandwidth = span))

dat %>% 
  mutate(smooth = predict(fit, as.numeric(date)), day = yday(date), year = as.character(year(date))) %>%
  ggplot(aes(day, smooth, col = year)) +
  geom_line(lwd = 2)


#Q3 Fit a loess line with degree = 1 to the data above and predict the 2s and 7s in the mnist_27$test dataset with just the second covariate. 
#What is the accuracy of the prediction if we use only the second covariate as predictor?

library(broom)
library(caret)
library(dslabs)
library(tidyverse)

data(mnist_27)

mnist_27$train %>% glm(y ~ x_2, family = "binomial", data = .) %>% tidy()



new <- mnist_27$train %>% mutate(ifelse(y==7,1,0)) %>% rename("y1"="ifelse(y == 7, 1, 0)")

fit_L <- loess(as.numeric(y1) ~ as.numeric(x_2),degree=1, data=new)

p_hat <- predict(fit_L, mnist_27$test, type="Response") #%>% tidy()

y_hat <- factor(ifelse(p_hat>.5,7,2))

confusionMatrix(y_hat,mnist_27$test$y)$overall[["Accuracy"]]
