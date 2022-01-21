rm(list = ls())
setwd("C:/Users/Lenovo/Documents/CSV/Chapter 1")
install.packages('tidyverse')
library(tidyverse)
kids <- read.csv('Good 0822 2021.csv')
View(kids)
#cleaning the data
kids <- na.omit(kids)
View(kids)
#????????????,state ???????????????????????????
kids %>%
  select(state)

KID<- kids %>%
  select(state,Q3AGE,Q1G33A)
View(KID) 

KIDS<- KID %>%
  filter(state == 39)
View(KIDS)

#Research questions:is there an association between Newyork's income and Ohio's income?
#H0:there is no association between the two variables
#H1:there is an association between the two variables
kidstable <- table(age = KIDS$Q3AGE, health = KIDS$Q1G33A)
mosaic(kidstable,shade = TRUE)

chitest <- chisq.test(kidstable)
chitest
chitest$residuals
chitest$p.value
View(chitest)
