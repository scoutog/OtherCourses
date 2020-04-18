mowers <- read.csv("RidingMowers(1).csv")

## A
library(ggplot2)
ggplot(mowers, aes(x = Income, y = Lot_Size, color = Ownership, shape = Ownership)) + 
  geom_point()

## B
log.reg <- glm(Ownership ~ Income + Lot_Size, data = mowers, family = "binomial")
summary(log.reg)
#income coef 0.1109
#lot size coef 0.9638

## C
pred <- predict(log.reg, newdata = mowers, type = "response")
library(caret)
confusionMatrix(as.factor(ifelse(pred > 0.5, "Owner","Nonowner")), mowers$Ownership)

## D
# Responded on the test
## E
library(tidyverse)
mowers1 <- mowers %>% add_row(Income = 60, Lot_Size = 20)
pred1 <- predict(log.reg, newdata = mowers1, type = "response")
pred1

## F
mowers2 <- mowers %>% add_row(Income = 94.9, Lot_Size = 16)
pred2 <- predict(log.reg, newdata = mowers2, type = "response")
pred2