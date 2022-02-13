library(caret)
library(lattice)
library(ggplot2)
library(gam)

data = read.csv("bank-additional.csv", sep = ";")
summary(data)
str(data)

bank = data

bank$y <- as.factor(bank$y)
summary(bank)

# Majority Null for pdays, duration should be removed
bank$pdays <- NULL
bank$duration <- NULL

bank2 <- na.omit(bank)
bank2[bank2 == "unknown"] <- NA
bank2 <- na.omit(bank2)
str(bank2)


levels(as.factor(bank2$default))
bank2$default = as.factor(bank2$default)
summary(bank2)
splitBank = sort(sample(nrow(bank2), nrow(bank2)*.7))
Btrain <- bank2[splitBank,]
Btest <- bank2[-splitBank,]

g1 = glm(formula = y ~., data = Btrain, family = binomial)
summary(g1)
g2 = glm(formula = y ~ age + job + marital + education + default + housing + loan + contact + month + day_of_week + campaign + previous + poutcome + cons.conf.idx, data = Btrain, family = binomial) 
summary(g2)
car::vif(g2)

Btrain$PredProb = predict.glm(g2, newdata = Btrain)
Btrain$Predy = ifelse(Btrain$PredProb >= 0.5,"yes","no")
levels(as.factor(Btrain$y))
levels(as.factor(Btrain$Predy))
caret::confusionMatrix(Btrain$y, as.factor(Btrain$Predy))

install.packages("MKclass")
library(MKclass)
optCutoff(Btrain$Predy, truth = Btrain$y, namePos = "yes")
library(car)
car::vif(g1)
