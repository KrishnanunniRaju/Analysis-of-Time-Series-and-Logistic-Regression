library(haven)
house_data <- read_sav("C:/Project DBs/Statistics/House Categories.SAV")
head(house_data)
house_data$PriceCat[house_data$PriceCat==1] <- 0
house_data$PriceCat[house_data$PriceCat==2] <- 1
summary(house_data$PriceCat)
str(house_data)
house_data[sapply(house_data, is.character)] <- lapply(house_data[sapply(house_data, is.character)], 
                                                       as.factor)
house_data[sapply(house_data, is.factor)] <- lapply(house_data[sapply(house_data, is.factor)],as.numeric)
house_data$PriceCat <- as.factor(house_data$PriceCat)
model1 <- glm(PriceCat~.,data=house_data,family='binomial')
summary(model1)
model2 <- update(model1,~.-fueloil)
model3 <- glm(formula = PriceCat ~ lotSize + age + landValue + livingArea +
                bathrooms + waterfront, family = "binomial", data = house_data)
model4 <- glm(formula = PriceCat ~  age + landValue + livingArea + 
                bathrooms + waterfront, family = "binomial", data = house_data)
pred <- round(fitted(model3))
summary(pred)
library(caret)
confusionMatrix(table(house_data$PriceCat,pred))
library(DescTools)
PseudoR2(model4 ,which = "all")
library(car)
vif(model4)
testModel<-glm(PriceCat~landValue+landValue:log(landValue) + livingArea +livingArea:log(livingArea)
                 ,data = house_data, family=binomial)
summary(testModel)