### Random Forest regreesion
## (1) Decision tree
## (2) Linear regression
## (3) Random forest

dir.data <- "../data"
in.fileName <- 'boston.csv'

boston <- read.csv(file.path(dir.data,in.fileName))

## Find the structure of data 
str(boston)

# Plot observations
plot(boston$LON, boston$LAT,xlab = "Longitude",ylab = "Latitude")

## Plot the points where air pollution is greater
points(boston$LON[boston$NOX>=0.55], boston$LAT[boston$NOX>=0.55], col="red", pch=20)



#### Regression Trees to the problem
library(rpart)
library(rpart.plot)

lattree = rpart(MEDV ~ LAT , data=boston)
plot(lattree)
text(lattree)


lontree = rpart(MEDV ~ LON , data=boston)
plot(lontree)
text(lontree)

latlontree = rpart(MEDV ~ LAT + LON, data=boston)

# Plot the tree using prp command defined in rpart.plot package
prp(latlontree)


### The number is the average of the values 
latlontree = rpart(MEDV ~ LAT + LON, data=boston, minbucket=50)
plot(latlontree)
text(latlontree)

latlontree = rpart(MEDV ~ LAT + LON, data=boston, minbucket=50)
plot(latlontree)
text(latlontree)



# Split the data
library(caTools)
set.seed(123)
split = sample.split(boston$MEDV, SplitRatio = 0.7)
train = subset(boston, split==TRUE)
test = subset(boston, split==FALSE)


# Create a CART model
tree = rpart(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO, data=train)
prp(tree)


tree.pred = predict(tree, newdata=test)
tree.rmse = sqrt(sum((tree.pred - test$MEDV)^2)/length(test$MEDV))


plot(boston[,c(5,6,11,13,14)],pch=3)
cor(boston[,c(2,3,4,5,6,7,8,9,10,11,12,13,14)],boston$MEDV)


#### Linear model -1
fit.lm <- lm(MEDVV~.,data = training)
#Check Coefficients
data.frame(coef = round(fit.lm$coefficients,2))

set.seed(12345)
#predict on test set
pred.lm <- predict(fit.lm, newdata = test)

# Root-mean squared error
rmse.lm <- sqrt(sum((pred.lm - test$MEDV)^2)/
                  length(test$MEDV))

c(RMSE = rmse.lm, R2 = summary(fit.lm)$r.squared
############


set.seed(12345)
#Try linear model using all features
fit.lm1 <- lm(log(MEDV)~.,data = train)

set.seed(12345)
#predict on test set
pred.lm1 <- predict(fit.lm1, newdata = test)

# Root-mean squared error
rmse.lm1 <- sqrt(sum((exp(pred.lm1) - test$MV)^2)/
                   length(test$MV))

c(RMSE = rmse.lm1, R2 = summary(fit.lm1)$r.squared)


layout(matrix(c(1,2,3,4),2,2))
plot(fit.lm2)


#####Model 3 : random Forest
set.seed(12345)
library(randomForest)
fit.rf <- randomForest(formula = MEDV ~ ., data = train[,-1])

set.seed(12345)
pred.rf <- predict(fit.rf, test)

rmse.rf <- sqrt(sum(((pred.rf) - test$MEDV)^2)/
                  length(test$MEDV))
c(RMSE = rmse.rf, pseudoR2 = mean(fit.rf$rsq))


library(ggplot2)
p02 <- ggplot(boston, aes(y = MEDV, x = DIS)) + geom_point(colour = "red") + geom_smooth(colour = "blue")


