#installing packages
install.packages("randomForest")
install.packages("caret")
install.packages("ggplot2")
install.packages("dplyr")

#Loading packages
library(randomForest)
library(caret)
library(ggplot2)
library(dplyr)

#Loading and reding the dataset
data=read.csv(file.choose())
dim(data)
View(data)

#Converting categorical columns into Factors
data$Make=as.factor(data$Make)
data$Model=as.factor(data$Model)
data$Vehicle.Class=as.factor(data$Vehicle.Class)
data$Transmission=as.factor(data$Transmission)
data$Fuel.Type=as.factor(data$Fuel.Type)

#Splitting into training and testing dataset
set.seed(123)
sample=sample(1:nrow(data), 0.7*nrow(data))
train_data=data[sample,]
test_data=data[-sample,]

#building the random forest model
rf_model=randomForest(CO2.Emissions.g.km. ~ . -Model -Make, data=train_data, ntree=500, importance=TRUE)

predictions= predict(rf_model, newdata=test_data)
head(predictions)

#calculation Mean Sq Error
mse=mean((predictions - test_data$CO2.Emissions.g.km.)^2)
mse

#Finding range of Target variable to judge model performance
range(data$CO2.Emissions.g.km.)

actual = test_data$CO2.Emissions.g.km.
predicted = predict(rf_model, newdata = test_data)

#calculating Root Mean Sq Error
rmse = sqrt(mean((actual - predicted)^2))

# MAE (Mean Absolute Error)
mae = mean(abs(actual - predicted))

# R-squared (coefficient of determination)
sst = sum((actual - mean(actual))^2)
sse = sum((actual - predicted)^2)
r_squared = 1 - (sse / sst)

# Print metrics
cat("RMSE:", round(rmse, 2), "\n")
cat("MAE:", round(mae, 2), "\n")
cat("R-squared:", round(r_squared, 4), "\n")

#checking Model Performance through Graph
residuals = actual - predicted
plot(residuals, main = "Residual Plot", ylab = "Residuals", xlab = "Index")
abline(h = 0, col = "red")

#creating sample test case
test_case= data.frame(
  Make= factor("ACURA", levels = levels(data$Make)),
  Model=factor("TSX", levels= levels(data$Model)),
  Vehicle.Class= factor("COMPACT", levels=levels(data$Vehicle.Class)),
  Engine.Size.L.=2.4,
  Cylinders=6,
  Transmission=factor("AS5", levels= levels(data$Transmission)),
  Fuel.Type=factor("Z", levels= levels(data$Fuel.Type)),
  Fuel.Consumption.City..L.100.km.=50.6,
  Fuel.Consumption.Hwy..L.100.km.=27.5,
  Fuel.Consumption.Comb..L.100.km.=39.2,
  Fuel.Consumption.Comb..mpg.=95
  
)

predicted_emmission= predict(rf_model, newdata = test_case)
predicted_emmission

