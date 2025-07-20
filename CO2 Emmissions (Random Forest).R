install.packages("randomForest")
install.packages("caret")
install.packages("ggplot2")
install.packages("dplyr")

library(randomForest)
library(caret)
library(ggplot2)
library(dplyr)

data=read.csv(file.choose())
dim(data)
View(data)

data$Make=as.factor(data$Make)
data$Model=as.factor(data$Model)
data$Vehicle.Class=as.factor(data$Vehicle.Class)
data$Transmission=as.factor(data$Transmission)
data$Fuel.Type=as.factor(data$Fuel.Type)

set.seed(123)
sample=sample(1:nrow(data), 0.7*nrow(data))
train_data=data[sample,]
test_data=data[-sample,]

rf_model=randomForest(CO2.Emissions.g.km. ~ . -Model -Make, data=train_data, ntree=500, importance=TRUE)

predictions= predict(rf_model, newdata=test_data)

mse=mean((predictions - test_data$CO2.Emissions.g.km.)^2)
mse

range(data$CO2.Emissions.g.km.)

actual = test_data$CO2.Emissions.g.km.
predicted = predict(rf_model, newdata = test_data)

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


residuals = actual - predicted
plot(residuals, main = "Residual Plot", ylab = "Residuals", xlab = "Index")
abline(h = 0, col = "red")
