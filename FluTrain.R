# Read dataset 
FluTrain = read.csv("~/Documents/Dataset_Explanation/Flu_SearchQueries_Data/FluTrain.csv")

# Which week correspond to highest percentage of ILI-related physician visits
subset(FluTrain, ILI == max(ILI))

# row which has highest percentage of ILI-related physician visits
which.max(FluTrain$ILI)
FluTrain$Week[303]

# Which week correspond to highest percentage of ILI-related Queries visits
subset(FluTrain, Queries == max(Queries))

# row which has highest percentage of ILI-related physician Queries visits
which.max(FluTrain$Queries)
FluTrain$Week[303]

# histogram of the dependent variable, ILI
hist(FluTrain$ILI)
# The histogram is right skewed

# There is a positive, linear relationship between log(ILI) and Queries
plot(FluTrain$Queries, log(FluTrain$ILI))

# we are predicting log(ILI) using the Queries variable. From the plot in the previous subproblem, we expect the coefficient on Queries to be positive
# Model : log(ILI) = intercept + coefficient x Queries, where the coefficient is positive correct

# Build linear regression model and check out the value of R squared using summary 
FluTrend1 = lm(log(ILI)~Queries, data=FluTrain)
summary(FluTrend1)

# compute the correlation between the independent variable used in the model (Queries) and the dependent variable (log(ILI))
Correlation = cor(FluTrain$Queries, log(FluTrain$ILI))
Correlation^2 # 0.7090201
log(1/Correlation) # 0.1719357
exp(-0.5*Correlation) # 0.6563792

# This gives the prediction in the log(ILI) value 
PredTest1 = predict(FluTrend1, newdata=FluTest)

# To convert it to ILI value we use exp
PredTest1 = exp(predict(FluTrend1, newdata=FluTest))

# we need to determine which element in the test set is for March 11, 2012
which(FluTest$Week == "2012-03-11 - 2012-03-17")
PredTest1[11]

# relative error betweeen the estimate (our prediction) and the observed value
FluTest$ILI[11]
(2.293422 - 2.187378)/2.293422

# Calculate SSE
SSE = sum((PredTest1-FluTest$ILI)^2)
RMSE = sqrt(SSE / nrow(FluTest))
# or sqrt(mean((PredTest1-FluTest$ILI)^2))

# install package zoo which provides a number of helpful methods for time series models
install.packages("zoo")
library(zoo)

# The value of -2 passed to lag means to return 2 observations before the current one
# A positive value would have returned future observations
# The parameter na.pad=TRUE means to add missing values for the first two weeks of our dataset, where we can't compute the data from 2 weeks earlier
ILILag2 = lag(zoo(FluTrain$ILI), -2, na.pad=TRUE)
FluTrain$ILILag2 = coredata(ILILag2)
summary(FluTrain$ILILag2)

# There is a strong positive relationship between log of ILILag2 against the log of ILI
plot(log(FluTrain$ILILag2), log(FluTrain$ILI))

# Train a linear model
FluTrend2 = lm(log(ILI)~Queries+log(ILILag2), data=FluTrain)
summary(FluTrend2)
# R^2 value is 0.9063

# Moving from FluTrend1 to FluTrend2, in-sample R^2 improved from 0.709 to 0.9063, and the new variable is highly significant
# As a result, there is no sign of overfitting, and FluTrend2 is superior to FluTrend1 on the training set

# Add  ILIlag2 to training datset
ILILag2 = lag(zoo(FluTest$ILI), -2, na.pad=TRUE)
FluTest$ILILag2 = coredata(ILILag2)

# Missing values on training set
summary(FluTest$ILILag2)

# ILIlag2 variable first and second observation in the FluTest should be filled by last and second last values of the FluTrain
nrow(FluTrain)
FluTest$ILILag2[1] = FluTrain$ILI[416]
FluTest$ILILag2[2] = FluTrain$ILI[417]

# Evaluating time series model on the test set
PredTest2 = exp(predict(FluTrend2, newdata=FluTest))

SSE = sum((PredTest2-FluTest$ILI)^2)
RMSE = sqrt(SSE / nrow(FluTest))
sqrt(mean((PredTest2-FluTest$ILI)^2))

# The test-set RMSE of FluTrend2 is 0.294

# RMSE of FluTrend2 is better than FluTrend1