#There are sales data available for 45 stores of Walmart. 
#The business is facing a challenge due to unforeseen demands and runs out of stock some times,
#due to the inappropriate machine learning algorithm. An
#ideal ML algorithm will predict demand accurately and ingest factors 
#like economic conditions including CPI, Unemployment Index, etc.

# ANALYSIS TASKS

#Which store has maximum sales

#Which store has maximum standard deviation i.e., the sales vary a lot. Also, find out the coefficient of mean to standard deviation

#Which store/s has good quarterly growth rate in Q3’2012

#Some holidays have a negative impact on sales. Find out holidays which have higher sales than the mean sales in non-holiday season for all stores together

#Provide a monthly and semester view of sales in units and give insights


#STATISTICAL MODEL

#For Store 1 – Build prediction models to forecast demand

#Linear Regression – Utilize variables like date and restructure dates 
#as 1 for 5 Feb 2010 (starting from the earliest date in order). 
#Hypothesize if CPI, unemployment, and fuel price have any impact on sales.
#Change dates into days by creating new variable.

#Select the model which gives best accuracy



# #PROJECT ----------------------------------------------------------------

#getting work directory
getwd()

data1 <- read.csv("C:\\Users\\Suraj\\OneDrive\\Desktop\\FODs\\dataset\\WALMART_SALES_DATA.csv")

#Calling libraries
library("dplyr") #Calling dplyr function for data manipulation 
library(ggplot2) # for data visualisation
library(scales) #for change of scales in data visualisation
library(zoo)
library(tidyverse)
library(tidyr)
library(lubridate)
library(car) #Companion to Applied Regression for Regression Visualisations
require(stats)
library(corrplot)
library(caTools)
library(MLmetrics)
library(repr)
library(dplyr)

#Understanding Data
head(data1)
view(data1)
#Data Exploration  - dimensions
dim(data1)

#Data Exploration  - class
class(data1)

#Data Exploration -structure
str(data1)

#Data Exploration - summary
summary(data1)

#Data Exploration - tables
table(data1$Store)

#finding unique values for store column
unique_values=unique(data1$Store)

table(data1$Holiday_Flag)

#Checking NA values 
colSums(is.na(data1)) #Observed no NA values

#Checking Duplicate Values
all(duplicated(data1) == TRUE)
#observed no duplicate values

#Summarisation of Data:
#Exploratory Data Analysis of Retail Analysis with Walmart Data
#Industry: Retail Industry
#This data is about different stores weekly sales and consists possible influence factors CPI, Un Employment Rate Fuel Price, Holiday Type that may affect sales
#Dimensions: 6435 observations, 8 variables.
#Missing Data / Null Values: No
#Duplicated Values: No
#Structure: Data frame


# ANALYSIS TASKS ----------------------------------------------------------

#Which Store has maximum sales?

#Aggregating data by 'Store' and Finding sum of 'Weekly_Sales' 
Store_Sales<- aggregate(Weekly_Sales ~ Store, data = data1, sum)

#Changing column name of sales 
colnames(Store_Sales)[2] <- "Total_Sales_by_Store"

#Finding out Store with highest Sales 
Store_Sales <-arrange(Store_Sales, desc(Total_Sales_by_Store)) #Arranged Stores based on Sales in descending order
Store_Sales[1,] #Choosing the first store that comes in this order

#Printing the output
print(paste('Store no.', Store_Sales[1,]$Store,
            'has the maximum sales and the value is = ', Store_Sales[1,]$Total_Sales_by_Store))


# Converting Store column into factor so that order won't change for graph 
Store_Sales$Store <- as.character(Store_Sales$Store)
Store_Sales$Store <- factor(Store_Sales$Store, levels=unique(Store_Sales$Store))


#Plotting Store vs TotalSales

options(repr.plot.width = 14, repr.plot.height = 8)

a<-ggplot(data=Store_Sales, aes(x=Store, y=Total_Sales_by_Store)) + geom_bar(stat="identity",fill="steelblue") +
  theme(axis.text.x = element_text(angle = 90,vjust = 0.5, hjust=0.5))+ scale_x_discrete(breaks = data1$Store)+
  scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6))+ ggtitle('Store vs Sales')+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Stores") + ylab("Total Sales")
a

#Insights:
#Store 20 has the maximum sales and the value is 301397792.46 (i.e., 301.39M)
#Store 4 is the second largest store in terms of sales and the value is 299543953 (i.e., 299.54M)
#Store 33 has the least sales 37160222 (i.e., 37.16M)


# Which store has maximum standard deviation i.e., the sales vary  --------

#Aggregating data by 'Store' and Finding Standard Deviation of 'Weekly_Sales' 
Store_Sales_Variation<-summarise(group_by(data1,Store),sd(Weekly_Sales), mean(Weekly_Sales))

#Changing column names
colnames(Store_Sales_Variation)[2] <- "StandardDeviation_Sales_by_Store"
colnames(Store_Sales_Variation)[3] <- "Mean_Sales_by_Store"

#Creating Coefficient of Variation for Sales by Store in Store_Sales_Variation dataframe 
Store_Sales_Variation<- mutate(Store_Sales_Variation,CV_Sales_by_Store = (StandardDeviation_Sales_by_Store/Mean_Sales_by_Store)*100)

#------Finding Store with highest Standard deviation-------#

#Finding out the row with highest standard deviation 
Store_Sales_Variation[which.max(Store_Sales_Variation$StandardDeviation_Sales_by_Store), ]

#Storing store number with max std deviation value
store_sales_max_std <- Store_Sales_Variation[which.max(Store_Sales_Variation$StandardDeviation_Sales_by_Store), ]$Store

#Storing max std deviation value
max_sd <- Store_Sales_Variation[which.max(Store_Sales_Variation$StandardDeviation_Sales_by_Store), ]$StandardDeviation_Sales_by_Store

#Storing CV value for max std deviation 
CV_max_sd <- Store_Sales_Variation[which.max(Store_Sales_Variation$StandardDeviation_Sales_by_Store), ]$CV_Sales_by_Store

#Store with highest variation in Sales - Store 14 & Standard Deviation - 317570, C.V - 5.7137

#printing the output
print(paste('Store no. ', store_sales_max_std,
            'has the maximum standard deviation of ', max_sd, 'Coefficient of Variation = ',CV_max_sd ))


options(repr.plot.width = 14, repr.plot.height = 8)

#Density Plot for Store 14
Store_14 <- data1[data1$Store == 14, ]
p <- ggplot(Store_14, aes(x=Weekly_Sales)) + geom_density(color="darkblue", fill="lightblue",alpha=0.2)+
  geom_vline(aes(xintercept= mean(Weekly_Sales)),color="steelblue", linetype="dashed", size=1)+
  theme(axis.text.x = element_text(vjust = 0.5, hjust=0.5))+ scale_x_continuous(labels = label_number(suffix = " M", scale = 1e-6))+ ggtitle('Store 14 Sales distribution')+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Weekly Sales") + ylab("Density")
p

#Insights:
#Store 14 has highest SD 317569.95 
#Store 14 sales are right skewed i.e, It had very high sales in few weeks which resulted in increasing of Standard deviation


# Which store/s has good quarterly growth rate -----------------

#Creating new dataframe to do alterations 
data2<-data1

#Creating a month- year column in data2 
data2$month_Year = substr(data2$Date, 4, 10)

#Subsetting Q3-2012 data (i.e, 07-2012,08-2012,09-2012), Q2-2012 data (i.e, 04-2012,05- 2012,06-2012)
Q3_2012 <- filter(data2,month_Year == "07-2012" | month_Year== "08-2012" | month_Year== "09-2012")
Q2_2012 <- filter(data2,month_Year == "04-2012" | month_Year== "05-2012" | month_Year== "06-2012")

#Aggregating sales by store for Q3-2012 
Q3_2012_Sales<-summarise(group_by(Q3_2012,Store),sum(Weekly_Sales))

#Changing column names
colnames(Q3_2012_Sales)[2] <- "Q3_2012_Sales_by_Store"

#Aggregating sales by store each Q2-2012 
Q2_2012_Sales<-summarise(group_by(Q2_2012,Store),sum(Weekly_Sales))

#Changing column names
colnames(Q2_2012_Sales)[2] <- "Q2_2012_Sales_by_Store"

#merging two quarters data by store
Q3_2012_Growthrate <- merge ( Q2_2012_Sales , Q3_2012_Sales , by = 'Store')

#Creating Growth rate column for Sales by Store in the above dataframe 
Q3_2012_Growthrate <- mutate(Q3_2012_Growthrate, Growth_Rate = ((Q3_2012_Sales_by_Store - Q2_2012_Sales_by_Store)*100) / Q2_2012_Sales_by_Store)

#Creating only positive growth rates
positive_growthrate <- filter(Q3_2012_Growthrate, Growth_Rate > 0 ) 
positive_growthrate<-arrange(positive_growthrate, desc(Growth_Rate)) 
View(positive_growthrate)
a<- positive_growthrate$Store

#printing the output
print(paste(c('The positive growth rate Stores are', a),collapse=" " )) 
print(paste('Store',positive_growthrate[1,1], 'has highest growth rate & it is',positive_growthrate[1,4]))

# Store 7 -13.33% , Store 16 - 8.49% , Store 35 - 4.47% and 7 more stores with positive growth rates.

# Visual representation of growth rates
c<-ggplot(data=Q3_2012_Growthrate, aes(x=Store, y=Growth_Rate)) +geom_bar(stat ="identity",fill="steelblue")+
  ggtitle('Growth rates of Q3- 2012')+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Stores") + ylab("Growth rate(%)") +
  scale_x_continuous("Stores", labels = as.character(Q3_2012_Growthrate$Store), breaks =
                       Q3_2012_Growthrate$Store)+
  theme(axis.text.x = element_text(angle = 90,vjust = 0.5, hjust=0.5))
c

#Insights:
#Store 7 has highest growth rate 13.33%, Store 16 second highest growth rate 8.49%, Store 35 third highest growth rate 4.47%
#It is observed that seven other stores has positive growth rates and they are 26,39,41,44,24,40,23
#Store 14 has highest negative growth rate.


# Some holidays have a negative impact on sales. Find out holidays --------

#Creating Holidays Data dataframe
Holiday_date <- c("12-02-2010", "11-02-2011", "10-02-2012", "08-02-2013","10-09-2010", "09-09-2011", "07-09-2012", "06-09-2013","26-11-2010", "25-11-2011", "23-11-2012", "29- 11-2013","31-12-2010", "30-12-2011", "28-12-2012", "27-12-2013")
Events <-c(rep("Super Bowl", 4), rep("Labour Day", 4),rep("Thanksgiving", 4), rep("Christmas", 4))
Holidays_Data <- data.frame(Events,Holiday_date)

#merging both dataframes
data3<-merge(data1,Holidays_Data, by.x= "Date", by.y="Holiday_date", all.x = TRUE)

#Replacing null values in Event with No_Holiday 
data3$Events = as.character(data3$Events) 
data3$Events[is.na(data3$Events)]= "No_Holiday" 
head(data3)

#Creating dataframe the mean of sales for No_Holiday and also mean of sales for different events
Holiday_Sales<-aggregate(Weekly_Sales ~ Events, data = data3, mean)
#Changing column names
colnames(Holiday_Sales)[2] <- "Mean_Sales_by_Event_Type"
View(Holiday_Sales)

# Christmas and Labour Day has negative impact on sales where as Thanks giving and Super Bowl has positive impact on sales

#Insights:
#Super Bowl, Thanks giving, Labour day has higher sales than mean sales of a Non Holiday and creating positive impact on sales.
#9th Sept 2011, 10th Sept 2010 ,30th Dec 2011, 31st Dec 2010 were the dates which created negative impact on sales
#All the dates related to Christmas have low sales than mean, whereas all the dates related to Super Bowl, Thanks giving have high sales than mean.
#It is interesting to note that Labour Day has overall positive impact on sales inspite of having two days days below mean value.

weekly_sales <- aggregate(Weekly_Sales~Date, data=data1,mean)
weekly_sales$Date <-as.Date(weekly_sales$Date, "%d-%m-%Y")
weekly_sales <-arrange(weekly_sales,Date)
weekly_sales$Date <-factor(weekly_sales$Date)#for discrete


options(repr.plot.width = 14, repr.plot.height = 8)

# plotting weekly mean sales
d <- ggplot(data=weekly_sales, aes(x=Date, y=Weekly_Sales, group=1)) +
  geom_line(color="steelblue")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_x_discrete(breaks = levels(weekly_sales$Date)[c(T, rep(F, 9))])+
  scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6))+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Week") + ylab("Mean Sales of Week")

#Plotting Christmas
d +ggtitle('CHRISTMAS')+
  geom_point(aes(x = factor("2010-12-31"), y = 898500.4), color = "red", size = 2) +
  geom_point(aes(x = factor("2011-12-30"), y = 1023165.8), color = "red", size = 2) +
  geom_hline(aes(yintercept = mean_non_holiday_sales), linetype="dashed")

#Plotting Labourday
d + ggtitle('LABOUR DAY')+
  geom_point(aes(x = factor("2010-09-10"), y = 1014097.7), color = "deeppink", size = 2) +
  geom_point(aes(x = factor("2011-09-09"), y = 1039182.8), color = "deeppink", size = 2) +
  geom_point(aes(x = factor("2012-09-07"), y = 	1074001.3), color = "deeppink", size = 2) +
  geom_hline(aes(yintercept = mean_non_holiday_sales), linetype="dashed")

#Plotting Thanks Giving
d + ggtitle('THANKS GIVING')+
  geom_point(aes(x = factor("2010-11-26"), y = 	1462689.0), color = "indianred4", size = 2) +
  geom_point(aes(x = factor("2011-11-25"), y = 1479857.9), color = "indianred4", size = 2) +
  geom_hline(aes(yintercept = mean_non_holiday_sales), linetype="dashed")

#Plotting Superbowl
d + ggtitle('SUPER BOWL')+
  geom_point(aes(x = factor("2010-02-12"), y = 	1074148.4), color = "goldenrod4", size = 2) +
  geom_point(aes(x = factor("2011-02-11"), y = 1051915.4), color = "goldenrod4", size = 2) +
  geom_point(aes(x = factor("2012-02-10"), y = 1111320.2), color = "goldenrod4", size = 2) +
  geom_hline(aes(yintercept = mean_non_holiday_sales), linetype="dashed")

#Insights:
#Though Super Bowl, Labour day have higher sales they are very close to mean
#Thanks giving does create a high positive impact on sales than others
#Christmas Holiday Flag has lower sales than mean sales of a Non Holiday. Both the dates (30- Dec-2011, 31-Dec-2010 ) related to Christmas has negative impact on sales.
#However, from the graph it is clear that the week just before the Christmas bagged highest sales. It may be because customers did shopping beforehand for preparation/ Advent is popularly celebrated there






# Detection and removal of outliers using Bi Variate Box Plots ------------

#Boxplot for checking outliers & removing them
par(mfrow=c(1,1))

#Creating a dataframe for outlier treatment
data5 <- data4

#As we are predicting sales, Thought of removing outliers in Sales based on Various parameters
#Temperature Outlier treatment -- found 5 outlier and removed them
boxplot(data5$Weekly_Sales ~ cut(data5$Temperature, pretty(data5$Temperature)), main="Temperature vs Weekly Sales", xlab ="Temperature", ylab="Weekly Sales", cex.axis=0.5, col="Steel Blue")
outliers_temp <- boxplot(data5$Weekly_Sales ~ cut(data5$Temperature, pretty(data5$Temperature)), main="Temperature vs Weekly Sales", cex.axis=0.5,plot=FALSE)$out
data5<- data5[-which(data5$Weekly_Sales %in% outliers_temp),]

#CPI Outlier treatment-found 1 outlier and removed them
boxplot(data5$Weekly_Sales ~ cut(data5$CPI, pretty(data5$CPI)), main="CPI vs Weekly Sales",xlab ="CPI", ylab="Weekly Sales", cex.axis=0.5,col="Steel Blue")
outliers_CPI <- boxplot(data5$Weekly_Sales ~ cut(data5$CPI, pretty(data5$CPI)), main="CPI vs Weekly Sales", cex.axis=0.5,plot=FALSE)$out
data5<- data5[-which(data5$Weekly_Sales %in% outliers_CPI),]

#Unemployment outlier treatment--found 3 outlier and removed them
boxplot(data5$Weekly_Sales ~ cut(data5$Unemployment, pretty(data5$Unemployment)), main="Unemployment vs Weekly Sales",xlab ="Unemployment", ylab="Weekly Sales",  cex.axis=0.5,col="Steel Blue")
outliers_Unemployment <- boxplot(data5$Weekly_Sales ~ cut(data5$Unemployment, pretty(data5$Unemployment)), main="Unemployment vs Weekly Sales", cex.axis=0.5,plot=FALSE)$out
data5<- data5[-which(data5$Weekly_Sales %in% outliers_Unemployment),]

#fuel price outlier treatment -- found 2 outliers and removed
boxplot(data5$Weekly_Sales ~ cut(data5$Fuel_Price, pretty(data5$Fuel_Price)), main="Fuel_Price vs Weekly Sales", xlab ="Fuel Price", ylab="Weekly Sales", cex.axis=0.5,col="Steel Blue")
outliers_fuel_price <- boxplot(data5$Weekly_Sales ~ cut(data5$Fuel_Price, pretty(data5$Fuel_Price)), main="Fuel_Price vs Weekly Sales", cex.axis=0.5,plot=FALSE)$out
data5<- data5[-which(data5$Weekly_Sales %in% outliers_fuel_price),]

#Outlier treatment for Holiday Flag - No outliers found
boxplot(data5$Weekly_Sales ~ data5$Holiday_Flag, main = 'Weekly Sales - Holiday_Flag',xlab ="Holiday Flag", ylab="Weekly Sales",col="Steel Blue" )

#outlier treatment for month - 4 outliers found and removed
boxplot(data5$Weekly_Sales ~ data5$month, main = 'Weekly Sales - month', xlab ="Month", ylab="Weekly Sales", col="Steel Blue")
outliers_month <- boxplot(data5$Weekly_Sales ~ data5$month, main = 'Weekly Sales - month',plot=FALSE)$out
data5<- data5[-which(data5$Weekly_Sales %in% outliers_month),]

#outlier treatment for quarter - 2 outliers found and removed
outliers_quarter <- boxplot(data5$Weekly_Sales ~ data5$quarter, main = 'Weekly Sales - quarter',xlab ="Quarters", ylab="Weekly Sales", col="Steel Blue")$out
data5<- data5[-which(data5$Weekly_Sales %in% outliers_quarter),]

#Outliers removed from data in order:
  
# Temperature 5 outliers
#CPI Outlier treatment 1 outliers
#Unemployment 3 outliers
#Fuel price 2 outliers
#Holiday Flag - No outliers
#Month - 4 outliers
#Quarter - 2 outliers




# Correlation Plot & Correlation Matrix -----------------------------------

#correlation matrix and corr plot
corr = cor(data5[, c(1:9)])
View(corr)
corrplot(corr, method = "color", cl.pos = 'n', rect.col = "black",  tl.col = "indianred4", addCoef.col = "black", number.digits = 2, number.cex = 0.60, tl.cex = 0.7, cl.cex = 1, col = colorRampPalette(c("blue","white","yellow"))(100))

#Insights:
#Observed very low correlation between Temp and Weekly Sales – So can omit Temperature
#Observed low correlation between month, quarter, Holiday Flag with Weekly_Sales may be due to considering categorical variables as continuous variables


# Creating Dummy Variables ------------------------------------------------

#Creating Dummy Variables 

Events <- as.factor(data5$Events)
dummy_Events <- data.frame(model.matrix(~Events))[,-1]

quarter <- as.factor(data5$quarter)
dummy_quarter <- data.frame(model.matrix(~quarter))[,-1]

month <- as.factor(data5$month)
dummy_month <- data.frame(model.matrix(~month))[,-1]


data5 <- cbind(data5,dummy_Events,dummy_quarter,dummy_month)


#Linear Regression Model ----------------------------------------------------------------

# Splitting dataset into training set and test set
set.seed(123) # Seed initializes the randomness -- set.seed helps to fix randomness fixed everytime you open. you can write any number inside the set.seed()
library(caTools)

#Considering all parameters - Weekly Sales, Holiday FlagTemp, Fuel, CPI,Unemployment, Weeknumber, Event(categorical), quarter(categorical), month(categorical)
dataset <- data5[, c(1,4,6,7,11:12, 17:27 )]

#Creating a sample split and divided test & training sets in 30-70 ratio respectively
sample = sample.split(dataset, SplitRatio = 0.7) # Returns a vector with T for 70% of data
trainingSet = subset(dataset, sample == T)
testSet = subset(dataset, sample == F)

# Create model 
model = lm(formula = Weekly_Sales ~ . , data = trainingSet)
summary(model)

options(repr.plot.width = 10, repr.plot.height = 10)

# Make predictions on the test set
linear_predictions <- predict(model, newdata = testSet)

# Visualizing train set results
y_pred_train = predict(model, newdata = trainingSet)
ggplot() + 
  geom_point(aes(x=trainingSet$Weekly_Sales,y=y_pred_train), size=3,colour = "Blue") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_y_continuous(labels = label_number(suffix = " K", scale = 1e-3))+
  scale_x_continuous(labels = label_number(suffix = " K", scale = 1e-3))+
  ggtitle('Comparision of Actual Sales vs Predicted Sales - Train Data')+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Actual Sales") + ylab("Predicted Sales")

# Visualizing the test set results
y_pred_test = predict(model, newdata = testSet)
ggplot() + 
  geom_point(aes(x=testSet$Weekly_Sales,y=y_pred_test), size =3, colour = "Blue") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_y_continuous(labels = label_number(suffix = " K", scale = 1e-3))+
  scale_x_continuous(labels = label_number(suffix = " K", scale = 1e-3))+
  ggtitle('Comparision of Actual Sales vs Predicted Sales - Test Data')+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Actual Sales") + ylab("Predicted Sales")
# Create a data frame with actual and predicted values
results <- data.frame(Actual_Weekly_Sales = testSet$Weekly_Sales, Predicted_Weekly_Sales = y_pred_test)

# Print the first few rows of the results
View(results)

# Create a scatterplot
ggplot(results, aes(x = Actual_Weekly_Sales, y = Predicted_Weekly_Sales)) +
  geom_point() +
  labs(x = "Actual Weekly Sales", y = "Predicted Weekly Sales") +
  ggtitle("Actual vs. Predicted Weekly Sales") +
  theme_minimal()

### Parameters to validate the accuracy of the model and improvise.
MAPE(y_pred_test,testSet$Weekly_Sales)
RMSE(y_pred_test,testSet$Weekly_Sales)


#Random Forest Model------------------------------------------------------------

# Load the required library
library(randomForest)

# Create a Random Forest model
rf_model <- randomForest(Weekly_Sales ~ ., data = trainingSet)

# Make predictions on the test set
rf_predictions <- predict(rf_model, newdata = testSet)

# Calculate MAPE and RMSE for the Random Forest model
rf_mape <- MAPE(rf_predictions, testSet$Weekly_Sales)
rf_rmse <- RMSE(rf_predictions, testSet$Weekly_Sales)

# Print the MAPE and RMSE for the Random Forest model
print(paste("Random Forest MAPE:", rf_mape))
print(paste("Random Forest RMSE:", rf_rmse))


# Create a data frame with actual and predicted values for both models
comparison_data <- data.frame(
  Actual = testSet$Weekly_Sales,
  Linear_Regression_Predicted = y_pred_test,
  Random_Forest_Predicted = rf_predictions
)

view(comparison_data)

# Create a scatter plot
ggplot(comparison_data, aes(x = Actual, y = Random_Forest_Predicted)) +
  geom_point() +
  labs(x = "Actual Weekly Sales", y = "Predicted Weekly Sales") +
  ggtitle("Actual vs. Predicted Weekly Sales (Random Forest Model)") +
  theme_minimal()# Create a scatterplot to compare the models

#ggplot(comparison_data, aes(x = Actual)) +
#  geom_point(aes(y = Linear_Regression_Predicted), color = "blue", alpha = 0.7, size = 3) +
#  geom_point(aes(y = Random_Forest_Predicted), color = "red", alpha = 0.7, size = 3) +
# labs(x = "Actual Weekly Sales", y = "Predicted Weekly Sales") +
#  ggtitle("Comparison of Linear Regression and Random Forest Models") +
# theme_minimal()


  
#Random Forest has a slightly higher MAPE (0.0466) compared to Linear Regression (0.0449).
#Lower MAPE indicates a better percentage accuracy in predicting sales.
#Therefore, Linear Regression has a slightly better performance in this regard.
#Random Forest has a slightly lower RMSE (82445.91) compared to Linear Regression (83085.05).
#Lower RMSE indicates better accuracy in predicting the actual sales values. 
#In this case, Random Forest has a slightly better RMSE.
#The differences in MAPE and RMSE between the two models are relatively small.
#If main concern is percentage accuracy (MAPE), Linear Regression performs slightly better.
#If main concern is predicting the actual sales values with smaller errors (RMSE), Random Forest performs slightly better.





