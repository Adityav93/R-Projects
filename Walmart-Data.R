#rm(list = ls())

#Read csv file into data frame
walmart.data = read.csv("Walmart_Store_sales.csv",header=T)
View(walmart.data)
str(walmart.data)
#Convert date to date format, since its a factor
walmart.data$Date = as.Date(walmart.data$Date,format = "%d-%m-%Y")

#Find the store which has max sale
#Approach: group weekly_sales based on store and find the sum
#of sales for each store.Then Find the maximum value among these.
total.sales = aggregate(Weekly_Sales ~ Store ,walmart.data, sum)
colnames(total.sales)[2]="TOTAL_SALE"
total.sales
max_sales = subset(total.sales, total.sales$TOTAL_SALE == max(total.sales$TOTAL_SALE))
colnames(max_sales)[2]="MAX_SALE"
max_sales
# Store  MAX_SALE
# 20     301397792

#Find the store having max Standard deviation
#Approach: group weekly_sales based on store and find the sd
#of sales for each store.Then Find the maximum value among these.
sd = aggregate(Weekly_Sales ~ Store ,walmart.data, sd)
colnames(sd)[2]="Standard_deviation"
sd
max_sd = subset(sd, sd$Standard_deviation == max(sd$Standard_deviation))
colnames(max_sd)[2]="MAX_SD"
max_sd
# Store   MAX_SD
# 14      317569.9

#Coeff of mean to std dev
#Approach: group weekly_sales based on store and find the mean
#of sales for each store.Then divide the sd with mean and get coeff of sd.and find the max value.
Mean = aggregate(Weekly_Sales ~ Store ,walmart.data, mean)
colnames(Mean)[2]="MEAN"
Mean
sd_mean = cbind(sd,Mean$MEAN)
colnames(sd_mean)[3]="MEAN"
sd_mean
sd_mean = cbind(sd_mean,sd_mean$Standard_deviation/sd_mean$MEAN)
colnames(sd_mean)[4]="coeff"
max(sd_mean$coeff)
# [1] 0.2296811


#Which store/s has good quarterly growth rate in Q32012
#Approach: add a new col year.quarter to the df (which specifies year nd the quarter)
#now group the weekly_sales wrt store and year.quarter, i.e find the sum of sales for
#each quarter for each store.
#now copy the values for 2012.Q2 and 2012.Q3 into another df
#now add a new col to this newly created df which specifes the growthrate for each store for q2-q3
#now select the stores for whihch the growth-rate is > 0. i.e they have a good growth.
#Also, find the store which has the highest growth-rate
library(lubridate)
library(dplyr)
walmart.data = mutate(walmart.data, year.quarter=quarter(ymd(walmart.data$Date),with_year = T))
walmart.data$year.quarter = as.factor(walmart.data$year.quarter)
str(walmart.data)

Q2_SUM = aggregate(walmart.data$Weekly_Sales, by = list(walmart.data$Store,walmart.data$year.quarter), FUN = sum)
colnames(Q2_SUM)[1]="Store"
colnames(Q2_SUM)[2]="Quarter"
colnames(Q2_SUM)[3]="Quarter_Sum"
Q2_SUM

Q2_2012_SUM = Q2_SUM[Q2_SUM$Quarter==2012.2,]
Q2_2012_SUM

Q3_2012_SUM = Q2_SUM[Q2_SUM$Quarter==2012.3,]
Q3_2012_SUM

Q2Q3Sum = cbind(Q2_2012_SUM,Q3_2012_SUM$Quarter_Sum)
Q2Q3Sum$Quarter = NULL
colnames(Q2Q3Sum)[1]="Store"
colnames(Q2Q3Sum)[2]="Q2_2012_SUM"
colnames(Q2Q3Sum)[3]="Q3_2012_SUM"
Q2Q3Sum

Q2Q3Sum = cbind(Q2Q3Sum,(Q2Q3Sum$Q3_2012_SUM - Q2Q3Sum$Q2_2012_SUM)/Q2Q3Sum$Q2_2012_SUM)
colnames(Q2Q3Sum)[4]="Growth_Rate"
Q2Q3Sum
Stores_with_good_q2q3_growth_rate = subset(Q2Q3Sum,Q2Q3Sum$Growth_Rate>0)
Stores_with_good_q2q3_growth_rate
#        Store Q2_2012_SUM Q3_2012_SUM Growth_Rate
# 412     7     7290859     8262787 0.133307760
# 421    16     6564336     7121542 0.084883781
# 428    23    18488883    18641489 0.008253951
# 429    24    17684219    17976378 0.016520877
# 431    26    13155336    13675692 0.039554775
# 440    35    10838313    11322421 0.044666372
# 444    39    20214128    20715116 0.024784040
# 445    40    12727738    12873195 0.011428413
# 446    41    17659943    18093844 0.024569801
# 449    44     4306406     4411251 0.024346377
max_q2q3_growth = subset(Q2Q3Sum,Q2Q3Sum$Growth_Rate==max(Q2Q3Sum$Growth_Rate))
max_q2q3_growth
#        Store Q2_2012_SUM Q3_2012_SUM Growth_Rate
# 412     7     7290859     8262787   0.1333078


#Some holidays have a negative impact on sales. Find out holidays which have higher sales than the mean sales in non-holiday season for all stores together

#Approach: split the walmart-data df into two DFs one for hol-season=1 and another for hol-season=0
#now find the mean of weekly_sales for non_hol_season df.
#now group the holiday_season_df wrt date and find the sum of sales for each holiday date
#irrsepective of the store.
#now subset the data where the sales on holiday-date is greater than mean sales on non holiday
#alternatively, also you can also subset based on month , to find the total sales for each holiday month
# and display those which have value greater than mean sales on non holiday season.
Holiday_Season_DF = walmart.data[walmart.data$Holiday_Flag=='1',]
Non_Holiday_Season_DF = walmart.data[walmart.data$Holiday_Flag=='0',]
Holiday_Season_DF
Non_Holiday_Season_DF
mean_non_hol_sale = mean(Non_Holiday_Season_DF$Weekly_Sales)
mean_non_hol_sale
#1041256

Holiday_Data = aggregate(Holiday_Season_DF$Weekly_Sales, list(Date = Holiday_Season_DF$Date), sum )
colnames(Holiday_Data)[2]="Holiday_Date_Sales"
Holiday_Data

subset(Holiday_Data,Holiday_Data$Holiday_Date_Sales>mean_non_hol_sale)
#    Date           Holiday_Date_Sales
# 1  2010-02-12           48336678
# 2  2010-09-10           45634398
# 3  2010-11-26           65821003
# 4  2010-12-31           40432519
# 5  2011-02-11           47336193
# 6  2011-09-09           46763228
# 7  2011-11-25           66593605
# 8  2011-12-30           46042461
# 9  2012-02-10           50009408
# 10 2012-09-07           48330059

Holiday_Month_Data = aggregate(Holiday_Data$Holiday_Date_Sales, list(mnth = month(ymd(Holiday_Data$Date))), sum)
colnames(Holiday_Month_Data)[2]="Holiday_Month_Sales"
Holiday_Month_Data
#    mnth     Holiday_Month_Sales
# 1    2           145682278
# 2    9           140727685
# 3   11           132414609
# 4   12            86474980
subset(Holiday_Month_Data,Holiday_Month_Data$Holiday_Month_Sales>mean_non_hol_sale)
#    mnth    Holiday_Month_Sales
# 1    2           145682278
# 2    9           140727685
# 3   11           132414609
# 4   12            86474980

#Provide a monthly and semester view of sales in units and give insights
# Approach: use year and month function in mutate and concatenate those and same a new col year.month in the same df
# use semestr function in mutate function to find year.semester and add the new col to same df
# use aggregate function to find sales for each month of each year.
# use aggregate function to find sales for each semester for each year.
walmart.data = mutate(walmart.data, year.month=paste(as.character(year(ymd(walmart.data$Date))),as.character(month(ymd(walmart.data$Date))),sep="."))
walmart.data

monthly_sales = aggregate(walmart.data$Weekly_Sales, list(year.month=walmart.data$year.month), sum)
colnames(monthly_sales)[2]="month_sales"
monthly_sales
filter(monthly_sales,monthly_sales$month_sales==max(monthly_sales$month_sales))
# year.month month_sales
# 1    2010.12   288760533

walmart.data = mutate(walmart.data, year.semester=semester(ymd(walmart.data$Date),with_year = T))
walmart.data

sem_sales = aggregate(walmart.data$Weekly_Sales, list(year.sem=walmart.data$year.semester), sum)
colnames(sem_sales)[2]="semester_sales"
filter(sem_sales,sem_sales$semester_sales==max(sem_sales$semester_sales))
# year.sem semester_sales
# 1   2011.2     1320860210

# For Store 1 - Build  prediction models to forecast demand
# 
# Linear Regression - Utilize variables like date and restructure dates as 1 for 5 Feb 2010 (starting from the earliest date in order). Hypothesize if CPI, unemployment, and fuel price have any impact on sales.
# 
# Change dates into days by creating new variable.
# 
# Select the model which gives best accuracy.

store1.data = subset(walmart.data,walmart.data$Store==1)
store1.data=cbind(store1.data,day=yday(store1.data$Date))
store1.data$day=store1.data$day-35
store1.data = store1.data %>% filter(store1.data$day>0)
head(store1.data)
#    Store       Date     Weekly_Sales    Holiday_Flag Temperature  Fuel_Price      CPI      Unemployment
# 1     1     2010-02-05      1643691            0       42.31        2.572      211.0964        8.106
# 2     1     2010-02-12      1641957            1       38.51        2.548      211.2422        8.106
# 3     1     2010-02-19      1611968            0       39.93        2.514      211.2891        8.106
# 4     1     2010-02-26      1409728            0       46.63        2.561      211.3196        8.106
# 5     1     2010-03-05      1554807            0       46.50        2.625      211.3501        8.106
# 6     1     2010-03-12      1439542            0       57.79        2.667      211.3806        8.106
#     year.quarter year.month  year.semester day
# 1       2010.1     2010.2        2010.1    36
# 2       2010.1     2010.2        2010.1    43
# 3       2010.1     2010.2        2010.1    50
# 4       2010.1     2010.2        2010.1    57
# 5       2010.1     2010.3        2010.1    64
# 6       2010.1     2010.3        2010.1    71

model <- lm(Weekly_Sales ~ CPI+Unemployment+Fuel_Price, data = store1.data)
summary(model)

# Call:
#   lm(formula = Weekly_Sales ~ CPI + Unemployment + Fuel_Price, 
#      data = store1.data)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -216752  -89173  -25678   64739  876221 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)   
# (Intercept)  -4011355    1810603  -2.215  0.02848 * 
#   CPI             22735       7106   3.199  0.00173 **
#   Unemployment   119599      59558   2.008  0.04672 * 
#   Fuel_Price     -76476      47772  -1.601  0.11186   
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 150300 on 129 degrees of freedom
# Multiple R-squared:  0.08847,	Adjusted R-squared:  0.06727 
# F-statistic: 4.173 on 3 and 129 DF,  p-value: 0.0074

# comparing p-values of cpi, unemployment and fuel_price wrt alpha(0.05)
# we can conclude that CPI and unemployment have effect on weekly_sales
# where as fuel.price has no effect on weekly_sales



