#### IXIS
##Import session count data
sessionCounts <- read.csv(file="C:/Users/Bunglee/Downloads/DataAnalyst_Ecom_data_sessionCounts.csv", header=TRUE, sep=",")

##Exploring dataset
head(sessionCounts)

hist(sessionCounts$transactions)
##Transactions has an exponential distribution with the vast majority of sessions having zero transactions
hist(sessionCounts$QTY)
##QTY has the same exponential distribution 
hist(sessionCounts$sessions)
##Session counts also has an exponential distribution 

## Data cleaning
typeof(sessionCounts$dim_date)
## Change date to date data type
sessionCounts$dim_date <- as.Date(sessionCounts$dim_date, "%m/%d/%y")

## Creating column for ECR, Transactions/Sessions 
sessionCounts$ecr <- sessionCounts$transactions/sessionCounts$sessions

##Extracting month from date
sessionCounts$month <- format(sessionCounts$dim_date, "%m")

##Final cleaned product with appropriate columns
head(sessionCounts)

## Creating worksheet number 1: Metrics by month and device
library(dplyr)

##Grouped by month and device
monthDevice <- sessionCounts %>% 
  select(dim_deviceCategory, month, sessions, transactions, QTY) %>%
  group_by(month, dim_deviceCategory) %>% summarise(across(everything(), list(sum)))

monthDevice <- as.data.frame(monthDevice)


##Recalculating ECR with aggregated data
monthDevice$ecr <- monthDevice$transactions/monthDevice$sessions

#########Exploring findings - Metrics by Device
boxplot(monthDevice$ECR~monthDevice$DeviceCategory)
##Desktop has the highest transactions per session while mobile has the least
boxplot(monthDevice$Quantity~monthDevice$DeviceCategory)
##Desktop additionally has the highest quantity of orders
boxplot(monthDevice$Transactions~monthDevice$DeviceCategory)
##Similar trend in transactions
boxplot(monthDevice$Sessions~monthDevice$DeviceCategory)
##Most importantly, desktop, mobile, and tablet sessions have a lesser distance between
##them than other metrics. This shows that the issue is not the traffic amount but instead
##that customers prefer to order on desktop. 


##Creating a more visually appealing output for Transactions and Sessions
tranBox <- ggplot(monthDevice, aes(x=DeviceCategory, y=Transactions, color=DeviceCategory)) + geom_boxplot() + xlab("")
tranBox
sessBox <- ggplot(monthDevice, aes(x=DeviceCategory, y=Sessions, color=DeviceCategory)) + geom_boxplot()+ xlab("")
sessBox
ecrBox <- ggplot(monthDevice, aes(x=DeviceCategory, y=ECR, color=DeviceCategory)) + geom_boxplot()+ xlab("")
ecrBox
##Confirming analysis using sessions and transactions


## Standardize variables using z transformation
monthDeviceSTD <- monthDevice %>% mutate(Sessions = scale(Sessions)) %>% mutate(Transactions = scale(Transactions))

##Device category statistically impacts Sessions and Transactions
summary(manova(cbind(monthDeviceSTD$Sessions, monthDeviceSTD$Transactions) ~ monthDeviceSTD$DeviceCategory))

##Device Impact on Sessions: Desktop .70, Mobile -.60 (Statistically significant at 90% cL), Tablet -1.52 
summary(lm(monthDeviceSTD$Sessions~monthDeviceSTD$DeviceCategory))

##Device Impact on Transactions: Desktop 1.18, Mobile -1/87, Tablet -1.69
summary(lm(monthDeviceSTD$Transactions~monthDeviceSTD$DeviceCategory))

##Therefore device type has a higher impact on transactions than sessions


##Renaming column names for more visually appealing and comprehensible output
colnames(monthDevice) <- c('Month', 'DeviceCategory', 'Sessions', 'Transactions', 'Quantity', 'ECR')

##Final product head
head(monthDevice)

## Obtaining adds to cart data

cartData <- read.csv(file="C:/Users/Bunglee/Downloads/DataAnalyst_Ecom_data_addsToCart.csv", header=TRUE, sep=",")

##Most recent month/yr is 06/2013, so the month over month will ultimately contain 05/2013 and 06/2013

##Cleaning data
##Creating date column
library(lubridate)

##Placeholder day to create date data type
cartData$date <- paste(as.character(cartData$dim_year),"-",as.character(cartData$dim_month), '-1', sep="")
cartData$date <- ymd(cartData$date)
typeof(cartData$date)

cartData$date <- format(cartData$date, format="%Y-%m")


##Exploring data
hist(cartData$addsToCart)
##Non-normal distribution 

summary(cartData$addsToCart)
boxplot(cartData$addsToCart, main='Adds To Cart Per Month')

##Wide range of monthly cart additions

##Plotting time series
library(ggplot2)

cartAddsOverTime <- ggplot(cartData, aes(x=date, y=addsToCart, group=1)) + 
  geom_line() + 
  ggtitle("Cart Additions by Month") +
  xlab("")
cartAddsOverTime

##This chart has a few clear trends: peaks in August, November, and April
## Troughs in September, March, and June


#######Setting up data for output

##Needed: Metrics by month


##Creating a date column with only month and year

sessionCounts$YMdate <- format(sessionCounts$dim_date, format="%Y-%m")
colnames(sessionCounts)
## Aggregating data with YMdate, sessions, transactions, QTY, and addsToCart from cartData joined on YMdate
monthYear <- sessionCounts %>% 
  select(YMdate, sessions, transactions, QTY) %>%
  group_by(YMdate) %>% 
  summarise(across(everything(), list(sum))) %>%
  inner_join(select(cartData, date, addsToCart), by = c("YMdate" = "date"))

## Cleaning data
monthYear <- as.data.frame(monthYear)

colnames(monthYear) <- c("Date", "Sessions", "Transactions", "Quantity", "AddsToCart")

##Adding monthly ECR
monthYear$ECR <- monthYear$Transactions/monthYear$Sessions


## Multiple time series plot many metrics
timeSeries <- ggplot(monthYear, aes(x=Date)) +
  geom_line(aes(y=Sessions, group=1, color="red"), color="red", show.legend=TRUE) + 
  geom_line(aes(y=Transactions, group=1, color="orange"), color="orange", show.legend=TRUE) + 
  geom_line(aes(y=Quantity, group=1, color="blue"), color="blue", show.legend=TRUE) +
  geom_line(aes(y=AddsToCart, group=1, color="gray"), show.legend=TRUE) +
  ggtitle("Monthly Metrics") +
  xlab("") +
  scale_color_manual(name = "Legend",
                     values = c("Sessions"="red", "Transactions"="orange", "Quantity"="blue", "AddsToCart"="gray"))

timeSeries 


##Grouped by YMdate and device for time series analysis
monthYrDevice <- sessionCounts %>% 
  select(dim_deviceCategory, sessions, transactions, QTY, YMdate) %>%
  group_by(YMdate, dim_deviceCategory) %>% summarise(across(everything(), list(sum)))
##Cleaning and creating new df
monthYrDevice <- as.data.frame(monthYrDevice)
colnames(monthYrDevice) <- c('Date', 'Device', 'Sessions', 'Transactions', 'Quantity')
monthYrDevice$ECR <- monthYrDevice$Transactions / monthYrDevice$Sessions

monthYrDeviceDesktop <- monthYrDevice %>% 
  select(Device, Date, ECR) %>%
  filter(Device == 'desktop')

##Time Series with ECR
deviceSessTime <- ggplot(monthYrDevice, aes(x=Date, y = ECR, color=Device, group=Device)) +
  geom_line() +
  xlab("") +
  ggtitle("ECR Over Time by Device")
deviceSessTime


##Log scale to delve into trends more visibly
timeSeries + scale_y_continuous(trans='log10') 

##It appears that a similar trend to session count can be seen among Quantity and Transactions
##This is expected, as sessions rise, it can be assumed that the other metrics will rise as well
##However, AddsToCart does not follow the same pattern


##Correlation plot to confirm findings
library(corrplot)
corMatrix <- cor(monthYear[,2:6])
corrplot(corMatrix, method="number")

##Sessions, transactions, and quantity maintain strong correlation 
##AddsToCart has a weak negative correlation with these variables while ECR has a weak positive correlation


##Modify monthYear for xlxs output

##Abs Session change
for (i in 1:12) {
  if (i == 1) {}
  else {monthYear$AbsSessChange[i] <- monthYear$Sessions[i] - monthYear$Sessions[i-1] }
}
##Percent Session change
for (i in 1:12) {
  if (i == 1) {}
  else {monthYear$PerSessChange[i] <- ((monthYear$Sessions[i] - monthYear$Sessions[i-1])/monthYear$Sessions[i-1]) }
}
##Abs Transaction change
for (i in 1:12) {
  if (i == 1) {}
  else {monthYear$AbsTranChange[i] <- monthYear$Transactions[i] - monthYear$Transactions[i-1] }
}
##Percent Transaction change
for (i in 1:12) {
  if (i == 1) {}
  else {monthYear$PerTranChange[i] <- ((monthYear$Transactions[i] - monthYear$Transactions[i-1])/monthYear$Transactions[i-1]) }
}
##Abs Quantity change
for (i in 1:12) {
  if (i == 1) {}
  else {monthYear$AbsQtyChange[i] <- monthYear$Quantity[i] - monthYear$Quantity[i-1] }
}
##Percent Quantity change
for (i in 1:12) {
  if (i == 1) {}
  else {monthYear$PerQtyChange[i] <- ((monthYear$Quantity[i] - monthYear$Quantity[i-1])/monthYear$Quantity[i-1]) }
}
##Abs AddsToCart change
for (i in 1:12) {
  if (i == 1) {}
  else {monthYear$AbsCartChange[i] <- monthYear$AddsToCart[i] - monthYear$AddsToCart[i-1] }
}
##Percent AddsToCart change
for (i in 1:12) {
  if (i == 1) {}
  else {monthYear$PerCartChange[i] <- ((monthYear$AddsToCart[i] - monthYear$AddsToCart[i-1])/monthYear$AddsToCart[i-1]) }
}
##Abs ECR change
for (i in 1:12) {
  if (i == 1) {}
  else {monthYear$AbsEcrChange[i] <- monthYear$ECR[i] - monthYear$ECR[i-1] }
}
##Percent ECR change
for (i in 1:12) {
  if (i == 1) {}
  else {monthYear$PerEcrChange[i] <- ((monthYear$ECR[i] - monthYear$ECR[i-1])/monthYear$ECR[i-1]) }
}



#######Export all outputs
library(openxlsx)
sheetList <- list("monthDevice" = monthDevice, "monthYear" = monthYear)
write.xlsx(sheetList, file="IXISanalytics.xlsx")
