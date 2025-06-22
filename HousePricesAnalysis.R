library(dplyr)
library(ggplot2)
df<-read.csv("C:/Users/s1720/OneDrive/Desktop/GitHub/DataSet/Housing.csv")
str(df) #Checking data structure
head(df) #Importing the first values of each column

#Checking if the dataset needs cleaning
mean(df$price)#If there are non numerical values we get NA
mean(df$area)
mean(df$bedrooms)
mean(df$bathrooms)
mean(df$stories)
mean(df$parking)
sum(is.na(df)) #Checking if there are NA values in general including char columns
sapply(df,length) #Checking that the values have the same length
df <- df[!duplicated(df), ]  

#I turn the various yes and no values into binary values
binary_cols <- c("mainroad", "guestroom", "basement", "hotwaterheating",
                 "airconditioning", "prefarea")
df[binary_cols]<-lapply(df[binary_cols],function(x) ifelse(x=="yes",1,0))
#Changing furnishing too into values (0, 1 and 2)
df[["furnishingstatus"]]<-lapply(df[["furnishingstatus"]],function(x) ifelse(x=="furnished",2,ifelse(x=="semi-furnished",1,0)))
str(df)
df$furnishingstatus <- unlist(df$furnishingstatus) #Changing the column from list to num

#Summary statistics for relevant non dummy variables
summary(df[,c("price","area","bedrooms","bathrooms","stories")])
hist(df$price)#Histogram of prices

#Regression
model<-lm(price~ area+bedrooms+bathrooms+stories+mainroad+guestroom+basement+hotwaterheating+airconditioning+parking+prefarea+furnishingstatus, data=df)
summary(model)
#Regression removing bedrooms
model<-lm(price~ area+bathrooms+stories+mainroad+guestroom+basement+hotwaterheating+airconditioning+parking+prefarea+furnishingstatus, data=df)
summary(model)

#by Said Mouhtaram
