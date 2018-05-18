#Assignment Liner Regression

#setting the path
setwd("C:\\Data Scientist\\Predictive analysis\\Nagu assginment")

#installing the required packages
#installing the require Packages
#install.packages("tidyr")
#install.packages("dplyr")
#install.packages("corrgram")
#install.packages("corrplot")
#install.packages("MASS")
#install.packages("car")
#install.packages("VIF")

#Loading the libraries
library(tidyr)
library(dplyr)
library(corrgram)
library(corrplot)
library(MASS)
library(car)
library(vif)


help(vif)


#importing the car_assignment dataset
cardata<-read.csv("CarPrice_Assignment.csv",stringsAsFactors = FALSE)

#Structure of the file
str(cardata)
summary(cardata)

#Observing the first few records
head(cardata)

#Checking the duplicate data in the data set
sum(duplicated(cardata$car_ID))

#Check for NA values
sum(is.na(cardata$car_ID)) ## 0 NA

#The data set some missing values
missing_values <- cardata %>%
  summarise_all(funs(sum(is.na(.))/n()))

View(missing_values) # 0 i.e. no NAs

#The car dataset contains CarName coloumn which contains Carname and Model Name
#Need to spilt the carname and ModelName 
cardata<- separate(cardata, col = CarName, into = c("CarName","Modelname"), sep = " ")

##### Two-variable ###

# Categorical variable with 2 level
#fueltype   : diesel, gas
#aspiration : std, turbo. 
#doornumber	: four, two. 
#enginelocation : front, rear.

#### Two-variable End #####

############# FUEL TYPE #######################

#Checking the structure of the variable and converting it into numeric variable for fueltype variable
# two variable gas nad diesel
str(cardata$fueltype)
cardata$fueltype <- factor(cardata$fueltype)
summary(factor(cardata$fueltype))

#Finding the levels of fuel type and converting it into numeric :- diesel:0 and gas:1
cardata$fueltype<-ifelse(cardata$fueltype=="diesel",0,1)
cardata$fueltype<-as.numeric(cardata$fueltype)
summary(factor(cardata$fueltype))


##Aspiration variable
#The aspiration variable contains std and turbo
str(cardata$aspiration)
cardata$aspiration <- factor(cardata$aspiration)
summary(factor(cardata$aspiration))

#Std:0 and turbo:1 and converting it to numeric
cardata$aspiration<-ifelse(cardata$aspiration=="std",0,1)
cardata$aspiration<-as.numeric(cardata$aspiration)


#Let us see the structure of variable "doornumber".
str(cardata$doornumber)
cardata$doornumber <- factor(cardata$doornumber)
summary(factor(cardata$doornumber))

#four:0 and two:1 and conveting it to numeric
cardata$doornumber<-ifelse(cardata$doornumber=="four",0,1)
cardata$doornumber<-as.numeric(cardata$doornumber)

#Engine location
str(cardata$enginelocation)
cardata$enginelocation <- factor(cardata$enginelocation)
summary(factor(cardata$enginelocation))

#Front:0 and rear:1 and converting to numeric
cardata$enginelocation<-ifelse(cardata$enginelocation=="front",0,1)
cardata$enginelocation<-as.numeric(cardata$enginelocation)


#### Three-Variable ####

#The carprice data set More than 3 variable
#drivewheel		: 4wd, fwd, rwd. 
#body-style   : hardtop, wagon, sedan, hatchback, convertible. 
#enginetype		: dohc, dohcv, l, ohc, ohcf, ohcv, rotor. 
#num-of-cylinders: eight, five, four, six, three, twelve, two
#fuelsystem: 1bbl, 2bbl, 4bbl, idi, mfi, mpfi, spdi, spfi.
#make: alfa-romero, audi, bmw, chevrolet, dodge, honda, isuzu, jaguar, mazda, mercedes-benz, mercury, 
#mitsubishi, nissan, peugot, plymouth, porsche, renault, saab, subaru, toyota, volkswagen, volvo 


### Variable:drivewheel:- 4wd, fwd, rwd. 
summary(factor(cardata$drivewheel))

#Converting "drivewheel" into dummies . 
dummy_drivewheel <- data.frame(model.matrix( ~drivewheel, data = cardata))

#View the dummy_drivewheel
View(dummy_drivewheel)

#This column should be removed from the newly created dummy_drivewheel dataframe containing the dummy values for the variable "drivewheel". 
dummy_drivewheel <- dummy_drivewheel[,-1]

# Combine the dummy variables to the main data set, after removing the original categorical "furnishingstatus" column
cardata_1 <- cbind(cardata[,-9], dummy_drivewheel)
View(cardata_1)


### Variable:carbody:- hardtop, wagon, sedan, hatchback, convertible.
summary(factor(cardata_1$carbody))

#Converting "carbody" into dummies . 
dummy_carbody <- data.frame(model.matrix( ~carbody, data = cardata_1))

#Viewig the dummy carody
View(dummy_carbody)

#This column should be removed from the newly created dummy_carbody dataframe containing the dummy values for the variable "carbody". 
dummy_carbody <- dummy_carbody[,-1]

cardata_2 <- cbind(cardata_1[,-8], dummy_carbody)
View(cardata_2)


###Variable:enginetype:- dohc, dohcv, l, ohc, ohcf, ohcv, rotor. 
summary(factor(cardata_2$enginetype))

#Converting "Engine type" into dummies . 
dummy_enginetype <- data.frame(model.matrix( ~enginetype, data = cardata_2))

#check the dummy_1 data frame.
View(dummy_enginetype)

#This column should be removed from the newly created dummy_enginetype dataframe containing the dummy values for the variable "enginetype". 
dummy_enginetype <- dummy_enginetype[,-1]

cardata_3 <- cbind(cardata_2[,-14], dummy_enginetype)
View(cardata_3)


###Variable:cylindernumber:- eight, five, four, six, three, twelve, two
summary(factor(cardata_3$cylindernumber))

#Converting "cylindernumber" into dummies 
dummy_cylindernumber <- data.frame(model.matrix( ~cylindernumber, data = cardata_3))

#check the dummy_1 data frame.
View(dummy_cylindernumber)

#This column should be removed from the newly created dummy_cylindernumber dataframe containing the dummy values for the variable "cylindernumber". 
dummy_cylindernumber <- dummy_cylindernumber[,-1]

cardata_4 <- cbind(cardata_3[,-14], dummy_cylindernumber)
View(cardata_4)

###Variable:fuelsystem: 1bbl, 2bbl, 4bbl, idi, mfi, mpfi, spdi, spfi.
summary(factor(cardata_4$fuelsystem))

#Converting "fuelsystem" into dummies . 
dummy_fuelsystem <- data.frame(model.matrix( ~fuelsystem, data = cardata_4))

#check the dummy_1 data frame.
View(dummy_fuelsystem)

#This column should be removed from the newly created dummy_fuelsystem dataframe containing the dummy values for the variable "fuelsystem". 
dummy_fuelsystem <- dummy_fuelsystem[,-1]

cardata_5 <- cbind(cardata_4[,-15], dummy_fuelsystem)

View(cardata_5)



#### Binning on symboling variable #####

# So, let us create three new levels by binning the levels of "symboling" into  
# 3 equal bins. Create three levels(high,mid,low) which will include factor levels 1,2 and 3 of the variable symboling.
summary(as.factor(cardata_5$symboling))
#high(2,3),mid(0,1),low(-1,-2)

cardata_5$symboling<-as.factor(cardata_5$symboling)

levels(cardata_5$symboling)[1:2] <- "low"
levels(cardata_5$symboling)[2:3] <- "mid"
levels(cardata_5$symboling)[3:4] <- "high"


#Converting "symboling" into dummies . 
dummy_symboling <- data.frame(model.matrix( ~symboling, data = cardata_5))

#check the dummy_1 data frame.
View(dummy_symboling)

#This column should be removed from the newly created dummy_symboling dataframe containing the dummy values for the variable "symboling". 
dummy_symboling <- dummy_symboling[,-1]

cardata_6 <- cbind(cardata_5[,-2], dummy_symboling)

View(cardata_6)


################Data Correction #################

#### Correction on Car name
#(There are few car CarName names which are duplicate or names are not correct)
levels(as.factor(cardata_6$CarName))
#from maxda to mazda
#from Nissan to nissan
#from porcshce to porsche
#from toyouta to toyota
#from vokswagen to volkswagen
#from vw to volkswagen
#from alfa-romero to alfa-romeo

cardata_6$CarName<-ifelse(cardata_6$CarName=="maxda", "mazda", 
                               ifelse(cardata_6$CarName=="Nissan", "nissan", 
                                      ifelse(cardata_6$CarName=="porcshce", "porsche", 
                                             ifelse(cardata_6$CarName=="toyouta", "toyota", 
                                                    ifelse(cardata_6$CarName=="vokswagen", "volkswagen", 
                                                           ifelse(cardata_6$CarName=="vw", "volkswagen",
                                                                  ifelse(cardata_6$CarName=="alfa-romero", "alfa-romeo",
                                                                         cardata_6$CarName)))))))

#Let us see the structure of variable "CarName" and create dummy varibales
str(cardata_6$CarName)
summary(factor(cardata_6$CarName))
#Converting "CarName" into dummies . 
dummy_1 <- data.frame(model.matrix( ~CarName, data = cardata_6))
#check the dummy_1 data frame.
View(dummy_1)
#This column should be removed from the newly created dummy_1 dataframe containing the dummy values for the variable "CarName". 
dummy_1 <- dummy_1[,-1]
# Combine the dummy variables to the main data set, after removing the original categorical "CarName" column
cardata_6 <- cbind(cardata_6[,-3], dummy_1)
View(cardata_6)

#### Removing car_ID and car Modelname from data frame.
# car_ID variable is identity variable and will not have impact on car price so removing from DF
# Scatter plot used check the relation with car_ID and price
scatter.smooth(x=cardata_6$car_ID, y=cardata_6$price, main="Price ~ car_ID")
cardata_6 <- cardata_6[,-1]



##### Outlier checks ######

# On wheelbase (Outliers) 
#Using Box plot for identiying outliers
boxplot(cardata_6$wheelbase, main="Wheel base", sub=paste("Outlier rows: ", boxplot.stats(cardata_6$wheelbase )$out)) 
quantile(cardata_6$wheelbase, seq(0,1,0.01))

# Note that there is a jump on 100% and hence caping values above 115.544 i.e. 120.900 is 100%.
cardata_6$wheelbase[which(cardata_6$wheelbase>115.544)]<-115.544

# On carlength(no Outliers) 
#Using Box plot for identiying outliers
boxplot(cardata_6$carlength, main="car length", sub=paste("Outlier rows: ", boxplot.stats(cardata_6$carlength )$out)) 
quantile(cardata_6$carlength, seq(0,1,0.01))


# On carwidth(no Outliers) 
#Using Box plot for identiying outliers
boxplot(cardata_6$carwidth, main="car width", sub=paste("Outlier rows: ", boxplot.stats(cardata_6$carwidth )$out)) 
quantile(cardata_6$carwidth, seq(0,1,0.01))

# On carheight(no Outliers) 
#Using Box plot for identiying outliers
boxplot(cardata_6$carheight, main="car height", sub=paste("Outlier rows: ", boxplot.stats(cardata_6$carheight )$out)) 
quantile(cardata_6$carheight, seq(0,1,0.01))

# On curbweight(Outliers) 
#Using Box plot for identiying outliers
boxplot(cardata_6$curbweight, main="car weight", sub=paste("Outlier rows: ", boxplot.stats(cardata_6$curbweight )$out)) 
quantile(cardata_6$curbweight, seq(0,1,0.01))

# On engine size(Outliers) 
#Using Box plot for identiying outliers
boxplot(cardata_6$enginesize, main = "engine size ", sub=paste("Outlier rows: ", boxplot.stats(cardata_6$enginesize )$out)) 
quantile(cardata_6$enginesize, seq(0,1,0.01))

#there is a jump between 32 to 33 % and 49 to 50% and 93 to 94 %. from this we can consider caping values above 95% as floor value

cardata_6$enginesize[which(cardata_6$enginesize>201.20)]<-201.20

# On boreratio(Outliers) 
#Using Box plot for identiying outliers
boxplot(cardata_6$boreratio, main="bore ratio", sub=paste("Outlier rows: ", boxplot.stats(cardata_6$boreratio )$out)) 
quantile(cardata_6$boreratio, seq(0,1,0.01))


# On stroke(Outliers) 
#Using Box plot for identiying outliers
boxplot(cardata_6$stroke, main="stroke", sub=paste("Outlier rows: ", boxplot.stats(cardata_6$stroke )$out)) 
quantile(cardata_6$stroke, seq(0,1,0.01))
#There is jump between 0 to 1%

# On compressionratio(Outliers) 
#Using Box plot for identiying outliers
boxplot(cardata_6$compressionratio, main = "compression ratio ", sub=paste("Outlier rows: ", boxplot.stats(cardata_6$compressionratio )$out)) 
quantile(cardata_6$compressionratio, seq(0,1,0.01))


# On horsepower(Outliers) 
#Using Box plot for identiying outliers
boxplot(cardata_6$horsepower, main="horse power", sub=paste("Outlier rows: ", boxplot.stats(cardata_6$horsepower)$out)) 
quantile(cardata_6$horsepower, seq(0,1,0.01))
#There is jump at 97 to 98 % hence caping above 97%

cardata_6$horsepower[which(cardata_6$horsepower>184.00)]<-184.00


# On peakrpm (no outliers)
boxplot(cardata_6$peakrpm, main="peakrpm", sub=paste("Outlier rows: ", boxplot.stats(cardata_6$peakrpm)$out)) 
quantile(cardata_6$peakrpm,seq(0,1,0.01))

# On citympg(outliers)
boxplot(cardata_6$citympg, main="citympg", sub=paste("Outlier rows: ", boxplot.stats(cardata_6$citympg)$out)) 
quantile(cardata_6$citympg,seq(0,1,0.01))

#Note that there is a jump on 99% and 100% Therefore, we cap all values above 38.0 (98%) to 38.0. 
cardata_6$citympg[which(cardata_6$citympg>38.0)]<-38.0

# On highwaympg(no outliers)
boxplot(cardata_6$highwaympg, main="highwaympg", sub=paste("Outlier rows: ", boxplot.stats(cardata_6$highwaympg)$out)) 
quantile(cardata_6$highwaympg,seq(0,1,0.01))

## Outliers Part completed

cardata_6 <- cardata_6[,-1]
# Modelling Part

cardata_Clean <- cardata_6

#Structure of the DF
str(cardata_Clean)


#Exporting the dataset
write.csv(cardata_Clean,"C:\\data.csv")

## Data cleaning and Converting to numeric part completed
#Now, Building the model for cardata_Clean data set

#creating test and train data set
set.seed(100)

#Generating the row indices for Data set
# Teh Data set can be divided into Train and Test Data set for Building the model
trainindices= sample(1:nrow(cardata_Clean), 0.7*nrow(cardata_Clean))

#Generating the Train_Data set
train_Dataset = cardata_Clean[trainindices,]

#Storing the observations into Test Data set
test_Dataset = cardata_Clean[-trainindices,]

#Execute the first model_1 multilinear model in the training set. 
Car_Model <-lm(price~.,data=train_Dataset)

summary(Car_Model)

#For using below linear Regression model beow are the observations
#Multiple R-squared:  0.9782,	Adjusted R-squared:  0.9626 
#F-statistic: 63.03 on 59 and 83 DF,  p-value: < 2.2e-16


#MASS Package is required of stepAIC
step <- stepAIC(Car_Model, direction="both")

#Checking the coefficients of the variable

Car_Model_1 <- lm(formula =price ~ fueltype + aspiration + enginelocation + wheelbase + 
                         carwidth + curbweight + stroke + compressionratio + carbodyhardtop + 
                         carbodyhatchback + carbodysedan + carbodywagon + enginetypel + 
                         enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberfour + 
                         cylindernumbersix + CarNamebmw + CarNamebuick + CarNamedodge + 
                        CarNamehonda + CarNameisuzu + CarNamejaguar + CarNamemazda + 
                    CarNamemercury + CarNamemitsubishi + CarNamenissan + CarNameplymouth + 
                    CarNamerenault + CarNamesaab + CarNametoyota + CarNamevolkswagen + 
                    CarNamevolvo + enginetypeohc, data = train_Dataset)
summary(Car_Model_1)

#Applied linear Regression
#Multiple R-squared:  0.9764,	Adjusted R-squared:  0.9687 
#F-statistic: 126.7 on 35 and 107 DF,  p-value: < 2.2e-16

##Appliying the VIF function to check multicollinearity of the independent variables and remove 
##the variables with VIF>2

vif(Car_Model_1)


#Removing compressionratio based on higher vif and insignificant p value

Car_Model_2 <- lm(formula =price ~ fueltype + aspiration + enginelocation + wheelbase + 
                         carwidth + curbweight + stroke  + carbodyhardtop + 
                         carbodyhatchback + carbodysedan + carbodywagon + enginetypel + 
                         enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberfour + 
                         cylindernumbersix + CarNamebmw + CarNamebuick + CarNamedodge + 
                         CarNamehonda + CarNameisuzu + CarNamejaguar + CarNamemazda + 
                         CarNamemercury + CarNamemitsubishi + CarNamenissan + CarNameplymouth + 
                         CarNamerenault + CarNamesaab + CarNametoyota + CarNamevolkswagen + 
                         CarNamevolvo + enginetypeohc, data = train_Dataset)
summary(Car_Model_2)


#Applying the Linear Regression for Model 2
#Multiple R-squared:  0.9756,	Adjusted R-squared:  0.9679 
#F-statistic: 127.1 on 34 and 108 DF,  p-value: < 2.2e-16

vif(Car_Model_2)

#Removing the fuel type
Car_Model_3 <- lm(formula =price ~  aspiration + enginelocation + wheelbase + 
                         carwidth + curbweight + stroke  + carbodyhardtop + 
                         carbodyhatchback + carbodysedan + carbodywagon + enginetypel + 
                         enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberfour + 
                         cylindernumbersix + CarNamebmw + CarNamebuick + CarNamedodge + 
                         CarNamehonda + CarNameisuzu + CarNamejaguar + CarNamemazda + 
                         CarNamemercury + CarNamemitsubishi + CarNamenissan + CarNameplymouth + 
                         CarNamerenault + CarNamesaab + CarNametoyota + CarNamevolkswagen + 
                         CarNamevolvo + enginetypeohc, data = train_Dataset)
summary(Car_Model_3)


#Multiple R-squared:  0.9753,	Adjusted R-squared:  0.9678 
#F-statistic: 130.2 on 33 and 109 DF,  p-value: < 2.2e-16

vif(Car_Model_3)

#Applying the Linear Regression
#Removing stroke
Car_Model_4 <-lm(formula =price ~  aspiration + enginelocation + wheelbase + 
                        carwidth + curbweight   + carbodyhardtop + 
                        carbodyhatchback + carbodysedan + carbodywagon + enginetypel + 
                        enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberfour + 
                        cylindernumbersix + CarNamebmw + CarNamebuick + CarNamedodge + 
                        CarNamehonda + CarNameisuzu + CarNamejaguar + CarNamemazda + 
                        CarNamemercury + CarNamemitsubishi + CarNamenissan + CarNameplymouth + 
                        CarNamerenault + CarNamesaab + CarNametoyota + CarNamevolkswagen + 
                      CarNamevolvo + enginetypeohc, data = train_Dataset)
summary(Car_Model_4)


#Multiple R-squared:  0.9748,	Adjusted R-squared:  0.9675 
#F-statistic:   133 on 32 and 110 DF,  p-value: < 2.2e-16

vif(Car_Model_4)

#Applying thr Liner Regression 
#Removing CarNamesaab
Car_Model_5 <- lm(formula =price ~  aspiration + enginelocation + wheelbase + 
                         carwidth + curbweight   + carbodyhardtop + 
                         carbodyhatchback + carbodysedan + carbodywagon + enginetypel + 
                         enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberfour + 
                         cylindernumbersix + CarNamebmw + CarNamebuick + CarNamedodge + 
                         CarNamehonda + CarNameisuzu + CarNamejaguar + CarNamemazda + 
                         CarNamemercury + CarNamemitsubishi + CarNamenissan + CarNameplymouth + 
                         CarNamerenault  + CarNametoyota + CarNamevolkswagen + 
                         CarNamevolvo + enginetypeohc, data = train_Dataset)
summary(Car_Model_5)


#Multiple R-squared:  0.9746,	Adjusted R-squared:  0.9675 
#F-statistic: 137.4 on 31 and 111 DF,  p-value: < 2.2e-16

vif(Car_Model_5)

#Applying thr Liner Regression 
#Removing CarNamehonda
Car_Model_6 <- lm(formula =price ~  aspiration + enginelocation + wheelbase + 
                         carwidth + curbweight   + carbodyhardtop + 
                         carbodyhatchback + carbodysedan + carbodywagon + enginetypel + 
                         enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberfour + 
                         cylindernumbersix + CarNamebmw + CarNamebuick + CarNamedodge + 
                         CarNameisuzu + CarNamejaguar + CarNamemazda + 
                         CarNamemercury + CarNamemitsubishi + CarNamenissan + CarNameplymouth + 
                         CarNamerenault  + CarNametoyota + CarNamevolkswagen + 
                         CarNamevolvo + enginetypeohc, data = train_Dataset)
summary(Car_Model_6)

#Multiple R-squared:  0.9738,	Adjusted R-squared:  0.9667 
#F-statistic: 138.5 on 30 and 112 DF,  p-value: < 2.2e-16

vif(Car_Model_6)

#Applying the Linear Regression
#Removing CarNameisuzu
Car_Model_7 <- lm(formula =price ~  aspiration + enginelocation + wheelbase + 
                         carwidth + curbweight   + carbodyhardtop + 
                         carbodyhatchback + carbodysedan + carbodywagon + enginetypel + 
                         enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberfour + 
                         cylindernumbersix + CarNamebmw + CarNamebuick + CarNamedodge + 
                         CarNamejaguar + CarNamemazda + 
                         CarNamemercury + CarNamemitsubishi + CarNamenissan + CarNameplymouth + 
                         CarNamerenault  + CarNametoyota + CarNamevolkswagen + 
                         CarNamevolvo + enginetypeohc, data = train_Dataset)
summary(Car_Model_7)

#Multiple R-squared:  0.9736,	Adjusted R-squared:  0.9668 
#F-statistic: 143.8 on 29 and 113 DF,  p-value: < 2.2e-16

vif(Car_Model_7)


#Applying the Linear Regression
#Removing CarNamevolvo
Car_Model_8 <- lm(formula =price ~  aspiration + enginelocation + wheelbase + 
                         carwidth + curbweight   + carbodyhardtop + 
                         carbodyhatchback + carbodysedan + carbodywagon + enginetypel + 
                         enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberfour + 
                         cylindernumbersix + CarNamebmw + CarNamebuick + CarNamedodge + 
                         CarNamejaguar + CarNamemazda + 
                         CarNamemercury + CarNamemitsubishi + CarNamenissan + CarNameplymouth + 
                         CarNamerenault  + CarNametoyota + CarNamevolkswagen + 
                         enginetypeohc, data = train_Dataset)
summary(Car_Model_8)

#Multiple R-squared:  0.9736,	Adjusted R-squared:  0.9671 
#F-statistic: 150.1 on 28 and 114 DF,  p-value: < 2.2e-16

vif(Car_Model_8)


#Applying the linear Regression
#Removing CarNamenissan
Car_Model_9 <- lm(formula =price ~  aspiration + enginelocation + wheelbase + 
                          carwidth + curbweight   + carbodyhardtop + 
                          carbodyhatchback + carbodysedan + carbodywagon + enginetypel + 
                          enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberfour + 
                          cylindernumbersix + CarNamebmw + CarNamebuick + CarNamedodge + 
                          CarNamejaguar + CarNamemazda + 
                          CarNamemercury + CarNamemitsubishi   + CarNameplymouth + 
                          CarNamerenault  + CarNametoyota + CarNamevolkswagen + 
                          enginetypeohc, data = train_Dataset)
summary(Car_Model_9)
#Multiple R-squared:  0.9732,	Adjusted R-squared:  0.9669 
#F-statistic: 154.5 on 27 and 115 DF,  p-value: < 2.2e-16


vif(Car_Model_9)

#Applying the Linear Regression
#Removing CarNamemercury
Car_Model_10 <- lm(formula =price ~  aspiration + enginelocation + wheelbase + 
                          carwidth + curbweight   + carbodyhardtop + 
                          carbodyhatchback + carbodysedan + carbodywagon + enginetypel + 
                          enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberfour + 
                          cylindernumbersix + CarNamebmw + CarNamebuick + CarNamedodge + 
                          CarNamejaguar + CarNamemazda + 
                          CarNamemitsubishi   + CarNameplymouth + 
                          CarNamerenault  + CarNametoyota + CarNamevolkswagen + 
                          enginetypeohc, data = train_Dataset)
summary(Car_Model_10)
#Multiple R-squared:  0.973,	Adjusted R-squared:  0.9669 
#F-statistic: 160.6 on 26 and 116 DF,  p-value: < 2.2e-16

vif(Car_Model_10)

#Aapplying the Linear Regression
#Removing carbodyhardtop
Car_Model_11 <- lm(formula =price ~  aspiration + enginelocation + wheelbase + 
                          carwidth + curbweight    + 
                          carbodyhatchback + carbodysedan + carbodywagon + enginetypel + 
                          enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberfour + 
                          cylindernumbersix + CarNamebmw + CarNamebuick + CarNamedodge + 
                          CarNamejaguar + CarNamemazda + 
                          CarNamemitsubishi   + CarNameplymouth + 
                          CarNamerenault  + CarNametoyota + CarNamevolkswagen + 
                          enginetypeohc, data = train_Dataset)
summary(Car_Model_11)
#Multiple R-squared:  0.9717,	Adjusted R-squared:  0.9656 
#F-statistic: 160.5 on 25 and 117 DF,  p-value: < 2.2e-16

vif(Car_Model_11)

#Applying the Linear Regression
#Removing wheelbase
Car_Model_12 <- lm(formula =price ~  aspiration + enginelocation  + 
                          carwidth + curbweight    + 
                          carbodyhatchback + carbodysedan + carbodywagon + enginetypel + 
                          enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberfour + 
                          cylindernumbersix + CarNamebmw + CarNamebuick + CarNamedodge + 
                          CarNamejaguar + CarNamemazda + 
                          CarNamemitsubishi   + CarNameplymouth + 
                          CarNamerenault  + CarNametoyota + CarNamevolkswagen + 
                          enginetypeohc, data = train_Dataset)
summary(Car_Model_12)

#Multiple R-squared:  0.9704,	Adjusted R-squared:  0.9644 
#F-statistic: 161.3 on 24 and 118 DF,  p-value: < 2.2e-16

vif(Car_Model_12)

#Applying the Linear Regression
#Removing CarNamedodge
Car_Model_13 <- lm(formula =price ~  aspiration + enginelocation  + 
                          carwidth + curbweight    + 
                          carbodyhatchback + carbodysedan + carbodywagon + enginetypel + 
                          enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberfour + 
                          cylindernumbersix + CarNamebmw + CarNamebuick  + 
                          CarNamejaguar + CarNamemazda + 
                          CarNamemitsubishi   + CarNameplymouth + 
                          CarNamerenault  + CarNametoyota + CarNamevolkswagen + 
                          enginetypeohc, data = train_Dataset)
summary(Car_Model_13)

#Multiple R-squared:  0.9696,	Adjusted R-squared:  0.9637 
#F-statistic: 164.8 on 23 and 119 DF,  p-value: < 2.2e-16

vif(Car_Model_13)

#Applying the Linear Regression
#Removing CarNameplymouth
Car_Model_14 <- lm(formula =price ~  aspiration + enginelocation  + curbweight+
                          carwidth +carbodyhatchback + carbodysedan + carbodywagon + enginetypel + 
                          enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberfour + 
                          cylindernumbersix + CarNamebmw + CarNamebuick  + 
                          CarNamejaguar + CarNamemazda +  CarNamemitsubishi + 
                          CarNamerenault  + CarNametoyota + CarNamevolkswagen + 
                          enginetypeohc, data = train_Dataset)
summary(Car_Model_14)

#Multiple R-squared:  0.9688,	Adjusted R-squared:  0.9631 
#F-statistic: 169.3 on 22 and 120 DF,  p-value: < 2.2e-16

vif(Car_Model_14)

#Applying the Linear Regression
#Removing carbodysedan
Car_Model_15 <- lm(formula =price ~  aspiration + enginelocation  + curbweight+
                          carwidth +carbodyhatchback  + carbodywagon + enginetypel + 
                          enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberfour + 
                          cylindernumbersix + CarNamebmw + CarNamebuick  + 
                          CarNamejaguar + CarNamemazda +  CarNamemitsubishi + 
                          CarNamerenault  + CarNametoyota + CarNamevolkswagen + 
                          enginetypeohc, data = train_Dataset)
summary(Car_Model_15)

#Multiple R-squared:  0.9682,	Adjusted R-squared:  0.9627

vif(Car_Model_15)


#Applying the Linear Regression
#Removing carbodyhatchback
Car_Model_16 <- lm(formula =price ~  aspiration + enginelocation  + curbweight+
                          carwidth   + carbodywagon + enginetypel + 
                          enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberfour + 
                          cylindernumbersix + CarNamebmw + CarNamebuick  + 
                          CarNamejaguar + CarNamemazda +  CarNamemitsubishi + 
                          CarNamerenault  + CarNametoyota + CarNamevolkswagen + 
                          enginetypeohc, data = train_Dataset)
summary(Car_Model_16)

#R-squared:  0.9676,	Adjusted R-squared:  0.9622
vif(Car_Model_16)

#Applying the Linear Regression
#Removing aspiration
Car_Model_17 <- lm(formula =price ~   enginelocation  + curbweight+
                          carwidth   + carbodywagon + enginetypel + 
                          enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberfour + 
                          cylindernumbersix + CarNamebmw + CarNamebuick  + 
                          CarNamejaguar + CarNamemazda +  CarNamemitsubishi + CarNamevolkswagen+
                          CarNamerenault  + CarNametoyota  + 
                          enginetypeohc, data = train_Dataset)
summary(Car_Model_17)
#R-squared:  0.9667,	Adjusted R-squared:  0.9616 
vif(Car_Model_17)


#Applying the Linear Regression
#Removing CarNamemazda
Car_Model_18 <- lm(formula =price ~   enginelocation  + curbweight+
                          carwidth   + carbodywagon + enginetypel + 
                          enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberfour + 
                          cylindernumbersix + CarNamebmw + CarNamebuick  + 
                          CarNamejaguar  +  CarNamemitsubishi + CarNamevolkswagen+
                          CarNamerenault  + CarNametoyota  +
                          enginetypeohc, data = train_Dataset)
summary(Car_Model_18)
#R-squared:  0.9651,	Adjusted R-squared:  0.9601

vif(Car_Model_18)

#Applying the Linear Regression
#Removing CarNamevolkswagen
Car_Model_19 <- lm(formula =price ~   enginelocation  + curbweight+
                          carwidth   + carbodywagon + enginetypel + 
                          enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberfour + 
                          cylindernumbersix + CarNamebmw + CarNamebuick  + 
                          CarNamejaguar  +  CarNamemitsubishi + 
                          CarNamerenault  + CarNametoyota  + 
                          enginetypeohc, data = train_Dataset)
summary(Car_Model_19)
#R-squared:   0.9643,	Adjusted R-squared:  0.9595 
vif(Car_Model_19)

#Applying the Linear Regression
#Removing carbodywagon
Car_Model_20 <- lm(formula =price ~   enginelocation  + curbweight+
                          carwidth    + enginetypel + 
                          enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberfour + 
                          cylindernumbersix + CarNamebmw + CarNamebuick  + 
                          CarNamejaguar  +  CarNamemitsubishi + 
                          CarNamerenault  + CarNametoyota  + 
                          enginetypeohc, data = train_Dataset)
summary(Car_Model_20)
#R-squared:  0.9626,	Adjusted R-squared:  0.9579 
vif(Car_Model_20)

#Applying the Linear Regression
#Removing CarNamerenault
Car_Model_21 <- lm(formula =price ~   enginelocation  + curbweight+
                          carwidth    + enginetypel + 
                          enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberfour + 
                          cylindernumbersix + CarNamebmw + CarNamebuick  + 
                          CarNamejaguar  +  CarNamemitsubishi + 
                          CarNametoyota  + 
                          enginetypeohc, data = train_Dataset)
summary(Car_Model_21)
#R-squared:  0.9609,	Adjusted R-squared:  0.9562
vif(Car_Model_21)

#Applying the Linear Regression
#Removing CarNamemitsubishi
Car_Model_22 <- lm(formula =price ~   enginelocation  + curbweight+
                          carwidth    + enginetypel + 
                          enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberfour + 
                          cylindernumbersix + CarNamebmw + CarNamebuick  + 
                          CarNamejaguar  +   
                          CarNametoyota  + 
                          enginetypeohc, data = train_Dataset)
summary(Car_Model_22)
#R-squared:  0.9594,	Adjusted R-squared:  0.955
vif(Car_Model_22)

#Applying the Linear Regression
#Removing CarNametoyota
Car_Model_23 <- lm(formula =price ~   enginelocation  + curbweight+
                          carwidth    + enginetypel + 
                          enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberfour + 
                          cylindernumbersix + CarNamebmw + CarNamebuick  + 
                          CarNamejaguar  +enginetypeohc, data = train_Dataset)
summary(Car_Model_23)
#R-squared:  0.9565,	Adjusted R-squared:  0.9521 
vif(Car_Model_23)


#Applying the Linear Regression
#Removing carwidth
Car_Model_24 <- lm(formula =price ~   enginelocation  +curbweight    + enginetypel + 
                          enginetypeohcf + enginetyperotor + cylindernumberfive  + cylindernumberfour+
                          cylindernumbersix + CarNamebmw + CarNamebuick  + 
                          CarNamejaguar  +enginetypeohc, data = train_Dataset)
summary(Car_Model_24)
#R-squared:  0.9428,	Adjusted R-squared:  0.9375
vif(Car_Model_24)

#Applying the Linear Regression
#Removing enginetypeohc
Car_Model_25 <- lm(formula =price ~   enginelocation  +curbweight    + enginetypel + 
                          enginetypeohcf + enginetyperotor + cylindernumberfive  + cylindernumberfour+
                          cylindernumbersix + CarNamebmw + CarNamebuick  + 
                          CarNamejaguar  , data = train_Dataset)
summary(Car_Model_25)
#R-squared:  0.9396,	Adjusted R-squared:  0.9345
vif(Car_Model_25)


#Applying the Linear Regression
#Removing enginetypeohcf
Car_Model_26 <- lm(formula =price ~   enginelocation  +curbweight    + enginetypel + 
                          enginetyperotor + cylindernumberfive  + cylindernumberfour+
                          cylindernumbersix + CarNamebmw + CarNamebuick  + 
                          CarNamejaguar  , data = train_Dataset)
summary(Car_Model_26)
#R-squared:  0.939,	Adjusted R-squared:  0.9344
vif(Car_Model_26)


#Applying the Linear Regression
#Removing enginetyperotor
Car_Model_27 <- lm(formula =price ~   enginelocation  +curbweight    + enginetypel + 
                          cylindernumberfive  +cylindernumberfour+
                          cylindernumbersix + CarNamebmw + CarNamebuick  + 
                          CarNamejaguar  , data = train_Dataset)
summary(Car_Model_27)
#R-squared:  0.9257,	Adjusted R-squared:  0.9207
vif(Car_Model_27)

#Applying the Linear Regression
#Removing CarNamejaguar
Car_Model_28 <- lm(formula =price ~   enginelocation  +curbweight    + enginetypel + 
                          cylindernumberfive  +cylindernumberfour+
                          cylindernumbersix + CarNamebmw + CarNamebuick  
                        , data = train_Dataset)
summary(Car_Model_28)
#R-squared:    0.9199,	Adjusted R-squared:  0.9151
vif(Car_Model_28)


#Applying the Linear Regression
#Removing cylindernumbersix
Car_Model_29 <- lm(formula =price ~   enginelocation+curbweight   + enginetypel+
                          cylindernumberfour+cylindernumberfive+
                          CarNamebmw + CarNamebuick  
                        , data = train_Dataset)
summary(Car_Model_29)
#R-squared:  0.9104,	Adjusted R-squared:  0.9057 
vif(Car_Model_29)

#Applying the Linear Regression
#Removing enginetypel
Car_Model_30 <- lm(formula =price ~   enginelocation+curbweight+
                          cylindernumberfour+cylindernumberfive+
                          CarNamebmw + CarNamebuick  
                        , data = train_Dataset)
summary(Car_Model_30)
# R-squared:  0.9045,	Adjusted R-squared:  0.9003 
vif(Car_Model_30)

#Applying the Linear Regression
#Removing cylindernumberfive
Car_Model_31 <- lm(formula =price ~   enginelocation+curbweight+
                          cylindernumberfour+
                          CarNamebmw + CarNamebuick  
                        , data = train_Dataset)
summary(Car_Model_31)
# R-squared:  0.8964,	Adjusted R-squared:  0.8927

vif(Car_Model_31)



# Predict the house prices in the testing dataset with Car_Model_31
Predict_1 <- predict(Car_Model_31,test_Dataset[,-10])
test_Dataset$test_price <- Predict_1

# Accuracy of the predictions
# Calculate correlation
r <- cor(test_Dataset$price,test_Dataset$test_price)
# calculate R squared by squaring correlation
rsquared <- cor(test_Dataset$price,test_Dataset$test_price)^2

# check R-squared
rsquared

# Predict the house prices in the testing dataset with carPrice_Model_31
Predict_1 <- predict(Car_Model_31,test_Dataset[,-1])
test_Dataset$test_price <- Predict_1

# Accuracy of the predictions
# Calculate correlation
r <- cor(test_Dataset$price,test_Dataset$test_price)
# calculate R squared by squaring correlation
rsquared <- cor(test_Dataset$price,test_Dataset$test_price)^2

# check R-squared
rsquared


# Predict the house prices in the testing dataset with Car_Model_30
Predict_1 <- predict(Car_Model_30,test_Dataset[,-1])
test_Dataset$test_price <- Predict_1

# Accuracy of the predictions
# Calculate correlation
r <- cor(test_Dataset$price,test_Dataset$test_price)
# calculate R squared by squaring correlation
rsquared <- cor(test_Dataset$price,test_Dataset$test_price)^2

# check R-squared
rsquared

#Final - Model
#Final variable- enginelocation+curbweight+cylindernumberfour+CarNamebmw + CarNamebuick
##Evaluating the model with the dataset####

Car_ModelTest <- lm(formula =price ~   enginelocation+curbweight+cylindernumberfour+CarNamebmw + CarNamebuick  
                          , data = cardata_Clean)
par(mfrow=c(2,2))
plot(Car_ModelTest)

#Need to Residuals vs Leverage as there are outliers
#checking outliers on price

#Using Box plot for identiying outliers
boxplot(cardata_Clean$price, main="Price", sub=paste("Outlier rows: ", boxplot.stats(cardata_Clean$price)$out))
quantile(cardata_Clean$price, seq(0,1,0.01))

#Removing 17 and 70 from df
cardata_Clean <- cardata_Clean[-c(17, 70), ] 

#checking the plot for retesting

Car_ModelTest <- lm(formula =price ~   enginelocation+curbweight+cylindernumberfour+CarNamebmw + CarNamebuick  
                          , data = cardata_Clean)
par(mfrow=c(2,2))
plot(Car_ModelTest)


#Re -creating the test data as the graph looks fine

set.seed(100)
trainindices= sample(1:nrow(cardata_Clean), 0.7*nrow(cardata_Clean))
train_Dataset1 = cardata_Clean[trainindices,]
test_DataSet1 = cardata_Clean[-trainindices,]

Car_ModelFinal <- lm(formula = price ~   enginelocation+curbweight+cylindernumberfour+CarNamebmw + CarNamebuick, 
                           data = train_Dataset1)

summary(Car_ModelFinal)

Predict_1 <- predict(Car_ModelFinal,test_DataSet1[,-1])
test_DataSet1$test_price <- Predict_1

# Accuracy of the predictions
# Calculate correlation
r <- cor(test_DataSet1$price,test_DataSet1$test_price)
# calculate R squared by squaring correlation
rsquared <- cor(test_DataSet1$price,test_DataSet1$test_price)^2

# check R-squared
rsquared

#Final Model- R-squared:  0.8962,	Adjusted R-squared:  0.8924
#test RSq- 0.832
# Here deviation is more than (~5%) so  it is always a subjective call to take
