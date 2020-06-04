dir <- "/Users/Jeylan/Documents/South-Korea-Covid-Project/"

setwd(dir)

#reading in table

data <- read.csv('Data/PatientInfo.csv')


#import libraries
library(ggplot2)
library(car)
library(dplyr)
library(pdp)
library(randomForest)
#Description of Data Set
#Available online: https://www.kaggle.com/kimjihoo/coronavirusdataset
#Data Science for COVID-19 provides patient data from more than 10,000 infected individuals in South Korea.
#The data is based on the report materials of  KCDC (Korea Centers for Disease Control & Prevention) 
#and local governments  published online. I analyze and visualize the data to predict whether a patient becomes
#isolated, released or deceased. This exercise would be particularly useful for a doctor developing a forecasting
#tool for when a patient arrives at a hospital. One can forecast the outcome in order to
# provide the best medical care while optimizing medical resources (i.e. an ICU  bed)


str(data)
#Rename column headings 
####
dim(data)
head(data)
summary(data)

datanew <- data  #creating  a duplicate dataset to work with

sapply(data, class)  ## Variables are already factor

sapply(data, function(x) {sum(is.na(x))}) ## Exploring missings


#Data cleaning

##Dependent variable:  State
summary(data$state)


##Variable: Sex 
summary(data$sex)

data$sex[data$sex ==''] <- NA #Replacing missings
NA_sex <- which(is.na(data$sex)) #identifying the NAs
data <- data[-NA_sex,] #removing the NAs
data$sex <- droplevels(data$sex) #removing empty factors

summary(data$sex)

##Variable: Age 
summary(data$age)  # 10 missing ages 

#Using age2 to correct for missing ages 
data$age2 <- 2020 - data$birth_year
  

data$cohort <- ifelse(data$age2 >= 0 & data$age2<10, 5, 
                      ifelse(data$age2 >= 10 & data$age2<20, 15, 
                             ifelse(data$age2 >= 20 & data$age2< 30, 25,             
                                    ifelse(data$age2 >= 30 & data$age2< 40,  35,
                                           ifelse(data$age2 >= 40 & data$age2< 50,  45,
                                                  ifelse(data$age2 >= 50 & data$age2< 60,  55, 
                                                         ifelse(data$age2 >= 60 & data$age2< 70,  65,  
                                                                ifelse(data$age2 >= 70 & data$age2< 80,  75, 
                                                                       ifelse(data$age2 >= 80 & data$age2< 90,  85,  
                                                                              ifelse(data$age2 >= 90 & data$age2< 100,  95,  
                                                                                     ifelse(data$age2 >= 100,  100, NA))))))))))) 

#data$age3 <- as.character(recode(data$age,"'0s'=5,'10s'=15,'20s'=25,'30s'=35,'40s'=45,'50s'=55,'60s'=65, '66s'=65,'70s'=75, '80s'= 85, '90s'=95, '100s'=100, 'NA'=45"))

data$age3 <- ifelse(data$age == "0s", 5, 
                    ifelse(data$age == "10s", 15, 
                           ifelse(data$age == "20s", 25,             
                                  ifelse(data$age == "30s",  35,
                                         ifelse(data$age == "40s",  45,
                                                ifelse(data$age == "50s",  55, 
                                                       ifelse(data$age == "60s",  65,  
                                                              ifelse(data$age == "70s",  75, 
                                                                     ifelse(data$age == "80s",  85,  
                                                                            ifelse(data$age == "90s",  95,  
                                                                                   ifelse(data$age == "100s",  100, NA))))))))))) 

##Replacing missing ages?
data$age3 <- ifelse(is.na(data$age3), data$cohort, data$age3)  

NA_age3 <- which(is.na(data$age3)) #identifying the NAs
data <- data[-NA_age3,] #removing the NAs


summary(data$age3)


##Variable: Province 
summary(data$province)


##Variable: City 
summary(data$city)  #76 missings



##Variable: Infection case 
summary(data$infection_case)  #765 missings

data$infection_case[data$infection_case ==''] <- NA #Replacing missings

data$infection_case3 <- as.character(data$infection_case)

data$infection_case3[is.na(data$infection_case3)] <- "Unknown"
data$infection_case3 <- as.factor(data$infection_case3)

sapply(data, class)


##Variable: Confirmed_date 

data$confirmed_date2 <- as.Date(data$confirmed_date)

summary(data$confirmed_date2)

NA_confirmed_date2 <- which(is.na(data$confirmed_date2)) #identifying the NAs
data <- data[-NA_confirmed_date2,] #removing the NAs

summary(data$confirmed_date2)

sapply(data, class)

## Creating the final data set
data2<-data %>% dplyr::select(sex, age3, infection_case3, province, confirmed_date2, state)
sapply(data2, class)
summary(data2)

#Checking the levels of variables
str(data2$state)
str(data2$age3)
str(data2$sex)
str(data2$province)
str(data2$confirmed_date2)
str(data2$infection_case3)

#### DESCRIPTIVE STATISTICS ####

# State
ggplot(data2, aes(x = state)) + geom_bar() +   geom_bar(fill = "cornflowerblue",  color="black") + labs(x = "State",  y = "Frequency", title = "Patients by state")

#Age
ggplot(data2, aes(x = age3)) + geom_bar()  + geom_bar(fill = "cornflowerblue",  color="black") + labs(x = "Age",  y = "Frequency", title = "Patients by age")

#Sex
ggplot(data2, aes(x = sex)) + geom_bar()  + geom_bar(fill = "cornflowerblue",  color="black") + labs(x = "Sex",  y = "Frequency", title = "Patients by sex")

#Province
ggplot(data2, aes(x = province)) + geom_bar()  + geom_bar(fill = "cornflowerblue",  color="black") + labs(x = "Province",  y = "Frequency", title = "Patients by Province") + theme(axis.text.x = element_text(angle = 90))

#Infection case
ggplot(data2, aes(x = infection_case3)) + geom_bar()  + geom_bar(fill = "cornflowerblue",  color="black") + labs(x = "Infection case",  y = "Frequency", title = "Patients by infection case") + theme(axis.text.x = element_text(angle = 90))

#Confirmation date
ggplot(data2, aes(x = confirmed_date2)) + geom_bar()  + geom_bar(fill = "cornflowerblue",  color="black") + labs(x = "Confirmation date",  y = "Frequency", title = "Patients by confirmation date")


ggplot(data2, aes(fill=data$state, y=100, x=data$age3)) + 
  geom_bar(position="fill", stat="identity") + scale_fill_brewer(palette="GnBu") + scale_linetype_manual(guide_legend(reverse = TRUE))+ 
  ggtitle("Percentage distribution of patients across ages") + xlab("Age") + ylab("Percentage") + labs(fill = "State")

ggplot(data2, aes(fill=data$state, y=100, x=data$sex)) + 
  geom_bar(position="fill", stat="identity") + scale_fill_brewer(palette="GnBu") + scale_linetype_manual(guide_legend(reverse = TRUE))+ 
  ggtitle("Percentage distribution of patients by sex") + xlab("Sex") + ylab("Percentage") + labs(fill = "State")


levels(data2$state)[1:3]


#### RANDOM FORESTS ####
### TUNING COST RATIO ###
##Use 2/3 of the smallest number 71*2/3 = 47, method recommended by Richard Berk 
## It is much worse to incorrectly classify someone as isolated or released than as deceased so those cost ratios 
## should be higher than to incorrectly classify someone as isolated or released who is isolated or released
##Deceased vs. isolated: Target cost ratio 2:1
##Deceased vs. released: Target cost ratio 2:1
##Released vs. isolated: Target cost ratio 1:1 

set.seed(10) 

rf0 <- randomForest(state ~ sex + age3 + province + confirmed_date2 + infection_case3, data=data2, sampsize=c(47,47,47)) #increase the first number 
print(rf0)                                                                                                                                  #c(623,623) gives me a cost ratio of about 2 to 1

rf2 <- randomForest(state ~ sex + age3 + province + confirmed_date2 + infection_case3, data=data2, sampsize=c(47,100,100)) #increase the first number 
print(rf2)                                                                                                                                  #c(623,623) gives me a cost ratio of about 2 to 1
                                                                                                                       #c(623,623) gives me a cost ratio of about 2 to 1
rf7 <- randomForest(state ~ sex + age3 + province + confirmed_date2 + infection_case3, data=data2, sampsize=c(47,200,350)) #increase the first number 
print(rf7)                                                                                                                                  #c(623,623) gives me a cost ratio of about 2 to 1

rf8 <- randomForest(state ~ sex + age3 + province + confirmed_date2 + infection_case3, data=data2, sampsize=c(47,200,275)) #increase the first number 
print(rf8)                                                                                                                                  #c(623,623) gives me a cost ratio of about 2 to 1

#this is quite good 

rf9 <- randomForest(state ~ sex + age3 + province + confirmed_date2 + infection_case3, data=data2, sampsize=c(47,200,250)) #increase the first number 
print(rf9)              

rf10 <- randomForest(state ~ sex + age3 + province + confirmed_date2 + infection_case3, data=data2, sampsize=c(47,210,250)) #increase the first number 
print(rf10)              

# quite good

rf11 <- randomForest(state ~ sex + age3 + province + confirmed_date2 + infection_case3, data=data2, sampsize=c(47,210,253)) #increase the first number 
print(rf11)              

rf12 <- randomForest(state ~ sex + age3 + province + confirmed_date2 + infection_case3, data=data2, sampsize=c(47,210,252)) #increase the first number 
print(rf12)              

rf13 <- randomForest(state ~ sex + age3 + province + confirmed_date2 + infection_case3, data=data2, sampsize=c(47,211,252)) #increase the first number 
print(rf13)              

rf14 <- randomForest(state ~ sex + age3 + province + confirmed_date2 + infection_case3, data=data2, sampsize=c(47,209,252)) #increase the first number 
print(rf14)              

rf15 <- randomForest(state ~ sex + age3 + province + confirmed_date2 + infection_case3, data=data2, sampsize=c(47,209,251)) #increase the first number 
print(rf15)              

rf16 <- randomForest(state ~ sex + age3 + province + confirmed_date2 + infection_case3, data=data2, sampsize=c(47,209,250)) #increase the first number 
print(rf16)              

## Good

rf17 <- randomForest(state ~ sex + age3 + province + confirmed_date2 + infection_case3, data=data2, sampsize=c(47,209,249)) #increase the first number 
print(rf17)              

rf18 <- randomForest(state ~ sex + age3 + province + confirmed_date2 + infection_case3, data=data2, sampsize=c(47,208,250)) #increase the first number 
print(rf18)              
## Excellent 

rf19 <- randomForest(state ~ sex + age3 + province + confirmed_date2 + infection_case3, data=data2, sampsize=c(47,225,270)) #increase the first number 
print(rf9)                            

rf20 <- randomForest(state ~ sex + age3 + province + confirmed_date2 + infection_case3, data=data2, sampsize=c(47,225,265)) #increase the first number 
print(rf10)                            

rf21 <- randomForest(state ~ sex + age3 + province + confirmed_date2 + infection_case3, data=data2, sampsize=c(47,224,265)) #increase the first number 
print(rf11)                            

rf22 <- randomForest(state ~ sex + age3 + province + confirmed_date2 + infection_case3, data=data2, sampsize=c(47,223,266)) #increase the first number 
print(rf12)        

rf23 <- randomForest(state ~ sex + age3 + province + confirmed_date2 + infection_case3, data=data2, sampsize=c(47,223,265)) #increase the first number 
print(rf13)                            

rf24 <- randomForest(state ~ sex + age3 + province + confirmed_date2 + infection_case3, data=data2, sampsize=c(47,223,264)) #increase the first number 
print(rf14)          

rf25 <- randomForest(state ~ sex + age3 + province + confirmed_date2 + infection_case3, data=data2, sampsize=c(47,226,265)) #increase the first number 
print(rf15)                            

rf26 <- randomForest(state ~ sex + age3 + province + confirmed_date2 + infection_case3, data=data2, sampsize=c(47,223,267)) #increase the first number 
print(rf16)        

rf27 <- randomForest(state ~ sex + age3 + province + confirmed_date2 + infection_case3, data=data2, sampsize=c(47,222,267)) #increase the first number 
print(rf17)        

rf28 <- randomForest(state ~ sex + age3 + province + confirmed_date2 + infection_case3, data=data2, sampsize=c(47,224,267)) #increase the first number 
print(rf18)        

rf29<- randomForest(state ~ sex + age3 + province + confirmed_date2 + infection_case3, data=data2, sampsize=c(47,222,268), importance=T) #increase the first number 
print(rf19)        

##Best

rf30<- randomForest(state ~ sex + age3 + province + confirmed_date2 + infection_case3, data=data, sampsize=c(47,222,266)) #increase the first number 
print(rf20)        



#Variable Importance Plots
# Unstandardized Plots

par(mfrow=c(2,2))

varImpPlot(rf19,class="deceased", type=1,scale=F, main="Var. Imp. Plot: Deceased")

varImpPlot(rf19,class="isolated", type=1,scale=F, main="Var. Imp. Plot: Isolated")

varImpPlot(rf19,class="released", type=1,scale=F, main="Var. Imp. Plot: Released")



# PARTIAL PLOTS
par(mfrow=c(1,1))

#Age - Deceased
partialPlot(rf19, pred.data= data2, x.var = age3, rug = T, which.class = "deceased",
            main = "Partial Dependence Plot for Deceased on Age", xlab = "Age", ylab = "Centered Log Odds")

#Province - Deceased
partialPlot(rf19, pred.data= data2, x.var = province, rug = T, which.class = "deceased",
            main = "Partial Dependence Plot for Deceased on Province", xlab = "Province", ylab = "Centered Log Odds")


#Province - released
partialPlot(rf19, pred.data= data2, x.var = province, rug = T, which.class = "released",
                     main = "Partial Dependence Plot for Released on Province", xlab = "Province", ylab = "Centered Log Odds")

#Confirmed date - released
partialPlot(rf19, pred.data= data2, x.var = confirmed_date2, rug = T, which.class = "released",
            main = "Partial Dependence Plot for Released on Confirmation Date", xlab = "Confirmation date", ylab = "Centered Log Odds")

#Province - isolated
partialPlot(rf19, pred.data= data2, x.var = province, rug = T, which.class = "isolated",
            main = "Partial Dependence Plot for Released on Province", xlab = "Province", ylab = "Centered Log Odds")

#Confirmation date - isolated
partialPlot(rf19, pred.data= data2, x.var = confirmed_date2, rug = T, which.class = "isolated",
            main = "Partial Dependence Plot for Released on Confirmation Date", xlab = "Confirmation date", ylab = "Centered Log Odds")


##Using ggplot2
par(mfrow=c(1,1))

#Age - Deceased
partial(rf19, pred.var = "age3", plot= TRUE, prob = TRUE, type = "classification", which.class = "deceased", plot.engine = "ggplot2") + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.3))+ labs(y = "Predicted Probability", x  = "Age")

#Province - Deceased
partial(rf19, pred.var = "province", plot= TRUE, prob = TRUE, type = "classification", which.class = "deceased", plot.engine = "ggplot2") + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.3))+ labs(y = "Predicted Probability", x  = "Province")

#Province - released
partial(rf19, pred.var = "province", plot= TRUE, prob = TRUE, type = "classification", which.class = "released", plot.engine = "ggplot2") + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.3))+ labs(y = "Predicted Probability", x  = "Province")

#Confirmed date - released
partial(rf19, pred.var = "confirmed_date2", plot= TRUE, prob = TRUE, type = "classification", which.class = "released", plot.engine = "ggplot2") + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.3)) + labs(y = "Predicted Probability", x  = "Confirmation date")

#Province - isolated
partial(rf19, pred.var = "province", plot= TRUE, prob = TRUE, type = "classification", which.class = "isolated", plot.engine = "ggplot2") + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.3)) + labs(y = "Predicted Probability", x  = "Province")

#Confirmation date - isolated

partial(rf19, pred.var = "confirmed_date2", plot= TRUE, prob = TRUE, type = "classification", which.class = "isolated", plot.engine = "ggplot2") + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.3))+ labs(y = "Predicted Probability", x  = "Confirmation date")


##MARGINS

hist(margin(rf19))


margin1 <- subset(m1, names(margin(rf19))=="deceased")
margin2 <- subset(m1, names(margin(rf19))=="isolated")
margin3 <- subset(m1, names(margin(rf19))=="released")

par(mfrow=c(2,2))

hist(margin1,xlab = "Margin", main = "Histogram - Deceased")
hist(margin2,xlab = "Margin", main = "Histogram - Isolated")
hist(margin3,xlab = "Margin", main = "Histogram - Released")

reliability <- predict(rf19, newdata = data2[5:10,], type = "vote")
reliability

