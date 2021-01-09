setwd("D:\\R CLASS\\Linear Case Study\\Linear Regression Case")


#IMPORTING  PACKAGES
library(dplyr)
library(lubridate)
library(readxl) 
library(XLConnect)
library(openxlsx)
library(MASS)
library(car)
require(sqldf)


custdata <- read_excel("linear regression case.xlsx")

custdata$total_spend = custdata$cardspent + custdata$card2spent



#USER DEFINED FUNCTION

cust_sum_fun <- function(x) {
  nmiss<-sum(is.na(x))
  a <- x[!is.na(x)]
  m <- mean(a)
  n <- length(a)
  s <- sd(a)
  min <- min(a)
  p1<-quantile(a,0.01)
  p5<-quantile(a,0.05)
  p10<-quantile(a,0.10)
  q1<-quantile(a,0.25)
  q2<-quantile(a,0.5)
  q3<-quantile(a,0.75)
  p90<-quantile(a,0.90)
  p95<-quantile(a,0.95)
  p99<-quantile(a,0.99)
  max <- max(a)
  UC <- m+3*s
  LC <- m-3*s
  outlier_flag<- max>p95 | min<p5
  return(c(n=n, nmiss=nmiss, outlier_flag=outlier_flag, mean=m,stdev=s,min = min, p1=p1,p5=p5,p10=p10,q1=q1,q2=q2,
           q3=q3,p90=p90,p95=p95,p99=p99,max=max, UC=UC, LC=LC ))
}


# SEPRATING NUMERICAL AND CATEGORICAL VARIABLES

Numeric_variables  = custdata[,sapply(custdata,is.numeric)]
character_variable = custdata[,!sapply(custdata,is.numeric)]

#ANALYSIS OF COMPLETE DATASET

dia_test <- apply(Numeric_variables,2,cust_sum_fun)
dia_test <- t(data.frame(dia_test))

#CREATING DATA FILE

write.csv(dia_test,"LR_Data.csv",row.names = TRUE)

# OUTLIER AND MISSING VALUE IMPUTATON

Numeric_chars1 <- c(  "age","ed",'employ',"income","lninc","debtinc","creddebt","lncreddebt","othdebt","lnothdebt" ,       
                      "spoused" ,"reside",  "pets","pets_cats","pets_dogs"  ,"pets_birds","pets_reptiles","pets_small",    
                      "pets_saltfish","pets_freshfish","carvalue", "commutetime","carditems" , "cardspent"  , "card2items"  ,"card2spent", 
                      "tenure","longmon","lnlongmon","longten" , "lnlongten","tollmon","lntollmon","tollten", "lntollten" ,
                      "equipmon","lnequipmon","equipten","lnequipten", "cardmon","lncardmon",        
                      "cardten","lncardten","wiremon","lnwiremon","wireten","lnwireten","hourstv",'total_spend')

# APPLYING UDF 

dia_test1 <- apply(Numeric_variables[Numeric_chars1], 2, cust_sum_fun)
dia_test1 <- t(data.frame(dia_test1))

write.csv(dia_test1,"data1.csv",row.names = TRUE)

#CAPPING THE OUTLIERS
OT_function <- function(x){
  quantiles <- quantile(x, c(.05, .95 ),na.rm=TRUE )
  x[x < quantiles[1] ] <- quantiles[1]  
  x[ x > quantiles[2] ] <- quantiles[2]  
  x
}

#TREATMENT OF OUTLIER
Numeric_variables[,Numeric_chars1] <- apply(data.frame(Numeric_variables[,Numeric_chars1]), 2, OT_function)


#MISSING VALUE TREATMENT
Numeric_variables[,Numeric_chars1] <- apply(data.frame(Numeric_variables[,Numeric_chars1]), 2, 
                                            
                                            function(x){x <- replace(x, is.na(x), mean(x, na.rm=TRUE))})


#APPLYING MISSING VALUE
dia_test2 <- apply(Numeric_variables[Numeric_chars1], 2, cust_sum_fun)

dia_test2 <- t(data.frame(dia_test2))

View(dia_test1)



write.csv(dia_test2,"output_data.csv",row.names = TRUE)




Categorical_varibales <- Numeric_variables[,!names(Numeric_variables) %in% c(  "age" , "ed", 'employ' , "income"  , "lninc",   "debtinc"  , "creddebt" ,"lncreddebt" , "othdebt", "lnothdebt" ,       
                                                                               "spoused" ,"reside",  "pets","pets_cats","pets_dogs"  ,  "pets_birds","pets_reptiles","pets_small",    
                                                                               "pets_saltfish","pets_freshfish", "carvalue", "commutetime", "carditems" , "cardspent"  , "card2items"  ,"card2spent", 
                                                                               "tenure","longmon",   "lnlongmon","longten" , "lnlongten", "tollmon" ,  "lntollmon" ,"tollten", "lntollten" ,
                                                                               "equipmon" ,  "lnequipmon"  ,"equipten",
                                                                               "lnequipten", "cardmon"    , "lncardmon",        
                                                                               "cardten" , "lncardten"  , "wiremon"   ,       
                                                                               "lnwiremon",  "wireten", "lnwireten",      
                                                                               "hourstv")]


# SAVE THE DATA THROUGH WHICH Y HAS BECOME NORMAL

Numeric_variables$ln_ttl_spnd <- log(Numeric_variables$total_spend)
names(Numeric_variables)

 


Categorical_varibales <- cbind(Categorical_varibales , lntotalspend = Numeric_variables$ln_ttl_spnd)
names(Categorical_varibales)

Categorical_varibales$total_spend = NULL


#APPLYING ANOVA

anova_test <- aov(lntotalspend ~. , data = Categorical_varibales)
options(scipen=999)
summary(anova_test)

View(Categorical_varibales)



Categorical_varibales1  <- Categorical_varibales[,c(1,3:5,7:10,23,33,42,48,62,64,71,72,82)]

names(Categorical_varibales1)




Categorical_varibales1 <- cbind(Categorical_varibales1 , lntotalspend = Categorical_varibales$lntotalspend)


#APPLYING ANOVA 

anova_test1 <- aov(lntotalspend ~. , data = Categorical_varibales1 )
summary(anova_test1)


#COMBINING CONTINOUS AND CATEGORICAL VARIABLES

Num_characters <- c(  "age","ed",'employ',"income","lninc","debtinc","creddebt","lncreddebt" , "othdebt", "lnothdebt" ,       
                      "spoused","reside",  "pets","pets_cats", "pets_dogs","pets_birds","pets_reptiles","pets_small",    
                      "pets_saltfish","pets_freshfish", "carvalue", "commutetime", "carditems" , "cardspent"  , "card2items","card2spent", "tenure","longmon",   "lnlongmon","longten" , "lnlongten",
                      "tollmon","lntollmon" ,"tollten", "lntollten" ,"equipmon","lnequipmon"  ,"equipten",
                      "lnequipten", "cardmon", "lncardmon", "cardten","lncardten"  , "wiremon"   ,       
                      "lnwiremon",  "wireten", "lnwireten","hourstv")

customer_data <- cbind(Numeric_variables[,Num_characters],Categorical_varibales1)
names(customer_data)



# CREATING A INITIAL MODEL

First_model <- lm(lntotalspend ~., data = customer_data)
summary(First_model)



step_1 <- stepAIC(First_model , direction = "both")


Model1 <- lm(lntotalspend ~ income + lninc + creddebt + lncreddebt + pets_dogs + 
               carditems + cardspent + card2items + card2spent + longmon + 
               lnlongmon + longten + tollten + lntollten + cardmon + lncardmon + 
               cardten + lncardten + lnwiremon + wireten + gender + edcat + 
               union + card + card2 + internet + owndvd + response_03 , data = customer_data)


summary(Model1)
step45 <- stepAIC(Model1, direction = "both")
vif(Model1)

Model_2 <- lm(lntotalspend ~  lninc  + lncreddebt + pets_dogs + 
                carditems  + lnlongmon  + tollten + lncardmon + 
                lncardten + lnwiremon + wireten + gender + edcat + 
                union + card + card2 + internet + owndvd + response_03 , data = customer_data)

summary(Model_2)
vif(Model_2)



rest_variables <- c('lninc' , 'lncreddebt' , 'pets_dogs' ,'lnlongmon' , 'tollten' , 'lncardmon' , 'lncardten','lnwiremon' , 'gender' , 'edcat' , 
                    'union' , 'card' , 'card2' , 'internet' , 'response_03', 'owndvd' ,"lntotalspend")


cust_data1234 <- customer_data[,rest_variables]
names(cust_data1234)
View(cust_data1234)



cust_data1234$owndvd <- as.factor(cust_data1234$owndvd)
cust_data1234$gender <- as.factor(cust_data1234$gender)

cust_data1234$edcat <- as.factor(cust_data1234$edcat)
cust_data1234$union <- as.factor(cust_data1234$union)
cust_data1234$card <- as.factor(cust_data1234$card)
cust_data1234$card2 <- as.factor(cust_data1234$card2)
cust_data1234$response_03 <- as.factor(cust_data1234$response_03)
cust_data1234$internet <- as.factor(cust_data1234$internet)


#SPLITTING TRAINING AND TESTING DATA

set.seed(999)
Traning_data123 <- sample(1:nrow(cust_data1234), size = floor(0.70 * nrow(cust_data1234)))

training_dataset <- cust_data1234[Traning_data123,]
testing_dataset <- cust_data1234[-Traning_data123,]

# APPLYING DATASET

final_model <- lm(lntotalspend ~. , data = training_dataset)
summary(final_model)


#Applying Cook's distance

training_dataset$Cd<- cooks.distance(final_model)
training_dataset1<-subset(training_dataset, Cd< (4/3500))

#Apply Model on variables from stepAIC 

Final_model1 <- lm(lntotalspend ~ lninc+lncreddebt+pets_dogs+lnlongmon+tollten+lncardmon+ 
                     lncardten+lnwiremon+gender+edcat+union+card+card2+internet+owndvd+response_03 , data = training_dataset1)

summary(Final_model1)
ls(Final_model1)
anova(Final_model1)
step_3 <- stepAIC(Final_model1)

#FINAL MODEL 

Final_model12 <- lm(lntotalspend ~lninc + gender + edcat + card + card2 + internet + owndvd , data = training_dataset1)
summary(Final_model12)


# TESTING DATASET

test_data1<-cbind(training_dataset, pred_spnd = exp(predict(Final_model12,training_dataset)))
test_data2<-cbind(testing_dataset, pred_spnd=exp(predict(Final_model12,testing_dataset)))

View(test_data1)
View(test_data2)

