#Clear the environment
rm(list = ls())

#Set working Directory
setwd("C:/Users/HP/Desktop/edwisor/Project 1")
getwd()


#Load Libraries
x = c("ggplot2", "corrgram", "caret", "randomForest", "dummies","rpart.plot",
      "rpart", 'sampling')

#install.packages(x)
lapply(x, require, character.only = TRUE)
rm(x)


#Load libraries
library("xlsx")
library(ggplot2)
library(gplots)

#Read the data

emp_df = read.xlsx("Absenteeism_at_work_Project.xls", sheetIndex = 1)

#####################################Exploratory Data Analysis##############################
#observe the class of data object
class(emp_df)

#Observe the dimension of data
dim(emp_df)

#get the names of variable
colnames(emp_df)


#Check the structure of the dataset variables
str(emp_df)

#Observe top 5 rows
head(emp_df)

#no of unique values in each variables
apply(emp_df, 2,function(x) length(table(x)))

# From the above EDA and problem statement categorising data in 2 category "continuous" and "catagorical"
continuous_vars = c('Distance.from.Residence.to.Work', 'Service.time', 'Age',
                    'Work.load.Average.day.', 'Transportation.expense',
                    'Hit.target', 'Weight', 'Height', 
                    'Body.mass.index', 'Absenteeism.time.in.hours')

categorical_vars = c('ID','Reason.for.absence','Month.of.absence','Day.of.the.week',
                     'Seasons','Disciplinary.failure', 'Education', 'Social.drinker',
                     'Social.smoker', 'Son', 'Pet')

#########################################Pre Processing ###################################################
#Missing value Analysis 
#Creating dataframe with missing values present in each variable
missing_val = data.frame(apply(emp_df,2,function(x){sum(is.na(x))}))
missing_val$Columns = row.names(missing_val)
names(missing_val)[1] =  "Missing_percentage"

#Calculating percentage missing value
missing_val$Missing_percentage = (missing_val$Missing_percentage/nrow(emp_df)) * 100

# Sorting missing_val in Descending order
missing_val = missing_val[order(-missing_val$Missing_percentage),]
row.names(missing_val) = NULL

# Reordering columns
missing_val = missing_val[,c(2,1)]

# Saving output result into csv file
write.csv(missing_val, "Missing_perc_R.csv", row.names = F)

# # Plot
ggplot(data = missing_val[1:18,], aes(x=reorder(Columns, -Missing_percentage),y = Missing_percentage))+geom_bar(stat = "identity",fill = "grey")+xlab("Variables")+
ggtitle("Missing data percentage") + theme_bw()

#--> Since the calculated missing percentage is less than 5% 
#--> Thus there is no need to drop any variable due to less number of data

#Now generate a missing value in the dataset and try various imputation methods on it

#make a backup for data object
data_back = emp_df
#emp_df = data_back

#Finding ID column values for which there are missing values in Transportation.expense

emp_df$ID[is.na(emp_df$Transportation.expense)]

# [1] 10  3  1 15 20 22 20

for (i in c(1,3,10,15,20,22)){
  emp_df$Transportation.expense[is.na(emp_df$Transportation.expense) & emp_df$ID==i] = mean(emp_df$Transportation.expense[emp_df$ID==i],na.rm = T)
}

as.data.frame(colSums(is.na(emp_df)))

#Finding ID column values for which there are missing values in Distance.from.Residence.to.Work 

emp_df$ID[is.na(emp_df$Distance.from.Residence.to.Work)]

# [1] 34 22 28

for (i in c(34,22,28)){
  emp_df$Distance.from.Residence.to.Work[is.na(emp_df$Distance.from.Residence.to.Work) & emp_df$ID==i] = mean(emp_df$Distance.from.Residence.to.Work[emp_df$ID==i],na.rm = T)
}

as.data.frame(colSums(is.na(emp_df)))

#Finding ID column values for which there are missing values in Service.time

emp_df$ID[is.na(emp_df$Service.time)]

# [1] 28 34 34

for (i in c(34,28)){
  emp_df$Service.time[is.na(emp_df$Service.time) & emp_df$ID==i] = mean(emp_df$Service.time[emp_df$ID==i],na.rm = T)
}

as.data.frame(colSums(is.na(emp_df)))

#Finding ID column values for which there are missing values in Age

emp_df$ID[is.na(emp_df$Age)]

# [1] 28 24 24

for (i in c(24,28)){
  emp_df$Age[is.na(emp_df$Age) & emp_df$ID==i] = mean(emp_df$Age[emp_df$ID==i],na.rm = T)
}

as.data.frame(colSums(is.na(emp_df)))

#ID column has been used to impute missing value for Education

emp_df$ID[is.na(emp_df$Education)]

# [1] 11 10 34 34 14 34 34 34 10 24

for (i in c(10,11,14,24,34)){
  emp_df$Education[is.na(emp_df$Education) & emp_df$ID==i] = mean(emp_df$Education[emp_df$ID==i],na.rm=T)
}

as.data.frame(colSums(is.na(emp_df)))

#ID column has been used to impute missing value for Son

emp_df$ID[is.na(emp_df$Son)]

# [1] 20 14 34 34 27  1

for (i in c(1,14,20,27,34)){
  emp_df$Son[is.na(emp_df$Son) & emp_df$ID==i] = mean(emp_df$Son[emp_df$ID==i],na.rm=T)
}

as.data.frame(colSums(is.na(emp_df)))

#ID column has been used to impute missing value for Social.drinker

emp_df$ID[is.na(emp_df$Social.drinker)]

# [1] 10 14 17

for (i in c(10,14,17)){
  emp_df$Social.drinker[is.na(emp_df$Social.drinker) & emp_df$ID==i] = mean(emp_df$Social.drinker[emp_df$ID==i],na.rm=T)
}

as.data.frame(colSums(is.na(emp_df)))

#ID column has been used to impute missing value for Social.smoker

emp_df$ID[is.na(emp_df$Social.smoker)]

# [1] 34  1 11 15

for (i in c(34,1,11,15)){
  emp_df$Social.smoker[is.na(emp_df$Social.smoker) & emp_df$ID==i] = mean(emp_df$Social.smoker[emp_df$ID==i],na.rm=T)
}

as.data.frame(colSums(is.na(emp_df)))

#ID column has been used to impute missing value for Pet

emp_df$ID[is.na(emp_df$Pet)]

# [1] 1 13

for (i in c(1,13)){
  emp_df$Pet[is.na(emp_df$Pet) & emp_df$ID==i] = mean(emp_df$Pet[emp_df$ID==i],na.rm=T)
}

as.data.frame(colSums(is.na(emp_df)))

#ID column has been used to impute missing value for Weight

emp_df$ID[is.na(emp_df$Weight)]

# [1] 27

for (i in c(27)){
  emp_df$Weight[is.na(emp_df$Weight) & emp_df$ID==i] = mean(emp_df$Weight[emp_df$ID==i],na.rm=T)
}

as.data.frame(colSums(is.na(emp_df)))

#ID column has been used to impute missing value for Height

emp_df$ID[is.na(emp_df$Height)]

# [1] 20 10 28 34 34 27 10 11  5 22 13 24 32 28

for (i in c(20,10,28,34,27,11,5,22,13,24,32)){
  emp_df$Height[is.na(emp_df$Height) & emp_df$ID==i] = mean(emp_df$Height[emp_df$ID==i],na.rm=T)
}

as.data.frame(colSums(is.na(emp_df)))

#ID column has been used to impute missing value for Body.mass.index 

emp_df$ID[is.na(emp_df$Body.mass.index)]

# [1]  3 24 11 30  2 19 34  3 28 34 28  3 13 36 11 14 34 36 28 20 28 28 18 28 17 15 20 22 24 11  5

for (i in c(3,24,11,30,2,19,34,28,13,36,14,20,18,17,15,22,5)){
  emp_df$Body.mass.index[is.na(emp_df$Body.mass.index) & emp_df$ID==i] = mean(emp_df$Body.mass.index[emp_df$ID==i],na.rm=T)
}

as.data.frame(colSums(is.na(emp_df)))


## create a missing value in any variable
#emp_df$Reason.for.absence[7] #22
#emp_df$Reason.for.absence[7] = NA

##Now apply mean method to impute this generated value and observe the result
#emp_df$Reason.for.absence[is.na(emp_df$Reason.for.absence)]  =  mean(emp_df$Reason.for.absence, na.rm = T)# 19.18478
#emp_df$Reason.for.absence[7]
#emp_df$Reason.for.absence[7] = NA

##Now apply median method to impute the missing value
#emp_df$Reason.for.absence[is.na(emp_df$Reason.for.absence)] = median(emp_df$Reason.for.absence, na.rm = T)#23
#emp_df$Reason.for.absence[7]
#emp_df$Reason.for.absence[7] = NA


#now apply KNN method to impute

library(DMwR)
emp_df = knnImputation(emp_df,k=3)  #22.21795
emp_df$Reason.for.absence[7]

#emp_df=data_back
#emp_df$Reason.for.absence[7] = NA
#emp_df = knnImputation(emp_df,k=5)
#emp_df$Reason.for.absence[7] #22.34878

#After applying all the method find,KNN imputation is the best method to fill missing values wih KNN= 3
#Now load the data again and impute the missing value by KNN
#Check presence of missing values once to confirm

apply(emp_df,2, function(x){sum(is.na(x))})

#NO missing value is found
############################## Data conversion################################

emp_df$ID = as.factor(as.character(emp_df$ID))
emp_df$Day.of.the.week = as.factor(as.character(emp_df$Day.of.the.week))
emp_df$Education = as.factor(as.character(emp_df$Education))
emp_df$Social.drinker = as.factor(as.character(emp_df$Social.drinker))
emp_df$Social.smoker = as.factor(as.character(emp_df$Social.smoker))
emp_df$Reason.for.absence = as.factor(as.character(emp_df$Reason.for.absence))
emp_df$Seasons = as.factor(as.character(emp_df$Seasons))
emp_df$Month.of.absence = as.factor(as.character(emp_df$Month.of.absence))
emp_df$Disciplinary.failure = as.factor(as.character(emp_df$Disciplinary.failure))
emp_df$Son = as.factor(as.character(emp_df$Son))
emp_df$Pet = as.factor(as.character(emp_df$Pet))

#_
#########################################Univarant and Bivarant ############################
#plotting boxplot of Reason.for.absence Vs. Absenteeism.time.in.hours
ggplot(emp_df,aes_string(emp_df$Absenteeism.time.in.hours))+geom_histogram(stat='bin',binwidth = 1)+xlab('Absenteeism.time.in.hours')


ggplot(emp_df,aes_string(y=emp_df$Absenteeism.time.in.hours,x=as.factor(emp_df$Reason.for.absence)))+geom_bar(stat='identity')+xlab('Reason.for.absence')+ylab('Absenteeism.time.in.hours')
ggplot(emp_df,aes_string(y=emp_df$Absenteeism.time.in.hours,x=as.factor(emp_df$Seasons)))+geom_bar(stat='identity')+xlab('Seasons')+ylab('Absenteeism.time.in.hours')
ggplot(emp_df,aes_string(y=emp_df$Absenteeism.time.in.hours,x=as.factor(emp_df$Disciplinary.failure)))+geom_bar(stat='identity')+xlab('Disciplinary Failure')+ylab('Absenteeism.time.in.hours')
ggplot(emp_df,aes_string(y=emp_df$Absenteeism.time.in.hours,x=as.factor(emp_df$Education)))+geom_bar(stat='identity')+xlab('Education')+ylab('Absenteeism.time.in.hours')
ggplot(emp_df,aes_string(y=emp_df$Absenteeism.time.in.hours,x=emp_df$Hit.target))+geom_bar(stat='identity')+xlab('Hit.target')+ylab('Absenteeism.time.in.hours')

#######################Outlier Analysis#####################################

#let us first check outliers 
#Transportation.expense, Distance.from.Residence.to.Work

boxplot(emp_df[,c('Transportation.expense','Distance.from.Residence.to.Work')])

#boxplot for  Service.time, Age, Hit.target
boxplot(emp_df[,c( 'Service.time', 'Age','Hit.target')])

#boxplot for Weight,Height,Body.mass.index

boxplot(emp_df[,c('Weight', 'Height', 'Body.mass.index')])

#boxplot for Absenteeism.time.in.hours ,Work.load.Average.day 

boxplot(emp_df[,c('Absenteeism.time.in.hours','Work.load.Average.day.')])

for (i in continuous_vars) {
  print(i)
  val = emp_df[,i][emp_df[,i] %in% boxplot.stats(emp_df[,i])$out]
  print(length(val))
  print(val)
}

#Make each outlier as NA
for (i in continuous_vars) {
  val = emp_df[,i][emp_df[,i] %in% boxplot.stats(emp_df[,i])$out]
  emp_df[,i][emp_df[,i] %in% val] = NA
}

#Check number of missing values
sum(is.na(emp_df))

#Get number of missing values in each variable
for (i in continuous_vars) {
  print(i)
  print(sum(is.na(emp_df[,i])))
}

#Impute the values using KNN method
emp_df = knnImputation(emp_df, k=3)

#Check again for missing value if present in case
sum(is.na(emp_df))

####################################Feature Selection ###################################
#Check for multicollinearity using corelation graph
library(corrgram)


corrgram(emp_df[,continuous_vars], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")


#Variable Reduction
emp_df = subset.data.frame(emp_df, select = -c(Body.mass.index,Service.time))

#Make a copy of Clean Data
clean_data = emp_df
write.xlsx(clean_data, "clean_data.xlsx", row.names = F)

## ANOVA test for Categorical variable

anova_test=aov(Absenteeism.time.in.hours~ID+Reason.for.absence+Month.of.absence+Day.of.the.week+Seasons+
                 Disciplinary.failure+Education+Son+Social.drinker+Social.smoker+Pet,data = emp_df)


summary(anova_test)



## remove all categorical variables except Reason for absence and Month of absence
##Removing Highly Corelated var
emp_df = subset(emp_df, select = -c(ID,Day.of.the.week,Seasons,
                                Disciplinary.failure,Education,Son,Social.drinker,Social.smoker,Pet))

#Make a backup of features selected

feature_selected = emp_df
write.csv(emp_df,"Selected_Features.csv", row.names = F)

colnames(emp_df)

######################################Feature Scaling ######################################
#draw histogram of each variable to know data is normal distributed or not


ggplot(emp_df,aes_string(emp_df$Reason.for.absence))+geom_histogram(stat='count',binwidth = 1)+xlab('Reason.for.absence')
hist(emp_df$Transportation.expense)
hist(emp_df$Distance.from.Residence.to.Work)
hist(emp_df$Age)
hist(emp_df$Work.load.Average.day.)
hist(emp_df$Hit.target)
hist(emp_df$Weight)
hist(emp_df$Height)

#Now select numerical variables to perform normalization

print(sapply(emp_df,is.numeric))
num_names = c("Transportation.expense", "Distance.from.Residence.to.Work","Age","Work.load.Average.day.","Hit.target","Weight","Height")
factor_columns = c("Reason.for.absence","Month.of.absence")

#Feature Scaling
#num_names object contains all the numerical features of selected features

for (i in num_names) {
  print(i)
  emp_df[,i] = ((emp_df[,i] - min(emp_df[,i])) /
                         (max(emp_df[,i]) - min(emp_df[,i])))
  
}

back_df = emp_df
#Create dummy variables of factor variables
emp_df = dummy.data.frame(emp_df, factor_columns)
head(emp_df)

dim(emp_df)

###################### MODEL DEVELOPMENT #################################
#Cleaning the environment
#rmExcept("emp_df")

## Divide the data into train and test data using simple random sampling
#Splitting data into train and test data

library("rpart")

set.seed(7)
train_index = sample(1:nrow(emp_df), 0.8* nrow(emp_df))

train = emp_df[train_index,]
test = emp_df[-train_index,]

########################################DECISION TREE########################################
#RMSE: 2.9360973
#MAE: 1.9640741
#R squared: 0.3401192


#Build decsion tree using rpart
dt_model = rpart(Absenteeism.time.in.hours ~ ., data = train, method = "anova")


#Perdict for test cases
dt_predictions = predict(dt_model, test[,-52])

#Create data frame for actual and predicted values
df_pred = data.frame("actual"=test[,52], "dt_pred"=dt_predictions)
head(df_pred)

#Calcuate MAE, RMSE, R-sqaured for testing data 
print(postResample(pred = dt_predictions, obs = test[,52]))

#Plot a graph for actual vs predicted values
plot(test$Absenteeism.time.in.hours,type="l",lty=2,col="green")
lines(dt_predictions,col="blue")

#write rules into disk
write(capture.output(summary(dt_model)), "Rules.txt")

#Summary of DT model
summary(dt_model)

########################################RANDOM FOREST########################################
#RMSE: 2.6872313
#MAE: 1.7624767 
#R squared: 0.4490218

##Train the model using training data
rf_model = randomForest(Absenteeism.time.in.hours~., data = train, ntree = 500)

#Predict the test cases
rf_predictions = predict(rf_model, test[,-52])

#Create dataframe for actual and predicted values
df_pred = cbind(df_pred,rf_predictions)
head(df_pred)

#Calcuate MAE, RMSE, R-sqaured for testing data 
print(postResample(pred = rf_predictions, obs = test[,52]))

#Plot a graph for actual vs predicted values
plot(test$Absenteeism.time.in.hours,type="l",lty=2,col="red")
lines(rf_predictions,col="blue")

########################################LINEAR REGRESSION########################################
#RMSE: 2.9470610
#MAE: 1.9320873 
#R squared: 0.3350476

##Train the model using training data
lr_model = lm(formula = Absenteeism.time.in.hours~., data = train)

#Get the summary of the model
summary(lr_model)

#Predict the test cases
lr_prediction = predict(lr_model, test[,-52])

#Create dataframe for actual and predicted values
df_pred = cbind(df_pred,lr_prediction)
#df_pred = subset.data.frame(df_pred, select = -c(lr_prediction))
head(df_pred)


#Calcuate MAE, RMSE, R-sqaured for testing data 
print(postResample(pred = lr_prediction, obs = test[,52]))


#Plot a graph for actual vs predicted values
plot(test$Absenteeism.time.in.hours,type="l",lty=2,col="green")
lines(lr_prediction,col="red")


#########################################DIMENSION REDUCTION USING PCA########################################
#Principal component analysis
prin_comp = prcomp(train)

#Compute standard deviation of each principal component
pr_stdev = prin_comp$sdev

#Compute variance
pr_var = pr_stdev^2

#Proportion of variance explained
prop_var = pr_var/sum(pr_var)

#Cumulative scree plot
plot(cumsum(prop_var), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

#Add a training set with principal components
train.data = data.frame(Absenteeism.time.in.hours = train$Absenteeism.time.in.hours, prin_comp$x)

# From the above plot selecting 20 components since it explains almost 95+ % data variance
train.data =train.data[,1:20]

#Transform test data into PCA
test.data = predict(prin_comp, newdata = test)
test.data = as.data.frame(test.data)

#Select the first 20 components
test.data=test.data[,1:20]

########################################DECISION TREE########################################
#RMSE:0.4098639
#MAE: 0.2650369 
#R squared: 0.9872817

#Build decsion tree using rpart
dt_model = rpart(Absenteeism.time.in.hours ~., data = train.data, method = "anova")

#Predict the test cases
dt_predictions = predict(dt_model,test.data)

#Create data frame for actual and predicted values
df_preds = data.frame("actual"=test[,52], "dt_pred"=dt_predictions)
head(df_preds)

#Calcuate MAE, RMSE, R-sqaured for testing data 
print(postResample(pred = dt_predictions, obs = test$Absenteeism.time.in.hours))

#Plot a graph for actual vs predicted values
plot(test$Absenteeism.time.in.hours,type="l",lty=2,col="red")
lines(dt_predictions,col="blue")

########################################RANDOM FOREST########################################
#RMSE:0.7057195
#MAE: 0.3670694
#R squared: 0.9732212


#Train the model using training data
rf_model = randomForest(Absenteeism.time.in.hours~., data = train.data, ntrees = 500)

#Predict the test cases
rf_predictions = predict(rf_model,test.data)

#Create dataframe for actual and predicted values
df_preds = cbind(df_preds,rf_predictions)
head(df_preds)

#Calcuate MAE, RMSE, R-sqaured for testing data 
print(postResample(pred = rf_predictions, obs = test$Absenteeism.time.in.hours))

#Plot a graph for actual vs predicted values
plot(test$Absenteeism.time.in.hours,type="l",lty=2,col="green")
lines(rf_predictions,col="blue")

########################################LINEAR REGRESSION########################################
#RMSE: 0.007397825
#MAE: 0.005452601 
#R squared:  0.999995826

#Train the model using training data
lr_model = lm(Absenteeism.time.in.hours ~ ., data = train.data)

#Get the summary of the model
summary(lr_model)

#Predict the test cases
lr_predictions = predict(lr_model,test.data)

#Create dataframe for actual and predicted values
df_preds = cbind(df_preds,lr_predictions)
head(df_preds)

#Calcuate MAE, RMSE, R-sqaured for testing data 
print(postResample(pred = lr_predictions, obs =test$Absenteeism.time.in.hours))

#Plot a graph for actual vs predicted values
plot(test$Absenteeism.time.in.hours,type="l",lty=2,col="red")
lines(lr_predictions,col="blue")
