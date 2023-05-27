#MACHINE LEARNING WITH R

#SUPERVISED LEARNING

install.packages("gmodels")

read.csv("car_data.csv")
mycardata<-read.csv("car_data.csv")
mycardata
attach(mycardata)
View(mycardata)
library(dplyr)
mycardata%>%
  select(-1)->mycardata
View(mycardata)

install.packages("caTools")
library(caTools)

sample.split(mycardata$Purchased,SplitRatio = 0.65) -> split_value
subset(mycardata, split_value==T)-> train_set
subset(mycardata, split_value==F)-> test_set

#Building Modelling Classification
install.packages("rpart")
library(rpart)
rpart(Purchased~.,data = train_set)->mod_car #build model on training data set
predict(mod_car,test_set,type = "class")->result_class #check model accuracy with data test set
table(test_set$Purchased,result_class) #evaluate results using confusion matrix

#result_class Numbers may differ everytime
#No Yes
#No  192  17
#Yes  12 129

#Model Accuracy = (Rightly classified/total) * 100

((192+129)/(192+17+12+129))*100

# Building Regression Model
library(tidyverse)
View(diamonds)
library(caTools)
sample.split(diamonds$price,SplitRatio = 0.65)->split_values
subset(diamonds,split_values==T)->train_regset
subset(diamonds,split_values==F)->test_regset

#Building lm Model
lm(price~.,data=train_regset)->mod_regress
predict(mod_regress,test_regset)->result_regress
cbind(actual=test_regset$price,predicted=result_regress)->final_data
as.data.frame(final_data)->final_data
final_data

#Finding error
(final_data$actual - final_data$predicted)->error
cbind(final_data, error)->final_data
final_data
View(final_data)


#UNSUPERVISED LEARNING

#Clustering demo
View(iris)
iris[1:4]->irisk
View(irisk)

class(irisk) #to check the class of the dataset
as.matrix(irisk)->irisk
class(irisk)
kmeans(irisk,3)->iris_cluster #dataset and number of clusters
cbind(iris, iris_cluster$cluster)->clustered_data
View(clustered_data)

#Principal Component Analysis (PCA)
library(stats) #is a base package
library(dplyr)
mydata<-select(iris, c(1,2,3,4))
head(mydata)
cor(mydata) #Check PCA eligibility
mean(cor(mydata))

PCA <-princomp(mydata) #princomp means principal component
PCA

PCA$loadings

PC<-PCA$scores
PC
cor(PC)

#PCA with Smaple Datset - Ethical Leadership, National Integrity Assessment
read.csv("EL.csv")
leadership<-read.csv("EL.csv")
leadership
attach(leadership)
View(leadership)

leadership[c(-1,-2)]->ethicalleadership
View(ethicalleadership)

library(dplyr)

el_numeric <- apply(ethicalleadership, 2, function(x) recode(x, 
                                                             "Strongly d" = 1, 
                                                             "Disagree" = 2, 
                                                             "Slightly d" = 3, 
                                                             "Neutral" = 4, 
                                                             "Slightly a" = 5, 
                                                             "Agree" = 6, 
                                                             "Strongly a" = 7,
                                                             "Don't know" = NaN))

View(el_numeric)

data_point <-el_numeric
na.omit(data_point)
na.omit(data_point)
View(data_point)
data_point[is.na(data_point)] <- ""
View(data_clean)

head(data_clean)
cor(data_clean) #Check PCA eligibility
mean(cor(data_clean))

princomp(data_point) #princomp means principal component
PCA

PCA$loadings

PC<-PCA$scores
PC
cor(PC)

drop.na(el_numeric)








