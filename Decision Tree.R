##load dataset
library(readr)
german_bank <- read_csv("C:/Users/FREDRICK/Desktop/fredrick's/R-lectures/german-bank.csv")
View(german_bank)


#####structure-of-the-dataset####
str(german_bank)
dim(german_bank)

###data-extraction by selection
german_bank<-german_bank[,2:32]
german_bank
View(german_bank)

###transform the variables to factors
for(i in c(1,3:9,11:21,23:31)){german_bank[,i]<-factor(
  x=german_bank[,i],levels = sort(unique(german_bank[,i])))
}

class(german_bank)

#####Generate random numbers for testing & training - set.seed
set.seed(25)
GermanB<-sample(x=c("Training","Testing"), size = nrow(german_bank),replace = T, prob = c(0.7,0.3))

GermanB


####spliting the datasets into Training & Testing########

TrngDT<-german_bank[GermanB=="Training",]
TstDT<- german_bank[GermanB=="Testing",]

#####select the independent variables######

indvariables<-colnames(german_bank[1:30])
indvariables

######collapse the indvariables#######
rhsofmodel<-paste(indvariables,collapse= "+")
rhsofmodel


#######using the relational operator#########

model<-paste("RESPONSE ~", rhsofmodel)
model


#######CONVERT THE MODEL TO A FORMULA############
formula<-as.formula(model)
formula

###############Building the training model####################

TrainingModel<- glm(formula = formula,data = TrngDT, family = "binomial")
TrainingModel
summary(TrainingModel)
class(TrainingModel)

########forward & backward regression###########

TrainingModel<-step(object = TrainingModel,direction = "both")
summary(TrainingModel)

#######load library######
library(pROC)
troc<-roc(response=TrainingModel$y,predictor = TrainingModel$fitted.values,plot = T)
troc$auc

###classification table for the training model###

TrPred<-ifelse(test=TrainingModel$fitted.values< 0.5,yes=0,no=1)
table(TrainingModel$y,TrPred)

#####load pRoc####
library(pROC)
tsPred<-predict.glm(object = TrainingModel,newdata = TstDT,type = "response")
tsPred

tsRoc<-roc(response=TstDT$RESPONSE,predictor = tsPred,plot = T)
tsRoc$auc


####load library#####

library(pROC)
trapred<-predict.glm(object = TrainingModel,data=TrngDT,type = "response")
trapred
tsPred<-ifelse(test = tsPred<0.5,yes=0,no=1)
table(TstDT$RESPONSE,tsPred)

###load the required library "party" for decision tree###
library(party)
library(sandwich)
library(mvtnorm)
library(modeltools)
library(grid)
library(strucchange)
library(stats4)
library(zoo)

TreeCntr1<-ctree_control(testtype = "Bonferroni",minsplit = 100,minbucket =50,maxdepth = 3)
print(TreeCntr1)
tree_credit<-ctree(formula = formula,data = TrngDT,controls = TreeCntr1)
plot(tree_credit)

par(mar=c(1,1,1,1))

table(predict(tree_credit),TrngDT$RESPONSE)

###calculate the accuracy of the model
(59+122)/(59+26+90+122)

table(predict(tree_credit),TstDT$RESPONSE)
