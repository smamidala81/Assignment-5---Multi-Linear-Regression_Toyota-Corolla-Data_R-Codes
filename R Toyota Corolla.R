#Assignement 5 - MLR - Toyota Corolla

Tcorolla1<-read.csv('E:\\Sree-Official\\Sree-Personal\\EXCELR\\Data Science\\Assignments\\Multi Linear Regression 5\\ToyotaCorolla.csv')
View(Tcorolla1)
colnames(Tcorolla1)
Tcorolla=Tcorolla1[,c(3,4,7,9,13,14,17,18)]
attach(Tcorolla)

#measures of central tendancy
mean(Price)
median(Price)
#mode
getmode <- function(x){
  uniquv <- unique(x)
  uniquv[which.max(tabulate(match(x,uniquv)))]
}
getmode(Price)

#Measures of Dispersion
var(Price)
sd(Price)
range(Price)
rangevalue <- function(x){max(x)-min(x)}
rangevalue(Price)

# Measures of skweness & kurtosis
library(moments)
skewness(Price)
kurtosis(Price)

hist(Price)
hist(KM)
hist(HP)
hist(Weight)

colnames(Tcorolla)
pairs(Tcorolla)
Correlation=cor(Tcorolla)

#1-Regression Model and Summary
Tcorolla_model=lm(Price~.,data=Tcorolla)
summary(Tcorolla_model)

####Experiment####
Tcorolla_model_CC=lm(Price~cc,data=Tcorolla)
summary(Tcorolla_model_CC)

Tcorolla_model_Doors=lm(Price~Doors,data=Tcorolla)
summary(Tcorolla_model_Doors)

Tcorolla_model_CC_Doors=lm(Price~cc+Doors,data=Tcorolla)
summary(Tcorolla_model_CC_Doors)

#Multi-colinearity
install.packages("car")
library(car)
car::vif(Tcorolla_model)

#Diagnostic Plots:
#Residual Plots, QQ-Plos, Std. Residuals vs Fitted
plot(Tcorolla_model)
#Residuals vs Regressors
residualPlots(Tcorolla_model)
#Added Variable Plots
avPlots(Tcorolla_model)
#QQ plots of studentized residuals
qqPlot(Tcorolla_model)
#Deletion Diagnostics
influenceIndexPlot(Tcorolla_model) # Index Plots of the influence measures

#Iteration 1
Tcorolla2=Tcorolla[-c(81,222),]
Tcorolla_model_2=lm(Price~.,data=Tcorolla2)
summary(Tcorolla_model_2)

plot(Tcorolla_model_2)
residualPlots(Tcorolla_model_2)
avPlots(Tcorolla_model_2)
qqPlot(Tcorolla_model_2)
influenceIndexPlot(Tcorolla_model_2)

#Iteration 2
Tcorolla3=Tcorolla[-c(81,222,961,602),]
Tcorolla_model_3=lm(Price~.,data=Tcorolla3)
summary(Tcorolla_model_3)

plot(Tcorolla_model_3)
residualPlots(Tcorolla_model_3)
avPlots(Tcorolla_model_3)
qqPlot(Tcorolla_model_3)
influenceIndexPlot(Tcorolla_model_3)

#Iteration 3 & Transformation
Tcorolla3['Age2']=Tcorolla3$Age_08_04*Tcorolla3$Age_08_04
Tcorolla_model_4=lm(Price~.,data=Tcorolla3)
summary(Tcorolla_model_4)

plot(Tcorolla_model_4)
residualPlots(Tcorolla_model_4)
avPlots(Tcorolla_model_4)
qqPlot(Tcorolla_model_4)
influenceIndexPlot(Tcorolla_model_4)

#Iteration 4 & Transformation
Tcorolla4=Tcorolla[-c(81,222,961,602,148,524,655),-c(6)]
Tcorolla4['Age2']=Tcorolla4$Age_08_04*Tcorolla4$Age_08_04
Tcorolla4['weight2']=Tcorolla4$Weight*Tcorolla4$Weight
Tcorolla4['Quarterly_Tax2']=Tcorolla4$Quarterly_Tax*Tcorolla4$Quarterly_Tax
Tcorolla_model_5=lm(Price~.,data=Tcorolla4)
summary(Tcorolla_model_5)

plot(Tcorolla_model_5)
residualPlots(Tcorolla_model_5)
avPlots(Tcorolla_model_5)
qqPlot(Tcorolla_model_5)
influenceIndexPlot(Tcorolla_model_5)

##Predict for new data
attach(Tcorolla4)
testdata<-data.frame(Age_08_04=20,KM =2000,HP =90,cc=2000,Doors =5,Quarterly_Tax=120,
                     Weight=1500,Age2=(Age_08_04*Age_08_04),Weight2=(Weight*Weight),Quarterly_Tax2=(Quarterly_Tax*Quarterly_Tax))
predict(Tcorolla_model_5,testdata)

pred=predict(Tcorolla_model_5)
pred
pred=predict(Tcorolla_model_5)
Tcorolla_Finaldata=data.frame(Tcorolla4,pred,"Error"=Tcorolla4$Price-pred)

write.csv(Tcorolla_Finaldata,'Tcorolla_Finaldata.csv',row.names = FALSE)
