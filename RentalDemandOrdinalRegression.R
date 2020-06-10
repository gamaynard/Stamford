## ---------------------------
##
## Script name: RentalDemandOrdinalRegression.R
##
## Purpose of script: Create ordinal regressions using Rental Demand survey data from the 
##    City of Stamford
##
## Author: George A. Maynard
##
## Date Created: 2020-06-09
##
## Copyright (c) George A. Maynard, 2020
## Email: galphonsemaynard@gmail.com
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------

## set working directory

## ---------------------------

## set options
options(scipen = 6, digits = 4)
## ---------------------------

## load up the packages we will need:  
library(XLConnect)
library(MASS)
## ---------------------------

## load up our functions into memory

## ---------------------------

## Read in the raw data
raw=readWorksheetFromFile(
  file=file.choose(),
  sheet=1,
  header=TRUE,
  startCol=1,
  startRow=1
)

## Order categorical variables
raw$Current.Demand=factor(
  raw$Current.Demand, 
  levels=seq(1,5,1), 
  ordered=TRUE
  )
raw$Rent=factor(
  raw$Rent,
  levels=c("Low","Moderate","High"),
  ordered=TRUE
)
raw$Change.in.Demand=factor(
  raw$Change.in.Demand,
  levels=seq(1,5,1),
  ordered=TRUE
)
raw$Building.Age=factor(
  raw$Building.Age,
  levels=c("Old","New"),
  ordered=TRUE
)
## Create ordinal regressions of current demand
fullModel= polr(
  formula=Current.Demand~Building.Age+Stories+Units+Rent,
  data = raw, 
  Hess = TRUE
  )
summary(fullModel)
ageModel= polr(
  formula=Current.Demand~Building.Age,
  data = raw, 
  Hess = TRUE
)
summary(ageModel)
sizeModel= polr(
  formula=Current.Demand~Stories+Units,
  data = raw, 
  Hess = TRUE
)
summary(sizeModel)
priceModel= polr(
  formula=Current.Demand ~ Rent,
  data = raw, 
  Hess = TRUE
)
summary(priceModel)

modList=list(fullModel,ageModel,sizeModel,priceModel)
## Record the results
## Create an empty object to store AIC comparison results
CurrentResults=matrix(nrow=4,ncol=2)
for(i in 1:4){
  model=modList[i]
  CurrentResults[i,1]=c("fullModel","ageModel","sizeModel","priceModel")[i]
  CurrentResults[i,2]=extractAIC(eval(parse(text = CurrentResults[i,1])))[2]
}
CurrentResults=CurrentResults[order(CurrentResults[,2]),]
CurrentResults=as.data.frame(CurrentResults)
colnames(CurrentResults)=c("Model","AIC")
CurrentResults$dAIC=as.numeric(as.character(CurrentResults$AIC))-as.numeric(as.character(min(CurrentResults$AIC)))

## Create ordinal regressions of changes in demand following the same process
fullModel= polr(
  formula=Change.in.Demand~Building.Age+Stories+Units+Rent,
  data = raw, 
  Hess = TRUE
)
summary(fullModel)
ageModel= polr(
  formula=Change.in.Demand~Building.Age,
  data = raw, 
  Hess = TRUE
)
summary(ageModel)
sizeModel= polr(
  formula=Change.in.Demand~Stories+Units,
  data = raw, 
  Hess = TRUE
)
summary(sizeModel)
priceModel= polr(
  formula=Change.in.Demand ~ Rent,
  data = raw, 
  Hess = TRUE
)
summary(priceModel)

modList=list(fullModel,ageModel,sizeModel,priceModel)
## Record the results
## Create an empty object to store AIC comparison results
ChangeResults=matrix(nrow=4,ncol=2)
for(i in 1:4){
  model=modList[i]
  ChangeResults[i,1]=c("fullModel","ageModel","sizeModel","priceModel")[i]
  ChangeResults[i,2]=extractAIC(eval(parse(text = ChangeResults[i,1])))[2]
}
ChangeResults=ChangeResults[order(ChangeResults[,2]),]
ChangeResults=as.data.frame(ChangeResults)
colnames(ChangeResults)=c("Model","AIC")
ChangeResults$dAIC=as.numeric(as.character(ChangeResults$AIC))-as.numeric(as.character(min(ChangeResults$AIC)))

                