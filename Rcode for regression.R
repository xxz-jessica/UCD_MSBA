#for mac type following command in terminal: xcode-select --install
rm(list=ls())
library(readxl)
install.packages('Hmisc')
library(Hmisc) 
#load data
fiat <- read_excel("fiat.xls")
#obtain summary stats for the data
summary(fiat)
colnames(fiat)

#Generate the Variables of interest
Aw<-fiat$Awareness
GRP<-fiat$GRP
Awtm1<-Lag(Aw,shift=1) #creating the lag of awareness value

#Diminishing Returns
LGRP<-log(1+GRP)


#Running first model with only intercept and the GRP
regint1<-lm(Aw~GRP)
summary(regint1) #display results
AIC(regint1) #use this information criterion to determine if the model is 'best'
BIC(regint1)



#Running second model with lagged awareness, GRP and intercept
regint2<-lm(Aw~Awtm1+GRP)
summary(regint2)
AIC(regint2)
BIC(regint2)


#Running third model without intercept
regmod<-lm(Aw~Awtm1+GRP-1)
summary(regmod)
AIC(regmod)
BIC(regmod)
