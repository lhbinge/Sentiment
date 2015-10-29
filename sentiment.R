##=========================================================================================##
## -------------------------------- SENTIMENT ---------------------------------------------##
##=========================================================================================##

##=====================##
## READING IN THE DATA ##
##=====================##

library(ggplot2)
library(plyr)
library(dplyr)
library(reshape2)
library(stargazer)

setwd("C:\\Users\\Laurie\\OneDrive\\Documents\\BING\\BER Confidence Surveys\\Data")
BER <- read.csv("Manufacturing.csv", header=TRUE, sep=";",na.strings = "", skipNul = TRUE)

BER$region <- as.factor(BER$region) #sit levels in
BER$sector <- as.factor(BER$sector) #sit levels in

# replace 1,2,3 (Up, Same, Down) responses with 1,0,-1
for(i in 8:62) {
    BER[,i] <- replace(BER[,i], BER[,i]==2, 0)
    BER[,i] <- replace(BER[,i], BER[,i]==3,-1)
}

BER$Q19 <- replace(BER$Q19, BER$Q19==0,-1) # replace 1,0 (Yes, No) responses with 1,-1
BER$Q20 <- replace(BER$Q20, BER$Q20==0,-1) # replace 1,0 (Satisfactory, Unsatisfactory) with 1,-1

##======================##
## CALCULATE INDICATORS ##
##======================##

# calculation check    
#t1 <- na.omit(subset(BER$Q20,BER$survey=="01Q2"))
#fr.increase <- length(subset(t1,t1==1))/length(t1)
#fr.decrease <- length(subset(t1,t1==-1))/length(t1)
#con <- fr.increase-fr.decrease

indicators <- aggregate(BER$Q20, by=list(BER$survey), FUN=mean, na.rm=TRUE)
colnames(indicators) <- c("Date","Conf_cc")
indicators <- cbind(indicators, Conf_fl = aggregate(BER$Q31, by=list(BER$survey), FUN=mean, na.rm=TRUE)[,2])
indicators <- cbind(indicators, Act_prod = aggregate(BER$Q3A, by=list(BER$survey), FUN=mean, na.rm=TRUE)[,2])
indicators <- cbind(indicators, Conf_prod = aggregate(BER$Q3P, by=list(BER$survey), FUN=mean, na.rm=TRUE)[,2])
indicators <- cbind(indicators, Act_GBC = aggregate(BER$Q7A, by=list(BER$survey), FUN=mean, na.rm=TRUE)[,2])
indicators <- cbind(indicators, Conf_GBC = aggregate(BER$Q7P, by=list(BER$survey), FUN=mean, na.rm=TRUE)[,2])
indicators <- cbind(indicators, Invest = aggregate(BER$Q10A, by=list(BER$survey), FUN=mean, na.rm=TRUE)[,2])
indicators <- cbind(indicators, Empl = aggregate(BER$Q8A, by=list(BER$survey), FUN=mean, na.rm=TRUE)[,2])

altBER <- BER
altBER$Q8A <- replace(altBER$Q8A, altBER$Q8A==-1,1) # replace -1 (Down) responses with 1
indicators <- cbind(indicators, Empl_turn = aggregate(altBER$Q8A, by=list(altBER$survey), FUN=mean, na.rm=TRUE)[,2])

# calculation check    
#t1 <- na.omit(subset(BER$Q31,BER$survey=="01Q3"))
#fr.increase <- length(subset(t1,t1==1))/length(t1)
#fr.decrease <- length(subset(t1,t1==-1))/length(t1)
#unc <- sqrt(fr.increase+fr.decrease-(fr.increase-fr.decrease)^2)

se <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x))*(length(na.omit(x))-1)) #adjust for (n-1)
indicators <- cbind(indicators, Uncert_fl = aggregate(BER$Q31, by=list(BER$survey), FUN=se)[,2])
indicators <- cbind(indicators, Uncert_fl.prod = aggregate(BER$Q3P, by=list(BER$survey), FUN=se)[,2])
indicators <- cbind(indicators, Uncert_fl.GBC = aggregate(BER$Q7P, by=list(BER$survey), FUN=se)[,2])

sum(duplicated(BER[,c("id","survey")]))

#The expectations of firms in question 31 in period t are compared to the realization of firms 
#in question 7A in period t+4. 
#Also compare the expectations of firms in questions 3P & 7P in period t to the realizations 
#in questions 3A & 7A in period t+1. 
errors1 <- indicators[,c(1,11)]
errors2 <- indicators[,c(1,11)]
errors3 <- indicators[,c(1,11)]
tel <- 2

for(i in levels(factor(BER$id))){
    tel <- tel + 1
    #BER$counter <- as.numeric(BER$survey)
    exp.error <- indicators[,c(1,11)]
    data <- subset(BER, BER$id==i)
    #data <- data[order(data$counter),]
    exp.error <- merge(exp.error, data, by.x="Date",by.y="survey", all.x = TRUE)

    for(t in 1:(nrow(exp.error))) {
        exp.error$error1[t] <- exp.error$Q7A[t+4] - exp.error$Q31[t]
        exp.error$error2[t] <- exp.error$Q3A[t+1] - exp.error$Q3P[t]
        exp.error$error3[t] <- exp.error$Q7A[t+1] - exp.error$Q7P[t]
    }
    errors1 <- cbind(errors1, exp.error$error1)
    colnames(errors1)[tel] <- as.character(i)
    errors2 <- cbind(errors2, exp.error$error2)
    colnames(errors2)[tel] <- as.character(i)
    errors3 <- cbind(errors3, exp.error$error3)
    colnames(errors3)[tel] <- as.character(i)
}

uncert <- transform(errors1, SD=apply(errors1[,c(-1,-2)],1,sd,na.rm = TRUE))[,c(1,ncol(errors1)+1)]
indicators <- cbind(indicators, Uncert_ee = uncert[,2])    
uncert <- transform(errors2, SD=apply(errors2[,c(-1,-2)],1,sd,na.rm = TRUE))[,c(1,ncol(errors2)+1)]
indicators <- cbind(indicators, Uncert_ee.prod = uncert[,2])
uncert <- transform(errors3, SD=apply(errors3[,c(-1,-2)],1,sd,na.rm = TRUE))[,c(1,ncol(errors3)+1)]
indicators <- cbind(indicators, Uncert_ee.GBC = uncert[,2])

# Kan dit verander na standard error?
    
# Calculate Response Rates

#sum(is.na(df$col))
#sapply(airquality, function(x) sum(is.na(x)))
#apply(is.na(BER),2,sum)

#missing <- function(x) sum(is.na(x))
#NNR <- aggregate(BER, by=list(BER$survey), FUN=missing)
#NNR <- as.data.frame(NNR)

for(j in levels(BER$survey)) { 
    data <- subset(BER,BER$survey==j)
    NNR[] <- sum(is.na(data)) 
}
