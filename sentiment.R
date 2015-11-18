##=========================================================================================##
## -------------------------------- SENTIMENT ---------------------------------------------##
##=========================================================================================##
setwd("C:\\Users\\Laurie\\OneDrive\\Documents\\BING\\BER Confidence Surveys\\Sentiment")

library(ggplot2)
library(plyr)
library(dplyr)
library(reshape2)
library(stargazer)

##====================================##
## READING IN THE DATA: MANUFACTURING ##
##====================================##
BER.M <- read.csv("Manufacturing.csv", header=TRUE, sep=";",na.strings = "", skipNul = TRUE)
colnames(BER.M)[1:7] <- c("region","id","sector","weight","turnover","factor","surveyQ")

BER.M$surveyQ <- toupper(BER.M$surveyQ)
BER.M[nrow(BER.M)+1,1:6] <- BER.M[nrow(BER.M),1:6] 
BER.M[nrow(BER.M),"surveyQ"] <- "05Q4" 

BER.M$region <- factor(BER.M$region)
#BER.M$region <- factor(BER.M$region, labels=c("WC","EC","NC","NW","FS","KZN","GP","MP","LP"))
BER.M$sector <- factor(BER.M$sector) #could include labels
BER.M$id <- factor(BER.M$id)
BER.M$surveyQ <- factor(BER.M$surveyQ)
#BER.M$surveyQ <- factor(BER.M$surveyQ, levels=c(levels(BER.M$surveyQ),"05Q4"))

# replace 1,2,3 (Up, Same, Down) responses with 1,0,-1
for(i in 8:62) {
    BER.M[,i] <- replace(BER.M[,i], BER.M[,i]==2, 0)
    BER.M[,i] <- replace(BER.M[,i], BER.M[,i]==3,-1)
}
BER.M$Q19 <- replace(BER.M$Q19, BER.M$Q19==0,-1) # replace 0 (No) responses with -1
BER.M$Q20 <- replace(BER.M$Q20, BER.M$Q20==0,-1) # replace 0 (Unsatisfactory) with -1

##=====================================##
## CALCULATE INDICATORS: MANUFACTURING ##
##=====================================##
# calculation check    
#t1 <- na.omit(subset(BER.M$Q20,BER.M$survey=="01Q2"))
#fr.increase <- length(subset(t1,t1==1))/length(t1)
#fr.decrease <- length(subset(t1,t1==-1))/length(t1)
#con <- fr.increase-fr.decrease

##IMPUTE 05Q4 with AGGREGATED NUMBERS!
## Weight the means and std devs?
#wt.mean(x, wt)
#wt.var(x, wt)
#wt.sd(x, wt)
#wtd.mean(x, weights=NULL, normwt="ignored", na.rm=TRUE)
#wtd.var(x, weights=NULL, normwt=FALSE, na.rm=TRUE)

indicators.M <- aggregate(BER.M$Q20, by=list(BER.M$surveyQ), FUN=mean, na.rm=TRUE)
colnames(indicators.M) <- c("Date","Conf_cc")
indicators.M <- cbind(indicators.M, Conf_fl = aggregate(BER.M$Q31, by=list(BER.M$surveyQ), FUN=mean, na.rm=TRUE)[,2])
indicators.M <- cbind(indicators.M, Act_prod = aggregate(BER.M$Q3A, by=list(BER.M$surveyQ), FUN=mean, na.rm=TRUE)[,2])
indicators.M <- cbind(indicators.M, Conf_prod = aggregate(BER.M$Q3P, by=list(BER.M$surveyQ), FUN=mean, na.rm=TRUE)[,2])
indicators.M <- cbind(indicators.M, Act_GBC = aggregate(BER.M$Q7A, by=list(BER.M$surveyQ), FUN=mean, na.rm=TRUE)[,2])
indicators.M <- cbind(indicators.M, Conf_GBC = aggregate(BER.M$Q7P, by=list(BER.M$surveyQ), FUN=mean, na.rm=TRUE)[,2])
indicators.M <- cbind(indicators.M, Invest = aggregate(BER.M$Q10A, by=list(BER.M$surveyQ), FUN=mean, na.rm=TRUE)[,2])
indicators.M <- cbind(indicators.M, Empl = aggregate(BER.M$Q8A, by=list(BER.M$surveyQ), FUN=mean, na.rm=TRUE)[,2])

altBER <- BER.M
altBER$Q8A <- replace(altBER$Q8A, altBER$Q8A==-1,1) # replace -1 (Down) responses with 1
indicators.M <- cbind(indicators.M, Empl_turn = aggregate(altBER$Q8A, by=list(altBER$surveyQ), FUN=mean, na.rm=TRUE)[,2])

# calculation check    
#t1 <- na.omit(subset(BER$Q31,BER$survey=="01Q3"))
#fr.increase <- length(subset(t1,t1==1))/length(t1)
#fr.decrease <- length(subset(t1,t1==-1))/length(t1)
#unc <- sqrt(fr.increase+fr.decrease-(fr.increase-fr.decrease)^2)

# Kan dit verander na standard error or standard deviation?
se <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x))*(length(na.omit(x))-1)) #adjust for (n-1)
indicators.M <- cbind(indicators.M, Uncert_fl = aggregate(BER.M$Q31, by=list(BER.M$surveyQ), FUN=se)[,2])
indicators.M <- cbind(indicators.M, Uncert_fl.prod = aggregate(BER.M$Q3P, by=list(BER.M$surveyQ), FUN=se)[,2])
indicators.M <- cbind(indicators.M, Uncert_fl.GBC = aggregate(BER.M$Q7P, by=list(BER.M$surveyQ), FUN=se)[,2])

#The expectations of firms in question 31 in period t are compared to the realization of firms 
#in question 7A in period t+4. 
#Also compare the expectations of firms in questions 3P & 7P in period t to the realizations 
#in questions 3A & 7A in period t+1. 

dups <- BER.M[duplicated(BER.M[,c("id","surveyQ")]) | duplicated(BER.M[,c("id","surveyQ")], fromLast = TRUE),]
uniBER.M <- BER.M[!duplicated(BER.M[,c("id","surveyQ")]),]

errors1 <- indicators.M[,c(1,11)]
errors2 <- indicators.M[,c(1,11)]
errors3 <- indicators.M[,c(1,11)]
tel <- 2

for(i in levels(uniBER.M$id)){
    tel <- tel + 1
    #BER.M$counter <- as.numeric(BER.M$survey)
    exp.error <- indicators.M[,c(1,11)]
    data <- subset(uniBER.M, uniBER.M$id==i)
    #data <- data[order(data$counter),]
    exp.error <- merge(exp.error, data, by.x="Date",by.y="surveyQ", all.x = TRUE)

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

# Kan dit verander na standard error or standard deviation?
uncert <- transform(errors1, SD=apply(errors1[,c(-1,-2)],1,se))[,c(1,ncol(errors1)+1)]
indicators.M <- cbind(indicators.M, Uncert_ee = uncert[,2])    
uncert <- transform(errors2, SD=apply(errors2[,c(-1,-2)],1,se))[,c(1,ncol(errors2)+1)]
indicators.M <- cbind(indicators.M, Uncert_ee.prod = uncert[,2])
uncert <- transform(errors3, SD=apply(errors3[,c(-1,-2)],1,se))[,c(1,ncol(errors3)+1)]
indicators.M <- cbind(indicators.M, Uncert_ee.GBC = uncert[,2])


indicator_plot <- indicators.M[,c(1,2,3,5,7)]
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_point(size = 3) 
g <- g + geom_line()
g <- g + ylab("Indicator")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank())
g

indicator_plot <- indicators.M[,c(1,14,15,16)]
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_point(size = 3) 
g <- g + geom_line()
g <- g + ylab("Indicator")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank())
g

indicator_plot <- indicators.M[,c(1,11,12,13)]
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_point(size = 3) 
g <- g + geom_line()
g <- g + ylab("Indicator")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank())
g


# Calculate Response Rates
countNR <- function(data) { sum(is.na(data))/NROW(data) }
NRR.M <- aggregate(BER.M, by=list(BER.M$surveyQ), FUN=countNR)

#sum(is.na(df$col))
#sapply(airquality, function(x) sum(is.na(x)))
#apply(is.na(BER),2,sum)
#missing <- function(x) sum(is.na(x))
#NNR <- aggregate(BER, by=list(BER$survey), FUN=missing)
#NNR <- as.data.frame(NNR)


##===============================##
## READING IN THE DATA: BUILDING ##
##===============================##
BER.B <- read.csv("Building.csv", header=TRUE, sep=";",na.strings = "", skipNul = TRUE)
colnames(BER.B)[1:6] <- c("region","id","sector","weight","factor","surveyQ")

BER.B$surveyQ <- toupper(BER.B$surveyQ)
BER.B[nrow(BER.B)+1,1:5] <- BER.B[nrow(BER.B),1:5] 
BER.B[nrow(BER.B),"surveyQ"] <- "05Q4" 

BER.B$region <- factor(BER.B$region)
BER.B$sector <- factor(BER.B$sector) #could include labels
BER.B$id <- factor(BER.B$id)
BER.B$surveyQ <- factor(BER.B$surveyQ)
#BER.B$surveyQ <- factor(BER.B$surveyQ, levels=c(levels(BER.B$surveyQ),"05Q4"))

# replace 1,2,3 (Up, Same, Down) responses with 1,0,-1
for(i in 7:22) {
    BER.B[,i] <- replace(BER.B[,i], BER.B[,i]==2, 0)
    BER.B[,i] <- replace(BER.B[,i], BER.B[,i]==3,-1)
}
BER.B$Q1 <- replace(BER.B$Q1, BER.B$Q1==0,-1) # replace 0 (Unsatisfactory) responses with -1

##================================##
## CALCULATE INDICATORS: BUILDING ##
##================================##
indicators.B <- aggregate(BER.B$Q1, by=list(BER.B$surveyQ), FUN=mean, na.rm=TRUE)
colnames(indicators.B) <- c("Date","Conf_cc")
indicators.B <- cbind(indicators.B, Act_prod = aggregate(BER.B$Q3A, by=list(BER.B$surveyQ), FUN=mean, na.rm=TRUE)[,2])
indicators.B <- cbind(indicators.B, Conf_prod = aggregate(BER.B$Q3P, by=list(BER.B$surveyQ), FUN=mean, na.rm=TRUE)[,2])
indicators.B <- cbind(indicators.B, Act_GBC = aggregate(BER.B$Q2A, by=list(BER.B$surveyQ), FUN=mean, na.rm=TRUE)[,2])
indicators.B <- cbind(indicators.B, Conf_GBC = aggregate(BER.B$Q2P, by=list(BER.B$surveyQ), FUN=mean, na.rm=TRUE)[,2])
indicators.B <- cbind(indicators.B, Empl = aggregate(BER.B$Q4A, by=list(BER.B$surveyQ), FUN=mean, na.rm=TRUE)[,2])

altBER <- BER.B
altBER$Q4A <- replace(altBER$Q4A, altBER$Q4A==-1,1) # replace -1 (Down) responses with 1
indicators.B <- cbind(indicators.B, Empl_turn = aggregate(altBER$Q4A, by=list(altBER$surveyQ), FUN=mean, na.rm=TRUE)[,2])

# Kan dit verander na standard error or standard deviation?
se <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x))*(length(na.omit(x))-1)) #adjust for (n-1)
indicators.B <- cbind(indicators.B, Uncert_fl.prod = aggregate(BER.B$Q3P, by=list(BER.B$surveyQ), FUN=se)[,2])
indicators.B <- cbind(indicators.B, Uncert_fl.GBC = aggregate(BER.B$Q2P, by=list(BER.B$surveyQ), FUN=se)[,2])

dups <- BER.B[duplicated(BER.B[,c("id","surveyQ")]) | duplicated(BER.B[,c("id","surveyQ")], fromLast = TRUE),]
uniBER.B <- BER.B[!duplicated(BER.B[,c("id","surveyQ")]),]

errors1 <- indicators.B[,c(1,9)]
errors2 <- indicators.B[,c(1,9)]
tel <- 2
for(i in levels(uniBER.B$id)){
    tel <- tel + 1
    #BER.B$counter <- as.numeric(BER.B$survey)
    exp.error <- indicators.B[,c(1,9)]
    data <- subset(uniBER.B, uniBER.B$id==i)
    #data <- data[order(data$counter),]
    exp.error <- merge(exp.error, data, by.x="Date",by.y="surveyQ", all.x = TRUE)
    for(t in 1:(nrow(exp.error))) {
        exp.error$error1[t] <- exp.error$Q3A[t+1] - exp.error$Q3P[t]
        exp.error$error2[t] <- exp.error$Q2A[t+1] - exp.error$Q2P[t]
    }
    errors1 <- cbind(errors1, exp.error$error1)
    colnames(errors1)[tel] <- as.character(i)
    errors2 <- cbind(errors2, exp.error$error2)
    colnames(errors2)[tel] <- as.character(i)
}

# Kan dit verander na standard error or standard deviation?
uncert <- transform(errors1, SD=apply(errors1[,c(-1,-2)],1,se))[,c(1,ncol(errors1)+1)]
indicators.B <- cbind(indicators.B, Uncert_ee.prod = uncert[,2])    
uncert <- transform(errors2, SD=apply(errors2[,c(-1,-2)],1,se))[,c(1,ncol(errors2)+1)]
indicators.B <- cbind(indicators.B, Uncert_ee.GBC = uncert[,2])

indicator_plot <- indicators.B[,c(1,2,4,6)]
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_point(size = 3) 
g <- g + geom_line()
g <- g + ylab("Indicator")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank())
g

indicator_plot <- indicators.B[,c(1,9,10,11,12)]
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_point(size = 3) 
g <- g + geom_line()
g <- g + ylab("Indicator")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank())
g

# Calculate Response Rates
countNR <- function(data) { sum(is.na(data))/NROW(data) }
NRR.B <- aggregate(BER.B, by=list(BER.B$surveyQ), FUN=countNR)


##============================##
## READING IN THE DATA: TRADE ##
##============================##
BER.R <- read.csv("Retail.csv", header=TRUE, sep=";",na.strings = "", skipNul = TRUE)
BER.W <- read.csv("Wholesale.csv", header=TRUE, sep=";",na.strings = "", skipNul = TRUE)
#BER.V <- read.csv("Motor.csv", header=TRUE, sep=";",na.strings = "", skipNul = TRUE)

BER.T <- rbind(BER.R,BER.W)
colnames(BER.T)[1:6] <- c("region","id","sector","weight","factor","surveyQ")

BER.T$surveyQ <- toupper(BER.T$surveyQ)
BER.T[nrow(BER.T)+1,1:5] <- BER.T[nrow(BER.T),1:5] 
BER.T[nrow(BER.T),"surveyQ"] <- "05Q4" 

BER.T$region <- factor(BER.T$region)
BER.T$sector <- factor(BER.T$sector) #could include labels
BER.T$id <- factor(BER.T$id)
BER.T$surveyQ <- factor(BER.T$surveyQ)
#BER.T$surveyQ <- factor(BER.T$surveyQ, levels=c(levels(BER.T$surveyQ),"05Q4"))

# replace 1,2,3 (Up, Same, Down) responses with 1,0,-1
for(i in 7:21) {
    BER.T[,i] <- replace(BER.T[,i], BER.T[,i]==2, 0)
    BER.T[,i] <- replace(BER.T[,i], BER.T[,i]==3,-1)
}
BER.T$Q1 <- replace(BER.T$Q1, BER.T$Q1==0,-1) # replace 0 (Unsatisfactory) responses with -1

##=============================##
## CALCULATE INDICATORS: TRADE ##
##=============================##
indicators.T <- aggregate(BER.T$Q1, by=list(BER.T$surveyQ), FUN=mean, na.rm=TRUE)
colnames(indicators.T) <- c("Date","Conf_cc")
indicators.T <- cbind(indicators.T, Act_prod = aggregate(BER.T$Q3A, by=list(BER.T$surveyQ), FUN=mean, na.rm=TRUE)[,2])
indicators.T <- cbind(indicators.T, Conf_prod = aggregate(BER.T$Q3P, by=list(BER.T$surveyQ), FUN=mean, na.rm=TRUE)[,2])
indicators.T <- cbind(indicators.T, Act_GBC = aggregate(BER.T$Q2A, by=list(BER.T$surveyQ), FUN=mean, na.rm=TRUE)[,2])
indicators.T <- cbind(indicators.T, Conf_GBC = aggregate(BER.T$Q2P, by=list(BER.T$surveyQ), FUN=mean, na.rm=TRUE)[,2])
indicators.T <- cbind(indicators.T, Empl = aggregate(BER.T$Q5A, by=list(BER.T$surveyQ), FUN=mean, na.rm=TRUE)[,2])

altBER <- BER.T
altBER$Q5A <- replace(altBER$Q5A, altBER$Q5A==-1,1) # replace -1 (Down) responses with 1
indicators.T <- cbind(indicators.T, Empl_turn = aggregate(altBER$Q5A, by=list(altBER$surveyQ), FUN=mean, na.rm=TRUE)[,2])

# Kan dit verander na standard error or standard deviation?
se <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x))*(length(na.omit(x))-1)) #adjust for (n-1)
indicators.T <- cbind(indicators.T, Uncert_fl.prod = aggregate(BER.T$Q3P, by=list(BER.T$surveyQ), FUN=se)[,2])
indicators.T <- cbind(indicators.T, Uncert_fl.GBC = aggregate(BER.T$Q2P, by=list(BER.T$surveyQ), FUN=se)[,2])

dups <- BER.T[duplicated(BER.T[,c("id","surveyQ")]) | duplicated(BER.T[,c("id","surveyQ")], fromLast = TRUE),]
uniBER.T <- BER.T[!duplicated(BER.T[,c("id","surveyQ")]),]

errors1 <- indicators.T[,c(1,9)]
errors2 <- indicators.T[,c(1,9)]
tel <- 2
for(i in levels(uniBER.T$id)){
    tel <- tel + 1
    #BER.T$counter <- as.numeric(BER.T$survey)
    exp.error <- indicators.T[,c(1,9)]
    data <- subset(uniBER.T, uniBER.T$id==i)
    #data <- data[order(data$counter),]
    exp.error <- merge(exp.error, data, by.x="Date",by.y="surveyQ", all.x = TRUE)
    for(t in 1:(nrow(exp.error))) {
        exp.error$error1[t] <- exp.error$Q3A[t+1] - exp.error$Q3P[t]
        exp.error$error2[t] <- exp.error$Q2A[t+1] - exp.error$Q2P[t]
    }
    errors1 <- cbind(errors1, exp.error$error1)
    colnames(errors1)[tel] <- as.character(i)
    errors2 <- cbind(errors2, exp.error$error2)
    colnames(errors2)[tel] <- as.character(i)
}

# Kan dit verander na standard error or standard deviation?
uncert <- transform(errors1, SD=apply(errors1[,c(-1,-2)],1,se))[,c(1,ncol(errors1)+1)]
indicators.T <- cbind(indicators.T, Uncert_ee.prod = uncert[,2])    
uncert <- transform(errors2, SD=apply(errors2[,c(-1,-2)],1,se))[,c(1,ncol(errors2)+1)]
indicators.T <- cbind(indicators.T, Uncert_ee.GBC = uncert[,2])

indicator_plot <- indicators.T[,c(1,2,4,6)]
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_point(size = 3) 
g <- g + geom_line()
g <- g + ylab("Indicator")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank())
g

indicator_plot <- indicators.T[,c(1,9,10,11,12)]
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_point(size = 3) 
g <- g + geom_line()
g <- g + ylab("Indicator")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank())
g

# Calculate Response Rates
countNR <- function(data) { sum(is.na(data))/NROW(data) }
NRR.T <- aggregate(BER.T, by=list(BER.T$surveyQ), FUN=countNR)

##=====================================##
## READING IN THE DATA: Motor Vehicles ##
##=====================================##
BER.V <- read.csv("Motor.csv", header=TRUE, sep=";",na.strings = "", skipNul = TRUE)
colnames(BER.V)[1:6] <- c("region","id","sector","weight","factor","surveyQ")

BER.V$surveyQ <- toupper(BER.V$surveyQ)
BER.V[nrow(BER.V)+1,1:5] <- BER.V[nrow(BER.V),1:5] 
BER.V[nrow(BER.V),"surveyQ"] <- "05Q4" 

BER.V$region <- factor(BER.V$region)
BER.V$sector <- factor(BER.V$sector) #could include labels
BER.V$id <- factor(BER.V$id)
BER.V$surveyQ <- factor(BER.V$surveyQ)
#BER.V$surveyQ <- factor(BER.V$surveyQ, levels=c(levels(BER.V$surveyQ),"05Q4"))

# replace 1,2,3 (Up, Same, Down) responses with 1,0,-1
for(i in 7:28) {
    BER.V[,i] <- replace(BER.V[,i], BER.V[,i]==2, 0)
    BER.V[,i] <- replace(BER.V[,i], BER.V[,i]==3,-1)
}
BER.V$Q1 <- replace(BER.V$Q1, BER.V$Q1==0,-1) # replace 0 (Unsatisfactory) responses with -1
BER.V$Q6 <- replace(BER.V$Q6, BER.V$Q6==0,-1) # replace 0 (Unsatisfactory) responses with -1
BER.V$Q10 <- replace(BER.V$Q10, BER.V$Q10==0,-1) # replace 0 (Unsatisfactory) responses with -1

##======================================##
## CALCULATE INDICATORS: Motor Vehicles ##
##======================================##
indicators.V <- aggregate(BER.V$Q1, by=list(BER.V$surveyQ), FUN=mean, na.rm=TRUE)
colnames(indicators.V) <- c("Date","Conf_cc.new")
indicators.V <- cbind(indicators.V, Conf_cc.used = aggregate(BER.V$Q6, by=list(BER.V$surveyQ), FUN=mean, na.rm=TRUE)[,2])
indicators.V <- cbind(indicators.V, Conf_cc.spare = aggregate(BER.V$Q10, by=list(BER.V$surveyQ), FUN=mean, na.rm=TRUE)[,2])

indicators.V <- cbind(indicators.V, Act_prod.new = aggregate(BER.V$Q3A, by=list(BER.V$surveyQ), FUN=mean, na.rm=TRUE)[,2])
indicators.V <- cbind(indicators.V, Act_prod.used = aggregate(BER.V$Q8A, by=list(BER.V$surveyQ), FUN=mean, na.rm=TRUE)[,2])
indicators.V <- cbind(indicators.V, Act_prod.spare = aggregate(BER.V$Q12A, by=list(BER.V$surveyQ), FUN=mean, na.rm=TRUE)[,2])

indicators.V <- cbind(indicators.V, Conf_prod.new = aggregate(BER.V$Q3P, by=list(BER.V$surveyQ), FUN=mean, na.rm=TRUE)[,2])
indicators.V <- cbind(indicators.V, Conf_prod.used = aggregate(BER.V$Q8P, by=list(BER.V$surveyQ), FUN=mean, na.rm=TRUE)[,2])
indicators.V <- cbind(indicators.V, Conf_prod.spare = aggregate(BER.V$Q12P, by=list(BER.V$surveyQ), FUN=mean, na.rm=TRUE)[,2])

indicators.V <- cbind(indicators.V, Act_GBC.new = aggregate(BER.V$Q2A, by=list(BER.V$surveyQ), FUN=mean, na.rm=TRUE)[,2])
indicators.V <- cbind(indicators.V, Act_GBC.used = aggregate(BER.V$Q7A, by=list(BER.V$surveyQ), FUN=mean, na.rm=TRUE)[,2])
indicators.V <- cbind(indicators.V, Act_GBC.spare = aggregate(BER.V$Q11A, by=list(BER.V$surveyQ), FUN=mean, na.rm=TRUE)[,2])

indicators.V <- cbind(indicators.V, Conf_GBC.new = aggregate(BER.V$Q2P, by=list(BER.V$surveyQ), FUN=mean, na.rm=TRUE)[,2])
indicators.V <- cbind(indicators.V, Conf_GBC.used = aggregate(BER.V$Q7P, by=list(BER.V$surveyQ), FUN=mean, na.rm=TRUE)[,2])
indicators.V <- cbind(indicators.V, Conf_GBC.spare = aggregate(BER.V$Q11P, by=list(BER.V$surveyQ), FUN=mean, na.rm=TRUE)[,2])

# Kan dit verander na standard error or standard deviation?
se <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x))*(length(na.omit(x))-1)) #adjust for (n-1)
indicators.V <- cbind(indicators.V, Uncert_fl.prod.new = aggregate(BER.V$Q3P, by=list(BER.V$surveyQ), FUN=se)[,2])
indicators.V <- cbind(indicators.V, Uncert_fl.prod.used = aggregate(BER.V$Q8P, by=list(BER.V$surveyQ), FUN=se)[,2])
indicators.V <- cbind(indicators.V, Uncert_fl.prod.spare = aggregate(BER.V$Q12P, by=list(BER.V$surveyQ), FUN=se)[,2])

indicators.V <- cbind(indicators.V, Uncert_fl.GBC.new = aggregate(BER.V$Q2P, by=list(BER.V$surveyQ), FUN=se)[,2])
indicators.V <- cbind(indicators.V, Uncert_fl.GBC.used = aggregate(BER.V$Q7P, by=list(BER.V$surveyQ), FUN=se)[,2])
indicators.V <- cbind(indicators.V, Uncert_fl.GBC.spare = aggregate(BER.V$Q11P, by=list(BER.V$surveyQ), FUN=se)[,2])


dups <- BER.V[duplicated(BER.V[,c("id","surveyQ")]) | duplicated(BER.V[,c("id","surveyQ")], fromLast = TRUE),]
uniBER.V <- BER.V[!duplicated(BER.V[,c("id","surveyQ")]),]

errors1 <- indicators.V[,c(1,9)]
errors2 <- indicators.V[,c(1,9)]
errors3 <- indicators.V[,c(1,9)]
errors4 <- indicators.V[,c(1,9)]
errors5 <- indicators.V[,c(1,9)]
errors6 <- indicators.V[,c(1,9)]

tel <- 2
for(i in levels(uniBER.V$id)){
    tel <- tel + 1
    #BER.V$counter <- as.numeric(BER.V$survey)
    exp.error <- indicators.V[,c(1,9)]
    data <- subset(uniBER.V, uniBER.V$id==i)
    #data <- data[order(data$counter),]
    exp.error <- merge(exp.error, data, by.x="Date",by.y="surveyQ", all.x = TRUE)
    for(t in 1:(nrow(exp.error))) {
        exp.error$error1[t] <- exp.error$Q3A[t+1] - exp.error$Q3P[t]
        exp.error$error2[t] <- exp.error$Q2A[t+1] - exp.error$Q2P[t]
        exp.error$error3[t] <- exp.error$Q8A[t+1] - exp.error$Q8P[t]
        exp.error$error4[t] <- exp.error$Q7A[t+1] - exp.error$Q7P[t]
        exp.error$error5[t] <- exp.error$Q12A[t+1] - exp.error$Q12P[t]
        exp.error$error6[t] <- exp.error$Q11A[t+1] - exp.error$Q11P[t]
    }
    errors1 <- cbind(errors1, exp.error$error1)
    colnames(errors1)[tel] <- as.character(i)
    errors2 <- cbind(errors2, exp.error$error2)
    colnames(errors2)[tel] <- as.character(i)
    errors3 <- cbind(errors3, exp.error$error3)
    colnames(errors3)[tel] <- as.character(i)
    errors4 <- cbind(errors4, exp.error$error4)
    colnames(errors4)[tel] <- as.character(i)
    errors5 <- cbind(errors5, exp.error$error5)
    colnames(errors5)[tel] <- as.character(i)
    errors6 <- cbind(errors6, exp.error$error6)
    colnames(errors6)[tel] <- as.character(i)
}

# Kan dit verander na standard error or standard deviation?
uncert <- transform(errors1, SD=apply(errors1[,c(-1,-2)],1,se))[,c(1,ncol(errors1)+1)]
indicators.V <- cbind(indicators.V, Uncert_ee.prod.new = uncert[,2])    
uncert <- transform(errors2, SD=apply(errors2[,c(-1,-2)],1,se))[,c(1,ncol(errors2)+1)]
indicators.V <- cbind(indicators.V, Uncert_ee.GBC.new = uncert[,2])
uncert <- transform(errors1, SD=apply(errors3[,c(-1,-2)],1,se))[,c(1,ncol(errors3)+1)]
indicators.V <- cbind(indicators.V, Uncert_ee.prod.used = uncert[,2])    
uncert <- transform(errors2, SD=apply(errors4[,c(-1,-2)],1,se))[,c(1,ncol(errors4)+1)]
indicators.V <- cbind(indicators.V, Uncert_ee.GBC.used = uncert[,2])
uncert <- transform(errors1, SD=apply(errors5[,c(-1,-2)],1,se))[,c(1,ncol(errors5)+1)]
indicators.V <- cbind(indicators.V, Uncert_ee.prod.spare = uncert[,2])    
uncert <- transform(errors2, SD=apply(errors6[,c(-1,-2)],1,se))[,c(1,ncol(errors6)+1)]
indicators.V <- cbind(indicators.V, Uncert_ee.GBC.spare = uncert[,2])

indicator_plot <- indicators.V[,c(1,2,3,4,8,9,10)]
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_point(size = 3) 
g <- g + geom_line()
g <- g + ylab("Indicator")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank())
g

indicator_plot <- indicators.V[,c(1,17:26)]
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_point(size = 3) 
g <- g + geom_line()
g <- g + ylab("Indicator")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank())
g

# Calculate Response Rates
countNR <- function(data) { sum(is.na(data))/NROW(data) }
NRR.V <- aggregate(BER.V, by=list(BER.V$surveyQ), FUN=countNR)


##===============================##
## READING IN THE DATA: SERVICES ##
##===============================##
BER.S <- read.csv("Services.csv", header=TRUE, sep=";",na.strings = "", skipNul = TRUE)
colnames(BER.S)[1:6] <- c("region","id","sector","weight","factor","surveyQ")

BER.S$surveyQ <- toupper(BER.S$surveyQ)
BER.S[nrow(BER.S)+1,1:5] <- BER.S[nrow(BER.S),1:5] 
BER.S[nrow(BER.S),"surveyQ"] <- "05Q4" 

BER.S$region <- factor(BER.S$region)
BER.S$sector <- factor(BER.S$sector) #could include labels
BER.S$id <- factor(BER.S$id)
BER.S$surveyQ <- factor(BER.S$surveyQ)
#BER.S$surveyQ <- factor(BER.S$surveyQ, levels=c(levels(BER.S$surveyQ),"05Q4"))

# replace 1,2,3 (Up, Same, Down) responses with 1,0,-1
for(i in 7:21) {
    BER.S[,i] <- replace(BER.S[,i], BER.S[,i]==2, 0)
    BER.S[,i] <- replace(BER.S[,i], BER.S[,i]==3,-1)
}
BER.S$Q1 <- replace(BER.S$Q1, BER.S$Q1==0,-1) # replace 0 (Unsatisfactory) responses with -1

##================================##
## CALCULATE INDICATORS: SERVICES ##
##================================##
indicators.S <- aggregate(BER.S$Q1, by=list(BER.S$surveyQ), FUN=mean, na.rm=TRUE)
colnames(indicators.S) <- c("Date","Conf_cc")
indicators.S <- cbind(indicators.S, Act_prod = aggregate(BER.S$Q3A, by=list(BER.S$surveyQ), FUN=mean, na.rm=TRUE)[,2])
indicators.S <- cbind(indicators.S, Conf_prod = aggregate(BER.S$Q3P, by=list(BER.S$surveyQ), FUN=mean, na.rm=TRUE)[,2])
indicators.S <- cbind(indicators.S, Act_GBC = aggregate(BER.S$Q2A, by=list(BER.S$surveyQ), FUN=mean, na.rm=TRUE)[,2])
indicators.S <- cbind(indicators.S, Conf_GBC = aggregate(BER.S$Q2P, by=list(BER.S$surveyQ), FUN=mean, na.rm=TRUE)[,2])
indicators.S <- cbind(indicators.S, Empl = aggregate(BER.S$Q4A, by=list(BER.S$surveyQ), FUN=mean, na.rm=TRUE)[,2])

altBER <- BER.S
altBER$Q4A <- replace(altBER$Q4A, altBER$Q4A==-1,1) # replace -1 (Down) responses with 1
indicators.S <- cbind(indicators.S, Empl_turn = aggregate(altBER$Q4A, by=list(altBER$surveyQ), FUN=mean, na.rm=TRUE)[,2])

# Kan dit verander na standard error or standard deviation?
se <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x))*(length(na.omit(x))-1)) #adjust for (n-1)
indicators.S <- cbind(indicators.S, Uncert_fl.prod = aggregate(BER.S$Q3P, by=list(BER.S$surveyQ), FUN=se)[,2])
indicators.S <- cbind(indicators.S, Uncert_fl.GBC = aggregate(BER.S$Q2P, by=list(BER.S$surveyQ), FUN=se)[,2])

dups <- BER.S[duplicated(BER.S[,c("id","surveyQ")]) | duplicated(BER.S[,c("id","surveyQ")], fromLast = TRUE),]
uniBER.S <- BER.S[!duplicated(BER.S[,c("id","surveyQ")]),]

errors1 <- indicators.S[,c(1,9)]
errors2 <- indicators.S[,c(1,9)]
tel <- 2
for(i in levels(uniBER.S$id)){
    tel <- tel + 1
    #BER.S$counter <- as.numeric(BER.S$survey)
    exp.error <- indicators.S[,c(1,9)]
    data <- subset(uniBER.S, uniBER.S$id==i)
    #data <- data[order(data$counter),]
    exp.error <- merge(exp.error, data, by.x="Date",by.y="surveyQ", all.x = TRUE)
    for(t in 1:(nrow(exp.error))) {
        exp.error$error1[t] <- exp.error$Q3A[t+1] - exp.error$Q3P[t]
        exp.error$error2[t] <- exp.error$Q2A[t+1] - exp.error$Q2P[t]
    }
    errors1 <- cbind(errors1, exp.error$error1)
    colnames(errors1)[tel] <- as.character(i)
    errors2 <- cbind(errors2, exp.error$error2)
    colnames(errors2)[tel] <- as.character(i)
}

# Kan dit verander na standard error or standard deviation?
uncert <- transform(errors1, SD=apply(errors1[,c(-1,-2)],1,se))[,c(1,ncol(errors1)+1)]
indicators.S <- cbind(indicators.S, Uncert_ee.prod = uncert[,2])    
uncert <- transform(errors2, SD=apply(errors2[,c(-1,-2)],1,se))[,c(1,ncol(errors2)+1)]
indicators.S <- cbind(indicators.S, Uncert_ee.GBC = uncert[,2])

indicator_plot <- indicators.S[,c(1,2,4,6)]
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_point(size = 3) 
g <- g + geom_line()
g <- g + ylab("Indicator")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank())
g

indicator_plot <- indicators.S[,c(1,9,10,11,12)]
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_point(size = 3) 
g <- g + geom_line()
g <- g + ylab("Indicator")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank())
g

# Calculate Response Rates
countNR <- function(data) { sum(is.na(data))/NROW(data) }
NRR.S <- aggregate(BER.S, by=list(BER.S$surveyQ), FUN=countNR)


##=================================##
## AGGREGATING as much as possible ##
##=================================##

#Rename BER.B$Q5A temporarily and create NAs for BER.V$empl
tempBER.M <- BER.M[,c("id","surveyQ","Q20","Q3A","Q3P","Q7A","Q7P","Q8A")]
colnames(tempBER.M) <- c("id","surveyQ","Q1","Q3A","Q3P","Q2A","Q2P","Q4A")
tempBER.T <- BER.T[,c("id","surveyQ","Q1","Q3A","Q3P","Q2A","Q2P","Q5A")]
colnames(tempBER.T) <- c("id","surveyQ","Q1","Q3A","Q3P","Q2A","Q2P","Q4A")
tempBER.V <- BER.V[,c("id","surveyQ","Q1","Q3A","Q3P","Q2A","Q2P","Q4A")]
tempBER.V[,"Q4A"] <- NA

BER <- tempBER.M
BER <- rbind(BER,BER.B[,c("id","surveyQ","Q1","Q3A","Q3P","Q2A","Q2P","Q4A")],tempBER.T,tempBER.V,
             BER.S[,c("id","surveyQ","Q1","Q3A","Q3P","Q2A","Q2P","Q4A")])

indicators <- aggregate(BER$Q1, by=list(BER$surveyQ), FUN=mean, na.rm=TRUE)
colnames(indicators) <- c("Date","Conf_cc")
indicators <- cbind(indicators, Act_prod = aggregate(BER$Q3A, by=list(BER$surveyQ), FUN=mean, na.rm=TRUE)[,2])
indicators <- cbind(indicators, Conf_prod = aggregate(BER$Q3P, by=list(BER$surveyQ), FUN=mean, na.rm=TRUE)[,2])
indicators <- cbind(indicators, Act_GBC = aggregate(BER$Q2A, by=list(BER$surveyQ), FUN=mean, na.rm=TRUE)[,2])
indicators <- cbind(indicators, Conf_GBC = aggregate(BER$Q2P, by=list(BER$surveyQ), FUN=mean, na.rm=TRUE)[,2])
indicators <- cbind(indicators, Empl = aggregate(BER$Q4A, by=list(BER$surveyQ), FUN=mean, na.rm=TRUE)[,2])
altBER <- BER
altBER$Q4A <- replace(altBER$Q4A, altBER$Q4A==-1,1) # replace -1 (Down) responses with 1
indicators <- cbind(indicators, Empl_turn = aggregate(altBER$Q4A, by=list(altBER$surveyQ), FUN=mean, na.rm=TRUE)[,2])

# Kan dit verander na standard error or standard deviation?
se <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x))*(length(na.omit(x))-1)) #adjust for (n-1)
indicators <- cbind(indicators, Uncert_fl.prod = aggregate(BER$Q3P, by=list(BER$surveyQ), FUN=se)[,2])
indicators <- cbind(indicators, Uncert_fl.GBC = aggregate(BER$Q2P, by=list(BER$surveyQ), FUN=se)[,2])

#The expectations of firms in question 31 in period t are compared to the realization of firms in question 7A in period t+4. 
#Also compare the expectations of firms in questions 3P & 7P in period t to the realizations in questions 3A & 7A in period t+1. 
dups <- BER[duplicated(BER[,c("id","surveyQ")]) | duplicated(BER[,c("id","surveyQ")], fromLast = TRUE),]
uniBER <- BER[!duplicated(BER[,c("id","surveyQ")]),]

errors1 <- indicators[,c(1,9)]
errors2 <- indicators[,c(1,9)]
tel <- 2
for(i in levels(uniBER$id)){
    tel <- tel + 1
    #BER.S$counter <- as.numeric(BER.S$survey)
    exp.error <- indicators[,c(1,9)]
    data <- subset(uniBER, uniBER$id==i)
    #data <- data[order(data$counter),]
    exp.error <- merge(exp.error, data, by.x="Date",by.y="surveyQ", all.x = TRUE)
    for(t in 1:(nrow(exp.error))) {
        exp.error$error1[t] <- exp.error$Q3A[t+1] - exp.error$Q3P[t]
        exp.error$error2[t] <- exp.error$Q2A[t+1] - exp.error$Q2P[t]
    }
    errors1 <- cbind(errors1, exp.error$error1)
    colnames(errors1)[tel] <- as.character(i)
    errors2 <- cbind(errors2, exp.error$error2)
    colnames(errors2)[tel] <- as.character(i)
}

# Kan dit verander na standard error or standard deviation?
uncert <- transform(errors1, SD=apply(errors1[,c(-1,-2)],1,se))[,c(1,ncol(errors1)+1)]
indicators <- cbind(indicators, Uncert_ee.prod = uncert[,2])    
uncert <- transform(errors2, SD=apply(errors2[,c(-1,-2)],1,se))[,c(1,ncol(errors2)+1)]
indicators <- cbind(indicators, Uncert_ee.GBC = uncert[,2])


indicator_plot <- indicators[,c(1,2,4,6)]
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_point(size = 3) 
g <- g + geom_line()
g <- g + ylab("Indicator")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank())
g

indicator_plot <- indicators[,c(1,9,10,11,12)]
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_point(size = 3) 
g <- g + geom_line()
g <- g + ylab("Indicator")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank())
g

# Calculate Response Rates
countNR <- function(data) { sum(is.na(data))/NROW(data) }
NRR <- aggregate(BER, by=list(BER$surveyQ), FUN=countNR)

NRR_plot <- NRR[,c(1,4,5,6,7,8,9)]
NRR_plot <- melt(NRR_plot, id="Group.1")  # convert to long format
g <- ggplot(data=NRR_plot,aes(x=Group.1, y=value, group=variable, colour=variable)) 
g <- g + geom_point(size = 3) 
g <- g + geom_line()
g <- g + ylab("Indicator")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank())
g

#====================================================#
# ------------------ VAR ANALYSIS ------------------ #
#====================================================#

library(vars)

VAR(y, p = 1, type = c("const", "trend", "both", "none"),
    season = NULL, exogen = NULL, lag.max = NULL,
    ic = c("AIC", "HQ", "SC", "FPE"))

SVAR(x, estmethod = c("scoring", "direct"), Amat = NULL, Bmat = NULL,
     start = NULL, max.iter = 100, conv.crit = 1e-07, maxls = 1,
     lrtest = TRUE, ...)

arch.test(x, lags.single = 16, lags.multi = 5, multivariate.only = TRUE)

normality.test(x, multivariate.only = TRUE)

serial.test(x, lags.pt = 16, lags.bg = 5,
            type = c("PT.asymptotic", "PT.adjusted", "BG", "ES"))

stability(x, type = c("OLS-CUSUM", "Rec-CUSUM", "Rec-MOSUM",
                      "OLS-MOSUM", "RE", "ME", "Score-CUSUM", "Score-MOSUM", "fluctuation"),
          h = 0.15, dynamic = FALSE, rescale = TRUE)

predict(object, ..., n.ahead = 10, ci = 0.95, dumvar = NULL)

fanchart(x, colors = NULL, cis = NULL, names = NULL, main = NULL,
         ylab = NULL, xlab = NULL, col.y = NULL, nc, 
         plot.type = c("multiple","single"), mar = par("mar"), oma = par("oma"), ...)

irf(x, impulse = NULL, response = NULL, n.ahead = 10, ortho = TRUE,
    cumulative = FALSE, boot = TRUE, ci = 0.95, runs = 100, seed = NULL, ...)

fevd(x, n.ahead = 10, ...)



