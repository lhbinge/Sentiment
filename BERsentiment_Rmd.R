##===================================================================================##
## -------------------------------- SENTIMENT ---------------------------------------##
##===================================================================================##
setwd("C:\\Users\\Laurie\\OneDrive\\Documents\\BING\\BER Confidence Surveys\\Sentiment")

suppressMessages(library(ggplot2))
suppressMessages(library(plyr))
suppressMessages(library(dplyr))
suppressMessages(library(reshape2))
suppressMessages(library(stargazer))
suppressMessages(library(xtable))
suppressMessages(library(scales))
suppressMessages(library(quantmod))
suppressMessages(library(vars))
suppressMessages(library(tseries))
suppressMessages(library(urca))

GDPdata <- read.csv("GDP Data.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)
GDPdata$X <- as.Date(GDPdata$X, format = "%Y/%m/%d")

##For Grpahing Business cycles
recessions.df = read.table(textConnection(
    "Peak, Trough
    1989-02-28, 1993-05-30
    1996-11-30, 1999-08-31
    2007-11-30, 2009-08-31"), sep=',',
    colClasses=c('Date', 'Date'), header=TRUE)

##====================================================================================##
## -------------------------------- CONFIDENCE ---------------------------------------##
##====================================================================================##

##====================================##
## READING IN THE DATA: MANUFACTURING ##
##====================================##
BER.M <- rbind.fill(read.csv("Manufacturing.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE),
                    read.csv("Manufacturing_pre2001.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE))
BER.M <- BER.M[,1:62]
colnames(BER.M)[1:7] <- c("region","id","sector","weight","turnover","factor","surveyQ")

BER.M$surveyQ <- toupper(BER.M$surveyQ)
BER.M[nrow(BER.M)+1,1:6] <- BER.M[nrow(BER.M),1:6] 
BER.M[nrow(BER.M),"surveyQ"] <- "2005Q4" 
BER.M[nrow(BER.M)+1,1:6] <- BER.M[nrow(BER.M),1:6] 
BER.M[nrow(BER.M),"surveyQ"] <- "1997Q4" 
BER.M[nrow(BER.M)+1,1:6] <- BER.M[nrow(BER.M),1:6] 
BER.M[nrow(BER.M),"surveyQ"] <- "2000Q1" 

BER.M$region <- factor(BER.M$region)
BER.M$sector <- factor(BER.M$sector) #could include labels
BER.M$id <- factor(BER.M$id)
BER.M$surveyQ <- factor(BER.M$surveyQ)

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


##===============================##
## READING IN THE DATA: BUILDING ##
##===============================##
#BER.B1 <- read.csv("Building.csv", header=TRUE, sep=";",na.strings = "", skipNul = TRUE)
#BER.B2 <- read.csv("Building_pre2001.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)
BER.B <- rbind.fill(read.csv("Building.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE),
                    read.csv("Building_pre2001.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE))
BER.B <- BER.B[,1:22]
colnames(BER.B)[1:6] <- c("region","id","sector","weight","factor","surveyQ")

BER.B$surveyQ <- toupper(BER.B$surveyQ)
BER.B[nrow(BER.B)+1,1:5] <- BER.B[nrow(BER.B),1:5] 
BER.B[nrow(BER.B),"surveyQ"] <- "2005Q4" 
BER.B[nrow(BER.B)+1,1:5] <- BER.B[nrow(BER.B),1:5] 
BER.B[nrow(BER.B),"surveyQ"] <- "1998Q3" 
BER.B[nrow(BER.B)+1,1:5] <- BER.B[nrow(BER.B),1:5] 
BER.B[nrow(BER.B),"surveyQ"] <- "1993Q4" 
BER.B[nrow(BER.B)+1,1:5] <- BER.B[nrow(BER.B),1:5] 
BER.B[nrow(BER.B),"surveyQ"] <- "2000Q2" 

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


##Weighted versions---------------------------------------------------------------------------------
w.Conf_cc <- NULL
w.Conf_fl <- NULL
w.Act_prod <- NULL
w.Conf_prod <- NULL
w.Act_GBC <- NULL
w.Conf_GBC <- NULL
w.Invest <- NULL
w.Empl <- NULL
i <- 0
for(kwartaal in levels(BER.M$surveyQ)) {
    i <- i+1
    temp <- subset(BER.M,BER.M$surveyQ==kwartaal)
    w.Conf_cc[i] <- sum(temp$Q20*temp$factor, na.rm=TRUE)/sum(temp$factor[!is.na(temp$Q20)], na.rm=TRUE)
    w.Conf_fl[i] <- sum(temp$Q31*temp$factor, na.rm=TRUE)/sum(temp$factor[!is.na(temp$Q31)], na.rm=TRUE)
    w.Act_prod[i] <- sum(temp$Q3A*temp$factor, na.rm=TRUE)/sum(temp$factor[!is.na(temp$Q3A)], na.rm=TRUE)
    w.Conf_prod[i] <- sum(temp$Q3P*temp$factor, na.rm=TRUE)/sum(temp$factor[!is.na(temp$Q3P)], na.rm=TRUE)
    w.Act_GBC[i] <- sum(temp$Q7A*temp$factor, na.rm=TRUE)/sum(temp$factor[!is.na(temp$Q7A)], na.rm=TRUE)
    w.Conf_GBC[i] <- sum(temp$Q7P*temp$factor, na.rm=TRUE)/sum(temp$factor[!is.na(temp$Q7P)], na.rm=TRUE)
    w.Invest[i] <- sum(temp$Q10A*temp$factor, na.rm=TRUE)/sum(temp$factor[!is.na(temp$Q10A)], na.rm=TRUE)
    w.Empl[i] <- sum(temp$Q8A*temp$factor, na.rm=TRUE)/sum(temp$factor[!is.na(temp$Q8A)], na.rm=TRUE)
}
w.indicators.M <- as.data.frame(cbind(w.Conf_cc,w.Conf_fl,w.Act_prod,w.Conf_prod,w.Act_GBC,w.Conf_GBC,w.Invest,w.Empl))
w.indicators.M <- cbind(Date=levels(BER.M$surveyQ),w.indicators.M)



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

##Weighted versions--------------------------------------------------------------------------------------------------------------------
w.Conf_cc <- NULL
w.Act_prod <- NULL
w.Conf_prod <- NULL
w.Act_GBC <- NULL
w.Conf_GBC <- NULL
w.Empl <- NULL
i <- 0
for(kwartaal in levels(BER.B$surveyQ)) {
    i <- i+1
    temp <- subset(BER.B,BER.B$surveyQ==kwartaal)
    w.Conf_cc[i] <- sum(temp$Q1*temp$factor, na.rm=TRUE)/sum(temp$factor[!is.na(temp$Q1)], na.rm=TRUE)
    w.Act_prod[i] <- sum(temp$Q3A*temp$factor, na.rm=TRUE)/sum(temp$factor[!is.na(temp$Q3A)], na.rm=TRUE)
    w.Conf_prod[i] <- sum(temp$Q3P*temp$factor, na.rm=TRUE)/sum(temp$factor[!is.na(temp$Q3P)], na.rm=TRUE)
    w.Act_GBC[i] <- sum(temp$Q2A*temp$factor, na.rm=TRUE)/sum(temp$factor[!is.na(temp$Q2A)], na.rm=TRUE)
    w.Conf_GBC[i] <- sum(temp$Q2P*temp$factor, na.rm=TRUE)/sum(temp$factor[!is.na(temp$Q2P)], na.rm=TRUE)
    w.Empl[i] <- sum(temp$Q4A*temp$factor, na.rm=TRUE)/sum(temp$factor[!is.na(temp$Q4A)], na.rm=TRUE)
}
w.indicators.B <- as.data.frame(cbind(w.Conf_cc,w.Act_prod,w.Conf_prod,w.Act_GBC,w.Conf_GBC,w.Empl))
w.indicators.B <- cbind(Date=levels(BER.B$surveyQ),w.indicators.B)

##============================##
## READING IN THE DATA: TRADE ##
##============================##
BER.R <- read.csv("Retail.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)
BER.W <- read.csv("Wholesale.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)
BER.T <- rbind(BER.R,BER.W)
BER.T <- rbind.fill(BER.T,read.csv("Trade_pre2001.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE))
BER.T <- BER.T[,1:21]
colnames(BER.T)[1:6] <- c("region","id","sector","weight","factor","surveyQ")

BER.T$surveyQ <- toupper(BER.T$surveyQ)
BER.T[nrow(BER.T)+1,1:5] <- BER.T[nrow(BER.T),1:5] 
BER.T[nrow(BER.T),"surveyQ"] <- "2005Q4" 
BER.T[nrow(BER.T)+1,1:5] <- BER.T[nrow(BER.T),1:5] 
BER.T[nrow(BER.T),"surveyQ"] <- "1993Q3"
BER.T[nrow(BER.T)+1,1:5] <- BER.T[nrow(BER.T),1:5] 
BER.T[nrow(BER.T),"surveyQ"] <- "1992Q4"

BER.T$region <- factor(BER.T$region)
BER.T$sector <- factor(BER.T$sector) #could include labels
BER.T$id <- factor(BER.T$id)
BER.T$surveyQ <- factor(BER.T$surveyQ)

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

##Weighted versions--------------------------------------------------------------------------------------------
w.Conf_cc <- NULL
w.Act_prod <- NULL
w.Conf_prod <- NULL
w.Act_GBC <- NULL
w.Conf_GBC <- NULL
w.Empl <- NULL
i <- 0
for(kwartaal in levels(BER.T$surveyQ)) {
    i <- i+1
    temp <- subset(BER.T,BER.T$surveyQ==kwartaal)
    w.Conf_cc[i] <- sum(temp$Q1*temp$factor, na.rm=TRUE)/sum(temp$factor[!is.na(temp$Q1)], na.rm=TRUE)
    w.Act_prod[i] <- sum(temp$Q3A*temp$factor, na.rm=TRUE)/sum(temp$factor[!is.na(temp$Q3A)], na.rm=TRUE)
    w.Conf_prod[i] <- sum(temp$Q3P*temp$factor, na.rm=TRUE)/sum(temp$factor[!is.na(temp$Q3P)], na.rm=TRUE)
    w.Act_GBC[i] <- sum(temp$Q2A*temp$factor, na.rm=TRUE)/sum(temp$factor[!is.na(temp$Q2A)], na.rm=TRUE)
    w.Conf_GBC[i] <- sum(temp$Q2P*temp$factor, na.rm=TRUE)/sum(temp$factor[!is.na(temp$Q2P)], na.rm=TRUE)
    w.Empl[i] <- sum(temp$Q5A*temp$factor, na.rm=TRUE)/sum(temp$factor[!is.na(temp$Q5A)], na.rm=TRUE)
}
w.indicators.T <- as.data.frame(cbind(w.Conf_cc,w.Act_prod,w.Conf_prod,w.Act_GBC,w.Conf_GBC,w.Empl))
w.indicators.T <- cbind(Date=levels(BER.T$surveyQ),w.indicators.T)

##=====================================##
## READING IN THE DATA: Motor Vehicles ##
##=====================================##
BER.V <- rbind.fill(read.csv("Motor.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE),
                    read.csv("Motor_pre2001.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE))
BER.V <- BER.V[,1:28]
colnames(BER.V)[1:6] <- c("region","id","sector","weight","factor","surveyQ")

BER.V$surveyQ <- toupper(BER.V$surveyQ)
BER.V[nrow(BER.V)+1,1:5] <- BER.V[nrow(BER.V),1:5] 
BER.V[nrow(BER.V),"surveyQ"] <- "2005Q4"
BER.V[nrow(BER.V)+1,1:5] <- BER.V[nrow(BER.V),1:5] 
BER.V[nrow(BER.V),"surveyQ"] <- "1992Q4"
BER.V[nrow(BER.V)+1,1:5] <- BER.V[nrow(BER.V),1:5] 
BER.V[nrow(BER.V),"surveyQ"] <- "1993Q3"

BER.V$region <- factor(BER.V$region)
BER.V$sector <- factor(BER.V$sector) #could include labels
BER.V$id <- factor(BER.V$id)
BER.V$surveyQ <- factor(BER.V$surveyQ)

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

##Weighted versions--------------------------------------------------------------------------------------------------------------------
w.Conf_cc <- NULL
w.Act_prod <- NULL
w.Conf_prod <- NULL
w.Act_GBC <- NULL
w.Conf_GBC <- NULL
i <- 0
for(kwartaal in levels(BER.V$surveyQ)) {
    i <- i+1
    temp <- subset(BER.V,BER.V$surveyQ==kwartaal)
    w.Conf_cc[i] <- sum(temp$Q1*temp$factor, na.rm=TRUE)/sum(temp$factor[!is.na(temp$Q1)], na.rm=TRUE)
    w.Act_prod[i] <- sum(temp$Q3A*temp$factor, na.rm=TRUE)/sum(temp$factor[!is.na(temp$Q3A)], na.rm=TRUE)
    w.Conf_prod[i] <- sum(temp$Q3P*temp$factor, na.rm=TRUE)/sum(temp$factor[!is.na(temp$Q3P)], na.rm=TRUE)
    w.Act_GBC[i] <- sum(temp$Q2A*temp$factor, na.rm=TRUE)/sum(temp$factor[!is.na(temp$Q2A)], na.rm=TRUE)
    w.Conf_GBC[i] <- sum(temp$Q2P*temp$factor, na.rm=TRUE)/sum(temp$factor[!is.na(temp$Q2P)], na.rm=TRUE)
}
w.indicators.V <- as.data.frame(cbind(w.Conf_cc,w.Act_prod,w.Conf_prod,w.Act_GBC,w.Conf_GBC,w.Empl))
w.indicators.V <- cbind(Date=levels(BER.V$surveyQ),w.indicators.V)

##===============================##
## READING IN THE DATA: SERVICES ##
##===============================##
BER.S <- read.csv("Services.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)
colnames(BER.S)[1:6] <- c("region","id","sector","weight","factor","surveyQ")

BER.S$surveyQ <- toupper(BER.S$surveyQ)
BER.S[nrow(BER.S)+1,1:5] <- BER.S[nrow(BER.S),1:5] 
BER.S[nrow(BER.S),"surveyQ"] <- "2005Q4" 

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

##Weighted versions--------------------------------------------------------------------------------------------------------------------
w.Conf_cc <- NULL
w.Act_prod <- NULL
w.Conf_prod <- NULL
w.Act_GBC <- NULL
w.Conf_GBC <- NULL
w.Empl <- NULL
i <- 0
for(kwartaal in levels(BER.S$surveyQ)) {
    i <- i+1
    temp <- subset(BER.S,BER.S$surveyQ==kwartaal)
    w.Conf_cc[i] <- sum(temp$Q1*temp$factor, na.rm=TRUE)/sum(temp$factor[!is.na(temp$Q1)], na.rm=TRUE)
    w.Act_prod[i] <- sum(temp$Q3A*temp$factor, na.rm=TRUE)/sum(temp$factor[!is.na(temp$Q3A)], na.rm=TRUE)
    w.Conf_prod[i] <- sum(temp$Q3P*temp$factor, na.rm=TRUE)/sum(temp$factor[!is.na(temp$Q3P)], na.rm=TRUE)
    w.Act_GBC[i] <- sum(temp$Q2A*temp$factor, na.rm=TRUE)/sum(temp$factor[!is.na(temp$Q2A)], na.rm=TRUE)
    w.Conf_GBC[i] <- sum(temp$Q2P*temp$factor, na.rm=TRUE)/sum(temp$factor[!is.na(temp$Q2P)], na.rm=TRUE)
    w.Empl[i] <- sum(temp$Q4A*temp$factor, na.rm=TRUE)/sum(temp$factor[!is.na(temp$Q4A)], na.rm=TRUE)
}
w.indicators.S <- as.data.frame(cbind(w.Conf_cc,w.Act_prod,w.Conf_prod,w.Act_GBC,w.Conf_GBC,w.Empl))
w.indicators.S <- cbind(Date=levels(BER.S$surveyQ),w.indicators.S)

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

indicators$Date <- GDPdata[,1]

##Weighted versions---------------------------------------------------------------------------------
#GDP Data
GDPdata <- read.csv("GDP Data.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)

weights <- GDPdata[,c(17,12,6)]
motor <- 0.05*weights[,3]
weights <- cbind(weights,MotorRGDP_sa=motor)
weights <- cbind(weights,SerRGDP_sa=GDPdata[,24])

colnames(w.indicators.M) <- c("Date","w.Conf_cc.M","w.Conf_fl.M","w.Act_prod.M","w.Conf_prod.M","w.Act_GBC.M","w.Conf_GBC.M","w.Invest.M","w.Empl.M")
colnames(w.indicators.B) <- c("Date","w.Conf_cc.B",              "w.Act_prod.B","w.Conf_prod.B","w.Act_GBC.B","w.Conf_GBC.B",             "w.Empl.B")
colnames(w.indicators.T) <- c("Date","w.Conf_cc.T",              "w.Act_prod.T","w.Conf_prod.T","w.Act_GBC.T","w.Conf_GBC.T",             "w.Empl.T")
colnames(w.indicators.V) <- c("Date","w.Conf_cc.V",              "w.Act_prod.V","w.Conf_prod.V","w.Act_GBC.V","w.Conf_GBC.V",             "w.Empl.V")
colnames(w.indicators.S) <- c("Date","w.Conf_cc.S",              "w.Act_prod.S","w.Conf_prod.S","w.Act_GBC.S","w.Conf_GBC.S",             "w.Empl.S")

CC <- merge(w.indicators.M, w.indicators.B, by.x="Date", by.y="Date",all.x=TRUE)
CC <- merge(CC, w.indicators.T, by.x="Date", by.y="Date",all.x=TRUE)
CC <- merge(CC, w.indicators.V, by.x="Date", by.y="Date",all.x=TRUE)
CC <- merge(CC, w.indicators.S, by.x="Date", by.y="Date",all.x=TRUE)

Conf_cc <- cbind(CC$w.Conf_cc.M,CC$w.Conf_cc.B,CC$w.Conf_cc.T,CC$w.Conf_cc.V,CC$w.Conf_cc.S)
Act_prod <- cbind(CC$w.Act_prod.M,CC$w.Act_prod.B,CC$w.Act_prod.T,CC$w.Act_prod.V,CC$w.Act_prod.S)
Conf_prod <- cbind(CC$w.Conf_prod.M,CC$w.Conf_prod.B,CC$w.Conf_prod.T,CC$w.Conf_prod.V,CC$w.Conf_prod.S)
Act_GBC <- cbind(CC$w.Act_GBC.M,CC$w.Act_GBC.B,CC$w.Act_GBC.T,CC$w.Act_GBC.V,CC$w.Act_GBC.S)
Conf_GBC <- cbind(CC$w.Conf_GBC.M,CC$w.Conf_GBC.B,CC$w.Conf_GBC.T,CC$w.Conf_GBC.V,CC$w.Conf_GBC.S)
Empl <- cbind(CC$w.Empl.M,CC$w.Empl.B,CC$w.Empl.T,CC$w.Empl.V,CC$w.Empl.S)

w.indicators <- indicators
for(i in 1:95) {
    w.indicators$Conf_cc[i] <- weighted.mean(Conf_cc[i,], weights[i,],na.rm=TRUE)
    w.indicators$Act_prod[i] <- weighted.mean(Act_prod[i,], weights[i,],na.rm=TRUE)
    w.indicators$Conf_prod[i] <- weighted.mean(Conf_prod[i,], weights[i,],na.rm=TRUE)
    w.indicators$Act_GBC[i] <- weighted.mean(Act_GBC[i,], weights[i,],na.rm=TRUE)
    w.indicators$Conf_GBC[i] <- weighted.mean(Conf_GBC[i,], weights[i,],na.rm=TRUE)
    w.indicators$Empl[i] <- weighted.mean(Empl[i,], weights[i,],na.rm=TRUE)
}

w.indicators$Date <- GDPdata[,1]


indicator_plot <- cbind(w.indicators[,c(1,2)],(GDPdata$Confidence-50)/50)
colnames(indicator_plot) <- c("Date","Current1","BER Confidence")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_line()
g <- g + ylab("Indicator")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g <- g + theme(legend.position="bottom")
g

indicator_plot <- cbind(indicators[,c(1,2)],w.indicators[,2])
colnames(indicator_plot) <- c("Date","Unweighted","Weighted")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_line()
g <- g + ylab("Indicator")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g <- g + theme(legend.position="bottom")
g

indicator_plot <- cbind(indicators[,c(1,6)],w.indicators[,6])
colnames(indicator_plot) <- c("Date","Unweighted","Weighted")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_line()
g <- g + ylab("Indicator")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g <- g + theme(legend.position="bottom")
g

indicator_plot <- indicators[,c(1,2,5,6)]
g <- ggplot(indicator_plot) 
g <- g + geom_line(aes(x=Date, y=Conf_cc, colour="Current_1"), size = 1)
g <- g + geom_line(aes(x=Date, y=Act_GBC, colour="Current_2"), size = 1)
g <- g + geom_line(aes(x=Date, y=Conf_GBC, colour="Forward-looking"), size = 1)
g <- g + theme_bw()
g <- g + labs(color="Legend text")
g <- g + geom_rect(data=recessions.df, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='grey', alpha=0.5)
g <- g + ylab("Confidence")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g <- g + theme(legend.position="bottom")
g

##====================================================================================##
## -------------------------------- UNCERTAINTY --------------------------------------##
##====================================================================================##
# Kan dit verander na standard error or standard deviation?
se <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x))*(length(na.omit(x))-1)) #adjust for (n-1)


##=====================================##
## CALCULATE INDICATORS: MANUFACTURING ##
##=====================================##
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

# Kan dit verander na standard error or standard deviation (maar maak geen verskil nie)
uncert <- transform(errors1, SD=apply(errors1[,c(-1,-2)],1,se))[,c(1,ncol(errors1)+1)]
#uncert1 <- transform(errors1, SD=apply(errors1[,c(-1,-2)],1,sd,na.rm=TRUE))[,c(1,ncol(errors1)+1)]
indicators.M <- cbind(indicators.M, Uncert_ee = uncert[,2])    
uncert <- transform(errors2, SD=apply(errors2[,c(-1,-2)],1,se))[,c(1,ncol(errors2)+1)]
indicators.M <- cbind(indicators.M, Uncert_ee.prod = uncert[,2])
uncert <- transform(errors3, SD=apply(errors3[,c(-1,-2)],1,se))[,c(1,ncol(errors3)+1)]
indicators.M <- cbind(indicators.M, Uncert_ee.GBC = uncert[,2])


##Weighted versions-----------------------------------------------------------------------------------------------
w.Uncert_fl <- NULL
w.Uncert_fl.prod <- NULL
w.Uncert_fl.GBC <- NULL
i <- 0
for(kwartaal in levels(BER.M$surveyQ)) {
    i <- i+1
    temp <- subset(BER.M,BER.M$surveyQ==kwartaal)
    fr.up <- sum((temp$Q31*temp$factor)[temp$Q31>0], na.rm=TRUE)/sum(temp$factor[!is.na(temp$Q31)],na.rm=TRUE)
    fr.down <- sum((temp$Q31*temp$factor)[temp$Q31<0], na.rm=TRUE)/sum(temp$factor[!is.na(temp$Q31)],na.rm=TRUE)
    w.Uncert_fl[i] <- sqrt(fr.up-fr.down-(fr.up+fr.down)^2)
    
    fr.up <- sum((temp$Q3P*temp$factor)[temp$Q3P>0], na.rm=TRUE)/sum(temp$factor[!is.na(temp$Q3P)],na.rm=TRUE)
    fr.down <- sum((temp$Q3P*temp$factor)[temp$Q3P<0], na.rm=TRUE)/sum(temp$factor[!is.na(temp$Q3P)],na.rm=TRUE)
    w.Uncert_fl.prod[i] <- sqrt(fr.up-fr.down-(fr.up+fr.down)^2)
    
    fr.up <- sum((temp$Q7P*temp$factor)[temp$Q7P>0], na.rm=TRUE)/sum(temp$factor[!is.na(temp$Q7P)],na.rm=TRUE)
    fr.down <- sum((temp$Q7P*temp$factor)[temp$Q7P<0], na.rm=TRUE)/sum(temp$factor[!is.na(temp$Q7P)],na.rm=TRUE)
    w.Uncert_fl.GBC[i] <- sqrt(fr.up-fr.down-(fr.up+fr.down)^2)
}
w.indicators.M <- cbind(w.indicators.M,w.Uncert_fl,w.Uncert_fl.prod,w.Uncert_fl.GBC)
#-------------------------------------------------------------------------------------------------------------------

errors1 <- w.indicators.M[,c(1,11)]
err.fac1 <- w.indicators.M[,c(1,11)]
w.fact1 <- w.indicators.M[,c(1,11)]
errors2 <- w.indicators.M[,c(1,11)]
err.fac2 <- w.indicators.M[,c(1,11)]
w.fact2 <- w.indicators.M[,c(1,11)]
errors3 <- w.indicators.M[,c(1,11)]
err.fac3 <- w.indicators.M[,c(1,11)]
w.fact3 <- w.indicators.M[,c(1,11)]
tel <- 2
for(i in levels(uniBER.M$id)){
    tel <- tel + 1
    exp.error <- w.indicators.M[,c(1,11)]
    data <- subset(uniBER.M, uniBER.M$id==i)
    exp.error <- merge(exp.error, data, by.x="Date",by.y="surveyQ", all.x = TRUE)
    for(t in 1:(nrow(exp.error))) {
        exp.error$error1[t] <- exp.error$Q7A[t+4] - exp.error$Q31[t]
        exp.error$error2[t] <- exp.error$Q3A[t+1] - exp.error$Q3P[t]
        exp.error$error3[t] <- exp.error$Q7A[t+1] - exp.error$Q7P[t]
    }
    
    errors1 <- cbind(errors1, exp.error$error1)
    err.fac1 <- cbind(err.fac1, exp.error$error1*exp.error$factor)
    w.fact1 <- cbind(w.fact1, exp.error$factor)
    errors2 <- cbind(errors2, exp.error$error2)
    err.fac2 <- cbind(err.fac2, exp.error$error2*exp.error$factor)
    w.fact2 <- cbind(w.fact2, exp.error$factor)
    errors3 <- cbind(errors3, exp.error$error3)
    err.fac3 <- cbind(err.fac3, exp.error$error3*exp.error$factor)
    w.fact3 <- cbind(w.fact3, exp.error$factor)
    
    colnames(errors1)[tel] <- as.character(i)
    colnames(err.fac1)[tel] <- as.character(i)
    colnames(w.fact1)[tel] <- as.character(i)
    colnames(errors2)[tel] <- as.character(i)
    colnames(err.fac2)[tel] <- as.character(i)
    colnames(w.fact2)[tel] <- as.character(i)
    colnames(errors3)[tel] <- as.character(i)
    colnames(err.fac3)[tel] <- as.character(i)
    colnames(w.fact3)[tel] <- as.character(i)
}

w.Uncert_ee <- NULL
w.Uncert_ee.prod <- NULL
w.Uncert_ee.GBC <- NULL
i <- 0
for(kwartaal in levels(err.fac1$Date)) {
    i <- i+1
    temp1 <- subset(err.fac1,err.fac1$Date==kwartaal)[,c(-1,-2)]
    temp2 <- subset(errors1,errors1$Date==kwartaal)[,c(-1,-2)]
    temp3 <- subset(w.fact1,w.fact1$Date==kwartaal)[,c(-1,-2)]
    temp4 <- replace(temp2, temp2==0,1)
    temp4 <- replace(temp4, temp4==-2,2)
    temp4 <- replace(temp4, temp4==-1,1)
    temp4 <- temp4*temp3
    
    fr.2 <- sum(temp1[temp2==2], na.rm=TRUE)/sum(temp4[!is.na(temp2)],na.rm=TRUE)
    fr.1 <- sum(temp1[temp2==1], na.rm=TRUE)/sum(temp4[!is.na(temp2)],na.rm=TRUE)
    fr.m1 <- sum(temp1[temp2==-1], na.rm=TRUE)/sum(temp4[!is.na(temp2)],na.rm=TRUE)
    fr.m2 <- sum(temp1[temp2==-2], na.rm=TRUE)/sum(temp4[!is.na(temp2)],na.rm=TRUE)
    w.Uncert_ee[i] <- sqrt(4*fr.2+fr.1-fr.m1-4*fr.m2-(2*fr.2+fr.1+fr.m1+2*fr.m2)^2)
    
    temp1 <- subset(err.fac2,err.fac2$Date==kwartaal)[,c(-1,-2)]
    temp2 <- subset(errors2,errors2$Date==kwartaal)[,c(-1,-2)]
    temp3 <- subset(w.fact2,w.fact2$Date==kwartaal)[,c(-1,-2)]
    temp4 <- replace(temp2, temp2==0,1)
    temp4 <- replace(temp4, temp4==-2,2)
    temp4 <- replace(temp4, temp4==-1,1)
    temp4 <- temp4*temp3
    
    fr.2 <- sum(temp1[temp2==2], na.rm=TRUE)/sum(temp4[!is.na(temp2)],na.rm=TRUE)
    fr.1 <- sum(temp1[temp2==1], na.rm=TRUE)/sum(temp4[!is.na(temp2)],na.rm=TRUE)
    fr.m1 <- sum(temp1[temp2==-1], na.rm=TRUE)/sum(temp4[!is.na(temp2)],na.rm=TRUE)
    fr.m2 <- sum(temp1[temp2==-2], na.rm=TRUE)/sum(temp4[!is.na(temp2)],na.rm=TRUE)
    w.Uncert_ee.prod[i] <- sqrt(4*fr.2+fr.1-fr.m1-4*fr.m2-(2*fr.2+fr.1+fr.m1+2*fr.m2)^2)
    
    temp1 <- subset(err.fac3,err.fac3$Date==kwartaal)[,c(-1,-2)]
    temp2 <- subset(errors3,errors3$Date==kwartaal)[,c(-1,-2)]
    temp3 <- subset(w.fact3,w.fact3$Date==kwartaal)[,c(-1,-2)]
    temp4 <- replace(temp2, temp2==0,1)
    temp4 <- replace(temp4, temp4==-2,2)
    temp4 <- replace(temp4, temp4==-1,1)
    temp4 <- temp4*temp3
    
    fr.2 <- sum(temp1[temp2==2], na.rm=TRUE)/sum(temp4[!is.na(temp2)],na.rm=TRUE)
    fr.1 <- sum(temp1[temp2==1], na.rm=TRUE)/sum(temp4[!is.na(temp2)],na.rm=TRUE)
    fr.m1 <- sum(temp1[temp2==-1], na.rm=TRUE)/sum(temp4[!is.na(temp2)],na.rm=TRUE)
    fr.m2 <- sum(temp1[temp2==-2], na.rm=TRUE)/sum(temp4[!is.na(temp2)],na.rm=TRUE)
    w.Uncert_ee.GBC[i] <- sqrt(4*fr.2+fr.1-fr.m1-4*fr.m2-(2*fr.2+fr.1+fr.m1+2*fr.m2)^2)
}
w.indicators.M <- cbind(w.indicators.M,w.Uncert_ee,w.Uncert_ee.prod,w.Uncert_ee.GBC)



##================================##
## CALCULATE INDICATORS: BUILDING ##
##================================##
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

##Weighted versions--------------------------------------------------------------------------------------

w.Uncert_fl.prod <- NULL
w.Uncert_fl.GBC <- NULL
i <- 0
for(kwartaal in levels(BER.B$surveyQ)) {
    i <- i+1
    temp <- subset(BER.B,BER.B$surveyQ==kwartaal)
    
    fr.up <- sum((temp$Q3P*temp$factor)[temp$Q3P>0], na.rm=TRUE)/sum(temp$factor[!is.na(temp$Q3P)],na.rm=TRUE)
    fr.down <- sum((temp$Q3P*temp$factor)[temp$Q3P<0], na.rm=TRUE)/sum(temp$factor[!is.na(temp$Q3P)],na.rm=TRUE)
    w.Uncert_fl.prod[i] <- sqrt(fr.up-fr.down-(fr.up+fr.down)^2)
    
    fr.up <- sum((temp$Q2P*temp$factor)[temp$Q2P>0], na.rm=TRUE)/sum(temp$factor[!is.na(temp$Q2P)],na.rm=TRUE)
    fr.down <- sum((temp$Q2P*temp$factor)[temp$Q2P<0], na.rm=TRUE)/sum(temp$factor[!is.na(temp$Q2P)],na.rm=TRUE)
    w.Uncert_fl.GBC[i] <- sqrt(fr.up-fr.down-(fr.up+fr.down)^2)
}
w.indicators.B <- cbind(w.indicators.B,w.Uncert_fl.prod,w.Uncert_fl.GBC)

#---------------------------------------------------------------------------------------------------------------------------------------
errors1 <- w.indicators.B[,c(1,9)]
err.fac1 <- w.indicators.B[,c(1,9)]
w.fact1 <- w.indicators.B[,c(1,9)]
errors2 <- w.indicators.B[,c(1,9)]
err.fac2 <- w.indicators.B[,c(1,9)]
w.fact2 <- w.indicators.B[,c(1,9)]
tel <- 2
for(i in levels(uniBER.B$id)){
    tel <- tel + 1
    exp.error <- w.indicators.B[,c(1,9)]
    data <- subset(uniBER.B, uniBER.B$id==i)
    exp.error <- merge(exp.error, data, by.x="Date",by.y="surveyQ", all.x = TRUE)
    for(t in 1:(nrow(exp.error))) {
        exp.error$error1[t] <- exp.error$Q3A[t+1] - exp.error$Q3P[t]
        exp.error$error2[t] <- exp.error$Q2A[t+1] - exp.error$Q2P[t]
    }
    
    errors1 <- cbind(errors1, exp.error$error1)
    err.fac1 <- cbind(err.fac1, exp.error$error1*exp.error$factor)
    w.fact1 <- cbind(w.fact1, exp.error$factor)
    errors2 <- cbind(errors2, exp.error$error2)
    err.fac2 <- cbind(err.fac2, exp.error$error2*exp.error$factor)
    w.fact2 <- cbind(w.fact2, exp.error$factor)
    
    colnames(errors1)[tel] <- as.character(i)
    colnames(err.fac1)[tel] <- as.character(i)
    colnames(w.fact1)[tel] <- as.character(i)
    colnames(errors2)[tel] <- as.character(i)
    colnames(err.fac2)[tel] <- as.character(i)
    colnames(w.fact2)[tel] <- as.character(i)
}


w.Uncert_ee.prod <- NULL
w.Uncert_ee.GBC <- NULL
i <- 0
for(kwartaal in levels(err.fac1$Date)) {
    i <- i+1
    temp1 <- subset(err.fac1,err.fac1$Date==kwartaal)[,c(-1,-2)]
    temp2 <- subset(errors1,errors1$Date==kwartaal)[,c(-1,-2)]
    temp3 <- subset(w.fact1,w.fact1$Date==kwartaal)[,c(-1,-2)]
    temp4 <- replace(temp2, temp2==0,1)
    temp4 <- replace(temp4, temp4==-2,2)
    temp4 <- replace(temp4, temp4==-1,1)
    temp4 <- temp4*temp3
    
    fr.2 <- sum(temp1[temp2==2], na.rm=TRUE)/sum(temp4[!is.na(temp2)],na.rm=TRUE)
    fr.1 <- sum(temp1[temp2==1], na.rm=TRUE)/sum(temp4[!is.na(temp2)],na.rm=TRUE)
    fr.m1 <- sum(temp1[temp2==-1], na.rm=TRUE)/sum(temp4[!is.na(temp2)],na.rm=TRUE)
    fr.m2 <- sum(temp1[temp2==-2], na.rm=TRUE)/sum(temp4[!is.na(temp2)],na.rm=TRUE)
    w.Uncert_ee.prod[i] <- sqrt(4*fr.2+fr.1-fr.m1-4*fr.m2-(2*fr.2+fr.1+fr.m1+2*fr.m2)^2)
    
    temp1 <- subset(err.fac2,err.fac2$Date==kwartaal)[,c(-1,-2)]
    temp2 <- subset(errors2,errors2$Date==kwartaal)[,c(-1,-2)]
    temp3 <- subset(w.fact2,w.fact2$Date==kwartaal)[,c(-1,-2)]
    temp4 <- replace(temp2, temp2==0,1)
    temp4 <- replace(temp4, temp4==-2,2)
    temp4 <- replace(temp4, temp4==-1,1)
    temp4 <- temp4*temp3
    
    fr.2 <- sum(temp1[temp2==2], na.rm=TRUE)/sum(temp4[!is.na(temp2)],na.rm=TRUE)
    fr.1 <- sum(temp1[temp2==1], na.rm=TRUE)/sum(temp4[!is.na(temp2)],na.rm=TRUE)
    fr.m1 <- sum(temp1[temp2==-1], na.rm=TRUE)/sum(temp4[!is.na(temp2)],na.rm=TRUE)
    fr.m2 <- sum(temp1[temp2==-2], na.rm=TRUE)/sum(temp4[!is.na(temp2)],na.rm=TRUE)
    w.Uncert_ee.GBC[i] <- sqrt(4*fr.2+fr.1-fr.m1-4*fr.m2-(2*fr.2+fr.1+fr.m1+2*fr.m2)^2)
    
}
w.indicators.B <- cbind(w.indicators.B,w.Uncert_ee.prod,w.Uncert_ee.GBC)

##=============================##
## CALCULATE INDICATORS: TRADE ##
##=============================##
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

##Weighted versions--------------------------------------------------------------------------------------------------------------------
w.Uncert_fl.prod <- NULL
w.Uncert_fl.GBC <- NULL
i <- 0
for(kwartaal in levels(BER.T$surveyQ)) {
    i <- i+1
    temp <- subset(BER.T,BER.T$surveyQ==kwartaal)
    
    fr.up <- sum((temp$Q3P*temp$factor)[temp$Q3P>0], na.rm=TRUE)/sum(temp$factor[!is.na(temp$Q3P)],na.rm=TRUE)
    fr.down <- sum((temp$Q3P*temp$factor)[temp$Q3P<0], na.rm=TRUE)/sum(temp$factor[!is.na(temp$Q3P)],na.rm=TRUE)
    w.Uncert_fl.prod[i] <- sqrt(fr.up-fr.down-(fr.up+fr.down)^2)
    
    fr.up <- sum((temp$Q2P*temp$factor)[temp$Q2P>0], na.rm=TRUE)/sum(temp$factor[!is.na(temp$Q2P)],na.rm=TRUE)
    fr.down <- sum((temp$Q2P*temp$factor)[temp$Q2P<0], na.rm=TRUE)/sum(temp$factor[!is.na(temp$Q2P)],na.rm=TRUE)
    w.Uncert_fl.GBC[i] <- sqrt(fr.up-fr.down-(fr.up+fr.down)^2)
}
w.indicators.T <- cbind(w.indicators.T,w.Uncert_fl.prod,w.Uncert_fl.GBC)
#---------------------------------------------------------------------------------------------------------------------------------------
errors1 <- w.indicators.T[,c(1,9)]
err.fac1 <- w.indicators.T[,c(1,9)]
w.fact1 <- w.indicators.T[,c(1,9)]
errors2 <- w.indicators.T[,c(1,9)]
err.fac2 <- w.indicators.T[,c(1,9)]
w.fact2 <- w.indicators.T[,c(1,9)]
tel <- 2
for(i in levels(uniBER.T$id)){
    tel <- tel + 1
    exp.error <- w.indicators.T[,c(1,9)]
    data <- subset(uniBER.T, uniBER.T$id==i)
    exp.error <- merge(exp.error, data, by.x="Date",by.y="surveyQ", all.x = TRUE)
    for(t in 1:(nrow(exp.error))) {
        exp.error$error1[t] <- exp.error$Q3A[t+1] - exp.error$Q3P[t]
        exp.error$error2[t] <- exp.error$Q2A[t+1] - exp.error$Q2P[t]
    }
    
    errors1 <- cbind(errors1, exp.error$error1)
    err.fac1 <- cbind(err.fac1, exp.error$error1*exp.error$factor)
    w.fact1 <- cbind(w.fact1, exp.error$factor)
    errors2 <- cbind(errors2, exp.error$error2)
    err.fac2 <- cbind(err.fac2, exp.error$error2*exp.error$factor)
    w.fact2 <- cbind(w.fact2, exp.error$factor)
    
    colnames(errors1)[tel] <- as.character(i)
    colnames(err.fac1)[tel] <- as.character(i)
    colnames(w.fact1)[tel] <- as.character(i)
    colnames(errors2)[tel] <- as.character(i)
    colnames(err.fac2)[tel] <- as.character(i)
    colnames(w.fact2)[tel] <- as.character(i)
}


w.Uncert_ee.prod <- NULL
w.Uncert_ee.GBC <- NULL
i <- 0
for(kwartaal in levels(err.fac1$Date)) {
    i <- i+1
    temp1 <- subset(err.fac1,err.fac1$Date==kwartaal)[,c(-1,-2)]
    temp2 <- subset(errors1,errors1$Date==kwartaal)[,c(-1,-2)]
    temp3 <- subset(w.fact1,w.fact1$Date==kwartaal)[,c(-1,-2)]
    temp4 <- replace(temp2, temp2==0,1)
    temp4 <- replace(temp4, temp4==-2,2)
    temp4 <- replace(temp4, temp4==-1,1)
    temp4 <- temp4*temp3
    
    fr.2 <- sum(temp1[temp2==2], na.rm=TRUE)/sum(temp4[!is.na(temp2)],na.rm=TRUE)
    fr.1 <- sum(temp1[temp2==1], na.rm=TRUE)/sum(temp4[!is.na(temp2)],na.rm=TRUE)
    fr.m1 <- sum(temp1[temp2==-1], na.rm=TRUE)/sum(temp4[!is.na(temp2)],na.rm=TRUE)
    fr.m2 <- sum(temp1[temp2==-2], na.rm=TRUE)/sum(temp4[!is.na(temp2)],na.rm=TRUE)
    w.Uncert_ee.prod[i] <- sqrt(4*fr.2+fr.1-fr.m1-4*fr.m2-(2*fr.2+fr.1+fr.m1+2*fr.m2)^2)
    
    temp1 <- subset(err.fac2,err.fac2$Date==kwartaal)[,c(-1,-2)]
    temp2 <- subset(errors2,errors2$Date==kwartaal)[,c(-1,-2)]
    temp3 <- subset(w.fact2,w.fact2$Date==kwartaal)[,c(-1,-2)]
    temp4 <- replace(temp2, temp2==0,1)
    temp4 <- replace(temp4, temp4==-2,2)
    temp4 <- replace(temp4, temp4==-1,1)
    temp4 <- temp4*temp3
    
    fr.2 <- sum(temp1[temp2==2], na.rm=TRUE)/sum(temp4[!is.na(temp2)],na.rm=TRUE)
    fr.1 <- sum(temp1[temp2==1], na.rm=TRUE)/sum(temp4[!is.na(temp2)],na.rm=TRUE)
    fr.m1 <- sum(temp1[temp2==-1], na.rm=TRUE)/sum(temp4[!is.na(temp2)],na.rm=TRUE)
    fr.m2 <- sum(temp1[temp2==-2], na.rm=TRUE)/sum(temp4[!is.na(temp2)],na.rm=TRUE)
    w.Uncert_ee.GBC[i] <- sqrt(4*fr.2+fr.1-fr.m1-4*fr.m2-(2*fr.2+fr.1+fr.m1+2*fr.m2)^2)
    
}
w.indicators.T <- cbind(w.indicators.T,w.Uncert_ee.prod,w.Uncert_ee.GBC)


##======================================##
## CALCULATE INDICATORS: Motor Vehicles ##
##======================================##
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

##Weighted versions--------------------------------------------------------------------------------------
w.Uncert_fl.prod <- NULL
w.Uncert_fl.GBC <- NULL
i <- 0
for(kwartaal in levels(BER.V$surveyQ)) {
    i <- i+1
    temp <- subset(BER.V,BER.V$surveyQ==kwartaal)
    
    fr.up <- sum((temp$Q3P*temp$factor)[temp$Q3P>0], na.rm=TRUE)/sum(temp$factor[!is.na(temp$Q3P)],na.rm=TRUE)
    fr.down <- sum((temp$Q3P*temp$factor)[temp$Q3P<0], na.rm=TRUE)/sum(temp$factor[!is.na(temp$Q3P)],na.rm=TRUE)
    w.Uncert_fl.prod[i] <- sqrt(fr.up-fr.down-(fr.up+fr.down)^2)
    
    fr.up <- sum((temp$Q2P*temp$factor)[temp$Q2P>0], na.rm=TRUE)/sum(temp$factor[!is.na(temp$Q2P)],na.rm=TRUE)
    fr.down <- sum((temp$Q2P*temp$factor)[temp$Q2P<0], na.rm=TRUE)/sum(temp$factor[!is.na(temp$Q2P)],na.rm=TRUE)
    w.Uncert_fl.GBC[i] <- sqrt(fr.up-fr.down-(fr.up+fr.down)^2)
}
w.indicators.V <- cbind(w.indicators.V,w.Uncert_fl.prod,w.Uncert_fl.GBC)

#---------------------------------------------------------------------------------------------------------------------------------------
errors1 <- w.indicators.V[,c(1,9)]
err.fac1 <- w.indicators.V[,c(1,9)]
w.fact1 <- w.indicators.V[,c(1,9)]
errors2 <- w.indicators.V[,c(1,9)]
err.fac2 <- w.indicators.V[,c(1,9)]
w.fact2 <- w.indicators.V[,c(1,9)]
tel <- 2
for(i in levels(uniBER.V$id)){
    tel <- tel + 1
    exp.error <- w.indicators.V[,c(1,9)]
    data <- subset(uniBER.V, uniBER.V$id==i)
    exp.error <- merge(exp.error, data, by.x="Date",by.y="surveyQ", all.x = TRUE)
    for(t in 1:(nrow(exp.error))) {
        exp.error$error1[t] <- exp.error$Q3A[t+1] - exp.error$Q3P[t]
        exp.error$error2[t] <- exp.error$Q2A[t+1] - exp.error$Q2P[t]
    }
    
    errors1 <- cbind(errors1, exp.error$error1)
    err.fac1 <- cbind(err.fac1, exp.error$error1*exp.error$factor)
    w.fact1 <- cbind(w.fact1, exp.error$factor)
    errors2 <- cbind(errors2, exp.error$error2)
    err.fac2 <- cbind(err.fac2, exp.error$error2*exp.error$factor)
    w.fact2 <- cbind(w.fact2, exp.error$factor)
    
    colnames(errors1)[tel] <- as.character(i)
    colnames(err.fac1)[tel] <- as.character(i)
    colnames(w.fact1)[tel] <- as.character(i)
    colnames(errors2)[tel] <- as.character(i)
    colnames(err.fac2)[tel] <- as.character(i)
    colnames(w.fact2)[tel] <- as.character(i)
}


w.Uncert_ee.prod <- NULL
w.Uncert_ee.GBC <- NULL
i <- 0
for(kwartaal in levels(err.fac1$Date)) {
    i <- i+1
    temp1 <- subset(err.fac1,err.fac1$Date==kwartaal)[,c(-1,-2)]
    temp2 <- subset(errors1,errors1$Date==kwartaal)[,c(-1,-2)]
    temp3 <- subset(w.fact1,w.fact1$Date==kwartaal)[,c(-1,-2)]
    temp4 <- replace(temp2, temp2==0,1)
    temp4 <- replace(temp4, temp4==-2,2)
    temp4 <- replace(temp4, temp4==-1,1)
    temp4 <- temp4*temp3
    
    fr.2 <- sum(temp1[temp2==2], na.rm=TRUE)/sum(temp4[!is.na(temp2)],na.rm=TRUE)
    fr.1 <- sum(temp1[temp2==1], na.rm=TRUE)/sum(temp4[!is.na(temp2)],na.rm=TRUE)
    fr.m1 <- sum(temp1[temp2==-1], na.rm=TRUE)/sum(temp4[!is.na(temp2)],na.rm=TRUE)
    fr.m2 <- sum(temp1[temp2==-2], na.rm=TRUE)/sum(temp4[!is.na(temp2)],na.rm=TRUE)
    w.Uncert_ee.prod[i] <- sqrt(4*fr.2+fr.1-fr.m1-4*fr.m2-(2*fr.2+fr.1+fr.m1+2*fr.m2)^2)
    
    temp1 <- subset(err.fac2,err.fac2$Date==kwartaal)[,c(-1,-2)]
    temp2 <- subset(errors2,errors2$Date==kwartaal)[,c(-1,-2)]
    temp3 <- subset(w.fact2,w.fact2$Date==kwartaal)[,c(-1,-2)]
    temp4 <- replace(temp2, temp2==0,1)
    temp4 <- replace(temp4, temp4==-2,2)
    temp4 <- replace(temp4, temp4==-1,1)
    temp4 <- temp4*temp3
    
    fr.2 <- sum(temp1[temp2==2], na.rm=TRUE)/sum(temp4[!is.na(temp2)],na.rm=TRUE)
    fr.1 <- sum(temp1[temp2==1], na.rm=TRUE)/sum(temp4[!is.na(temp2)],na.rm=TRUE)
    fr.m1 <- sum(temp1[temp2==-1], na.rm=TRUE)/sum(temp4[!is.na(temp2)],na.rm=TRUE)
    fr.m2 <- sum(temp1[temp2==-2], na.rm=TRUE)/sum(temp4[!is.na(temp2)],na.rm=TRUE)
    w.Uncert_ee.GBC[i] <- sqrt(4*fr.2+fr.1-fr.m1-4*fr.m2-(2*fr.2+fr.1+fr.m1+2*fr.m2)^2)
    
}
w.indicators.V <- cbind(w.indicators.V,w.Uncert_ee.prod,w.Uncert_ee.GBC)


##================================##
## CALCULATE INDICATORS: SERVICES ##
##================================##
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

##Weighted versions--------------------------------------------------------------------------------------------------------------------
w.Uncert_fl.prod <- NULL
w.Uncert_fl.GBC <- NULL
i <- 0
for(kwartaal in levels(BER.S$surveyQ)) {
    i <- i+1
    temp <- subset(BER.S,BER.S$surveyQ==kwartaal)
    
    fr.up <- sum((temp$Q3P*temp$factor)[temp$Q3P>0], na.rm=TRUE)/sum(temp$factor[!is.na(temp$Q3P)],na.rm=TRUE)
    fr.down <- sum((temp$Q3P*temp$factor)[temp$Q3P<0], na.rm=TRUE)/sum(temp$factor[!is.na(temp$Q3P)],na.rm=TRUE)
    w.Uncert_fl.prod[i] <- sqrt(fr.up-fr.down-(fr.up+fr.down)^2)
    
    fr.up <- sum((temp$Q2P*temp$factor)[temp$Q2P>0], na.rm=TRUE)/sum(temp$factor[!is.na(temp$Q2P)],na.rm=TRUE)
    fr.down <- sum((temp$Q2P*temp$factor)[temp$Q2P<0], na.rm=TRUE)/sum(temp$factor[!is.na(temp$Q2P)],na.rm=TRUE)
    w.Uncert_fl.GBC[i] <- sqrt(fr.up-fr.down-(fr.up+fr.down)^2)
}
w.indicators.S <- cbind(w.indicators.S,w.Uncert_fl.prod,w.Uncert_fl.GBC)
#---------------------------------------------------------------------------------------------------------------------------------------
errors1 <- w.indicators.S[,c(1,9)]
err.fac1 <- w.indicators.S[,c(1,9)]
w.fact1 <- w.indicators.S[,c(1,9)]
errors2 <- w.indicators.S[,c(1,9)]
err.fac2 <- w.indicators.S[,c(1,9)]
w.fact2 <- w.indicators.S[,c(1,9)]
tel <- 2
for(i in levels(uniBER.S$id)){
    tel <- tel + 1
    exp.error <- w.indicators.S[,c(1,9)]
    data <- subset(uniBER.S, uniBER.S$id==i)
    exp.error <- merge(exp.error, data, by.x="Date",by.y="surveyQ", all.x = TRUE)
    for(t in 1:(nrow(exp.error))) {
        exp.error$error1[t] <- exp.error$Q3A[t+1] - exp.error$Q3P[t]
        exp.error$error2[t] <- exp.error$Q2A[t+1] - exp.error$Q2P[t]
    }
    
    errors1 <- cbind(errors1, exp.error$error1)
    err.fac1 <- cbind(err.fac1, exp.error$error1*exp.error$factor)
    w.fact1 <- cbind(w.fact1, exp.error$factor)
    errors2 <- cbind(errors2, exp.error$error2)
    err.fac2 <- cbind(err.fac2, exp.error$error2*exp.error$factor)
    w.fact2 <- cbind(w.fact2, exp.error$factor)
    
    colnames(errors1)[tel] <- as.character(i)
    colnames(err.fac1)[tel] <- as.character(i)
    colnames(w.fact1)[tel] <- as.character(i)
    colnames(errors2)[tel] <- as.character(i)
    colnames(err.fac2)[tel] <- as.character(i)
    colnames(w.fact2)[tel] <- as.character(i)
}


w.Uncert_ee.prod <- NULL
w.Uncert_ee.GBC <- NULL
i <- 0
for(kwartaal in levels(err.fac1$Date)) {
    i <- i+1
    temp1 <- subset(err.fac1,err.fac1$Date==kwartaal)[,c(-1,-2)]
    temp2 <- subset(errors1,errors1$Date==kwartaal)[,c(-1,-2)]
    temp3 <- subset(w.fact1,w.fact1$Date==kwartaal)[,c(-1,-2)]
    temp4 <- replace(temp2, temp2==0,1)
    temp4 <- replace(temp4, temp4==-2,2)
    temp4 <- replace(temp4, temp4==-1,1)
    temp4 <- temp4*temp3
    
    fr.2 <- sum(temp1[temp2==2], na.rm=TRUE)/sum(temp4[!is.na(temp2)],na.rm=TRUE)
    fr.1 <- sum(temp1[temp2==1], na.rm=TRUE)/sum(temp4[!is.na(temp2)],na.rm=TRUE)
    fr.m1 <- sum(temp1[temp2==-1], na.rm=TRUE)/sum(temp4[!is.na(temp2)],na.rm=TRUE)
    fr.m2 <- sum(temp1[temp2==-2], na.rm=TRUE)/sum(temp4[!is.na(temp2)],na.rm=TRUE)
    w.Uncert_ee.prod[i] <- sqrt(4*fr.2+fr.1-fr.m1-4*fr.m2-(2*fr.2+fr.1+fr.m1+2*fr.m2)^2)
    
    temp1 <- subset(err.fac2,err.fac2$Date==kwartaal)[,c(-1,-2)]
    temp2 <- subset(errors2,errors2$Date==kwartaal)[,c(-1,-2)]
    temp3 <- subset(w.fact2,w.fact2$Date==kwartaal)[,c(-1,-2)]
    temp4 <- replace(temp2, temp2==0,1)
    temp4 <- replace(temp4, temp4==-2,2)
    temp4 <- replace(temp4, temp4==-1,1)
    temp4 <- temp4*temp3
    
    fr.2 <- sum(temp1[temp2==2], na.rm=TRUE)/sum(temp4[!is.na(temp2)],na.rm=TRUE)
    fr.1 <- sum(temp1[temp2==1], na.rm=TRUE)/sum(temp4[!is.na(temp2)],na.rm=TRUE)
    fr.m1 <- sum(temp1[temp2==-1], na.rm=TRUE)/sum(temp4[!is.na(temp2)],na.rm=TRUE)
    fr.m2 <- sum(temp1[temp2==-2], na.rm=TRUE)/sum(temp4[!is.na(temp2)],na.rm=TRUE)
    w.Uncert_ee.GBC[i] <- sqrt(4*fr.2+fr.1-fr.m1-4*fr.m2-(2*fr.2+fr.1+fr.m1+2*fr.m2)^2)
    
}
w.indicators.S <- cbind(w.indicators.S,w.Uncert_ee.prod,w.Uncert_ee.GBC)


##=================================##
## AGGREGATING as much as possible ##
##=================================##
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

##Weighted versions---------------------------------------------------------------------------------
#GDP Data
GDPdata <- read.csv("GDP Data.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)
GDPdata$X <- as.Date(GDPdata$X, format = "%Y/%m/%d")

weights <- GDPdata[,c(17,12,6)]
motor <- 0.05*weights[,3]
weights <- cbind(weights,MotorRGDP_sa=motor)
weights <- cbind(weights,SerRGDP_sa=GDPdata[,24])

#weights$total <- rowSums(weights,1)

colnames(w.indicators.M) <- c("Date","w.Conf_cc.M","w.Conf_fl.M","w.Act_prod.M","w.Conf_prod.M","w.Act_GBC.M","w.Conf_GBC.M","w.Invest.M","w.Empl.M")
colnames(w.indicators.B) <- c("Date","w.Conf_cc.B",              "w.Act_prod.B","w.Conf_prod.B","w.Act_GBC.B","w.Conf_GBC.B",             "w.Empl.B")
colnames(w.indicators.T) <- c("Date","w.Conf_cc.T",              "w.Act_prod.T","w.Conf_prod.T","w.Act_GBC.T","w.Conf_GBC.T",             "w.Empl.T")
colnames(w.indicators.V) <- c("Date","w.Conf_cc.V",              "w.Act_prod.V","w.Conf_prod.V","w.Act_GBC.V","w.Conf_GBC.V",             "w.Empl.V")
colnames(w.indicators.S) <- c("Date","w.Conf_cc.S",              "w.Act_prod.S","w.Conf_prod.S","w.Act_GBC.S","w.Conf_GBC.S",             "w.Empl.S")

CC <- merge(w.indicators.M, w.indicators.B, by.x="Date", by.y="Date",all.x=TRUE)
CC <- merge(CC, w.indicators.T, by.x="Date", by.y="Date",all.x=TRUE)
CC <- merge(CC, w.indicators.V, by.x="Date", by.y="Date",all.x=TRUE)
CC <- merge(CC, w.indicators.S, by.x="Date", by.y="Date",all.x=TRUE)

Conf_cc <- cbind(CC$w.Conf_cc.M,CC$w.Conf_cc.B,CC$w.Conf_cc.T,CC$w.Conf_cc.V,CC$w.Conf_cc.S)
Act_prod <- cbind(CC$w.Act_prod.M,CC$w.Act_prod.B,CC$w.Act_prod.T,CC$w.Act_prod.V,CC$w.Act_prod.S)
Conf_prod <- cbind(CC$w.Conf_prod.M,CC$w.Conf_prod.B,CC$w.Conf_prod.T,CC$w.Conf_prod.V,CC$w.Conf_prod.S)
Act_GBC <- cbind(CC$w.Act_GBC.M,CC$w.Act_GBC.B,CC$w.Act_GBC.T,CC$w.Act_GBC.V,CC$w.Act_GBC.S)
Conf_GBC <- cbind(CC$w.Conf_GBC.M,CC$w.Conf_GBC.B,CC$w.Conf_GBC.T,CC$w.Conf_GBC.V,CC$w.Conf_GBC.S)
Empl <- cbind(CC$w.Empl.M,CC$w.Empl.B,CC$w.Empl.T,CC$w.Empl.V,CC$w.Empl.S)

w.indicators <- indicators
for(i in 1:95) {
    w.indicators$Conf_cc[i] <- weighted.mean(Conf_cc[i,], weights[i,],na.rm=TRUE)
    w.indicators$Act_prod[i] <- weighted.mean(Act_prod[i,], weights[i,],na.rm=TRUE)
    w.indicators$Conf_prod[i] <- weighted.mean(Conf_prod[i,], weights[i,],na.rm=TRUE)
    w.indicators$Act_GBC[i] <- weighted.mean(Act_GBC[i,], weights[i,],na.rm=TRUE)
    w.indicators$Conf_GBC[i] <- weighted.mean(Conf_GBC[i,], weights[i,],na.rm=TRUE)
    w.indicators$Empl[i] <- weighted.mean(Empl[i,], weights[i,],na.rm=TRUE)
}

##------------------------------------------------------------------------------------------

colnames(w.indicators.M)[10:15] <- c("w.Uncert_fl.M","w.Uncert_fl.prod.M","w.Uncert_fl.GBC.M","w.Uncert_ee.M","w.Uncert_ee.prod.M","w.Uncert_ee.GBC.M")
colnames(w.indicators.B)[8:11] <-  c(                "w.Uncert_fl.prod.B","w.Uncert_fl.GBC.B",                "w.Uncert_ee.prod.B","w.Uncert_ee.GBC.B")
colnames(w.indicators.T)[8:11] <-  c(                "w.Uncert_fl.prod.T","w.Uncert_fl.GBC.T",                "w.Uncert_ee.prod.T","w.Uncert_ee.GBC.T")
colnames(w.indicators.V)[8:11] <-  c(                "w.Uncert_fl.prod.V","w.Uncert_fl.GBC.V",                "w.Uncert_ee.prod.V","w.Uncert_ee.GBC.V")
colnames(w.indicators.S)[8:11] <-  c(                "w.Uncert_fl.prod.S","w.Uncert_fl.GBC.S",                "w.Uncert_ee.prod.S","w.Uncert_ee.GBC.S")

UN <- merge(w.indicators.M[,c(1,10:15)], w.indicators.B[,c(1,8:11)], by.x="Date", by.y="Date",all.x=TRUE)
UN <- merge(UN, w.indicators.T[,c(1,8:11)], by.x="Date", by.y="Date",all.x=TRUE)
UN <- merge(UN, w.indicators.V[,c(1,8:11)], by.x="Date", by.y="Date",all.x=TRUE)
UN <- merge(UN, w.indicators.S[,c(1,8:11)], by.x="Date", by.y="Date",all.x=TRUE)

Uncert_fl.prod <- cbind(UN$w.Uncert_fl.prod.M,UN$w.Uncert_fl.prod.B,UN$w.Uncert_fl.prod.T,UN$w.Uncert_fl.prod.V,UN$w.Uncert_fl.prod.S)
Uncert_fl.GBC <- cbind(UN$w.Uncert_fl.GBC.M,UN$w.Uncert_fl.GBC.B,UN$w.Uncert_fl.GBC.T,UN$w.Uncert_fl.GBC.V,UN$w.Uncert_fl.GBC.S)
Uncert_ee.prod <- cbind(UN$w.Uncert_ee.prod.M,UN$w.Uncert_ee.prod.B,UN$w.Uncert_ee.prod.T,UN$w.Uncert_ee.prod.V,UN$w.Uncert_ee.prod.S)
Uncert_ee.GBC <- cbind(UN$w.Uncert_ee.GBC.M,UN$w.Uncert_ee.GBC.B,UN$w.Uncert_ee.GBC.T,UN$w.Uncert_ee.GBC.V,UN$w.Uncert_ee.GBC.S)

for(i in 1:95) {
    w.indicators$Uncert_fl.prod[i] <- weighted.mean(Uncert_fl.prod[i,], weights[i,],na.rm=TRUE)
    w.indicators$Uncert_fl.GBC[i] <- weighted.mean(Uncert_fl.GBC[i,], weights[i,],na.rm=TRUE)
    w.indicators$Uncert_ee.prod[i] <- weighted.mean(Uncert_ee.prod[i,], weights[i,],na.rm=TRUE)
    w.indicators$Uncert_ee.GBC[i] <- weighted.mean(Uncert_ee.GBC[i,], weights[i,],na.rm=TRUE)
}


#-----------------------------------------------------------------------------
#Load pre-calculated datasets (for speed)
indicators <- read.csv2("indicators.csv", header=TRUE)[,-1]
indicators.M <- read.csv2("indicators_M.csv", header=TRUE)[,-1]
indicators.B <- read.csv2("indicators_B.csv", header=TRUE)[,-1]
indicators.T <- read.csv2("indicators_T.csv", header=TRUE)[,-1]
indicators.V <- read.csv2("indicators_V.csv", header=TRUE)[,-1]
indicators.S <- read.csv2("indicators_S.csv", header=TRUE)[,-1]

w.indicators <- read.csv2("w_indicators.csv")[,-1]
w.indicators.M <- read.csv2("w_indicators_M.csv")[,-1]
w.indicators.B <- read.csv2("w_indicators_B.csv")[,-1]
w.indicators.T <- read.csv2("w_indicators_T.csv")[,-1]
w.indicators.V <- read.csv2("w_indicators_V.csv")[,-1]
w.indicators.S <- read.csv2("w_indicators_S.csv")[,-1]

GDPdata$X <- as.Date(GDPdata$X, format = "%Y/%m/%d")
w.indicators$Date <- GDPdata$X
indicators$Date <- GDPdata$X

w.uncert.norm <- cbind(Date=w.indicators[,1],as.data.frame(scale(w.indicators[,9:12])))
uncert.norm <- cbind(Date=indicators[,1],as.data.frame(scale(indicators[,9:12])))


indicator_plot <- cbind(uncert.norm[,c(1,3)],w.uncert.norm[,3])
colnames(indicator_plot) <- c("Date","Unweighted","Weighted")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_line()
g <- g + ylab("Uncertainty")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g <- g + theme(legend.position="bottom")
g

indicator_plot <- cbind(uncert.norm[,c(1,5)],w.uncert.norm[,5])
colnames(indicator_plot) <- c("Date","Unweighted","Weighted")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_line()
g <- g + ylab("Uncertainty")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g <- g + theme(legend.position="bottom")
g

indicator_plot <- uncert.norm[,c(1,3,5)]
g <- ggplot(indicator_plot) 
g <- g + geom_line(aes(x=Date, y=Uncert_fl.GBC, colour="Forward_looking"), size = 1)
g <- g + geom_line(aes(x=Date, y=Uncert_ee.GBC, colour="Expectation Errors"), size = 1)
g <- g + theme_bw()
g <- g + labs(color="Legend text")
g <- g + geom_rect(data=recessions.df, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='grey', alpha=0.5)
g <- g + ylab("Uncertainty")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g <- g + theme(legend.position="bottom")
g


# Check correlations
temp_indices <- cbind(uncert.norm[,c(1,3,5)],w.uncert.norm[,c(3,5)])
colnames(temp_indices) <- c("Date","Unw_FL","Unw_EE","Weighted_FL","Weighted_EE")
source("corstarsl.R")
for(i in 2:ncol(temp_indices)) {temp_indices[,i] <- as.numeric(temp_indices[,i]) }
ts.all_indices <- as.ts(temp_indices[,-1],start =c(2000,1),end=c(2015,3),frequency=4) 
#cor(temp_indices[,-1],use="complete.obs")
xt <- xtable(corstarsl(ts.all_indices), caption="Correlations in Levels")
print(xt, "latex",comment=FALSE, caption.placement = getOption("xtable.caption.placement", "top"))


##========================================================
##COMOVEMENT----------------------------------------------
##========================================================
ts.indicators <- ts(indicators[,c(2,3,4,5,6,9,10,11,12)],start =c(1992,1),end=c(2015,3),frequency=4) 
ts.indicators.M <- ts(indicators.M[,c(2,4,5,6,7,12,13,15,16)],start =c(1992,1),end=c(2015,3),frequency=4) 
ts.indicators.B <- ts(indicators.B[,c(2,3,4,5,6,9,10,11,12)],start =c(1993,2),end=c(2015,3),frequency=4) 
ts.indicators.T <- ts(indicators.T[,c(2,3,4,5,6,9,10,11,12)],start =c(1992,2),end=c(2015,3),frequency=4) 

ts.windicators <- ts(w.indicators[,c(2,3,4,5,6,9,10,11,12)],start =c(1992,1),end=c(2015,3),frequency=4) 
ts.windicators.M <- ts(w.indicators.M[,c(2,4,5,6,7,11,12,14,15)],start =c(1992,1),end=c(2015,3),frequency=4) 
ts.windicators.B <- ts(w.indicators.B[,c(2,3,4,5,6,8,9,10,11)],start =c(1993,2),end=c(2015,3),frequency=4) 
ts.windicators.T <- ts(w.indicators.T[,c(2,3,4,5,6,8,9,10,11)],start =c(1992,2),end=c(2015,3),frequency=4) 

realGDP <- read.csv("RealGDP.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)
realGDP$X <- as.Date(realGDP$X, format = "%Y/%m/%d")
ts.realGDP <- ts(realGDP,start =c(1991,1),end=c(2015,3),frequency=4)
GDPgrowth4 <- sapply(log(ts.realGDP[,-1]), diff, lag =4)
ts.GDPgrowth4 <- ts(GDPgrowth4,start =c(1992,1),end=c(2015,3),frequency=4)
GDPgrowth1 <- sapply(log(ts.realGDP[,-1]), diff, lag =1)
ts.GDPgrowth1 <- ts(GDPgrowth1,start =c(1991,2),end=c(2015,3),frequency=4)

dum94 <- ts(0,start =c(1992,1),end=c(2015,3),frequency=4)
dum94[10] <- 1
dum94 <- as.data.frame(dum94)
names(dum94) <- "dum94"

#plot hulle saam
temp_indices <- cbind(indicators[,c(1,6)],Uncert_EE=indicators[,12],GDPgr=GDPgrowth4[,1])
ts.temp_indices <- ts(temp_indices[,-1],start =c(1992,1),end=c(2015,3),frequency=4) 
plot(ts.temp_indices,plot.type = "m",main="")

all_indices <- cbind(Date=GDPdata$X,w.indicators[,c(2,5,6,10,12)],GDPgrowth4[,1])
colnames(all_indices) <- c("Date", "Conf_CC", "Act_GBC", "Conf_FL", "Uncert_FL", "Uncert_EE", "GDPgrowth")

Conf_cc <- all_indices[,2]
Act_GBC <- all_indices[,3]
Conf_GBC <- all_indices[,4]
Unc_fl <- all_indices[,5] 
Unc_ee <- all_indices[,6]
GDPgrowth <- all_indices[,7]

par(mfrow=c(2,2))
ccf(Act_GBC, GDPgrowth, na.action = na.pass)
ccf(Conf_GBC, GDPgrowth, na.action = na.pass)
ccf(Unc_fl, GDPgrowth, na.action = na.pass)
ccf(Unc_ee, GDPgrowth, na.action = na.pass)

##========================================================
##-------------VAR Analysis-------------------------------
##========================================================
y1 <- ts.windicators[,1]
y2 <- ts.GDPgrowth4[,1]
name1 <- "Conf_CC"
name2 <- "RGDPGrowth"  
y1 <- na.approx(y1)
y2 <- na.approx(y2)
vardat <- ts.intersect(y1, y2)  
colnames(vardat) <- c(name1,name2)
infocrit <- VARselect(vardat, lag.max = 12, type = "const",exogen = dum94)
k_aic <- infocrit$selection[1]
k_hq  <- infocrit$selection[2]
k_sic <- infocrit$selection[3]
k <- min(k_aic,k_sic,k_hq)
var1 <- VAR(vardat,p=k,type="const",exogen = dum94)

y1 <- ts.windicators[,4]
y2 <- ts.GDPgrowth4[,1]
name1 <- "Act_GBC"
name2 <- "RGDPGrowth"  
y1 <- na.approx(y1)
y2 <- na.approx(y2)
vardat <- ts.intersect(y1, y2)  
colnames(vardat) <- c(name1,name2)
infocrit <- VARselect(vardat, lag.max = 12, type = "const",exogen = dum94)
k_aic <- infocrit$selection[1]
k_hq  <- infocrit$selection[2]
k_sic <- infocrit$selection[3]
k <- min(k_aic,k_sic,k_hq)
var2 <- VAR(vardat,p=k,type="const",exogen = dum94)

y1 <- ts.windicators[,5]
y2 <- ts.GDPgrowth4[,1]
name1 <- "Conf_GBC"
name2 <- "RGDPGrowth"  
y1 <- na.approx(y1)
y2 <- na.approx(y2)
vardat <- ts.intersect(y1, y2)  
colnames(vardat) <- c(name1,name2)
infocrit <- VARselect(vardat, lag.max = 12, type = "const",exogen = dum94)
k_aic <- infocrit$selection[1]
k_hq  <- infocrit$selection[2]
k_sic <- infocrit$selection[3]
k <- min(k_aic,k_sic,k_hq)
var3 <- VAR(vardat,p=k,type="const",exogen = dum94)

y1 <- ts.windicators[,7]
y2 <- ts.GDPgrowth4[,1]
name1 <- "Uncert_fl"
name2 <- "RGDPGrowth"  
y1 <- na.approx(y1)
y2 <- na.approx(y2)
vardat <- ts.intersect(y1, y2)  
colnames(vardat) <- c(name1,name2)
infocrit <- VARselect(vardat, lag.max = 12, type = "const")
k_aic <- infocrit$selection[1]
k_hq  <- infocrit$selection[2]
k_sic <- infocrit$selection[3]
k <- min(k_aic,k_sic,k_hq)
var4 <- VAR(vardat,p=k,type="const")

y1 <- ts.windicators[,9]
y2 <- ts.GDPgrowth4[,1]
name1 <- "Uncert_ee"
name2 <- "RGDPGrowth"  
y1 <- na.approx(y1)
y2 <- na.approx(y2)
vardat <- ts.intersect(y1, y2)  
colnames(vardat) <- c(name1,name2)
infocrit <- VARselect(vardat, lag.max = 12, type = "const")
k_aic <- infocrit$selection[1]
k_hq  <- infocrit$selection[2]
k_sic <- infocrit$selection[3]
k <- min(k_aic,k_sic,k_hq)
var5 <- VAR(vardat,p=k,type="const")

y1 <- ts.indicators[,9]
y2 <- ts.GDPgrowth4[,1]
name1 <- "unw.Uncert_ee"
name2 <- "RGDPGrowth"  
y1 <- na.approx(y1)
y2 <- na.approx(y2)
vardat <- ts.intersect(y1, y2)  
colnames(vardat) <- c(name1,name2)
infocrit <- VARselect(vardat, lag.max = 12, type = "const")
k_aic <- infocrit$selection[1]
k_hq  <- infocrit$selection[2]
k_sic <- infocrit$selection[3]
k <- min(k_aic,k_sic,k_hq)
var6 <- VAR(vardat,p=k,type="const")


##Granger causality tests
G <- data.frame()
G[1,1] <- causality(var1,cause = "Conf_CC")$Granger[4]
G[1,2] <- as.numeric(as.character(causality(var1,cause = "Conf_CC")$Granger[1]))
G[1,3] <- as.numeric(as.character(causality(var1,cause = "Conf_CC")$Granger[3]))
G[2,1] <- causality(var1,cause = "RGDPGrowth")$Granger[4]
G[2,2] <- as.numeric(as.character(causality(var1,cause = "RGDPGrowth")$Granger[1]))
G[2,3] <- as.numeric(as.character(causality(var1,cause = "RGDPGrowth")$Granger[3]))

G[3,1] <- causality(var2,cause = "Act_GBC")$Granger[4]
G[3,2] <- as.numeric(as.character(causality(var2,cause = "Act_GBC")$Granger[1]))
G[3,3] <- as.numeric(as.character(causality(var2,cause = "Act_GBC")$Granger[3]))
G[4,1] <- causality(var2,cause = "RGDPGrowth")$Granger[4]
G[4,2] <- as.numeric(as.character(causality(var2,cause = "RGDPGrowth")$Granger[1]))
G[4,3] <- as.numeric(as.character(causality(var2,cause = "RGDPGrowth")$Granger[3]))

G[5,1] <- causality(var3,cause = "Conf_GBC")$Granger[4]
G[5,2] <- as.numeric(as.character(causality(var3,cause = "Conf_GBC")$Granger[1]))
G[5,3] <- as.numeric(as.character(causality(var3,cause = "Conf_GBC")$Granger[3]))
G[6,1] <- causality(var3,cause = "RGDPGrowth")$Granger[4]
G[6,2] <- as.numeric(as.character(causality(var3,cause = "RGDPGrowth")$Granger[1]))
G[6,3] <- as.numeric(as.character(causality(var3,cause = "RGDPGrowth")$Granger[3]))

G[8,1] <- causality(var4,cause = "Uncert_fl")$Granger[4]
G[8,2] <- as.numeric(as.character(causality(var4,cause = "Uncert_fl")$Granger[1]))
G[8,3] <- as.numeric(as.character(causality(var4,cause = "Uncert_fl")$Granger[3]))
G[9,1] <- causality(var4,cause = "RGDPGrowth")$Granger[4]
G[9,2] <- as.numeric(as.character(causality(var4,cause = "RGDPGrowth")$Granger[1]))
G[9,3] <- as.numeric(as.character(causality(var4,cause = "RGDPGrowth")$Granger[3]))

G[10,1] <- causality(var5,cause = "Uncert_ee")$Granger[4]
G[10,2] <- as.numeric(as.character(causality(var5,cause = "Uncert_ee")$Granger[1]))
G[10,3] <- as.numeric(as.character(causality(var5,cause = "Uncert_ee")$Granger[3]))
G[11,1] <- causality(var5,cause = "RGDPGrowth")$Granger[4]
G[11,2] <- as.numeric(as.character(causality(var5,cause = "RGDPGrowth")$Granger[1]))
G[11,3] <- as.numeric(as.character(causality(var5,cause = "RGDPGrowth")$Granger[3]))

G[12,1] <- causality(var6,cause = "unw.Uncert_ee")$Granger[4]
G[12,2] <- as.numeric(as.character(causality(var6,cause = "unw.Uncert_ee")$Granger[1]))
G[12,3] <- as.numeric(as.character(causality(var6,cause = "unw.Uncert_ee")$Granger[3]))
G[13,1] <- causality(var6,cause = "RGDPGrowth")$Granger[4]
G[13,2] <- as.numeric(as.character(causality(var6,cause = "RGDPGrowth")$Granger[1]))
G[13,3] <- as.numeric(as.character(causality(var6,cause = "RGDPGrowth")$Granger[3]))

G[,2:3] <- round(G[,2:3],3)
mystars <- ifelse(G[,3] < .01, "***", ifelse(G[,3] < .05, "** ", ifelse(G[,3] < .1, "* ", " ")))
Gnew <- matrix(paste(G[,2], mystars, sep=""), ncol=1) 
Gnew[7] <- ""
G[,1] <- sub(".*: ", "", G[,1])
G[,2] <- Gnew
colnames(G) <- c("Granger causality H0:","statistic","p-value")

xt <- xtable(G, caption="Granger causality tests")
print(xt, "latex", include.rownames=FALSE,comment=FALSE, caption.placement = getOption("xtable.caption.placement", "top"), scalebox = 0.8)

#----------------------------------------------------------
##IRFs
#----------------------------------------------------------
irf.y1 <- irf(var3,impulse = "Conf_GBC", response = "RGDPGrowth", n.ahead = 12,runs = 1000, seed=12345) 
irf.y2 <- irf(var3,impulse = "RGDPGrowth", response = "Conf_GBC", n.ahead = 12,runs = 1000, seed=12345)

par(mfrow=c(1,2),mar=c(7,5,7,2), cex=0.6, new=FALSE)
plot(irf.y1,plot.type = c("single"))
par(mfrow=c(1,2),mar=c(6,4,4.2,1), cex=0.6, new = TRUE)
plot(irf.y2,plot.type = c("single"))

irf.y1 <- irf(var5,impulse = "Uncert_ee", response = "RGDPGrowth", n.ahead = 12,runs = 1000, seed=12345) 
irf.y2 <- irf(var5,impulse = "RGDPGrowth", response = "Uncert_ee", n.ahead = 12,runs = 1000, seed=12345)

par(mfrow=c(1,2),mar=c(7,5,7,2), cex=0.6, new=FALSE)
plot(irf.y1,plot.type = c("single"))
par(mfrow=c(1,2),mar=c(6,4,4.2,1), cex=0.6, new = TRUE)
plot(irf.y2,plot.type = c("single"))

irf.y1 <- irf(var6,impulse = "unw.Uncert_ee", response = "RGDPGrowth", n.ahead = 12,runs = 1000, seed=12345) 
irf.y2 <- irf(var6,impulse = "RGDPGrowth", response = "unw.Uncert_ee", n.ahead = 12,runs = 1000, seed=12345)

par(mfrow=c(1,2),mar=c(7,5,7,2), cex=0.6, new=FALSE)
plot(irf.y1,plot.type = c("single"))
par(mfrow=c(1,2),mar=c(6,4,4.2,1), cex=0.6, new = TRUE)
plot(irf.y2,plot.type = c("single"))

#Three-variable VAR

variable.3 <- function(y1, name1, y2, name2, y3, name3) {
    y1 <- na.approx(y1)
    y2 <- na.approx(y2)
    y3 <- na.approx(y3)
    vardat <- ts.intersect(y1, y2, y3)  
    colnames(vardat) <- c(name1,name2,name3)
    #infocrit <- VARselect(vardat, lag.max = 12, type = "const",exogen = dum94)
    infocrit <- VARselect(vardat, lag.max = 12, type = "const")
    k_aic <- infocrit$selection[1]
    k_hq  <- infocrit$selection[2]
    k_sic <- infocrit$selection[3]
    k <- min(k_aic,k_sic,k_hq)
    #var <- VAR(vardat,p=k,type="const",exogen = dum94)
    var <- VAR(vardat,p=k,type="const")
    return(var)
}

var <- variable.3(ts.indicators[,5],"Confidence",
                  ts.indicators[,9],"Uncertainty",
                  ts.GDPgrowth4[,1],"RGDPGrowth")

name1 <- "Confidence"
name2 <- "Uncertainty"   
name3 <- "RGDPGrowth"

irf.y1 <- irf(var,impulse = c(name1,name2), response = name3, n.ahead = 12,runs = 1000, seed=12345) 
par(mfrow=c(1,2),mar=c(5,4,4,2), cex=0.5)
plot(irf.y1,plot.type = c("single"))

irf.y2 <- irf(var,impulse = name3, response = c(name1,name2), n.ahead = 12,runs = 1000, seed=12345)
par(mfrow=c(1,2),mar=c(5,4,4,2), cex=0.5)
plot(irf.y2,plot.type = c("single"))




