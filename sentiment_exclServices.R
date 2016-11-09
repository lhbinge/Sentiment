##=========================================================================================##
## -------------------------------- SENTIMENT ---------------------------------------------##
##=========================================================================================##
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

##=====================##
## READING IN THE DATA ##
##=====================##
## GDP Data
##---------------------##
GDPdata <- read.csv("GDP Data.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)
GDPdata$X <- as.Date(GDPdata$X, format = "%Y/%m/%d")

##For Grpahing Business cycles
recessions.df = read.table(textConnection(
    "Peak, Trough
    1989-02-28, 1993-05-30
    1996-11-30, 1999-08-31
    2007-11-30, 2009-08-31"), sep=',',
    colClasses=c('Date', 'Date'), header=TRUE)

##---------------------##
## MANUFACTURING 
##---------------------##
BER.M <- rbind.fill(read.csv("Manufacturing.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE),
                    read.csv("Manufacturing_pre2001.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE))
BER.M <- BER.M[,1:62]
colnames(BER.M)[1:7] <- c("region","id","sector","weight","turnover","factor","surveyQ")

BER.M$surveyQ <- toupper(BER.M$surveyQ)
BER.M[nrow(BER.M)+1,1:6] <- BER.M[nrow(BER.M),1:6]   #fill in missing dates for easier aggregation.
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

##---------------------##
## BUILDING 
##---------------------##
BER.B <- rbind.fill(read.csv("Building.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE),
                    read.csv("Building_pre2001.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE))
BER.B <- BER.B[,1:22]
colnames(BER.B)[1:6] <- c("region","id","sector","weight","factor","surveyQ")

BER.B$surveyQ <- toupper(BER.B$surveyQ)
BER.B[nrow(BER.B)+1,1:5] <- BER.B[nrow(BER.B),1:5] #fill in missing dates for easier aggregation.
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

# replace 1,2,3 (Up, Same, Down) responses with 1,0,-1
for(i in 7:22) {
    BER.B[,i] <- replace(BER.B[,i], BER.B[,i]==2, 0)
    BER.B[,i] <- replace(BER.B[,i], BER.B[,i]==3,-1)
}
BER.B$Q1 <- replace(BER.B$Q1, BER.B$Q1==0,-1) # replace 0 (Unsatisfactory) responses with -1

##---------------------##
## TRADE 
##---------------------##
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

##---------------------##
## Motor Vehicles 
##---------------------##
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

##---------------------##
## SERVICES 
##---------------------##
BER.S <- read.csv("Services.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)
colnames(BER.S)[1:6] <- c("region","id","sector","weight","factor","surveyQ")

BER.S$surveyQ <- toupper(BER.S$surveyQ)
BER.S[nrow(BER.S)+1,1:5] <- BER.S[nrow(BER.S),1:5] 
BER.S[nrow(BER.S),"surveyQ"] <- "2005Q4" 

BER.S$region <- factor(BER.S$region)
BER.S$sector <- factor(BER.S$sector) #could include labels
BER.S$id <- factor(BER.S$id)
BER.S$surveyQ <- factor(BER.S$surveyQ)

# replace 1,2,3 (Up, Same, Down) responses with 1,0,-1
for(i in 7:21) {
    BER.S[,i] <- replace(BER.S[,i], BER.S[,i]==2, 0)
    BER.S[,i] <- replace(BER.S[,i], BER.S[,i]==3,-1)
}
BER.S$Q1 <- replace(BER.S$Q1, BER.S$Q1==0,-1) # replace 0 (Unsatisfactory) responses with -1

#---------

#Match the same or similar questions from the different surveys
#Create NAs for missing questions
tempBER.M <- cbind(BER.M[,c("id","surveyQ","Q20","Q7A","Q7P","Q1A","Q1P","Q8A","Q8P",            "Q4A","Q4P")],"Manucfaturing")
colnames(tempBER.M) <-    c("id","surveyQ", "Q1","Q2A","Q2P","Q3A","Q3P","Q4A","Q4P",            "Q6A","Q6P",  "Sector")
tempBER.M[,c("Q5A","Q5P")] <- NA
tempBER.B <- cbind(BER.B[,c("id","surveyQ", "Q1","Q2A","Q2P","Q3A","Q3P","Q4A","Q4P","Q5A","Q5P")],            "Construction")
colnames(tempBER.B) <-    c("id","surveyQ", "Q1","Q2A","Q2P","Q3A","Q3P","Q4A","Q4P","Q5A","Q5P",              "Sector")
tempBER.B[,c("Q6A","Q6P")] <- NA
tempBER.T <- cbind(BER.T[,c("id","surveyQ", "Q1","Q2A","Q2P","Q3A","Q3P","Q5A","Q5P","Q8",       "Q4A","Q4P")],"Trade")
colnames(tempBER.T) <-    c("id","surveyQ", "Q1","Q2A","Q2P","Q3A","Q3P","Q4A","Q4P","Q5A",      "Q6A","Q6P"  ,"Sector")
tempBER.T[,c("Q5P","Q6A","Q6P")] <- NA
tempBER.V <- cbind(BER.V[,c("id","surveyQ", "Q1","Q2A","Q2P","Q3A","Q3P",                        "Q4A","Q4P")],"Motor")
colnames(tempBER.V) <-    c("id","surveyQ", "Q1","Q2A","Q2P","Q3A","Q3P",                        "Q6A","Q6P"  ,"Sector")
tempBER.V[,c("Q4A","Q4P","Q5A","Q5P")] <- NA
tempBER.S <- cbind(BER.S[,c("id","surveyQ", "Q1","Q2A","Q2P","Q3A","Q3P","Q4A","Q4P","Q5A","Q5P")],            "Services")
colnames(tempBER.S) <-    c("id","surveyQ", "Q1","Q2A","Q2P","Q3A","Q3P","Q4A","Q4P","Q5A","Q5P",              "Sector")
tempBER.S[,c("Q6A","Q6P")] <- NA

BER <- tempBER.M
BER <- rbind(BER,tempBER.B,tempBER.T,tempBER.V)
BER <- BER[,c(12,1,2,3,4,5,6,7,8,9,10,11,13,14)]
rm(tempBER.M,tempBER.B,tempBER.T,tempBER.V,tempBER.S)
#--------------
#Plot Data
BERplot <- aggregate(BER$id, by=list(BER$surveyQ,BER$Sector), FUN = length)
BERplot$Group.1 <- as.Date(as.yearqtr(BERplot$Group.1, format = "%YQ%q"))
g <- ggplot(BERplot, aes(x=Group.1, y=x,fill=Group.2))
g <- g + geom_bar(stat="identity")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + scale_fill_discrete(name="Sector")
g <- g + scale_y_continuous(labels=comma)
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g <- g + ylab("Number of Respondents")
g <- g + xlab("Date")
g

##====================================================================================##
## -------------------------------- CONFIDENCE ---------------------------------------##
##====================================================================================##
## MANUFACTURING 
##---------------------##
confidence.M <- aggregate(BER.M[,(match("surveyQ",colnames(BER.M))+1):ncol(BER.M)], by=list(BER.M$surveyQ), FUN=mean, na.rm=TRUE)
confidence.M$Conf_cc.M <- rowMeans(confidence.M[,c("Q1A","Q4A","Q7A","Q8A","Q20")],na.rm = TRUE, dims = 1)
confidence.M$Conf_fl.M <- rowMeans(confidence.M[,c("Q1P","Q4P","Q7P","Q8P")],na.rm = TRUE, dims = 1)
#Row means for simple composite indicators

##Weighted versions
weeg <- function(temp) {
    temp <- cbind(factor=temp$factor,temp$factor*temp[(match("surveyQ",colnames(temp))+1):ncol(temp)])
    #temp <- colSums(temp, na.rm=TRUE, dims = 1)/sum(temp$factor, na.rm=TRUE)
    temp <- colSums(temp, na.rm=TRUE, dims = 1)/         #weight only by those that responded to a specific question
        sapply(colnames(temp), function(x) sum(temp$factor[!is.na(temp[colnames(temp) == x])]))
    return(temp)
}

w.confidence.M <- as.data.frame(t(sapply(levels(BER.M$surveyQ), function(kwartaal) weeg(BER.M[BER.M$surveyQ==kwartaal,]))))
w.confidence.M$Conf_cc.M <- rowMeans(w.confidence.M[,c("Q1A","Q4A","Q7A","Q8A","Q20")],na.rm = TRUE, dims = 1)
w.confidence.M$Conf_fl.M <- rowMeans(w.confidence.M[,c("Q1P","Q4P","Q7P","Q8P")],na.rm = TRUE, dims = 1)

##---------------------##
## BUILDING 
##---------------------##
confidence.B <- aggregate(BER.B[,(match("surveyQ",colnames(BER.B))+1):ncol(BER.B)], by=list(BER.B$surveyQ), FUN=mean, na.rm=TRUE)
confidence.B$Conf_cc.B <- rowMeans(confidence.B[,c("Q2A","Q3A","Q4A","Q5A","Q1")],na.rm = TRUE, dims = 1)
confidence.B$Conf_fl.B <- rowMeans(confidence.B[,c("Q2P","Q3P","Q4P","Q5P")],na.rm = TRUE, dims = 1)

##Weighted versions
w.confidence.B <- as.data.frame(t(sapply(levels(BER.B$surveyQ), function(kwartaal) weeg(BER.B[BER.B$surveyQ==kwartaal,]))))
w.confidence.B$Conf_cc.B <- rowMeans(w.confidence.B[,c("Q2A","Q3A","Q4A","Q5A","Q1")],na.rm = TRUE, dims = 1)
w.confidence.B$Conf_fl.B <- rowMeans(w.confidence.B[,c("Q2P","Q3P","Q4P","Q5P")],na.rm = TRUE, dims = 1)

##---------------------##
## TRADE 
##---------------------##
confidence.T <- aggregate(BER.T[,(match("surveyQ",colnames(BER.T))+1):ncol(BER.T)], by=list(BER.T$surveyQ), FUN=mean, na.rm=TRUE)
confidence.T$Conf_cc.T <- rowMeans(confidence.T[,c("Q2A","Q3A","Q4A","Q5A","Q1")],na.rm = TRUE, dims = 1)
confidence.T$Conf_fl.T <- rowMeans(confidence.T[,c("Q2P","Q3P","Q4P","Q5P")],na.rm = TRUE, dims = 1)

##Weighted versions
w.confidence.T <- as.data.frame(t(sapply(levels(BER.T$surveyQ), function(kwartaal) weeg(BER.T[BER.T$surveyQ==kwartaal,]))))
w.confidence.T$Conf_cc.T <- rowMeans(w.confidence.T[,c("Q2A","Q3A","Q4A","Q5A","Q1")],na.rm = TRUE, dims = 1)
w.confidence.T$Conf_fl.T <- rowMeans(w.confidence.T[,c("Q2P","Q3P","Q4P","Q5P")],na.rm = TRUE, dims = 1)

##---------------------##
## Motor Vehicles 
##---------------------##
confidence.V <- aggregate(BER.V[,(match("surveyQ",colnames(BER.V))+1):ncol(BER.V)], by=list(BER.V$surveyQ), FUN=mean, na.rm=TRUE)
confidence.V$Conf_cc.V <- rowMeans(confidence.V[,c("Q2A","Q3A","Q4A","Q7A","Q8A","Q11A","Q12A","Q13A","Q1")],na.rm = TRUE, dims = 1)
confidence.V$Conf_fl.V <- rowMeans(confidence.V[,c("Q2P","Q3P","Q4P","Q7P","Q8P","Q11P","Q12P","Q13P")],na.rm = TRUE, dims = 1)

##Weighted versions
w.confidence.V <- as.data.frame(t(sapply(levels(BER.V$surveyQ), function(kwartaal) weeg(BER.V[BER.V$surveyQ==kwartaal,]))))
w.confidence.V$Conf_cc.V <- rowMeans(w.confidence.V[,c("Q2A","Q3A","Q4A","Q7A","Q8A","Q11A","Q12A","Q13A","Q1")],na.rm = TRUE, dims = 1)
w.confidence.V$Conf_fl.V <- rowMeans(w.confidence.V[,c("Q2P","Q3P","Q4P","Q7P","Q8P","Q11P","Q12P","Q13P")],na.rm = TRUE, dims = 1)

##---------------------##
## SERVICES 
##---------------------##
confidence.S <- aggregate(BER.S[,(match("surveyQ",colnames(BER.S))+1):ncol(BER.S)], by=list(BER.S$surveyQ), FUN=mean, na.rm=TRUE)
confidence.S$Conf_cc.S <- rowMeans(confidence.S[,c("Q2A","Q3A","Q4A","Q5A","Q1")],na.rm = TRUE, dims = 1)
confidence.S$Conf_fl.S <- rowMeans(confidence.S[,c("Q2P","Q3P","Q4P","Q5P")],na.rm = TRUE, dims = 1)

##Weighted versions
w.confidence.S <- as.data.frame(t(sapply(levels(BER.S$surveyQ), function(kwartaal) weeg(BER.S[BER.S$surveyQ==kwartaal,]))))
w.confidence.S$Conf_cc.S <- rowMeans(w.confidence.S[,c("Q2A","Q3A","Q4A","Q5A","Q1")],na.rm = TRUE, dims = 1)
w.confidence.S$Conf_fl.S <- rowMeans(w.confidence.S[,c("Q2P","Q3P","Q4P","Q5P")],na.rm = TRUE, dims = 1)

##=================================##
## AGGREGATING                              
##=================================##
##Weighted versions
weights <- cbind(Date=levels(BER.M$surveyQ),GDPdata[,2:5])

names(w.confidence.M)[grep("Q7", colnames(w.confidence.M))] <- paste(names(w.confidence.M)[grep("Q7", colnames(w.confidence.M))],"M",sep=".")
names(w.confidence.B)[grep("Q2", colnames(w.confidence.B))] <- paste(names(w.confidence.B)[grep("Q2", colnames(w.confidence.B))],"B",sep=".")
names(w.confidence.T)[grep("Q2", colnames(w.confidence.T))] <- paste(names(w.confidence.T)[grep("Q2", colnames(w.confidence.T))],"T",sep=".")
names(w.confidence.V)[grep("Q2", colnames(w.confidence.V))] <- paste(names(w.confidence.V)[grep("Q2", colnames(w.confidence.V))],"V",sep=".")

w.confidence <- merge(w.confidence.M[,c("Q7A.M","Q7P.M","Conf_cc.M","Conf_fl.M")], 
                      w.confidence.B[,c("Q2A.B","Q2P.B","Conf_cc.B","Conf_fl.B")], by.x="row.names", by.y="row.names",all.x=TRUE)
w.confidence <- merge(w.confidence, w.confidence.T[,c("Q2A.T","Q2P.T","Conf_cc.T","Conf_fl.T")], by.x="Row.names", by.y="row.names",all.x=TRUE)
w.confidence <- merge(w.confidence, w.confidence.V[,c("Q2A.V","Q2P.V","Conf_cc.V","Conf_fl.V")], by.x="Row.names", by.y="row.names",all.x=TRUE)
colnames(w.confidence)[1] <- "Date"

w.confidence$Q2A <- sapply(w.confidence$Date, function(x) weighted.mean(w.confidence[which(w.confidence$Date==x),c(2,6,10,14)], 
                                                                        weights[weights$Date==x,-1],na.rm=TRUE))
w.confidence$Q2P <- sapply(w.confidence$Date, function(x) weighted.mean(w.confidence[which(w.confidence$Date==x),c(3,7,11,15)], 
                                                                        weights[weights$Date==x,-1],na.rm=TRUE))
w.confidence$Conf_cc <- sapply(w.confidence$Date, function(x) weighted.mean(w.confidence[which(w.confidence$Date==x),c(4,8,12,16)], 
                                                                            weights[weights$Date==x,-1],na.rm=TRUE))
w.confidence$Conf_fl <- sapply(w.confidence$Date, function(x) weighted.mean(w.confidence[which(w.confidence$Date==x),c(5,9,13,17)], 
                                                                            weights[weights$Date==x,-1],na.rm=TRUE))
w.confidence$Date <- GDPdata[,1]
#-------------------
#Unweighted versions
confidence <- aggregate(BER[,(match("surveyQ",colnames(BER))+1):ncol(BER)], by=list(BER$surveyQ), FUN=mean, na.rm=TRUE)
confidence$Conf_cc <- rowMeans(confidence[,c("Q2A","Q3A","Q4A","Q5A","Q6A","Q1")],na.rm = TRUE, dims = 1)
confidence$Conf_fl <- rowMeans(confidence[,c("Q2P","Q3P","Q4P","Q5P","Q6P")],na.rm = TRUE, dims = 1)
confidence[,1] <- GDPdata[,1]
colnames(confidence)[1] <- "Date"

##--------------------------------------------------------------------------------
indicator_plot <- w.confidence[,c("Date","Conf_cc","Conf_fl")]
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_line()
g <- g + ylab("Indicator")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) 
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g <- g + theme(legend.position="bottom")
g

indicator_plot <- w.confidence[,c("Date","Conf_cc","Conf_fl","Q2A","Q2P")]
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_line()
g <- g + ylab("Indicator")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) 
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g <- g + theme(legend.position="bottom")
g

indicator_plot <- cbind(w.confidence[,c("Date","Conf_cc","Conf_fl")],confidence[,c("Conf_cc","Conf_fl")])
colnames(indicator_plot) <- c("Date","Weighted.Conf_cc","Weighted.Conf_fl","Unw.Conf_cc","Unw.Conf_fl")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_line()
g <- g + ylab("Indicator")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) 
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g <- g + theme(legend.position="bottom")
g

indicator_plot <- w.confidence[,c("Date","Conf_cc","Conf_fl")]
g <- ggplot(indicator_plot) 
g <- g + geom_line(aes(x=Date, y=Conf_cc, colour="Current Conditions"), size = 1)
g <- g + geom_line(aes(x=Date, y=Conf_fl, colour="Forward_looking"), size = 1)
g <- g + theme_bw()
g <- g + labs(color="Legend text")
g <- g + geom_rect(data=recessions.df, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='grey', alpha=0.5)
g <- g + ylab("Uncertainty")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g <- g + theme(legend.position="bottom")
g

indicator_plot <- w.confidence[,c("Date","Q2A","Q2P")]
g <- ggplot(indicator_plot) 
g <- g + geom_line(aes(x=Date, y=Q2A, colour="Current Conditions"), size = 1)
g <- g + geom_line(aes(x=Date, y=Q2P, colour="Forward_looking"), size = 1)
g <- g + theme_bw()
g <- g + labs(color="Legend text")
g <- g + geom_rect(data=recessions.df, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='grey', alpha=0.5)
g <- g + ylab("Uncertainty")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g <- g + theme(legend.position="bottom")
g



##====================================================================================##
## -------------------------------- UNCERTAINTY --------------------------------------##
##====================================================================================##
se <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x))*(length(na.omit(x))-1)) #adjust for (n-1)
##---------------------##
## MANUFACTURING                    
##---------------------##
uncertainty.M <- aggregate(BER.M[,(match("surveyQ",colnames(BER.M))+1):ncol(BER.M)], by=list(BER.M$surveyQ), FUN=se)
uncertainty.M$Uncert_cc.M <- rowMeans(uncertainty.M[,c("Q1A","Q4A","Q7A","Q8A","Q20")],na.rm = TRUE, dims = 1)
uncertainty.M$Uncert_fl.M <- rowMeans(uncertainty.M[,c("Q1P","Q4P","Q7P","Q8P")],na.rm = TRUE, dims = 1)
colnames(uncertainty.M)[1] <- "Date"

##Weighted versions
weeg.2 <- function(temp) {
    temp <- cbind(factor=temp$factor,temp$factor*temp[(match("surveyQ",colnames(temp))+1):ncol(temp)])
    frac.up <- sapply(1:ncol(temp), function(x) sum(temp[which(temp[,x]>0),x],na.rm=TRUE))/
        sapply(colnames(temp), function(x) sum(temp$factor[!is.na(temp[colnames(temp) == x])]))
    frac.dn <- sapply(1:ncol(temp), function(x) sum(temp[which(temp[,x]<0),x],na.rm=TRUE))/
        sapply(colnames(temp), function(x) sum(temp$factor[!is.na(temp[colnames(temp) == x])]))
    #weight only by those that responded to a specific question 
    ind <- sqrt(frac.up-frac.dn-(frac.up+frac.dn)^2)        #this is the standard devation
    return(ind)
}
w.uncertainty.M <- as.data.frame(t(sapply(levels(BER.M$surveyQ), function(kwartaal) weeg.2(BER.M[BER.M$surveyQ==kwartaal,]))))[,-1]
colnames(w.uncertainty.M) <- colnames(BER.M[(match("surveyQ",colnames(BER.M))+1):ncol(BER.M)])
w.uncertainty.M$Uncert_cc.M <- rowMeans(w.uncertainty.M[,c("Q1A","Q4A","Q7A","Q8A","Q20")],na.rm = TRUE, dims = 1)
w.uncertainty.M$Uncert_fl.M <- rowMeans(w.uncertainty.M[,c("Q1P","Q4P","Q7P","Q8P")],na.rm = TRUE, dims = 1)

##Expectations Errors-----------------
#remove duplicates
dups <- BER.M[duplicated(BER.M[,c("id","surveyQ")]) | duplicated(BER.M[,c("id","surveyQ")], fromLast = TRUE),]
BER.M <- BER.M[!duplicated(BER.M[,c("id","surveyQ")]),]

#Compare the expectations of firms in Q7P (forward-looking) in period t to the realizations in Q7A in period t+1.
exp.error <- function(temp) {
    error <- merge(uncertainty.M[,c(1,ncol(uncertainty.M))],temp,by.x="Date",by.y="surveyQ", all.x=TRUE)
    for(t in 1:nrow(error)) {
        error$eQ1[t]  <- error$Q1A[(t+1)] - error$Q1P[t]
        error$eQ4[t]  <- error$Q4A[(t+1)] - error$Q4P[t]
        error$eQ7[t]  <- error$Q7A[(t+1)] - error$Q7P[t]
        error$eQ8[t]  <- error$Q8A[(t+1)] - error$Q8P[t]
    }
    return(error[,c(1,8,grep("eQ", colnames(error)))])
}

errors.M <- data.frame()
for(i in levels(BER.M$id)){
    errors.M <- rbind(errors.M, exp.error(BER.M[which(BER.M$id==i),])) 
}
exp.errors.M <- aggregate(errors.M, by=list(errors.M$Date), FUN=se)[-2]
uncertainty.M$eQ7.M <- exp.errors.M$eQ7
uncertainty.M$Uncert_ee.M <- rowMeans(exp.errors.M[,-1:-2],na.rm = TRUE, dims = 1)

##Weighted versions
weeg.3 <- function(errors) {
    temp <- cbind(factor=errors$factor,errors$factor*errors[,3:ncol(errors)])
    xbar <- colSums(temp, na.rm=TRUE, dims = 1)/
        sapply(colnames(temp), function(x) sum(temp$factor[!is.na(temp[colnames(temp) == x])]))
    temp <- errors[,-1]
    #this is the weighted standard devation
    ind <- sqrt(sapply(colnames(temp), function(x) sum((temp[,x]-xbar[x])*(temp[,x]-xbar[x])*temp$factor,na.rm=TRUE))/
                    sapply(colnames(temp), function(x) sum(temp$factor[!is.na(temp[colnames(temp) == x])]))) 
    return(ind)
}

w.errors <- as.data.frame(t(sapply(levels(BER.M$surveyQ), function(kwartaal) weeg.3(errors.M[errors.M$Date==kwartaal,]))))[,-1]
w.uncertainty.M$eQ7 <- w.errors$eQ7
w.uncertainty.M$Uncert_ee.M <- rowMeans(w.errors,na.rm = TRUE, dims = 1)

##---------------------##
## BUILDING 
##---------------------##
uncertainty.B <- aggregate(BER.B[,(match("surveyQ",colnames(BER.B))+1):ncol(BER.B)], by=list(BER.B$surveyQ), FUN=se)
uncertainty.B$Uncert_cc.B <- rowMeans(uncertainty.B[,c("Q2A","Q3A","Q4A","Q5A","Q1")],na.rm = TRUE, dims = 1)
uncertainty.B$Uncert_fl.B <- rowMeans(uncertainty.B[,c("Q2P","Q3P","Q4P","Q5P")],na.rm = TRUE, dims = 1)
colnames(uncertainty.B)[1] <- "Date"

##Weighted versions
w.uncertainty.B <- as.data.frame(t(sapply(levels(BER.B$surveyQ), function(kwartaal) weeg.2(BER.B[BER.B$surveyQ==kwartaal,]))))[,-1]
colnames(w.uncertainty.B) <- colnames(BER.B[(match("surveyQ",colnames(BER.B))+1):ncol(BER.B)])
w.uncertainty.B$Uncert_cc.B <- rowMeans(w.uncertainty.B[,c("Q2A","Q3A","Q4A","Q5A","Q1")],na.rm = TRUE, dims = 1)
w.uncertainty.B$Uncert_fl.B <- rowMeans(w.uncertainty.B[,c("Q2P","Q3P","Q4P","Q5P")],na.rm = TRUE, dims = 1)

##Expectations Errors-----------------
dups <- BER.B[duplicated(BER.B[,c("id","surveyQ")]) | duplicated(BER.B[,c("id","surveyQ")], fromLast = TRUE),]
BER.B <- BER.B[!duplicated(BER.B[,c("id","surveyQ")]),]

#Compare the expectations of firms in Q7P (forward-looking) in period t to the realizations in Q7A in period t+1.
exp.error <- function(temp) {
    error <- merge(uncertainty.B[,c(1,ncol(uncertainty.B))],temp,by.x="Date",by.y="surveyQ", all.x=TRUE)
    for(t in 1:nrow(error)) {
        error$eQ2[t]  <- error$Q2A[(t+1)] - error$Q2P[t]
        error$eQ3[t]  <- error$Q3A[(t+1)] - error$Q3P[t]
        error$eQ4[t]  <- error$Q4A[(t+1)] - error$Q4P[t]
        error$eQ5[t]  <- error$Q5A[(t+1)] - error$Q5P[t]
    }
    return(error[,c(1,7,grep("eQ", colnames(error)))])
}
errors.B <- data.frame()
for(i in levels(BER.B$id)){
    errors.B <- rbind(errors.B, exp.error(BER.B[which(BER.B$id==i),])) 
}
exp.errors.B <- aggregate(errors.B, by=list(errors.B$Date), FUN=se)[-2]
uncertainty.B$eQ2.B <- exp.errors.B$eQ2
uncertainty.B$Uncert_ee.B <- rowMeans(exp.errors.B[,-1:-2],na.rm = TRUE, dims = 1)

##Weighted versions
w.errors <- as.data.frame(t(sapply(levels(BER.B$surveyQ), function(kwartaal) weeg.3(errors.B[errors.B$Date==kwartaal,]))))[,-1]
w.uncertainty.B$eQ2 <- w.errors$eQ2
w.uncertainty.B$Uncert_ee.B <- rowMeans(w.errors,na.rm = TRUE, dims = 1)

##---------------------##
## TRADE 
##---------------------##
uncertainty.T <- aggregate(BER.T[,(match("surveyQ",colnames(BER.T))+1):ncol(BER.T)], by=list(BER.T$surveyQ), FUN=se)
uncertainty.T$Uncert_cc.T <- rowMeans(uncertainty.T[,c("Q2A","Q3A","Q4A","Q5A","Q1")],na.rm = TRUE, dims = 1)
uncertainty.T$Uncert_fl.T <- rowMeans(uncertainty.T[,c("Q2P","Q3P","Q4P","Q5P")],na.rm = TRUE, dims = 1)
colnames(uncertainty.T)[1] <- "Date"

##Weighted versions
w.uncertainty.T <- as.data.frame(t(sapply(levels(BER.T$surveyQ), function(kwartaal) weeg.2(BER.T[BER.T$surveyQ==kwartaal,]))))[,-1]
colnames(w.uncertainty.T) <- colnames(BER.T[(match("surveyQ",colnames(BER.T))+1):ncol(BER.T)])
w.uncertainty.T$Uncert_cc.T <- rowMeans(w.uncertainty.T[,c("Q2A","Q3A","Q4A","Q5A","Q1")],na.rm = TRUE, dims = 1)
w.uncertainty.T$Uncert_fl.T <- rowMeans(w.uncertainty.T[,c("Q2P","Q3P","Q4P","Q5P")],na.rm = TRUE, dims = 1)

##Expectations Errors-----------------
exp.error <- function(temp) {
    error <- merge(uncertainty.T[,c(1,ncol(uncertainty.T))],temp,by.x="Date",by.y="surveyQ", all.x=TRUE)
    for(t in 1:nrow(error)) {
        error$eQ2[t]  <- error$Q2A[(t+1)] - error$Q2P[t]
        error$eQ3[t]  <- error$Q3A[(t+1)] - error$Q3P[t]
        error$eQ4[t]  <- error$Q4A[(t+1)] - error$Q4P[t]
        error$eQ5[t]  <- error$Q5A[(t+1)] - error$Q5P[t]
    }
    return(error[,c(1,7,grep("eQ", colnames(error)))])
}
errors.T <- data.frame()
for(i in levels(BER.T$id)){
    errors.T <- rbind(errors.T, exp.error(BER.T[which(BER.T$id==i),])) 
}
exp.errors.T <- aggregate(errors.T, by=list(errors.T$Date), FUN=se)[-2]
uncertainty.T$eQ2.T <- exp.errors.T$eQ2
uncertainty.T$Uncert_ee.T <- rowMeans(exp.errors.T[,-1:-2],na.rm = TRUE, dims = 1)

##Weighted versions
w.errors <- as.data.frame(t(sapply(levels(BER.T$surveyQ), function(kwartaal) weeg.3(errors.T[errors.T$Date==kwartaal,]))))[,-1]
w.uncertainty.T$eQ2 <- w.errors$eQ2
w.uncertainty.T$Uncert_ee.T <- rowMeans(w.errors,na.rm = TRUE, dims = 1)

##---------------------##
## Motor Vehicles 
##---------------------##
uncertainty.V <- aggregate(BER.V[,(match("surveyQ",colnames(BER.V))+1):ncol(BER.V)], by=list(BER.V$surveyQ), FUN=se)
uncertainty.V$Uncert_cc.V <- rowMeans(uncertainty.V[,c("Q2A","Q3A","Q4A","Q7A","Q8A","Q11A","Q12A","Q13A","Q1")],na.rm = TRUE, dims = 1)
uncertainty.V$Uncert_fl.V <- rowMeans(uncertainty.V[,c("Q2P","Q3P","Q4P","Q7P","Q8P","Q11P","Q12P","Q13P")],na.rm = TRUE, dims = 1)
colnames(uncertainty.V)[1] <- "Date"

##Weighted versions
w.uncertainty.V <- as.data.frame(t(sapply(levels(BER.V$surveyQ), function(kwartaal) weeg.2(BER.V[BER.V$surveyQ==kwartaal,]))))[,-1]
colnames(w.uncertainty.V) <- colnames(BER.V[(match("surveyQ",colnames(BER.V))+1):ncol(BER.V)])
w.uncertainty.V$Uncert_cc.V <- rowMeans(w.uncertainty.V[,c("Q2A","Q3A","Q4A","Q7A","Q8A","Q11A","Q12A","Q13A","Q1")],na.rm = TRUE, dims = 1)
w.uncertainty.V$Uncert_fl.V <- rowMeans(w.uncertainty.V[,c("Q2P","Q3P","Q4P","Q7P","Q8P","Q11P","Q12P","Q13P")],na.rm = TRUE, dims = 1)

##Expectations Errors-----------------
dups <- BER.V[duplicated(BER.V[,c("id","surveyQ")]) | duplicated(BER.V[,c("id","surveyQ")], fromLast = TRUE),]
BER.V <- BER.V[!duplicated(BER.V[,c("id","surveyQ")]),]

exp.error <- function(temp) {
    error <- merge(uncertainty.V[,c(1,ncol(uncertainty.V))],temp,by.x="Date",by.y="surveyQ", all.x=TRUE)
    for(t in 1:nrow(error)) {
        error$eQ2[t]  <- error$Q2A[(t+1)] - error$Q2P[t]
        error$eQ3[t]  <- error$Q3A[(t+1)] - error$Q3P[t]
        error$eQ4[t]  <- error$Q4A[(t+1)] - error$Q4P[t]
        error$eQ7[t]  <- error$Q7A[(t+1)] - error$Q7P[t]
        error$eQ8[t]  <- error$Q8A[(t+1)] - error$Q8P[t]
        error$eQ11[t]  <- error$Q11A[(t+1)] - error$Q11P[t]
        error$eQ12[t]  <- error$Q12A[(t+1)] - error$Q12P[t]
        error$eQ13[t]  <- error$Q13A[(t+1)] - error$Q13P[t]
    }
    return(error[,c(1,7,grep("eQ", colnames(error)))])
}
errors.V <- data.frame()
for(i in levels(BER.V$id)){
    errors.V <- rbind(errors.V, exp.error(BER.V[which(BER.V$id==i),])) 
}
exp.errors.V <- aggregate(errors.V, by=list(errors.V$Date), FUN=se)[-2]
uncertainty.V$eQ2.V <- exp.errors.V$eQ2
uncertainty.V$Uncert_ee.V <- rowMeans(exp.errors.V[,-1:-2],na.rm = TRUE, dims = 1)

##Weighted versions
w.errors <- as.data.frame(t(sapply(levels(BER.V$surveyQ), function(kwartaal) weeg.3(errors.V[errors.V$Date==kwartaal,]))))[,-1]
w.uncertainty.V$eQ2 <- w.errors$eQ2
w.uncertainty.V$Uncert_ee.V <- rowMeans(w.errors,na.rm = TRUE, dims = 1)

##---------------------##
## SERVICES 
##---------------------##
uncertainty.S <- aggregate(BER.S[,(match("surveyQ",colnames(BER.S))+1):ncol(BER.S)], by=list(BER.S$surveyQ), FUN=se)
uncertainty.S$Uncert_cc.S <- rowMeans(uncertainty.S[,c("Q2A","Q3A","Q4A","Q5A","Q1")],na.rm = TRUE, dims = 1)
uncertainty.S$Uncert_fl.S <- rowMeans(uncertainty.S[,c("Q2P","Q3P","Q4P","Q5P")],na.rm = TRUE, dims = 1)
colnames(uncertainty.S)[1] <- "Date"

##Weighted versions
w.uncertainty.S <- as.data.frame(t(sapply(levels(BER.S$surveyQ), function(kwartaal) weeg.2(BER.S[BER.S$surveyQ==kwartaal,]))))[,-1]
colnames(w.uncertainty.S) <- colnames(BER.S[(match("surveyQ",colnames(BER.S))+1):ncol(BER.S)])
w.uncertainty.S$Uncert_cc.S <- rowMeans(w.uncertainty.S[,c("Q2A","Q3A","Q4A","Q5A","Q1")],na.rm = TRUE, dims = 1)
w.uncertainty.S$Uncert_fl.S <- rowMeans(w.uncertainty.S[,c("Q2P","Q3P","Q4P","Q5P")],na.rm = TRUE, dims = 1)

##Expectations Errors-----------------
dups <- BER.S[duplicated(BER.S[,c("id","surveyQ")]) | duplicated(BER.S[,c("id","surveyQ")], fromLast = TRUE),]
BER.S <- BER.S[!duplicated(BER.S[,c("id","surveyQ")]),]

#Compare the expectations of firms in Q7P (forward-looking) in period t to the realizations in Q7A in period t+1.
exp.error <- function(temp) {
    error <- merge(uncertainty.S[,c(1,ncol(uncertainty.S))],temp,by.x="Date",by.y="surveyQ", all.x=TRUE)
    for(t in 1:nrow(error)) {
        error$eQ2[t]  <- error$Q2A[(t+1)] - error$Q2P[t]
        error$eQ3[t]  <- error$Q3A[(t+1)] - error$Q3P[t]
        error$eQ4[t]  <- error$Q4A[(t+1)] - error$Q4P[t]
        error$eQ5[t]  <- error$Q5A[(t+1)] - error$Q5P[t]
    }
    return(error[,c(1,7,grep("eQ", colnames(error)))])
}
errors.S <- data.frame()
for(i in levels(BER.S$id)){
    errors.S <- rbind(errors.S, exp.error(BER.S[which(BER.S$id==i),])) 
}
exp.errors.S <- aggregate(errors.S, by=list(errors.S$Date), FUN=se)[-2]
uncertainty.S$eQ2.S <- exp.errors.S$eQ2
uncertainty.S$Uncert_ee.S <- rowMeans(exp.errors.S[,-1:-2],na.rm = TRUE, dims = 1)

##Weighted versions
w.errors <- as.data.frame(t(sapply(levels(BER.S$surveyQ), function(kwartaal) weeg.3(errors.S[errors.S$Date==kwartaal,]))))[,-1]
w.uncertainty.S$eQ2 <- w.errors$eQ2
w.uncertainty.S$Uncert_ee.S <- rowMeans(w.errors,na.rm = TRUE, dims = 1)

##=================================##
## AGGREGATING                              
##=================================##
##Weighted versions
names(w.uncertainty.M)[grep("Q7", colnames(w.uncertainty.M))] <- paste(names(w.uncertainty.M)[grep("Q7", colnames(w.uncertainty.M))],"M",sep=".")
names(w.uncertainty.B)[grep("Q2", colnames(w.uncertainty.B))] <- paste(names(w.uncertainty.B)[grep("Q2", colnames(w.uncertainty.B))],"B",sep=".")
names(w.uncertainty.T)[grep("Q2", colnames(w.uncertainty.T))] <- paste(names(w.uncertainty.T)[grep("Q2", colnames(w.uncertainty.T))],"T",sep=".")
names(w.uncertainty.V)[grep("Q2", colnames(w.uncertainty.V))] <- paste(names(w.uncertainty.V)[grep("Q2", colnames(w.uncertainty.V))],"V",sep=".")

w.uncertainty <- merge(w.uncertainty.M[,c("Uncert_cc.M","Q7P.M","Uncert_fl.M","eQ7.M","Uncert_ee.M")], 
                       w.uncertainty.B[,c("Uncert_cc.B","Q2P.B","Uncert_fl.B","eQ2.B","Uncert_ee.B")], by.x="row.names", by.y="row.names",all.x=TRUE)
w.uncertainty <- merge(w.uncertainty, w.uncertainty.T[,c("Uncert_cc.T","Q2P.T","Uncert_fl.T","eQ2.T","Uncert_ee.T")], by.x="Row.names", by.y="row.names",all.x=TRUE)
w.uncertainty <- merge(w.uncertainty, w.uncertainty.V[,c("Uncert_cc.V","Q2P.V","Uncert_fl.V","eQ2.V","Uncert_ee.V")], by.x="Row.names", by.y="row.names",all.x=TRUE)
colnames(w.uncertainty)[1] <- "Date"

w.uncertainty$Uncert_cc<- sapply(w.uncertainty$Date, function(x) weighted.mean(w.uncertainty[which(w.uncertainty$Date==x),c(2,7,12,17)], 
                                                                               weights[weights$Date==x,-1],na.rm=TRUE))
w.uncertainty$Q2P      <- sapply(w.uncertainty$Date, function(x) weighted.mean(w.uncertainty[which(w.uncertainty$Date==x),c(3,8,13,18)], 
                                                                               weights[weights$Date==x,-1],na.rm=TRUE))
w.uncertainty$Uncert_fl<- sapply(w.uncertainty$Date, function(x) weighted.mean(w.uncertainty[which(w.uncertainty$Date==x),c(4,9,14,19)], 
                                                                               weights[weights$Date==x,-1],na.rm=TRUE))
w.uncertainty$eQ2      <- sapply(w.uncertainty$Date, function(x) weighted.mean(w.uncertainty[which(w.uncertainty$Date==x),c(5,10,15,20)], 
                                                                               weights[weights$Date==x,-1],na.rm=TRUE))
w.uncertainty$Uncert_ee<- sapply(w.uncertainty$Date, function(x) weighted.mean(w.uncertainty[which(w.uncertainty$Date==x),c(6,11,16,21)], 
                                                                               weights[weights$Date==x,-1],na.rm=TRUE))
w.uncertainty$Date <- GDPdata[,1]
#-------------------
#Unweighted versions
uncertainty <- aggregate(BER[,(match("surveyQ",colnames(BER))+1):ncol(BER)], by=list(BER$surveyQ), FUN=se)
uncertainty$Uncert_cc <- rowMeans(uncertainty[,c("Q2A","Q3A","Q4A","Q5A","Q6A","Q1")],na.rm = TRUE, dims = 1)
uncertainty$Uncert_fl <- rowMeans(uncertainty[,c("Q2P","Q3P","Q4P","Q5P","Q6P")],na.rm = TRUE, dims = 1)
colnames(uncertainty)[1] <- "Date"

#Match the same or similar questions from the different surveys
#Create NAs for missing questions
tempE.M <- cbind(errors.M[,c("Date","eQ7","eQ1","eQ8",      "eQ4")],"Manucfaturing")
colnames(tempE.M) <-       c("Date","eQ2","eQ3","eQ4",      "eQ6",  "Sector")
tempE.M[,c("eQ5")] <- NA
tempE.B <- cbind(errors.B[,c("Date","eQ2","eQ3","eQ4","eQ5")],      "Construction")
colnames(tempE.B) <-       c("Date","eQ2","eQ3","eQ4","eQ5",        "Sector")
tempE.B[,c("eQ6")] <- NA
tempE.T <- cbind(errors.T[,c("Date","eQ2","eQ3","eQ5")],            "Trade")
colnames(tempE.T) <-       c("Date","eQ2","eQ3","eQ4",              "Sector")
tempE.T[,c("eQ5","eQ6")] <- NA
tempE.V <- cbind(errors.V[,c("Date","eQ2","eQ3",            "eQ4")],"Motor")
colnames(tempE.V) <-       c("Date","eQ2","eQ3",            "eQ6",  "Sector")
tempE.V[,c("eQ4","eQ5")] <- NA
tempE.S <- cbind(errors.S[,c("Date","eQ2","eQ3","eQ4","eQ5")],      "Services")
colnames(tempE.S) <-       c("Date","eQ2","eQ3","eQ4","eQ5",        "Sector")
tempE.S[,c("eQ6")] <- NA

errors <- tempE.M
errors <- rbind(errors,tempE.B,tempE.T,tempE.V)
errors <- errors[,c(1,6,2,3,4,7,5)]
rm(tempE.M,tempE.B,tempE.T,tempE.V,tempE.S)

##Could plot errors
exp.errors <- aggregate(errors[,-1:-2], by=list(errors$Date), FUN=se)
uncertainty$eQ2 <- exp.errors$eQ2
uncertainty$Uncert_ee <- rowMeans(exp.errors[,-1:-2],na.rm = TRUE, dims = 1)
uncertainty$Date <- GDPdata[,1]

##-------------------------------------------------------------------------------
w.uncert.norm <- cbind(Date=w.uncertainty[,1],as.data.frame(scale(w.uncertainty[,-1])))
uncert.norm <- cbind(Date=uncertainty[,1],as.data.frame(scale(uncertainty[,-1])))


indicator_plot <- uncert.norm[,c("Date","Q2P","Uncert_fl","eQ2","Uncert_ee")]
#colnames(indicator_plot) <- c("Date","Q2P","eQ2","wUncert_ee","wUncert_fl")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_line()
g <- g + ylab("Indicator")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) 
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g <- g + theme(legend.position="bottom")
g

indicator_plot <- cbind(uncert.norm[,c("Date","eQ2","Uncert_ee")],w.uncert.norm[,c("eQ2","Uncert_ee")])
colnames(indicator_plot) <- c("Date","unwQ2","unwUncert_ee","wQ2","wUncert_e")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_line()
g <- g + ylab("Indicator")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) 
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g <- g + theme(legend.position="bottom")
g


indicator_plot <- w.uncert.norm[,c("Date","Uncert_cc","Uncert_fl","Uncert_ee")]
g <- ggplot(indicator_plot) 
g <- g + geom_line(aes(x=Date, y=Uncert_cc, colour="Current Conditions"), size = 1)
g <- g + geom_line(aes(x=Date, y=Uncert_fl, colour="Forward_looking"), size = 1)
g <- g + geom_line(aes(x=Date, y=Uncert_ee, colour="Expectation Errors"), size = 1)
g <- g + theme_bw()
g <- g + labs(color="Legend text")
g <- g + geom_rect(data=recessions.df, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='grey', alpha=0.5)
g <- g + ylab("Uncertainty")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g <- g + theme(legend.position="bottom")
g


indicator_plot <- uncert.norm[,c("Date","Uncert_cc","Uncert_fl","Uncert_ee")]
g <- ggplot(indicator_plot) 
g <- g + geom_line(aes(x=Date, y=Uncert_cc, colour="Current Conditions"), size = 1)
g <- g + geom_line(aes(x=Date, y=Uncert_fl, colour="Forward_looking"), size = 1)
g <- g + geom_line(aes(x=Date, y=Uncert_ee, colour="Expectation Errors"), size = 1)
g <- g + theme_bw()
g <- g + labs(color="Legend text")
g <- g + geom_rect(data=recessions.df, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='grey', alpha=0.5)
g <- g + ylab("Uncertainty")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g <- g + theme(legend.position="bottom")
g


# Check correlations of Uncertainty
temp_indices <- cbind(w.uncert.norm[,c("Date","Uncert_cc","Q2P","Uncert_fl","eQ2","Uncert_ee")],
                      uncert.norm[,c("Uncert_cc","Q2P","Uncert_fl","eQ2","Uncert_ee")])
colnames(temp_indices) <- c("Date","W_CC","W_Q2P","W_FL","W_eQ2","W_EE","Unw_CC","Unw_Q2P","Unw_FL","Unw_eQ2","Unw_EE")
source("corstarsl.R")
for(i in 2:ncol(temp_indices)) {temp_indices[,i] <- as.numeric(temp_indices[,i]) }
ts.all_indices <- as.ts(temp_indices[,-1],start =c(1992,1),end=c(2015,3),frequency=4) 
#cor(temp_indices[,-1],use="complete.obs")
xt <- xtable(corstarsl(ts.all_indices), caption="Correlations in Levels")
print(xt, "latex",comment=FALSE, caption.placement = getOption("xtable.caption.placement", "top"))


##==============================================================================================
##COMOVEMENT------------------------------------------------------------------------------------
##==============================================================================================
#Maak hulle time series
#Interpoleer NAs
#-------------------------------------------------------------------------------------------------

realGDP <- read.csv("RealGDP.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)
realGDP$X <- as.Date(realGDP$X, format = "%Y/%m/%d")
ts.realGDP <- ts(realGDP,start =c(1991,1),end=c(2015,3),frequency=4)
GDPgrowth4 <- sapply(log(ts.realGDP[,-1]), diff, lag =4)
GDPgrowth1 <- sapply(log(ts.realGDP[,-1]), diff, lag =1)

dum94 <- ts(0,start =c(1992,1),end=c(2015,3),frequency=4)
dum94[10] <- 1
dum94 <- as.data.frame(dum94)
names(dum94) <- "dum94"

#plot hulle saam
temp_indices <- cbind(Conf_fl=confidence[,"Q2P"],Uncert_ee=uncertainty[,"eQ2"],GDPgrowth=GDPgrowth4[,"RGDP"])
ts.temp_indices <- ts(temp_indices,start =c(1992,1),end=c(2015,3),frequency=4) 
plot(ts.temp_indices,plot.type = "m",main="")


Conf_cc <- confidence[,"Conf_cc"]
Conf_fl <- confidence[,"Conf_fl"]
Uncert_fl <- uncertainty[,"Uncert_fl"]
Uncert_ee <- uncertainty[,"Uncert_ee"]
GDPgrowth <- GDPgrowth4[,"RGDP"]

par(mfrow=c(2,2))
ccf(Conf_cc, GDPgrowth, na.action = na.pass)
ccf(Conf_fl, GDPgrowth, na.action = na.pass)
ccf(Uncert_fl, GDPgrowth, na.action = na.pass)
ccf(Uncert_ee, GDPgrowth, na.action = na.pass)


