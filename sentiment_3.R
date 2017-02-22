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
GDPdata$Date <- as.Date(GDPdata$Date, format = "%Y/%m/%d")

datums <- read.csv("dates2.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)
datums$Datum <- as.Date(datums$Datum, format = "%Y/%m/%d")

##For Grpahing Business cycles
recessions.df = read.table(textConnection(
    "Peak, Trough
    1990-12-31, 1993-05-30
    1996-11-30, 1999-08-31
    2007-11-30, 2009-08-31
    2013-11-30, 2016-12-31"), sep=',',
    colClasses=c('Date', 'Date'), header=TRUE)

##====================================##
## READING IN THE DATA ##
##====================================##
BER.M <- rbind.fill(read.csv("Manufacturing.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE),
                    read.csv("Manufacturing_pre2001.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE))
BER.M <- BER.M[,1:62]
colnames(BER.M)[1:7] <- c("region","id","sector","weight","turnover","factor","surveyQ")

##===============================##
BER.B <- read.csv("Building_corrected.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)
BER.B <- BER.B[!(BER.B$surveyQ %in% c("2015Q4","2016Q1","2016Q2","2016Q3")),1:21]

##============================##
BER.R <- read.csv("Retail.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)
BER.W <- read.csv("Wholesale.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)
BER.T <- rbind(BER.R,BER.W)
BER.T <- rbind.fill(BER.T,read.csv("Trade_pre2001.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE))
BER.T <- BER.T[,1:21]
colnames(BER.T)[1:6] <- c("region","id","sector","weight","factor","surveyQ")

##=====================================##
BER.V <- rbind.fill(read.csv("Motor.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE),
                    read.csv("Motor_pre2001.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE))
BER.V <- BER.V[,1:28]
colnames(BER.V)[1:6] <- c("region","id","sector","weight","factor","surveyQ")

##===============================##
BER.S <- read.csv("Services.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)
colnames(BER.S)[1:6] <- c("region","id","sector","weight","factor","surveyQ")

#==================
#AGGREGATING
#==================
#Create an unweighted stacked version of all the surveys
#Match the same or similar questions from the different surveys (see survey question examples)
#Create NAs for missing questions
tempBER.M <- cbind(BER.M[,c("id","sector","weight","factor","surveyQ","Q20","Q7A","Q7P","Q1A","Q1P","Q8A","Q8P",            "Q4A","Q4P")],"Manufacturing")
colnames(tempBER.M) <-    c("id","sector","weight","factor","surveyQ", "Q1","Q2A","Q2P","Q3A","Q3P","Q4A","Q4P",            "Q6A","Q6P",  "Sector")
tempBER.M[,c("Q5A","Q5P")] <- NA
tempBER.B <- cbind(BER.B[,c("id","sector","weight","factor","surveyQ", "Q1","Q2A","Q2P","Q3A","Q3P","Q4A","Q4P","Q5A","Q5P")],            "Construction")
colnames(tempBER.B) <-    c("id","sector","weight","factor","surveyQ", "Q1","Q2A","Q2P","Q3A","Q3P","Q4A","Q4P","Q5A","Q5P",              "Sector")
tempBER.B[,c("Q6A","Q6P")] <- NA
tempBER.T <- cbind(BER.T[,c("id","sector","weight","factor","surveyQ", "Q1","Q2A","Q2P","Q3A","Q3P","Q5A","Q5P","Q8",       "Q4A","Q4P")],"Trade")
colnames(tempBER.T) <-    c("id","sector","weight","factor","surveyQ", "Q1","Q2A","Q2P","Q3A","Q3P","Q4A","Q4P","Q5A",      "Q6A","Q6P"  ,"Sector")
tempBER.T[,c("Q5P","Q6A","Q6P")] <- NA
tempBER.V <- cbind(BER.V[,c("id","sector","weight","factor","surveyQ", "Q1","Q2A","Q2P","Q3A","Q3P",                        "Q4A","Q4P")],"Trade")
colnames(tempBER.V) <-    c("id","sector","weight","factor","surveyQ", "Q1","Q2A","Q2P","Q3A","Q3P",                        "Q6A","Q6P"  ,"Sector")
tempBER.V[,c("Q4A","Q4P","Q5A","Q5P")] <- NA
tempBER.S <- cbind(BER.S[,c("id","sector","weight","factor","surveyQ", "Q1","Q2A","Q2P","Q3A","Q3P","Q4A","Q4P","Q5A","Q5P")],            "Services")
colnames(tempBER.S) <-    c("id","sector","weight","factor","surveyQ", "Q1","Q2A","Q2P","Q3A","Q3P","Q4A","Q4P","Q5A","Q5P",              "Sector")
tempBER.S[,c("Q6A","Q6P")] <- NA

BER <- tempBER.M
BER <- rbind(BER,tempBER.B,tempBER.T,tempBER.V,tempBER.S)
BER <- BER[,c(15,1:12,16,17,13,14)]
rm(tempBER.M,tempBER.B,tempBER.T,tempBER.V,tempBER.S)
rm(BER.M,BER.B,BER.T,BER.V,BER.S,BER.R,BER.W)

#Clean data
BER$surveyQ <- toupper(BER$surveyQ)
BER$sector <- factor(BER$sector) #could include labels
BER$id <- factor(BER$id)
BER$surveyQ <- factor(BER$surveyQ)

# replace 1,2,3 (Up, Same, Down) responses with 1,0,-1
for(i in 7:ncol(BER)) {
    BER[,i] <- replace(BER[,i], BER[,i]==2, 0)
    BER[,i] <- replace(BER[,i], BER[,i]==3,-1)
}
BER$Q1 <- replace(BER$Q1, BER$Q1==0,-1) # replace 0 (Unsatisfactory) responses with -1

#---------------------------------------------------------------
#Sample characteristics
tafel <- aggregate(BER$id, by=list(BER$surveyQ,BER$Sector), FUN = length)
tafel <- cbind(obs=aggregate(tafel$x, by=list(tafel$Group.2), FUN = sum ),ave=aggregate(tafel$x, by=list(tafel$Group.2), FUN = mean ))[,-3]
tafel$obs.Group.1 <- as.character(tafel$obs.Group.1)
tafel$Resp <- tafel$ave.x/c(1000,1400,1400,1000)
tafel$Sample <- c("1992Q1-2015Q3","1993Q2-2015Q3","1992Q2-2015Q3","2005Q2-2015Q3") 
tafel$Missing <- c("1997Q4,2000Q1,2005Q4","1993Q4,1998Q3,2000Q2,2005Q4","1992Q4,1993Q3,2005Q4","2005Q4") 
tafel <- tafel[,c(1,5,2:4,6)]
tafel <- rbind(tafel,c("Total","1992Q1-2015Q3",nrow(BER),mean(aggregate(BER$id, by=list(BER$surveyQ),FUN = length)[,2]),
                       mean(aggregate(BER$id, by=list(BER$surveyQ), FUN = length)[,2])/4800,"2005Q4"))
tafel[, c(4:5)] <- sapply(tafel[, c(4:5)], as.numeric)
colnames(tafel) <- c("Sector","Sample","Total Observations","Ave Obs per Quarter", "Ave Response Rate","Missing Quarters")     
xt <- xtable(tafel, caption="Sample Characteristics", digits=c(2), align= c('r', "p{2cm}", rep('c',5) ) )
print(xt, "latex",comment=FALSE, caption.placement = getOption("xtable.caption.placement", "top"), scalebox = 0.8,
      include.rownames=FALSE)


#Plot Data
BERplot <- aggregate(BER$id, by=list(BER$surveyQ,BER$Sector), FUN = length)
BERplot$Group.1 <- as.Date(as.yearqtr(BERplot$Group.1, format = "%YQ%q"), frac = 1)
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

calc_conf <- function(data) {
    confidence <- aggregate(data[,(match("surveyQ",colnames(data))+1):ncol(data)], by=list(data$surveyQ), FUN=mean, na.rm=TRUE)
    confidence$Conf_cc <- rowMeans(confidence[,c("Q1","Q2A","Q3A","Q4A","Q5A","Q6A")],na.rm = TRUE, dims = 1)
    confidence$Conf_fl <- rowMeans(confidence[,c("Q2P","Q3P","Q4P","Q5P","Q6P")],na.rm = TRUE, dims = 1)
    #Row means for simple composite indicators (the question is which questions to include)
    confidence <- merge(datums,confidence,by.x="Date",by.y="Group.1", all=TRUE)
    confidence[,14:15] <- na.approx(confidence[,14:15],na.rm = FALSE)
    return(confidence)
}

calc_wconf <- function(data) {
    ##Weighted versions
    weeg <- function(temp) {  #calculate weighted mean for each quarter for all columns
        temp <- cbind(factor=temp$factor,temp$factor*temp[(match("surveyQ",colnames(temp))+1):ncol(temp)])
        #temp <- colSums(temp, na.rm=TRUE, dims = 1)/sum(temp$factor, na.rm=TRUE)
        #calculate the sum(wi*xi)/sum(wi)
        temp <- colSums(temp, na.rm=TRUE, dims = 1)/    
            sapply(colnames(temp), function(x) sum(temp$factor[!is.na(temp[colnames(temp) == x])]))
        #weight only by those that responded to a specific question
        return(temp)
    }
    
    w.confidence <- as.data.frame(t(sapply(levels(data$surveyQ), function(kwartaal) weeg(data[data$surveyQ==kwartaal,]))))
    w.confidence$Conf_cc <- rowMeans(w.confidence[,c("Q1","Q2A","Q3A","Q4A","Q5A","Q6A")],na.rm = TRUE, dims = 1)
    w.confidence$Conf_fl <- rowMeans(w.confidence[,c("Q2P","Q3P","Q4P","Q5P","Q6P")],na.rm = TRUE, dims = 1)
    w.confidence <- merge(datums,w.confidence,by.x="Date",by.y="row.names", all=TRUE)[,-3]
    w.confidence[,14:15] <- na.approx(w.confidence[,14:15],na.rm = FALSE)
    
    return(w.confidence)
}
##=====================================##
## CALCULATE INDICATORS ##
##=====================================##
indicators.M <- calc_conf(BER[BER$Sector=="Manufacturing",])
w.indicators.M <- calc_wconf(BER[BER$Sector=="Manufacturing",])

indicators.B <- calc_conf(BER[BER$Sector=="Construction",])
w.indicators.B <- calc_wconf(BER[BER$Sector=="Construction",])

indicators.T <- calc_conf(BER[BER$Sector=="Trade",])
w.indicators.T <- calc_wconf(BER[BER$Sector=="Trade",])

indicators.S <- calc_conf(BER[BER$Sector=="Services",])
w.indicators.S <- calc_wconf(BER[BER$Sector=="Services",])

indicators <- calc_conf(BER)
w.indicators <- calc_wconf(BER)

##=================================##
##Weighted versions
weights <- GDPdata[,c(1:4,6)]
activity <- cbind(w.indicators.M[,c(2,14)],w.indicators.B[,14],w.indicators.T[,14],w.indicators.S[,14])
colnames(activity) <- c("Date","Manufacturing","Construction","Trade","Services")
conf <- cbind(w.indicators.M[,c(2,15)],w.indicators.B[,15],w.indicators.T[,15],w.indicators.S[,15])
colnames(conf) <- c("Date","Manufacturing","Construction","Trade","Services")

#create weighted means by GDP share
activity$Activity <- sapply(activity$Date, function(x) weighted.mean(activity[which(activity$Date==x),c(2:5)], 
                                                                     weights[weights$Date==x,-1],na.rm=TRUE))
conf$Confidence <- sapply(conf$Date, function(x) weighted.mean(conf[which(conf$Date==x),c(2:5)], 
                                                               weights[weights$Date==x,-1],na.rm=TRUE))
w.indicators <- cbind(activity[,c(1,6)],conf[,6])
colnames(w.indicators) <- c("Date","Activity","Confidence")


#-----------------------------------------------
#Plot examples for sectors
index_plot <- w.indicators.M[,c(2,14:15)]
colnames(index_plot) <- c("Date","Activity","Confidence")
index_plot <- melt(index_plot, id="Date")  # convert to long format
g1 <- ggplot(index_plot, aes(x=Date,y=value,group=variable,colour=variable)) 
g1 <- g1 + geom_line()
g1 <- g1 + theme(legend.title=element_blank())
g1 <- g1 + ggtitle("Manufacturing") 
g1 <- g1 + ylab("Indicator") + xlab("")
g1 <- g1 + theme(legend.position="none")
g1 <- g1 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g1 <- g1 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

index_plot <- w.indicators.B[,c(2,14:15)]
colnames(index_plot) <- c("Date","Activity","Confidence")
index_plot <- melt(index_plot, id="Date")  # convert to long format
g2 <- ggplot(index_plot, aes(x=Date,y=value,group=variable,colour=variable)) 
g2 <- g2 + geom_line()
g2 <- g2 + theme(legend.title=element_blank())
g2 <- g2 + ggtitle("Construction") 
g2 <- g2 + ylab("") + xlab("")
g2 <- g2 + theme(legend.position="none")
g2 <- g2 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g2 <- g2 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

index_plot <- w.indicators.T[,c(2,14:15)]
colnames(index_plot) <- c("Date","Activity","Confidence")
index_plot <- melt(index_plot, id="Date")  # convert to long format
g3 <- ggplot(index_plot, aes(x=Date,y=value,group=variable,colour=variable)) 
g3 <- g3 + geom_line()
g3 <- g3 + theme(legend.title=element_blank())
g3 <- g3 + ggtitle("Trade") 
g3 <- g3 + ylab("Indicator") + xlab("")
g3 <- g3 + theme(legend.position="bottom",plot.margin=unit(c(-0.5,0.4,0,0.4), "cm"))
g3 <- g3 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g3 <- g3 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

index_plot <- w.indicators.S[-1:-52,c(2,14:15)]
colnames(index_plot) <- c("Date","Activity","Confidence")
index_plot <- melt(index_plot, id="Date")  # convert to long format
g4 <- ggplot(index_plot, aes(x=Date,y=value,group=variable,colour=variable)) 
g4 <- g4 + geom_line()
g4 <- g4 + theme(legend.title=element_blank())
g4 <- g4 + ggtitle("Services") 
g4 <- g4 + ylab("") + xlab("")
g4 <- g4 + theme(legend.position="bottom",plot.margin=unit(c(-0.5,0.4,0,0.4), "cm"))
g4 <- g4 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g4 <- g4 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

library(gridExtra)
grid.arrange(g1, g2, g3, g4, ncol=2, nrow =2)


index_plot <- w.indicators
g <- ggplot(index_plot) 
g <- g + geom_line(aes(x=Date, y=Activity, colour="Activity"), size = 1)
g <- g + geom_line(aes(x=Date, y=Confidence, colour="Confidence"), size = 1)
#g <- g + theme_bw()
g <- g + labs(color="Legend text")
g <- g + geom_rect(data=recessions.df, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='grey', alpha=0.5)
g <- g + ylab("Indicator") + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) 
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"), expand=c(0,0), 
                      limits = as.Date(c("1990-12-31", NA)))
g <- g + theme(legend.position="bottom")
g

index_plot <- cbind(w.indicators,indicators[,14:15])
colnames(index_plot) <- c("Date","Weighted Activity","Weighetd Confidence","Unweighted Activity","Unweighetd Confidence")
index_plot <- melt(index_plot, id="Date")  # convert to long format
g <- ggplot(data=index_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_line(size = 1)
g <- g + ylab("Indicator") + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) 
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g <- g + theme(legend.position="bottom")
g


index_plot <- cbind(w.indicators,GDPdata$BER_BCI, GDPdata$SACCI_BCI)
index_plot[,2:5] <- scale(index_plot[,2:5])
colnames(index_plot) <- c("Date","Activity","Confidence","BER_BCI","SACCI_BCI")
g <- ggplot(index_plot) 
g <- g + geom_line(aes(x=Date, y=Activity, colour="Activity"), size = 1)
g <- g + geom_line(aes(x=Date, y=Confidence, colour="Confidence"), size = 1)
g <- g + geom_line(aes(x=Date, y=BER_BCI, colour="BER_BCI"), size = 1)
g <- g + geom_line(aes(x=Date, y=SACCI_BCI, colour="SACCI_BCI"), size = 1)
#g <- g + theme_bw()
g <- g + labs(color="Legend text")
g <- g + geom_rect(data=recessions.df, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='grey', alpha=0.5)
g <- g + ylab("Indicator") + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) 
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"), expand=c(0,0),
                      limits = as.Date(c("1990-12-31", NA)))
g <- g + theme(legend.position="bottom")
g


#Plot examples for sectors
index_plot <- cbind(w.indicators.M[,c(2,14:15)],(GDPdata$ManuConf-50)/50)
colnames(index_plot) <- c("Date","Activity","Confidence","BER BCI")
index_plot <- melt(index_plot, id="Date")  # convert to long format
g1 <- ggplot(index_plot, aes(x=Date,y=value,group=variable,colour=variable)) 
g1 <- g1 + geom_line()
g1 <- g1 + theme(legend.title=element_blank())
g1 <- g1 + ggtitle("Manufacturing") 
g1 <- g1 + ylab("Indicator") + xlab("")
g1 <- g1 + theme(legend.position="none")
g1 <- g1 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g1 <- g1 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

index_plot <- cbind(w.indicators.B[,c(2,14:15)],(GDPdata$BuildingConf-50)/50)
colnames(index_plot) <- c("Date","Activity","Confidence","BER BCI")
index_plot <- melt(index_plot, id="Date")  # convert to long format
g2 <- ggplot(index_plot, aes(x=Date,y=value,group=variable,colour=variable)) 
g2 <- g2 + geom_line()
g2 <- g2 + theme(legend.title=element_blank())
g2 <- g2 + ggtitle("Construction") 
g2 <- g2 + ylab("") + xlab("")
g2 <- g2 + theme(legend.position="none")
g2 <- g2 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g2 <- g2 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

GDPdata$TradeConf <- rowMeans(GDPdata[,c(14,15,17)])
index_plot <- cbind(w.indicators.T[,c(2,14:15)],(GDPdata$TradeConf-50)/50)
colnames(index_plot) <- c("Date","Activity","Confidence","BER BCI")
index_plot <- melt(index_plot, id="Date")  # convert to long format
g3 <- ggplot(index_plot, aes(x=Date,y=value,group=variable,colour=variable)) 
g3 <- g3 + geom_line()
g3 <- g3 + theme(legend.title=element_blank())
g3 <- g3 + ggtitle("Trade") 
g3 <- g3 + ylab("Indicator") + xlab("")
g3 <- g3 + theme(legend.position="bottom",plot.margin=unit(c(-0.5,0.4,0,0.4), "cm"))
g3 <- g3 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g3 <- g3 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))


library(gridExtra)
grid.arrange(g1, g2, g3, ncol=2, nrow =2)

##====================================================================================##
## -------------------------------- UNCERTAINTY --------------------------------------##
##====================================================================================##
se <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x))*(length(na.omit(x))-1)) #adjust for (n-1)
##---------------------##
## Dispersion                    
##---------------------##

calc_uncert <- function(data) {
    uncertainty <- aggregate(data[,(match("surveyQ",colnames(data))+1):ncol(data)], by=list(data$surveyQ), FUN=se)
    uncertainty$Uncert_cc <- rowMeans(uncertainty[,c("Q2A","Q3A","Q4A","Q5A","Q6A")],na.rm = TRUE, dims = 1)
    uncertainty$Uncert_fl <- rowMeans(uncertainty[,c("Q2P","Q3P","Q4P","Q5P","Q6P")],na.rm = TRUE, dims = 1)
    #Row means for simple composite indicators (the question is which questions to include)
    uncertainty <- merge(datums,uncertainty,by.x="Date",by.y="Group.1", all=TRUE)
    uncertainty[,14:15] <- na.approx(uncertainty[,14:15],na.rm = FALSE)
    for(t in 2:nrow(uncertainty)) { uncertainty$Disp[t-1] <- uncertainty$Uncert_fl[t-1]/uncertainty$Uncert_cc[t] }
    uncertainty$Disp[t] <- NA
    return(uncertainty)
}

#calc_uncert <- function(data) {  #Dit is waar mens die individual questions match en scale
#    uncertainty <- aggregate(data[,(match("surveyQ",colnames(data))+1):ncol(data)], by=list(data$surveyQ), FUN=se)
#    for(i in c(4,6,8,10,12)) {
#        for(t in 2:nrow(uncertainty)) { uncertainty[t-1,i] <- uncertainty[t-1,i]/uncertainty[t,i-1] }
#    }
#    uncertainty$Uncert_fl <- rowMeans(uncertainty[,c("Q2P","Q3P","Q4P","Q5P","Q6P")],na.rm = TRUE, dims = 1)
#    #Row means for simple composite indicators (the question is which questions to include)
#    uncertainty <- merge(datums,uncertainty,by.x="Date",by.y="Group.1", all=TRUE)
#    uncertainty[,14] <- na.approx(uncertainty[,14],na.rm = FALSE)    
#   return(uncertainty)
#}


calc_wuncert <- function(data) {
    ##Weighted versions
    weeg.2 <- function(temp) {  #calculate weighted standard deviation for each quarter for all columns
        temp <- cbind(factor=temp$factor,temp$factor*temp[(match("surveyQ",colnames(temp))+1):ncol(temp)])
        #calculate total that responded up (1) and down (-1) over sum(wi) = fractions up and down
        frac.up <- sapply(1:ncol(temp), function(x) sum(temp[which(temp[,x]>0),x],na.rm=TRUE))/
            sapply(colnames(temp), function(x) sum(temp$factor[!is.na(temp[colnames(temp) == x])]))
        frac.dn <- sapply(1:ncol(temp), function(x) sum(temp[which(temp[,x]<0),x],na.rm=TRUE))/
            sapply(colnames(temp), function(x) sum(temp$factor[!is.na(temp[colnames(temp) == x])]))
        #weight only by those that responded to a specific question 
        ind <- sqrt(frac.up-frac.dn-(frac.up+frac.dn)^2)        #this is the standard devation
        return(ind)
    }
    
    w.uncertainty <- as.data.frame(t(sapply(levels(data$surveyQ), function(kwartaal) weeg.2(data[data$surveyQ==kwartaal,]))))
    w.uncertainty$Uncert_cc <- rowMeans(w.uncertainty[,c("Q1","Q2A","Q3A","Q4A","Q5A","Q6A")],na.rm = TRUE, dims = 1)
    w.uncertainty$Uncert_fl <- rowMeans(w.uncertainty[,c("Q2P","Q3P","Q4P","Q5P","Q6P")],na.rm = TRUE, dims = 1)
    w.uncertainty <- merge(datums,w.uncertainty,by.x="Date",by.y="row.names", all=TRUE)[,-3]
    w.uncertainty[,14:15] <- na.approx(w.uncertainty[,14:15],na.rm = FALSE)
    for(t in 2:nrow(w.uncertainty)) { w.uncertainty$Disp[t-1] <- w.uncertainty$Uncert_fl[t-1]/w.uncertainty$Uncert_cc[t] }
    w.uncertainty$Disp[t] <- NA
    return(w.uncertainty)
}


uncertainty.M <- calc_uncert(BER[BER$Sector=="Manufacturing",])
w.uncertainty.M <- calc_wuncert(BER[BER$Sector=="Manufacturing",])

uncertainty.B <- calc_uncert(BER[BER$Sector=="Construction",])
w.uncertainty.B <- calc_wuncert(BER[BER$Sector=="Construction",])

uncertainty.T <- calc_uncert(BER[BER$Sector=="Trade",])
w.uncertainty.T <- calc_wuncert(BER[BER$Sector=="Trade",])

uncertainty.S <- calc_uncert(BER[BER$Sector=="Services",])
w.uncertainty.S <- calc_wuncert(BER[BER$Sector=="Services",])

uncertainty <- calc_uncert(BER)
w.uncertainty <- calc_wuncert(BER)


index_plot <- w.uncertainty[,c(2,14:16)]
#index_plot[,2:5] <- scale(index_plot[,2:5])
colnames(index_plot) <- c("Date","CC","FL","DISP")
g <- ggplot(index_plot) 
g <- g + geom_line(aes(x=Date, y=CC, colour="CC"), size = 1)
g <- g + geom_line(aes(x=Date, y=FL, colour="FL"), size = 1)
g <- g + geom_line(aes(x=Date, y=DISP, colour="DISP"), size = 1)
#g <- g + theme_bw()
g <- g + labs(color="Legend text")
g <- g + geom_rect(data=recessions.df, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='grey', alpha=0.5)
g <- g + ylab("Indicator") + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) 
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"), expand=c(0,0),
                      limits = as.Date(c("1990-12-31", NA)))
g <- g + theme(legend.position="bottom")
g



##---------------------##
##Expectations Errors--
##---------------------##

calc_errors <- function(data) {
    #remove duplicates
    dups <- data[duplicated(data[,c("id","surveyQ")]) | duplicated(data[,c("id","surveyQ")], fromLast = TRUE),]
    data <- data[!duplicated(data[,c("id","surveyQ")]),]
    
    #Compare the expectations of firms in Q7P (forward-looking) in period t to the realisations in Q7A in period t+1.
    #see example survey questions
    exp.error <- function(temp) { #calculate errors for each respondent
        #merge to create easier format
        error <- merge(datums,temp,by.x="Date",by.y="surveyQ", all.x=TRUE)
        for(t in 1:nrow(error)) {
            error$eQ2[t]  <- error$Q2A[(t+1)] - error$Q2P[t]
            error$eQ3[t]  <- error$Q3A[(t+1)] - error$Q3P[t]
            error$eQ4[t]  <- error$Q4A[(t+1)] - error$Q4P[t]
            error$eQ5[t]  <- error$Q5A[(t+1)] - error$Q5P[t]
            error$eQ6[t]  <- error$Q6A[(t+1)] - error$Q6P[t]
        }
        error <- error[,c(2,7,19:23)]
        error <- error[rowSums(is.na(error[,3:7]))!=5, ]
        return(error)
    }
    
    errors <- data.frame()
    for(i in levels(data$id)){
        errors <- rbind(errors, exp.error(data[which(data$id==i),])) 
    }
    #errors <- rbind(errors,sapply(head(levels(data$id),1), function(i) exp.error(data[which(data$id==i),]) ) )
    return(errors)
}

#Save for speed
m_errors <- calc_errors(BER[BER$Sector=="Manufacturing",])
write.csv2(m_errors,"Manufacturing_errors.csv")

b_errors <- calc_errors(BER[BER$Sector=="Construction",])
write.csv2(b_errors,"Building_errors.csv")

t_errors <- calc_errors(BER[BER$Sector=="Trade",])
write.csv2(t_errors,"Trade_errors.csv")

s_errors <- calc_errors(BER[BER$Sector=="Services",])
write.csv2(s_errors,"Services_errors.csv")

#-----------------------------------------------------------------
#Read for speed
m_errors <- read.csv2("Manufacturing_errors.csv", header=TRUE)[,-1]
b_errors <- read.csv2("Building_errors.csv", header=TRUE)[,-1]
t_errors <- read.csv2("Trade_errors.csv", header=TRUE)[,-1]
s_errors <- read.csv2("Services_errors.csv", header=TRUE)[,-1]
errors <- rbind(m_errors,b_errors,t_errors,s_errors)

calc_uncert.ee <- function(errors) {
    idio.errors <- aggregate(errors[,-1:-2], by=list(errors$Datum), FUN=sd,na.rm=TRUE)
    #idio.errors[,2:6] <- scale(idio.errors[,2:6])
    idio.errors$idio <- rowMeans(idio.errors[,-1:-2],na.rm = TRUE, dims = 1)
    idio.errors$idio <- na.approx(idio.errors$idio,na.rm=FALSE)
    
    agg.errors <- aggregate(errors[,-1:-2], by=list(errors$Datum), FUN= function(x) {mean(x, na.rm = TRUE)^2})
    #agg.errors[,2:6] <- scale(agg.errors[,2:6])
    agg.errors$aggregate <- rowMeans(agg.errors[,-1:-2],na.rm = TRUE, dims = 1)
    agg.errors$aggregate <- na.approx(agg.errors$aggregate,na.rm=FALSE)
    
    exp.errors <- cbind(idio.errors[,c(1,7)],agg.errors[,13])
    colnames(exp.errors) <- c("Date","Uncert_Idiosyncratic","Uncert_Aggregate")
    return(exp.errors)
}

calc_wuncert.ee <- function(errors) {
    ##Weighted versions
    weeg.3 <- function(errors) {  #calculate weighted standard deviation for each quarter for all columns
        temp <- cbind(factor=errors$factor,errors$factor*errors[,3:ncol(errors)])
        xbar <- colSums(temp, na.rm=TRUE, dims = 1)/
            sapply(colnames(temp), function(x) sum(temp$factor[!is.na(temp[colnames(temp) == x])]))
        temp <- errors[,-1]
        #this is the weighted standard devation: sum[wi*(xi-xbar)^2]/sum(wi) 
        ind <- sqrt(sapply(colnames(temp), function(x) sum((temp[,x]-xbar[x])*(temp[,x]-xbar[x])*temp$factor,na.rm=TRUE))/
                        sapply(colnames(temp), function(x) sum(temp$factor[!is.na(temp[colnames(temp) == x])]))) 
        return(ind)
    }
    w.errors <- as.data.frame(t(sapply(levels(BER$surveyQ), function(kwartaal) weeg.3(errors[errors$Date==kwartaal,]))))[,-1]
    w.uncertainty$Uncert_ee <- rowMeans(w.errors,na.rm = TRUE, dims = 1)
    
}


uncert_error.M <- calc_uncert.ee(m_errors)
w.uncert_error.M <- calc_wuncert.ee(m_errors)

uncert_error.B <- calc_uncert.ee(b_errors)
w.uncert_error.B <- calc_wuncert.ee(b_errors)

uncert_error.T <- calc_uncert.ee(t_errors)
w.uncert_error.T <- calc_wuncert.ee(t_errors)

uncert_error.S <- calc_uncert.ee(s_errors)
w.uncert_error.S <- calc_wuncert.ee(s_errors)

uncert_error <- calc_uncert.ee(errors)
w.uncert_error <- calc_wuncert.ee(errors)


index_plot <- cbind(exp.errors[,c(1,7,13)])
colnames(index_plot) <- c("Date","Idiosyncratic","Aggregate")
index_plot <- melt(index_plot, id="Date")  # convert to long format
g <- ggplot(data=index_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_line(size = 1)
g <- g + ylab("Indicator") + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) 
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g <- g + theme(legend.position="bottom")
g



##=================================##
## AGGREGATING                              
##=================================##
##Weighted versions
names(w.uncertainty.M)[grep("Q7", colnames(w.uncertainty.M))] <- paste(names(w.uncertainty.M)[grep("Q7", colnames(w.uncertainty.M))],"M",sep=".")
names(w.uncertainty.B)[grep("Q2", colnames(w.uncertainty.B))] <- paste(names(w.uncertainty.B)[grep("Q2", colnames(w.uncertainty.B))],"B",sep=".")
names(w.uncertainty.T)[grep("Q2", colnames(w.uncertainty.T))] <- paste(names(w.uncertainty.T)[grep("Q2", colnames(w.uncertainty.T))],"T",sep=".")
names(w.uncertainty.V)[grep("Q2", colnames(w.uncertainty.V))] <- paste(names(w.uncertainty.V)[grep("Q2", colnames(w.uncertainty.V))],"V",sep=".")
names(w.uncertainty.S)[grep("Q2", colnames(w.uncertainty.S))] <- paste(names(w.uncertainty.S)[grep("Q2", colnames(w.uncertainty.S))],"S",sep=".")

w.uncertainty <- merge(w.uncertainty.M[,c("Uncert_cc.M","Q7P.M","Uncert_fl.M","eQ7.M","Uncert_ee.M")], 
                       w.uncertainty.B[,c("Uncert_cc.B","Q2P.B","Uncert_fl.B","eQ2.B","Uncert_ee.B")], by.x="row.names", by.y="row.names",all.x=TRUE)
w.uncertainty <- merge(w.uncertainty, w.uncertainty.T[,c("Uncert_cc.T","Q2P.T","Uncert_fl.T","eQ2.T","Uncert_ee.T")], by.x="Row.names", by.y="row.names",all.x=TRUE)
w.uncertainty <- merge(w.uncertainty, w.uncertainty.V[,c("Uncert_cc.V","Q2P.V","Uncert_fl.V","eQ2.V","Uncert_ee.V")], by.x="Row.names", by.y="row.names",all.x=TRUE)
w.uncertainty <- merge(w.uncertainty, w.uncertainty.S[,c("Uncert_cc.S","Q2P.S","Uncert_fl.S","eQ2.S","Uncert_ee.S")], by.x="Row.names", by.y="row.names",all.x=TRUE)
colnames(w.uncertainty)[1] <- "Date"

#Calculate weighted means by GDP share
w.uncertainty$Uncert_cc<- sapply(w.uncertainty$Date, function(x) weighted.mean(w.uncertainty[which(w.uncertainty$Date==x),c(2,7,12,17,22)], 
                                                                               weights[weights$Date==x,-1],na.rm=TRUE))
w.uncertainty$Q2P      <- sapply(w.uncertainty$Date, function(x) weighted.mean(w.uncertainty[which(w.uncertainty$Date==x),c(3,8,13,18,23)], 
                                                                               weights[weights$Date==x,-1],na.rm=TRUE))
w.uncertainty$Uncert_fl<- sapply(w.uncertainty$Date, function(x) weighted.mean(w.uncertainty[which(w.uncertainty$Date==x),c(4,9,14,19,24)], 
                                                                               weights[weights$Date==x,-1],na.rm=TRUE))
w.uncertainty$eQ2      <- sapply(w.uncertainty$Date, function(x) weighted.mean(w.uncertainty[which(w.uncertainty$Date==x),c(5,10,15,20,25)], 
                                                                               weights[weights$Date==x,-1],na.rm=TRUE))
w.uncertainty$Uncert_ee<- sapply(w.uncertainty$Date, function(x) weighted.mean(w.uncertainty[which(w.uncertainty$Date==x),c(6,11,16,21,26)], 
                                                                               weights[weights$Date==x,-1],na.rm=TRUE))
w.uncertainty$Date <- GDPdata[,1]
#-------------------
#Unweighted versions (standard error of stacked database)
uncertainty <- aggregate(BER[,(match("surveyQ",colnames(BER))+1):ncol(BER)], by=list(BER$surveyQ), FUN=se)
uncertainty$Uncert_cc <- rowMeans(uncertainty[,c("Q2A","Q3A","Q4A","Q5A","Q6A","Q1")],na.rm = TRUE, dims = 1)
uncertainty$Uncert_fl <- rowMeans(uncertainty[,c("Q2P","Q3P","Q4P","Q5P","Q6P")],na.rm = TRUE, dims = 1)
colnames(uncertainty)[1] <- "Date"

#Match the same or similar questions from the different surveys to combine all errors
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
errors <- rbind(errors,tempE.B,tempE.T,tempE.V,tempE.S)
errors <- errors[,c(1,6,2,3,4,7,5)]
rm(tempE.M,tempE.B,tempE.T,tempE.V,tempE.S)

##Could plot errors?
exp.errors <- aggregate(errors[,-1:-2], by=list(errors$Date), FUN=se)
uncertainty$eQ2 <- exp.errors$eQ2
uncertainty$Uncert_ee <- rowMeans(exp.errors[,-1:-2],na.rm = TRUE, dims = 1)
uncertainty$Date <- GDPdata[,1]

##-------------------------------------------------------------------------------
#standardise the uncertainty indicators for plotting
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




