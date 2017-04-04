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

realGDP <- read.csv("RealGDP.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)
realGDP$Date <- as.Date(realGDP$Date, format = "%Y/%m/%d")

GDPgrowth4 <- as.data.frame(sapply(log(realGDP[,-1]), diff, lag =4))
GDPgrowth1 <- as.data.frame(sapply(log(realGDP[,-1]), diff, lag =1))


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
BER.T$factor <- as.numeric(as.character(BER.T$factor))

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

indicators <- calc_conf(BER)[c(2,14:15)]
colnames(indicators) <- c("Date","Activity","Confidence")

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

#==============================
#Comparison with Other measures
#==============================
conf_indices <- cbind(w.indicators,GDPdata$BER_BCI, GDPdata$SACCI_BCI,GDPgrowth4$RGDP)
colnames(conf_indices) <- c("Date","Activity","Confidence","BER_BCI","SACCI_BCI","RGDP_Growth")

index_plot <- conf_indices
index_plot[,2:6] <- scale(index_plot[,2:6])
g <- ggplot(index_plot) 
g <- g + geom_line(aes(x=Date, y=Activity, colour="Activity"), size = 1)
g <- g + geom_line(aes(x=Date, y=Confidence, colour="Confidence"), size = 1)
g <- g + geom_line(aes(x=Date, y=BER_BCI, colour="BER_BCI"), size = 1)
g <- g + geom_line(aes(x=Date, y=SACCI_BCI, colour="SACCI_BCI"), size = 1)
g <- g + geom_line(aes(x=Date, y=RGDP_Growth, colour="RGDP_Growth"), size = 1)
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


#Check correlations
source("corstarsl.R")
xt <- xtable(corstarsl(conf_indices[,-1]), caption="Correlations in Levels")
print(xt, "latex",comment=FALSE, caption.placement = getOption("xtable.caption.placement", "top"), scalebox = 0.8,
      include.rownames=FALSE)

Activity <- conf_indices[,2]
Confidence <- conf_indices[,3]
BER_BCI <- conf_indices[,4]
SACCI_BCI <- conf_indices[,5] 
RGDP_Growth <- conf_indices[,6]

par(mfrow=c(2,2))
ccf(Activity, RGDP_Growth, na.action = na.pass)
ccf(Confidence, RGDP_Growth, na.action = na.pass)
ccf(BER_BCI, RGDP_Growth, na.action = na.pass)
ccf(SACCI_BCI, RGDP_Growth, na.action = na.pass)

#-----------------------------
#Sectoral Analysis
manufac <- cbind(w.indicators.M[,c(2,14:15)],(GDPdata$ManuConf-50)/50, GDPgrowth4$Manufacturing)
colnames(manufac) <- c("Date","Activity","Confidence","BER_BCI","RGDP_Growth")

construct <- cbind(w.indicators.B[,c(2,14:15)],(GDPdata$BuildingConf-50)/50, GDPgrowth4$Construction)
colnames(construct) <- c("Date","Activity","Confidence","BER_BCI","RGDP_Growth")

GDPdata$TradeConf <- rowMeans(GDPdata[,c(14,15,17)])
trade <- cbind(w.indicators.T[,c(2,14:15)],(GDPdata$TradeConf-50)/50, GDPgrowth4$Trade)
colnames(trade) <- c("Date","Activity","Confidence","BER_BCI","RGDP_Growth")

services <- cbind(w.indicators.S[,c(2,14:15)], GDPgrowth4$Services)
colnames(services) <- c("Date","Activity","Confidence","RGDP_Growth")
services$BER_BCI <- 1
services <- services[,c(1,2,3,5,4)]

index_plot <- manufac
index_plot[,-1] <- scale(index_plot[,-1])
index_plot <- melt(index_plot, id="Date")  # convert to long format
g1 <- ggplot(index_plot, aes(x=Date,y=value,group=variable,colour=variable)) 
g1 <- g1 + geom_line()
g1 <- g1 + theme(legend.title=element_blank())
g1 <- g1 + ggtitle("Manufacturing") 
g1 <- g1 + ylab("Indicator") + xlab("")
g1 <- g1 + theme(legend.position="none")
g1 <- g1 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g1 <- g1 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

index_plot <- construct
index_plot[,-1] <- scale(index_plot[,-1])
index_plot <- melt(index_plot, id="Date")  # convert to long format
g2 <- ggplot(index_plot, aes(x=Date,y=value,group=variable,colour=variable)) 
g2 <- g2 + geom_line()
g2 <- g2 + theme(legend.title=element_blank())
g2 <- g2 + ggtitle("Construction") 
g2 <- g2 + ylab("") + xlab("")
g2 <- g2 + theme(legend.position="none")
g2 <- g2 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g2 <- g2 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

index_plot <- trade
index_plot[,-1] <- scale(index_plot[,-1])
index_plot <- melt(index_plot, id="Date")  # convert to long format
g3 <- ggplot(index_plot, aes(x=Date,y=value,group=variable,colour=variable)) 
g3 <- g3 + geom_line()
g3 <- g3 + theme(legend.title=element_blank())
g3 <- g3 + ggtitle("Trade") 
g3 <- g3 + ylab("Indicator") + xlab("")
g3 <- g3 + theme(legend.position="bottom",plot.margin=unit(c(-0.5,0.4,0,0.4), "cm"))
g3 <- g3 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g3 <- g3 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

index_plot <- services
index_plot[,-1] <- scale(index_plot[,-1])
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


#Check correlations
source("corstarsl.R")
xt1 <- cbind(corstarsl(manufac[,-1]),corstarsl(construct[,-1]))
xt2 <- cbind(corstarsl(trade[,-1]),corstarsl(services[,-1]))
xt1 <- sapply(xt1,as.character)
xt2 <- sapply(xt2,as.character)
xt2[3,4:5] <- ""
xt2[4,6] <- ""
xt1[1,] <- c("Activity","Confidence","BER_BCI","Activity","Confidence","BER_BCI")
xt2[1,] <- c("Activity","Confidence","BER_BCI","Activity","Confidence","BER_BCI")
colnames(xt1) <- c(" ","Manufacturing"," "," ","Construction"," ")
colnames(xt2) <- c(" ","Trade"," "," ","Services"," ")
row.names(xt1) <- c("Activity","Confidence","BER_BCI","RGDP_Growth")
row.names(xt2) <- c("Activity","Confidence","BER_BCI","RGDP_Growth")

xt <- xtable(xt1, caption="Correlations in Levels")
print(xt, "latex",comment=FALSE, caption.placement = getOption("xtable.caption.placement", "top"), scalebox = 0.8)
xt <- xtable(xt2)
print(xt, "latex",comment=FALSE, caption.placement = getOption("xtable.caption.placement", "top"), scalebox = 0.85)


calc_ccf <- function(data, serv=0) {
    Activity <- data[,2]
    Confidence <- data[,3]
    BER_BCI <- data[,4]
    RGDP_Growth <- data[,5]

    par(mfrow=c(2,2))
    ccf(Activity, RGDP_Growth, na.action = na.pass)
    ccf(Confidence, RGDP_Growth, na.action = na.pass)
    if(serv==0) { ccf(BER_BCI, RGDP_Growth, na.action = na.pass) }
}

calc_ccf(manufac)
calc_ccf(construct)
calc_ccf(trade)
calc_ccf(services, 1)

#-------------------------------------------------------
#Turning Points
suppressMessages(library(BCDating))

turning.df = read.table(textConnection(
    "Peaks,  Troughs 
         ,  1993Q2 
    1996Q4, 1999Q3 
    2007Q4, 2009Q3 
    2013Q4, "), 
    sep=',', header=TRUE)

dat <- BBQ(ts(conf_indices[,2],start =c(1992,1),end=c(2015,3),frequency=4), mincycle = 6, minphase = 3, name="Activity")
tp1 <- as.data.frame(show(dat))[,-3]
dat <- BBQ(ts(conf_indices[,3],start =c(1992,1),end=c(2015,3),frequency=4), mincycle = 6, minphase = 3, name="Activity")
tp2 <- as.data.frame(show(dat))[,-3]
dat <- BBQ(ts(conf_indices[,4],start =c(1992,1),end=c(2015,3),frequency=4), mincycle = 6, minphase = 3, name="Activity")
tp3 <- as.data.frame(show(dat))[,-3]
dat <- BBQ(ts(conf_indices[,5],start =c(1992,1),end=c(2015,3),frequency=4), mincycle = 6, minphase = 3, name="Activity")
tp4 <- as.data.frame(show(dat))[,-3]
dat <- BBQ(ts(conf_indices[,6],start =c(1992,1),end=c(2015,3),frequency=4), mincycle = 6, minphase = 3, name="Activity")
tp5 <- as.data.frame(show(dat))[,-3]

cbindPad <- function(...){
    args <- list(...)
    n <- sapply(args,nrow)
    mx <- max(n)
    pad <- function(x, mx){
        if (nrow(x) < mx){
            nms <- colnames(x)
            padTemp <- matrix(NA, mx - nrow(x), ncol(x))
            colnames(padTemp) <- nms
            if (ncol(x)==0) {
                return(padTemp)
            } else {
                return(rbind(x,padTemp))
            }
        }
        else{
            return(x)
        }
    }
    rs <- lapply(args,pad,mx)
    return(do.call(cbind,rs))
}

turning <- cbindPad(turning.df,tp1,tp2,tp3,tp4,tp5)
turning <- sapply(turning,as.character)
turning <- rbind(colnames(turning),turning)
colnames(turning) <- c("SARB"," ","Activity"," ","Confidence"," ","BER","BCI","SACCI","BCI","RGDP","Growth")
xt <- xtable(turning, caption="Turning Points")
print(xt, "latex",comment=FALSE, caption.placement = getOption("xtable.caption.placement", "top"),
      scalebox = 0.8, include.rownames = FALSE)

detach("package:BCDating", unload=TRUE)

#data("Iran.non.Oil.GDP.Cycle")
#dat <- BBQ(Iran.non.Oil.GDP.Cycle, name="Dating Business Cycles of Iran")
#show(dat)
#summary(dat)
#plot(dat)
#data(MBRI.Iran.Dating)
#plot(dat,MBRI.Iran.Dating)


##========================================================
##-------------VAR Analysis-------------------------------
##========================================================
calc_var <- function(data) {
    vardat <- data
    infocrit <- VARselect(vardat, lag.max = 12, type = "const")
    k_aic <- infocrit$selection[1]
    k_hq  <- infocrit$selection[2]
    k_sic <- infocrit$selection[3]
    k <- min(k_aic,k_sic,k_hq)
    var_model <- VAR(vardat,p=k,type="const")
    return(var_model)
}

var1 <- calc_var(cbind(Activity,RGDP_Growth))
var2 <- calc_var(cbind(Confidence, RGDP_Growth))
var3 <- calc_var(cbind(BER_BCI, RGDP_Growth))
var4 <- calc_var(cbind(SACCI_BCI, RGDP_Growth))


##Granger causality tests
G <- data.frame()
G[1,1] <- causality(var1,cause = "Activity")$Granger[4]
G[1,2] <- as.numeric(as.character(causality(var1,cause = "Activity")$Granger[1]))
G[1,3] <- as.numeric(as.character(causality(var1,cause = "Activity")$Granger[3]))
G[2,1] <- causality(var1,cause = "RGDP_Growth")$Granger[4]
G[2,2] <- as.numeric(as.character(causality(var1,cause = "RGDP_Growth")$Granger[1]))
G[2,3] <- as.numeric(as.character(causality(var1,cause = "RGDP_Growth")$Granger[3]))

G[3,1] <- causality(var2,cause = "Confidence")$Granger[4]
G[3,2] <- as.numeric(as.character(causality(var2,cause = "Confidence")$Granger[1]))
G[3,3] <- as.numeric(as.character(causality(var2,cause = "Confidence")$Granger[3]))
G[4,1] <- causality(var2,cause = "RGDP_Growth")$Granger[4]
G[4,2] <- as.numeric(as.character(causality(var2,cause = "RGDP_Growth")$Granger[1]))
G[4,3] <- as.numeric(as.character(causality(var2,cause = "RGDP_Growth")$Granger[3]))

G[5,1] <- causality(var3,cause = "BER_BCI")$Granger[4]
G[5,2] <- as.numeric(as.character(causality(var3,cause = "BER_BCI")$Granger[1]))
G[5,3] <- as.numeric(as.character(causality(var3,cause = "BER_BCI")$Granger[3]))
G[6,1] <- causality(var3,cause = "RGDP_Growth")$Granger[4]
G[6,2] <- as.numeric(as.character(causality(var3,cause = "RGDP_Growth")$Granger[1]))
G[6,3] <- as.numeric(as.character(causality(var3,cause = "RGDP_Growth")$Granger[3]))

G[7,1] <- causality(var4,cause = "SACCI_BCI")$Granger[4]
G[7,2] <- as.numeric(as.character(causality(var4,cause = "SACCI_BCI")$Granger[1]))
G[7,3] <- as.numeric(as.character(causality(var4,cause = "SACCI_BCI")$Granger[3]))
G[8,1] <- causality(var4,cause = "RGDP_Growth")$Granger[4]
G[8,2] <- as.numeric(as.character(causality(var4,cause = "RGDP_Growth")$Granger[1]))
G[8,3] <- as.numeric(as.character(causality(var4,cause = "RGDP_Growth")$Granger[3]))


G[,2:3] <- round(G[,2:3],3)
mystars <- ifelse(G[,3] < .01, "***", ifelse(G[,3] < .05, "** ", ifelse(G[,3] < .1, "* ", " ")))
Gnew <- matrix(paste(G[,2], mystars, sep=""), ncol=1) 
G[,1] <- sub(".*: ", "", G[,1])
G[,2] <- Gnew
colnames(G) <- c("Granger causality H0:","statistic","p-value")

xt <- xtable(G, caption="Granger causality tests")
print(xt, "latex", include.rownames=FALSE,comment=FALSE, caption.placement = getOption("xtable.caption.placement", "top"), 
      scalebox = 0.8)

#-----------------------------------------
#Sectoral

calc_sectoralvar <- function(data, serv=0) {
    Activity <- data[,2]
    Confidence <- data[,3]
    BER_BCI <- data[,4]
    RGDP_Growth <- data[,5]
    
    var1 <- calc_var(cbind(Activity,RGDP_Growth))
    var2 <- calc_var(cbind(Confidence, RGDP_Growth))
    var3 <- calc_var(cbind(BER_BCI, RGDP_Growth))

    ##Granger causality tests
    G <- data.frame()
    G[1,1] <- causality(var1,cause = "Activity")$Granger[4]
    G[1,2] <- as.numeric(as.character(causality(var1,cause = "Activity")$Granger[1]))
    G[1,3] <- as.numeric(as.character(causality(var1,cause = "Activity")$Granger[3]))
    G[2,1] <- causality(var1,cause = "RGDP_Growth")$Granger[4]
    G[2,2] <- as.numeric(as.character(causality(var1,cause = "RGDP_Growth")$Granger[1]))
    G[2,3] <- as.numeric(as.character(causality(var1,cause = "RGDP_Growth")$Granger[3]))
    
    G[3,1] <- causality(var2,cause = "Confidence")$Granger[4]
    G[3,2] <- as.numeric(as.character(causality(var2,cause = "Confidence")$Granger[1]))
    G[3,3] <- as.numeric(as.character(causality(var2,cause = "Confidence")$Granger[3]))
    G[4,1] <- causality(var2,cause = "RGDP_Growth")$Granger[4]
    G[4,2] <- as.numeric(as.character(causality(var2,cause = "RGDP_Growth")$Granger[1]))
    G[4,3] <- as.numeric(as.character(causality(var2,cause = "RGDP_Growth")$Granger[3]))
    
    if(serv==0) {
        G[5,1] <- causality(var3,cause = "BER_BCI")$Granger[4]
        G[5,2] <- as.numeric(as.character(causality(var3,cause = "BER_BCI")$Granger[1]))
        G[5,3] <- as.numeric(as.character(causality(var3,cause = "BER_BCI")$Granger[3]))
        G[6,1] <- causality(var3,cause = "RGDP_Growth")$Granger[4]
        G[6,2] <- as.numeric(as.character(causality(var3,cause = "RGDP_Growth")$Granger[1]))
        G[6,3] <- as.numeric(as.character(causality(var3,cause = "RGDP_Growth")$Granger[3]))
    }    
    G[,2:3] <- round(G[,2:3],3)
    mystars <- ifelse(G[,3] < .01, "***", ifelse(G[,3] < .05, "** ", ifelse(G[,3] < .1, "* ", " ")))
    Gnew <- matrix(paste(G[,2], mystars, sep=""), ncol=1) 
    G[,1] <- sub(".*: ", "", G[,1])
    G[,2] <- Gnew
    colnames(G) <- c("Granger causality H0:","statistic","p-value")
    
    return(G)
}

G_manu <- calc_sectoralvar(manufac)
G_build <- calc_sectoralvar(construct[-1:-5,])
G_trade <- calc_sectoralvar(trade[-1,])
G_serv <- calc_sectoralvar(services[-1:-53,],1)
G_serv <- rbind(G_serv, c("BER_BCI do not Granger-cause RGDP_Growth","",""), 
                        c("RGDP_Growth do not Granger-cause BER_BCI","",""))
                
G_sector <- cbind(G_manu[,1:2],G_build[,2],G_trade[,2],G_serv[,2])
colnames(G_sector)[2:5] <- c("Manufacturing", "Construction","Trade","Services")

xt <- xtable(G_sector, caption="Granger causality tests")
print(xt, "latex", include.rownames=FALSE,comment=FALSE, caption.placement = getOption("xtable.caption.placement", "top"), 
      scalebox = 0.8)

#----------------------------------------------
#Unit root tests
adf.test(services[-1:-53,5], alternative = "stationary")

summary(ur.df(services[-1:-53,5], c("none"), selectlags = c("AIC")))
summary(ur.df(services[-1:-53,5], c("drift"), selectlags = c("AIC")))
summary(ur.df(services[-1:-53,5], c("trend"), selectlags = c("AIC")))

G_manu <- calc_sectoralvar(manufac)
G_build <- calc_sectoralvar(construct[-1:-5,])
G_trade <- calc_sectoralvar(trade[-1,])
G_serv <- calc_sectoralvar(services[-1:-53,],1)

#----------------------------------------------------------
##IRFs
#----------------------------------------------------------
detach("package:BCDating", unload=TRUE)

vardat <- cbind(Activity,RGDP_Growth)
infocrit <- VARselect(vardat, lag.max = 12, type = "const")
k_aic <- infocrit$selection[1]
k_hq  <- infocrit$selection[2]
k_sic <- infocrit$selection[3]
k <- min(k_aic,k_sic,k_hq)
var1 <- VAR(vardat,p=k,type="const")

irf.y1 <- irf(var1,impulse = "Activity", response = "RGDP_Growth", n.ahead = 12,runs = 1000, seed=12345) 
irf.y2 <- irf(var1,impulse = "RGDP_Growth", response = "Activity", n.ahead = 12,runs = 1000, seed=12345)
par(mfrow=c(1,2),mar=c(7,5,7,2), cex=0.6, new=FALSE)
plot(irf.y1,plot.type = c("single"))
par(mfrow=c(1,2),mar=c(6,4,4.2,1), cex=0.6, new = TRUE)
plot(irf.y2,plot.type = c("single"))

irf.y1 <- irf(var2,impulse = "Confidence", response = "RGDP_Growth", n.ahead = 12,runs = 1000, seed=12345) 
irf.y2 <- irf(var2,impulse = "RGDP_Growth", response = "Confidence", n.ahead = 12,runs = 1000, seed=12345)
par(mfrow=c(1,2),mar=c(7,5,7,2), cex=0.6, new=FALSE)
plot(irf.y1,plot.type = c("single"))
par(mfrow=c(1,2),mar=c(6,4,4.2,1), cex=0.6, new = TRUE)
plot(irf.y2,plot.type = c("single"))

irf.y1 <- irf(var3,impulse = "BER_BCI", response = "RGDP_Growth", n.ahead = 12,runs = 1000, seed=12345) 
irf.y2 <- irf(var3,impulse = "RGDP_Growth", response = "BER_BCI", n.ahead = 12,runs = 1000, seed=12345)
par(mfrow=c(1,2),mar=c(7,5,7,2), cex=0.6, new=FALSE)
plot(irf.y1,plot.type = c("single"))
par(mfrow=c(1,2),mar=c(6,4,4.2,1), cex=0.6, new = TRUE)
plot(irf.y2,plot.type = c("single"))

irf.y1 <- irf(var4,impulse = "SACCI_BCI", response = "RGDP_Growth", n.ahead = 12,runs = 1000, seed=12345) 
irf.y2 <- irf(var4,impulse = "RGDP_Growth", response = "SACCI_BCI", n.ahead = 12,runs = 1000, seed=12345)
par(mfrow=c(1,2),mar=c(7,5,7,2), cex=0.6, new=FALSE)
plot(irf.y1,plot.type = c("single"))
par(mfrow=c(1,2),mar=c(6,4,4.2,1), cex=0.6, new = TRUE)
plot(irf.y2,plot.type = c("single"))


#------------------------------------------------
#FEVD
#par(mfrow=c(1,1))
#win.graph(width=13,height=8)
plot(fevd(var1, n.ahead = 10))
plot(fevd(var2, n.ahead = 10))
plot(fevd(var3, n.ahead = 10))
plot(fevd(var4, n.ahead = 10))

source("plot_varfevd.R")
#dev.off()
par(mfrow=c(1,2))
plot.varfevd(fevd(var1, n.ahead = 10 ),plot.type = "single")
#-------------------------------------------------
#Sectoral
data <- manufac
Activity <- data[,2]
RGDP_Growth <- data[,5]

vardat <- cbind(Activity,RGDP_Growth)
infocrit <- VARselect(vardat, lag.max = 12, type = "const")
k_aic <- infocrit$selection[1]
k_hq  <- infocrit$selection[2]
k_sic <- infocrit$selection[3]
k <- min(k_aic,k_sic,k_hq)
varm <- VAR(vardat,p=k,type="const")

irf.y1 <- irf(varm,impulse = "Activity", response = "RGDP_Growth", n.ahead = 12,runs = 1000, seed=12345) 
irf.y2 <- irf(varm,impulse = "RGDP_Growth", response = "Activity", n.ahead = 12,runs = 1000, seed=12345)
par(mfrow=c(1,2),mar=c(7,5,7,2), cex=0.6, new=FALSE)
plot(irf.y1,plot.type = c("single"))
par(mfrow=c(1,2),mar=c(6,4,4.2,1), cex=0.6, new = TRUE)
plot(irf.y2,plot.type = c("single"))

source("plot_varfevd.R")
#dev.off()
par(mfrow=c(1,2))
plot.varfevd(fevd(varm, n.ahead = 10 ),plot.type = "single")
    



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

##Weighted versions
weights <- GDPdata[,c(1:4,6)]
w.uncertainty <- cbind(w.uncertainty.M[,c(2,16)],w.uncertainty.B[,16],w.uncertainty.T[,16],w.uncertainty.S[,16])
colnames(w.uncertainty) <- c("Date","Manufacturing","Construction","Trade","Services")
w.uncertainty[,-1] <- scale(w.uncertainty[,-1])

w.uncertainty$Dispersion <- sapply(w.uncertainty$Date, function(x) weighted.mean(w.uncertainty[which(w.uncertainty$Date==x),c(2:5)], 
                                                                           weights[weights$Date==x,-1],na.rm=TRUE))

#index_plot <- cbind(uncertainty[,c(2,16)],w.uncertainty[,6])
index_plot <- cbind(w.uncert_error[,c(1,3,4)],uncert_error[,3:4])
index_plot[,-1] <- scale(index_plot[,-1])
#colnames(index_plot) <- c("Date","unweighted","DISP")
colnames(index_plot) <- c("Date","Idio","Aggre","Unw_Idio","Unw_Aggre")
g <- ggplot(index_plot) 
g <- g + geom_line(aes(x=Date, y=Idio, colour="Idio"), size = 1)
g <- g + geom_line(aes(x=Date, y=Unw_Idio, colour="Unw_Idio"), size = 1)
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
m_errors$Datum <- as.Date(m_errors$Datum)
b_errors <- read.csv2("Building_errors.csv", header=TRUE)[,-1]
b_errors$Datum <- as.Date(b_errors$Datum)
t_errors <- read.csv2("Trade_errors.csv", header=TRUE)[,-1]
t_errors$Datum <- as.Date(t_errors$Datum)
s_errors <- read.csv2("Services_errors.csv", header=TRUE)[,-1]
s_errors$Datum <- as.Date(s_errors$Datum)
errors <- rbind(m_errors,b_errors,t_errors,s_errors)

calc_uncert.ee <- function(data) {
    idio.errors <- aggregate(data[,-1:-2], by=list(data$Datum), FUN=se)
    #idio.errors[,2:6] <- scale(idio.errors[,2:6])
    idio.errors$idio <- rowMeans(idio.errors[,-1:-2],na.rm = TRUE, dims = 1)
    idio.errors$idio <- na.approx(idio.errors$idio,na.rm=FALSE)
    
    agg.errors <- aggregate(data[,-1:-2], by=list(data$Datum), FUN= function(x) {mean(x, na.rm = TRUE)^2})
    #agg.errors[,2:6] <- scale(agg.errors[,2:6])
    agg.errors$aggregate <- rowMeans(agg.errors[,-1:-2],na.rm = TRUE, dims = 1)
    agg.errors$aggregate <- na.approx(agg.errors$aggregate,na.rm=FALSE)
    
    #total.errors <- aggregate(data[,-1:-2], by=list(data$Datum), FUN= function(x) {sum(x^2, na.rm = TRUE)*mean(x, na.rm = TRUE)/sum(x, na.rm = TRUE)})
    #total.errors[,2:6] <- scale(total.errors[,2:6])
    #total.errors$total <- rowMeans(total.errors[,-1:-2],na.rm = TRUE, dims = 1)
    #total.errors$total  <- na.approx(total.errors$total,na.rm=FALSE)
    
    exp.errors <- cbind(idio.errors[,c(1,7)],agg.errors[,7])
    colnames(exp.errors) <- c("Date","Uncert_Idiosyncratic","Uncert_Aggregate")
    exp.errors$Date <- as.Date(exp.errors$Date)
    exp.errors <- merge(datums,exp.errors,by.x="Datum",by.y="Date", all=TRUE)
    return(exp.errors)
}

calc_wuncert.ee <- function(data) {
    ##Weighted versions
    weeg.3 <- function(data) {  #calculate weighted standard deviation for each quarter for all columns
        temp <- cbind(factor=data$factor,data$factor*data[,3:ncol(data)])
        xbar <- colSums(temp, na.rm=TRUE, dims = 1)/
            sapply(colnames(temp), function(x) sum(temp$factor[!is.na(temp[colnames(temp) == x])]))
        temp <- data[,-1]
        #this is the weighted standard devation: sum[wi*(xi-xbar)^2]/sum(wi) 
        idio <- sqrt(sapply(colnames(temp), function(x) sum((temp[,x]-xbar[x])*(temp[,x]-xbar[x])*temp$factor,na.rm=TRUE))/
                        sapply(colnames(temp), function(x) sum(temp$factor[!is.na(temp[colnames(temp) == x])],na.rm=TRUE))) 
        aggr <- sapply(colnames(temp), function(x) xbar[x]*xbar[x]) 
        ind <- cbind(idio,aggr)
        return(ind)
    }
    w.errors <- as.data.frame(t(sapply(datums$Datum, function(kwartaal) weeg.3(data[data$Datum==kwartaal,]))))
    w.errors <- cbind(datums$Datum,w.errors)
    colnames(w.errors) <- c("Date","Idio.factor","Idio.Q2","Idio.Q3","Idio.Q4","Idio.Q5","Idio.Q6",
                            "Aggr.factor","Aggr.Q2","Aggr.Q3","Aggr.Q4","Aggr.Q5","Aggr.Q6")
    w.errors[w.errors==0] <- NA
    w.errors[,-1] <- scale(w.errors[,-1])
    w.errors$Idio <- rowMeans(w.errors[,3:7],na.rm = TRUE, dims = 1)
    w.errors$Aggr <- rowMeans(w.errors[,9:13],na.rm = TRUE, dims = 1)
    w.errors[,14:15] <- na.approx(w.errors[,14:15],na.rm=FALSE)
    w.errors <- w.errors[,c(1,14:15)]
    return(w.errors)
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

#----------------------------------------------------------

##Weighted versions
weights <- GDPdata[,c(1:4,6)]
idio.errors <- cbind(w.uncert_error.M[,c(1,2)],w.uncert_error.B[,2],w.uncert_error.T[,2],w.uncert_error.S[,2])
colnames(idio.errors) <- c("Date","Manufacturing","Construction","Trade","Services")

aggr.errors <- cbind(w.uncert_error.M[,c(1,3)],w.uncert_error.B[,3],w.uncert_error.T[,3],w.uncert_error.S[,3])
colnames(aggr.errors) <- c("Date","Manufacturing","Construction","Trade","Services")

#create weighted means by GDP share
idio.errors$Idio.errors <- sapply(idio.errors$Date, function(x) weighted.mean(idio.errors[which(idio.errors$Date==x),c(2:5)], 
                                                               weights[weights$Date==x,-1],na.rm=TRUE))
aggr.errors$Aggr.errors <- sapply(aggr.errors$Date, function(x) weighted.mean(aggr.errors[which(aggr.errors$Date==x),c(2:5)], 
                                                                              weights[weights$Date==x,-1],na.rm=TRUE))

w.uncert_error <- cbind(w.uncertainty[,c(1,6)],idio.errors[,6],aggr.errors[,6])
colnames(w.uncert_error) <- c("Date","Dispersion","Idiosyncratic","Aggregate")

uncert_error <- cbind(uncertainty[,c(2,6)],uncert_error[,3:4])
colnames(uncert_error) <- c("Date","Dispersion","Idiosyncratic","Aggregate")
uncert_error[,-1] <- na.approx(uncert_error[,-1])
uncert_error[,-1] <- scale(uncert_error[,-1])

#-----------------------------------------------
#Plot examples for sectors
index_plot <- cbind(w.uncert_error.M,w.uncertainty.M[,16])
index_plot[,-1] <- scale(index_plot[,-1])
colnames(index_plot) <- c("Date","Idiosyncratic","Aggregate","Dispersion")
index_plot <- melt(index_plot, id="Date")  # convert to long format
g1 <- ggplot(index_plot, aes(x=Date,y=value,group=variable,colour=variable)) 
g1 <- g1 + geom_line()
g1 <- g1 + theme(legend.title=element_blank())
g1 <- g1 + ggtitle("Manufacturing") 
g1 <- g1 + ylab("Indicator") + xlab("")
g1 <- g1 + theme(legend.position="none")
g1 <- g1 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g1 <- g1 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

index_plot <- cbind(w.uncert_error.B,w.uncertainty.B[,16])
index_plot[,-1] <- scale(index_plot[,-1])
colnames(index_plot) <- c("Date","Idiosyncratic","Aggregate","Dispersion")
index_plot <- melt(index_plot, id="Date")  # convert to long format
g2 <- ggplot(index_plot, aes(x=Date,y=value,group=variable,colour=variable)) 
g2 <- g2 + geom_line()
g2 <- g2 + theme(legend.title=element_blank())
g2 <- g2 + ggtitle("Construction") 
g2 <- g2 + ylab("") + xlab("")
g2 <- g2 + theme(legend.position="none")
g2 <- g2 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g2 <- g2 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

index_plot <- cbind(w.uncert_error.T,w.uncertainty.T[,16])
index_plot[,-1] <- scale(index_plot[,-1])
colnames(index_plot) <- c("Date","Idiosyncratic","Aggregate","Dispersion")
index_plot <- melt(index_plot, id="Date")  # convert to long format
g3 <- ggplot(index_plot, aes(x=Date,y=value,group=variable,colour=variable)) 
g3 <- g3 + geom_line()
g3 <- g3 + theme(legend.title=element_blank())
g3 <- g3 + ggtitle("Trade") 
g3 <- g3 + ylab("Indicator") + xlab("")
g3 <- g3 + theme(legend.position="bottom",plot.margin=unit(c(-0.5,0.4,0,0.4), "cm"))
g3 <- g3 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g3 <- g3 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

index_plot <- cbind(w.uncert_error.S[-1:-52,],w.uncertainty.S[-1:-52,16])
index_plot[,-1] <- scale(index_plot[,-1])
colnames(index_plot) <- c("Date","Idiosyncratic","Aggregate","Dispersion")
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


index_plot <- w.uncert_error 
g <- ggplot(index_plot) 
g <- g + geom_line(aes(x=Date, y=Idiosyncratic, colour="Idiosyncratic"), size = 1)
g <- g + geom_line(aes(x=Date, y=Aggregate, colour="Aggregate"), size = 1)
g <- g + geom_line(aes(x=Date, y=Dispersion, colour="Dispersion"), size = 1)
g <- g + labs(color="Legend text")
g <- g + geom_rect(data=recessions.df, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='grey', alpha=0.5)
g <- g + ylab("Indicator") + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) 
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"), expand=c(0,0),
                      limits = as.Date(c("1990-12-31", NA)))
g <- g + theme(legend.position="bottom")
g


#-------------------------------------------------
uncert_indices <- cbind(w.uncert_error,GDPdata$IMF_Uncertainty, GDPdata$SAVI,GDPgrowth4$RGDP)
colnames(uncert_indices) <-c("Date","Dispersion","Idiosyncratic","Aggregate","EPU","SAVI","RGDP_Growth")
uncert_indices[,-1] <- scale(uncert_indices[,-1])
uncert_indices$Uncertainty <- rowMeans(uncert_indices[,c(2:6)],na.rm = TRUE)
uncert_indices <- uncert_indices[,c(1:6,8,7)]

index_plot <- uncert_indices
g <- ggplot(index_plot) 
g <- g + geom_line(aes(x=Date, y=EPU, colour="EPU"), size = 1)
g <- g + geom_line(aes(x=Date, y=SAVI, colour="SAVI"), size = 1)
g <- g + geom_line(aes(x=Date, y=Uncertainty, colour="Uncertainty"), size = 1)
g <- g + labs(color="Legend text")
g <- g + geom_rect(data=recessions.df, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='grey', alpha=0.5)
g <- g + ylab("Indicator") + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) 
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"), expand=c(0,0),
                      limits = as.Date(c("1990-12-31", NA)))
g <- g + theme(legend.position="bottom")
g


#Check correlations
source("corstarsl.R")
xt <- xtable(corstarsl(uncert_indices[,-1]), caption="Correlations in Levels")
print(xt, "latex",comment=FALSE, caption.placement = getOption("xtable.caption.placement", "top"), scalebox = 0.8,
      include.rownames=FALSE)

Dispersion <- uncert_indices[,2]
Idiosyncratic <- uncert_indices[,3]
Aggregate <- uncert_indices[,4]
EPU <- uncert_indices[,5] 
SAVI <- uncert_indices[,6] 
Uncertainty <- uncert_indices[,7]
RGDP_Growth <- uncert_indices[,8]

    
par(mfrow=c(3,2))
ccf(Dispersion, RGDP_Growth, na.action = na.pass)
ccf(Idiosyncratic, RGDP_Growth, na.action = na.pass)
ccf(Aggregate, RGDP_Growth, na.action = na.pass)
#par(mfrow=c(2,2))
ccf(EPU, RGDP_Growth, na.action = na.pass)
ccf(SAVI, RGDP_Growth, na.action = na.pass)
ccf(Uncertainty, RGDP_Growth, na.action = na.pass)

#Sectoral Analysis
manufac <- cbind(w.uncert_error.M,w.uncertainty.M[,16], GDPgrowth4$Manufacturing)
colnames(manufac) <- c("Date","Idiosyncratic","Aggregate","Dispersion","RGDP_Growth")
manufac[,-1] <- scale(manufac[,-1])
manufac$Uncertainty <- rowMeans(manufac[,c(2:4)],na.rm = TRUE)
manufac <- manufac[,c(1:4,6,5)]

construct <- cbind(w.uncert_error.B,w.uncertainty.B[,16], GDPgrowth4$Construction)
colnames(construct) <- c("Date","Idiosyncratic","Aggregate","Dispersion","RGDP_Growth")
construct[,-1] <- scale(construct[,-1])
construct$Uncertainty <- rowMeans(construct[,c(2:4)],na.rm = TRUE)
construct <- construct[,c(1:4,6,5)]

trade <- cbind(w.uncert_error.T,w.uncertainty.T[,16], GDPgrowth4$Trade)
colnames(trade) <- c("Date","Idiosyncratic","Aggregate","Dispersion","RGDP_Growth")
trade[,-1] <- scale(trade[,-1])
trade$Uncertainty <- rowMeans(trade[,c(2:4)],na.rm = TRUE)
trade <- trade[,c(1:4,6,5)]

services <- cbind(w.uncert_error.S,w.uncertainty.S[,16], GDPgrowth4$Services)
colnames(services) <- c("Date","Idiosyncratic","Aggregate","Dispersion","RGDP_Growth")
services[,-1] <- scale(services[,-1])
services$Uncertainty <- rowMeans(services[,c(2:4)],na.rm = TRUE)
services <- services[,c(1:4,6,5)]

#Check correlations
source("corstarsl.R")
xt1 <- cbind(corstarsl(manufac[,-1]),corstarsl(construct[,-1]))
xt2 <- cbind(corstarsl(trade[,-1]),corstarsl(services[,-1]))
xt1 <- sapply(xt1,as.character)
xt2 <- sapply(xt2,as.character)
xt1[1,] <- c("Idiosyncratic","Aggregate","Dispersion","Uncertainty","Idiosyncratic","Aggregate","Dispersion","Uncertainty")
xt2[1,] <- c("Idiosyncratic","Aggregate","Dispersion","Uncertainty","Idiosyncratic","Aggregate","Dispersion","Uncertainty")
colnames(xt1) <- c(" "," ","Manufacturing"," "," "," ","Construction"," ")
colnames(xt2) <- c(" "," ","Trade"," "," "," ","Services"," ")
row.names(xt1) <- c("Idiosyncratic","Aggregate","Dispersion","Uncertainty","RGDP")
row.names(xt2) <- c("Idiosyncratic","Aggregate","Dispersion","Uncertainty","RGDP")

xt <- xtable(xt1, caption="Correlations in Levels")
print(xt, "latex",comment=FALSE, caption.placement = getOption("xtable.caption.placement", "top"), scalebox = 0.8)
xt <- xtable(xt2)
print(xt, "latex",comment=FALSE, caption.placement = getOption("xtable.caption.placement", "top"), scalebox = 0.85)


calc_ccf <- function(data) {
    Idiosyncratic <- data[,2]
    Aggregate <- data[,3]
    Dispersion <- data[,4]
    Uncertainty <- data[,5]
    RGDP_Growth <- data[,6]
    
    par(mfrow=c(2,2))
    ccf(Idiosyncratic, RGDP_Growth, na.action = na.pass)
    ccf(Aggregate, RGDP_Growth, na.action = na.pass)
    ccf(Dispersion, RGDP_Growth, na.action = na.pass)
    ccf(Uncertainty, RGDP_Growth, na.action = na.pass)
    
}

calc_ccf(manufac)
calc_ccf(construct)
calc_ccf(trade)
calc_ccf(services)


##========================================================
##-------------VAR Analysis-------------------------------
##========================================================
calc_var <- function(data) {
    vardat <- data
    infocrit <- VARselect(vardat, lag.max = 12, type = "const")
    k_aic <- infocrit$selection[1]
    k_hq  <- infocrit$selection[2]
    k_sic <- infocrit$selection[3]
    k <- min(k_aic,k_sic,k_hq)
    var_model <- VAR(vardat,p=k,type="const")
    return(var_model)
}

var1 <- calc_var(cbind(Dispersion,RGDP_Growth)[-95,])
var2 <- calc_var(cbind(Idiosyncratic, RGDP_Growth)[-95,])
var3 <- calc_var(cbind(Aggregate, RGDP_Growth)[-95,])
var4 <- calc_var(cbind(EPU, RGDP_Growth))
var5 <- calc_var(cbind(SAVI,RGDP_Growth)[-1:-14,])
var6 <- calc_var(cbind(Uncertainty, RGDP_Growth)[-95,])

##Granger causality tests
G <- data.frame()
G[1,1] <- causality(var1,cause = "Dispersion")$Granger[4]
G[1,2] <- as.numeric(as.character(causality(var1,cause = "Dispersion")$Granger[1]))
G[1,3] <- as.numeric(as.character(causality(var1,cause = "Dispersion")$Granger[3]))
G[2,1] <- causality(var1,cause = "RGDP_Growth")$Granger[4]
G[2,2] <- as.numeric(as.character(causality(var1,cause = "RGDP_Growth")$Granger[1]))
G[2,3] <- as.numeric(as.character(causality(var1,cause = "RGDP_Growth")$Granger[3]))

G[3,1] <- causality(var2,cause = "Idiosyncratic")$Granger[4]
G[3,2] <- as.numeric(as.character(causality(var2,cause = "Idiosyncratic")$Granger[1]))
G[3,3] <- as.numeric(as.character(causality(var2,cause = "Idiosyncratic")$Granger[3]))
G[4,1] <- causality(var2,cause = "RGDP_Growth")$Granger[4]
G[4,2] <- as.numeric(as.character(causality(var2,cause = "RGDP_Growth")$Granger[1]))
G[4,3] <- as.numeric(as.character(causality(var2,cause = "RGDP_Growth")$Granger[3]))

G[5,1] <- causality(var3,cause = "Aggregate")$Granger[4]
G[5,2] <- as.numeric(as.character(causality(var3,cause = "Aggregate")$Granger[1]))
G[5,3] <- as.numeric(as.character(causality(var3,cause = "Aggregate")$Granger[3]))
G[6,1] <- causality(var3,cause = "RGDP_Growth")$Granger[4]
G[6,2] <- as.numeric(as.character(causality(var3,cause = "RGDP_Growth")$Granger[1]))
G[6,3] <- as.numeric(as.character(causality(var3,cause = "RGDP_Growth")$Granger[3]))

G[7,1] <- causality(var4,cause = "EPU")$Granger[4]
G[7,2] <- as.numeric(as.character(causality(var4,cause = "EPU")$Granger[1]))
G[7,3] <- as.numeric(as.character(causality(var4,cause = "EPU")$Granger[3]))
G[8,1] <- causality(var4,cause = "RGDP_Growth")$Granger[4]
G[8,2] <- as.numeric(as.character(causality(var4,cause = "RGDP_Growth")$Granger[1]))
G[8,3] <- as.numeric(as.character(causality(var4,cause = "RGDP_Growth")$Granger[3]))

G[9,1] <- causality(var5,cause = "SAVI")$Granger[4]
G[9,2] <- as.numeric(as.character(causality(var5,cause = "SAVI")$Granger[1]))
G[9,3] <- as.numeric(as.character(causality(var5,cause = "SAVI")$Granger[3]))
G[10,1] <- causality(var5,cause = "RGDP_Growth")$Granger[4]
G[10,2] <- as.numeric(as.character(causality(var5,cause = "RGDP_Growth")$Granger[1]))
G[10,3] <- as.numeric(as.character(causality(var5,cause = "RGDP_Growth")$Granger[3]))

G[11,1] <- causality(var6,cause = "Uncertainty")$Granger[4]
G[11,2] <- as.numeric(as.character(causality(var6,cause = "Uncertainty")$Granger[1]))
G[11,3] <- as.numeric(as.character(causality(var6,cause = "Uncertainty")$Granger[3]))
G[12,1] <- causality(var6,cause = "RGDP_Growth")$Granger[4]
G[12,2] <- as.numeric(as.character(causality(var6,cause = "RGDP_Growth")$Granger[1]))
G[12,3] <- as.numeric(as.character(causality(var6,cause = "RGDP_Growth")$Granger[3]))

G[,2:3] <- round(G[,2:3],3)
mystars <- ifelse(G[,3] < .01, "***", ifelse(G[,3] < .05, "** ", ifelse(G[,3] < .1, "* ", " ")))
Gnew <- matrix(paste(G[,2], mystars, sep=""), ncol=1) 
G[,1] <- sub(".*: ", "", G[,1])
G[,2] <- Gnew
colnames(G) <- c("Granger causality H0:","statistic","p-value")

xt <- xtable(G, caption="Granger causality tests")
print(xt, "latex", include.rownames=FALSE,comment=FALSE, caption.placement = getOption("xtable.caption.placement", "top"), 
      scalebox = 0.8)

#----------------------------------------------
calc_sectoralvar <- function(data) {
    Idiosyncratic <- data[,2]
    Aggregate <- data[,3]
    Dispersion <- data[,4]
    Uncertainty <- data[,5]
    RGDP_Growth <- data[,6]
    
    var1 <- calc_var(cbind(Idiosyncratic,RGDP_Growth))
    var2 <- calc_var(cbind(Aggregate, RGDP_Growth))
    var3 <- calc_var(cbind(Dispersion, RGDP_Growth))
    var4 <- calc_var(cbind(Uncertainty, RGDP_Growth))
    
    ##Granger causality tests
    G <- data.frame()
    G[1,1] <- causality(var1,cause = "Idiosyncratic")$Granger[4]
    G[1,2] <- as.numeric(as.character(causality(var1,cause = "Idiosyncratic")$Granger[1]))
    G[1,3] <- as.numeric(as.character(causality(var1,cause = "Idiosyncratic")$Granger[3]))
    G[2,1] <- causality(var1,cause = "RGDP_Growth")$Granger[4]
    G[2,2] <- as.numeric(as.character(causality(var1,cause = "RGDP_Growth")$Granger[1]))
    G[2,3] <- as.numeric(as.character(causality(var1,cause = "RGDP_Growth")$Granger[3]))
    
    G[3,1] <- causality(var2,cause = "Aggregate")$Granger[4]
    G[3,2] <- as.numeric(as.character(causality(var2,cause = "Aggregate")$Granger[1]))
    G[3,3] <- as.numeric(as.character(causality(var2,cause = "Aggregate")$Granger[3]))
    G[4,1] <- causality(var2,cause = "RGDP_Growth")$Granger[4]
    G[4,2] <- as.numeric(as.character(causality(var2,cause = "RGDP_Growth")$Granger[1]))
    G[4,3] <- as.numeric(as.character(causality(var2,cause = "RGDP_Growth")$Granger[3]))
    
    G[5,1] <- causality(var3,cause = "Dispersion")$Granger[4]
    G[5,2] <- as.numeric(as.character(causality(var3,cause = "Dispersion")$Granger[1]))
    G[5,3] <- as.numeric(as.character(causality(var3,cause = "Dispersion")$Granger[3]))
    G[6,1] <- causality(var3,cause = "RGDP_Growth")$Granger[4]
    G[6,2] <- as.numeric(as.character(causality(var3,cause = "RGDP_Growth")$Granger[1]))
    G[6,3] <- as.numeric(as.character(causality(var3,cause = "RGDP_Growth")$Granger[3]))

    G[7,1] <- causality(var4,cause = "Uncertainty")$Granger[4]
    G[7,2] <- as.numeric(as.character(causality(var4,cause = "Uncertainty")$Granger[1]))
    G[7,3] <- as.numeric(as.character(causality(var4,cause = "Uncertainty")$Granger[3]))
    G[8,1] <- causality(var4,cause = "RGDP_Growth")$Granger[4]
    G[8,2] <- as.numeric(as.character(causality(var4,cause = "RGDP_Growth")$Granger[1]))
    G[8,3] <- as.numeric(as.character(causality(var4,cause = "RGDP_Growth")$Granger[3]))
    
    G[,2:3] <- round(G[,2:3],3)
    mystars <- ifelse(G[,3] < .01, "***", ifelse(G[,3] < .05, "** ", ifelse(G[,3] < .1, "* ", " ")))
    Gnew <- matrix(paste(G[,2], mystars, sep=""), ncol=1) 
    G[,1] <- sub(".*: ", "", G[,1])
    G[,2] <- Gnew
    colnames(G) <- c("Granger causality H0:","statistic","p-value")
    
    return(G)
}

G_manu <- calc_sectoralvar(manufac[-95,])
G_build <- calc_sectoralvar(construct[c(-1:-5,-95),])
G_trade <- calc_sectoralvar(trade[c(-1,-95),])
G_serv <- calc_sectoralvar(services[c(-1:-53,-95),])

G_sector <- cbind(G_manu[,1:2],G_build[,2],G_trade[,2],G_serv[,2])
colnames(G_sector)[2:5] <- c("Manufacturing", "Construction","Trade","Services")

xt <- xtable(G_sector, caption="Granger causality tests")
print(xt, "latex", include.rownames=FALSE,comment=FALSE, caption.placement = getOption("xtable.caption.placement", "top"), 
      scalebox = 0.8)


#----------------------------------------------
#Unit root tests
adf.test(services[-1:-53,5], alternative = "stationary")

summary(ur.df(w.uncert_error[-95,4], c("none"), selectlags = c("AIC")))
summary(ur.df(manufac[-95,4], c("drift"), selectlags = c("AIC")))
summary(ur.df(services[-1:-53,5], c("trend"), selectlags = c("AIC")))

G_manu <- calc_sectoralvar(manufac)
G_build <- calc_sectoralvar(construct[-1:-5,])
G_trade <- calc_sectoralvar(trade[-1,])
G_serv <- calc_sectoralvar(services[-1:-53,],1)

#----------------------------------------------------------
##IRFs
#----------------------------------------------------------
detach("package:BCDating", unload=TRUE)

vardat <- cbind(Dispersion,RGDP_Growth)[-95,]
infocrit <- VARselect(vardat, lag.max = 12, type = "const")
k_aic <- infocrit$selection[1]
k_hq  <- infocrit$selection[2]
k_sic <- infocrit$selection[3]
k <- min(k_aic,k_sic,k_hq)
var1 <- VAR(vardat,p=k,type="const")

irf.y1 <- irf(var1,impulse = "Dispersion", response = "RGDP_Growth", n.ahead = 12,runs = 1000, seed=12345) 
irf.y2 <- irf(var1,impulse = "RGDP_Growth", response = "Dispersion", n.ahead = 12,runs = 1000, seed=12345)
par(mfrow=c(1,2),mar=c(7,5,7,2), cex=0.6, new=FALSE)
plot(irf.y1,plot.type = c("single"))
par(mfrow=c(1,2),mar=c(6,4,4.2,1), cex=0.6, new = TRUE)
plot(irf.y2,plot.type = c("single"))

irf.y1 <- irf(var2,impulse = "Idiosyncratic", response = "RGDP_Growth", n.ahead = 12,runs = 1000, seed=12345) 
irf.y2 <- irf(var2,impulse = "RGDP_Growth", response = "Idiosyncratic", n.ahead = 12,runs = 1000, seed=12345)
par(mfrow=c(1,2),mar=c(7,5,7,2), cex=0.6, new=FALSE)
plot(irf.y1,plot.type = c("single"))
par(mfrow=c(1,2),mar=c(6,4,4.2,1), cex=0.6, new = TRUE)
plot(irf.y2,plot.type = c("single"))

irf.y1 <- irf(var3,impulse = "Aggregate", response = "RGDP_Growth", n.ahead = 12,runs = 1000, seed=12345) 
irf.y2 <- irf(var3,impulse = "RGDP_Growth", response = "Aggregate", n.ahead = 12,runs = 1000, seed=12345)
par(mfrow=c(1,2),mar=c(7,5,7,2), cex=0.6, new=FALSE)
plot(irf.y1,plot.type = c("single"))
par(mfrow=c(1,2),mar=c(6,4,4.2,1), cex=0.6, new = TRUE)
plot(irf.y2,plot.type = c("single"))

irf.y1 <- irf(var4,impulse = "EPU", response = "RGDP_Growth", n.ahead = 12,runs = 1000, seed=12345) 
irf.y2 <- irf(var4,impulse = "RGDP_Growth", response = "EPU", n.ahead = 12,runs = 1000, seed=12345)
par(mfrow=c(1,2),mar=c(7,5,7,2), cex=0.6, new=FALSE)
plot(irf.y1,plot.type = c("single"))
par(mfrow=c(1,2),mar=c(6,4,4.2,1), cex=0.6, new = TRUE)
plot(irf.y2,plot.type = c("single"))

irf.y1 <- irf(var5,impulse = "SAVI", response = "RGDP_Growth", n.ahead = 12,runs = 1000, seed=12345) 
irf.y2 <- irf(var5,impulse = "RGDP_Growth", response = "SAVI", n.ahead = 12,runs = 1000, seed=12345)
par(mfrow=c(1,2),mar=c(7,5,7,2), cex=0.6, new=FALSE)
plot(irf.y1,plot.type = c("single"))
par(mfrow=c(1,2),mar=c(6,4,4.2,1), cex=0.6, new = TRUE)
plot(irf.y2,plot.type = c("single"))

irf.y1 <- irf(var6,impulse = "Uncertainty", response = "RGDP_Growth", n.ahead = 12,runs = 1000, seed=12345) 
irf.y2 <- irf(var6,impulse = "RGDP_Growth", response = "Uncertainty", n.ahead = 12,runs = 1000, seed=12345)
par(mfrow=c(1,2),mar=c(7,5,7,2), cex=0.6, new=FALSE)
plot(irf.y1,plot.type = c("single"))
par(mfrow=c(1,2),mar=c(6,4,4.2,1), cex=0.6, new = TRUE)
plot(irf.y2,plot.type = c("single"))

#------------------------------------------------
#FEVD
source("plot_varfevd.R")
#dev.off()
par(mfrow=c(1,2))
plot.varfevd(fevd(var6, n.ahead = 10 ),plot.type = "single")
#-------------------------------------------------
#Sectoral
manufac[-95,]
construct[c(-1:-5,-95),]
trade[c(-1,-95),]
services[c(-1:-53,-95),]

data <- manufac[-95,]
Idiosyncratic <- data[,2]
Aggregate <- data[,3]
Dispersion <- data[,4]
RGDP_Growth <- data[,5]

vardat <- cbind(Dispersion,RGDP_Growth)
infocrit <- VARselect(vardat, lag.max = 12, type = "const")
k_aic <- infocrit$selection[1]
k_hq  <- infocrit$selection[2]
k_sic <- infocrit$selection[3]
k <- min(k_aic,k_sic,k_hq)
varm <- VAR(vardat,p=k,type="const")

irf.y1 <- irf(varm,impulse = "Dispersion", response = "RGDP_Growth", n.ahead = 12,runs = 1000, seed=12345) 
irf.y2 <- irf(varm,impulse = "RGDP_Growth", response = "Dispersion", n.ahead = 12,runs = 1000, seed=12345)
par(mfrow=c(1,2),mar=c(7,5,7,2), cex=0.6, new=FALSE)
plot(irf.y1,plot.type = c("single"))
par(mfrow=c(1,2),mar=c(6,4,4.2,1), cex=0.6, new = TRUE)
plot(irf.y2,plot.type = c("single"))

source("plot_varfevd.R")
#dev.off()
par(mfrow=c(1,2))
plot.varfevd(fevd(varm, n.ahead = 10 ),plot.type = "single")



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


#---------------------------------------
png(file = "Sentiment_plot.png", width=720,height=480)
index_plot <- cbind(conf_indices[,c(1,3)],uncert_indices$Uncertainty)
index_plot[,-1] <- scale(index_plot[,-1])
colnames(index_plot) <- c("Date","Confidence","Uncertainty")
g <- ggplot(index_plot) 
g <- g + geom_line(aes(x=Date, y=Confidence, colour="Confidence"), size = 1)
g <- g + geom_line(aes(x=Date, y=Uncertainty, colour="Uncertainty"), size = 1)
g <- g + labs(color="Legend text")
g <- g + geom_rect(data=recessions.df, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='grey', alpha=0.5)
g <- g + ylab("Indicator") + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) 
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"), expand=c(0,0),
                      limits = as.Date(c("1990-12-31", NA)))
g <- g + theme(legend.position="bottom")
g
dev.off()





