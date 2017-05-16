##===========================##
##------ SENTIMENT ----------##
##===========================##
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

brics <- read.csv("BRICS.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)
brics$Date <- as.Date(brics$Date, format = "%Y/%m/%d")


##For Grpahing Business cycles
recessions.l = read.table(textConnection(
    "Peak, Trough
    1960-04-30, 1961-08-31
    1965-04-30, 1965-12-31
    1967-05-31, 1967-12-31
    1970-12-31, 1972-08-31
    1974-08-31, 1977-12-31
    1981-08-31, 1983-03-31
    1984-06-30, 1986-03-31
    1989-02-28, 1993-05-30
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

##========================##
##----- CONFIDENCE -------##
##========================##
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
    #w.confidence$Conf_all <- rowMeans(w.confidence[,c("Q1","Q2A","Q3A","Q4A","Q5A","Q6A","Q2P","Q3P","Q4P","Q5P","Q6P")],na.rm = TRUE, dims = 1)
    w.confidence <- merge(datums,w.confidence,by.x="Date",by.y="row.names", all=TRUE)[,-3]
    w.confidence[,14:15] <- na.approx(w.confidence[,14:15],na.rm = FALSE)
    
    return(w.confidence)
}


##======================##
## CALCULATE INDICATORS ##
##======================##
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
g <- g + labs(color="Legend text")
g <- g + geom_rect(data=recessions.df, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='grey', alpha=0.5)
g <- g + ylab("Indicator") + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) 
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"), expand=c(0,0),
                      limits = as.Date(c("1990-12-31", NA)))
g <- g + theme(legend.position="bottom")
g


#------------------------------
#Leading and Coincident
conf_indices <- cbind(w.indicators,GDPdata$BER_BCI, GDPdata$SACCI_BCI,GDPgrowth4$RGDP,GDPgrowth4$Leading,GDPgrowth4$Coincident)
colnames(conf_indices) <- c("Date","Activity","Confidence","BER_BCI","SACCI_BCI","RGDP_Growth","Leading","Coincident")

index_plot <- conf_indices
index_plot[,2:8] <- scale(index_plot[,2:8])
g <- ggplot(index_plot) 
#g <- g + geom_line(aes(x=Date, y=Activity, colour="Activity"), size = 1)
#g <- g + geom_line(aes(x=Date, y=Confidence, colour="Confidence"), size = 1)
g <- g + geom_line(aes(x=Date, y=BER_BCI, colour="BER_BCI"), size = 1)
#g <- g + geom_line(aes(x=Date, y=SACCI_BCI, colour="SACCI_BCI"), size = 1)
g <- g + geom_line(aes(x=Date, y=Leading, colour="Leading"), size = 1)
g <- g + geom_line(aes(x=Date, y=Coincident, colour="Coincident"), size = 1)
g <- g + geom_line(aes(x=Date, y=RGDP_Growth, colour="RGDP_Growth"), size = 1)
g <- g + labs(color="Legend text")
g <- g + geom_rect(data=recessions.df, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='grey', alpha=0.5)
g <- g + ylab("Indicator") + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) 
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"), expand=c(0,0),
                      limits = as.Date(c("1990-12-31", NA)))
g <- g + theme(legend.position="bottom")
g

#---------------
#Long term
conf_indices <- cbind(brics[,c(1,5:8)])
colnames(conf_indices) <- c("Date","RGDP_Growth","Coincident","Leading","BER_BCI")

index_plot <- conf_indices[-1:-60,]
index_plot[,-1] <- scale(index_plot[,-1])
g <- ggplot(index_plot) 
g <- g + geom_line(aes(x=Date, y=BER_BCI, colour="BER_BCI"), size = 1)
g <- g + geom_line(aes(x=Date, y=Leading, colour="Leading"), size = 1)
g <- g + geom_line(aes(x=Date, y=Coincident, colour="Coincident"), size = 1)
g <- g + geom_line(aes(x=Date, y=RGDP_Growth, colour="RGDP_Growth"), size = 1)
g <- g + labs(color="Legend text")
g <- g + geom_rect(data=recessions.l, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='grey', alpha=0.5)
g <- g + ylab("Indicator") + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) 
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"), expand=c(0,0),
                      limits = as.Date(c("1973-12-31", NA)))
g <- g + theme(legend.position="bottom")
g

#Check correlations
source("corstarsl.R")
xt <- xtable(corstarsl(conf_indices[,-1]), caption="Correlations in Levels")
print(xt, "latex",comment=FALSE, caption.placement = getOption("xtable.caption.placement", "top"), scalebox = 0.8)

corstarsl(conf_indices[,c(4,7,8,6)])

Activity <- conf_indices[,2]
Confidence <- conf_indices[,3]
BER_BCI <- conf_indices[,4]
SACCI_BCI <- conf_indices[,5] 
RGDP_Growth <- conf_indices[,6]
Leading <- conf_indices[,7]
Coincident <- conf_indices[,8]

par(mfrow=c(2,2))
ccf(Activity, RGDP_Growth, na.action = na.pass, ylim=c(-0.3, 0.8))
ccf(Confidence, RGDP_Growth, na.action = na.pass, ylim=c(-0.3, 0.8))
ccf(BER_BCI, RGDP_Growth, na.action = na.pass, ylim=c(-0.3, 0.8))
ccf(SACCI_BCI, RGDP_Growth, na.action = na.pass, ylim=c(-0.3, 0.8))


par(mfrow=c(2,2),mar=c(3,4,4.2,1))
ccf(BER_BCI, RGDP_Growth, na.action = na.pass, ylim=c(-0.5, 0.9))
ccf(Leading, RGDP_Growth, na.action = na.pass, ylim=c(-0.5, 0.9))
ccf(Coincident, RGDP_Growth, na.action = na.pass, ylim=c(-0.5, 0.9))


#-------------------------
#Long term
BER_BCI <- conf_indices[,5]
RGDP_Growth <- conf_indices[,2]
Leading <- conf_indices[,4]
Coincident <- conf_indices[,3]

par(mfrow=c(2,2),mar=c(3,4,4.2,1))
ccf(BER_BCI, RGDP_Growth, na.action = na.pass, ylim=c(-0.4, 0.8))
ccf(Leading, RGDP_Growth, na.action = na.pass, ylim=c(-0.4, 0.8))
ccf(Coincident, RGDP_Growth, na.action = na.pass, ylim=c(-0.4, 0.8))

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
    ccf(Activity, RGDP_Growth, na.action = na.pass, ylim=c(-0.3, 0.8))
    ccf(Confidence, RGDP_Growth, na.action = na.pass, ylim=c(-0.3, 0.8))
    if(serv==0) { ccf(BER_BCI, RGDP_Growth, na.action = na.pass, ylim=c(-0.3, 0.8)) }
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

dat <- BBQ(ts(conf_indices[,2],start =c(1992,1),end=c(2015,3),frequency=4), mincycle = 5, minphase = 2, name="Activity")
tp1 <- as.data.frame(show(dat))[,-3]
dat <- BBQ(ts(conf_indices[,3],start =c(1992,1),end=c(2015,3),frequency=4), mincycle = 5, minphase = 2, name="Activity")
tp2 <- as.data.frame(show(dat))[,-3]
dat <- BBQ(ts(conf_indices[,4],start =c(1992,1),end=c(2015,3),frequency=4), mincycle = 5, minphase = 2, name="Activity")
tp3 <- as.data.frame(show(dat))[,-3]
dat <- BBQ(ts(conf_indices[,5],start =c(1992,1),end=c(2015,3),frequency=4), mincycle = 5, minphase = 2, name="Activity")
tp4 <- as.data.frame(show(dat))[,-3]
dat <- BBQ(ts(conf_indices[,6],start =c(1992,1),end=c(2015,3),frequency=4), mincycle = 5, minphase = 2, name="Activity")
tp5 <- as.data.frame(show(dat))[,-3]

dat <- BBQ(ts(conf_indices[,7],start =c(1992,1),end=c(2015,3),frequency=4), mincycle = 5, minphase = 2, name="Activity")
tp6 <- as.data.frame(show(dat))[,-3]
dat <- BBQ(ts(conf_indices[,8],start =c(1992,1),end=c(2015,3),frequency=4), mincycle = 5, minphase = 2, name="Activity")
tp7 <- as.data.frame(show(dat))[,-3]

detach("package:BCDating", unload=TRUE)

maak_datums <- function(data) {
    data$Peak <- as.Date(as.yearqtr(data[,1], format = "%YQ%q"), frac = 1)
    data$Trough <- as.Date(as.yearqtr(data[,2], format = "%YQ%q"), frac = 1)
    data$Peak[1] <- "1990-12-31"
    data$Trough[nrow(data)] <- "2016-12-31"
    return(data)
}

tp1 <- maak_datums(tp1)
tp2 <- maak_datums(tp2)
tp3 <- maak_datums(tp3)
tp4 <- maak_datums(tp4)
tp5 <- maak_datums(tp5)
tp6 <- maak_datums(tp6)
tp7 <- maak_datums(tp7)


index_plot <- conf_indices[,c(1,2)]
index_plot[,2] <- scale(index_plot[,2])
colnames(index_plot) <- c("Date","Confidence")
g1 <- ggplot(index_plot) 
g1 <- g1 + geom_line(aes(x=Date, y=Confidence, colour="Confidence"), size = 0.5)
g1 <- g1 + labs(color="Legend text")
g1 <- g1 + geom_rect(data=recessions.df, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=0), fill='grey', alpha=0.5)
g1 <- g1 + geom_rect(data=tp1, aes(xmin=Peak, xmax=Trough, ymin=0, ymax=+Inf), fill='blue', alpha=0.5)
g1 <- g1 + ylab("Indicator") + xlab("")
g1 <- g1 + ggtitle("Activity") 
g1 <- g1 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g1 <- g1 + theme(legend.title=element_blank()) 
g1 <- g1 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"), expand=c(0,0),
                      limits = as.Date(c("1990-12-31", NA)))
g1 <- g1 + theme(legend.position="none")


index_plot <- conf_indices[,c(1,3)]
index_plot[,2] <- scale(index_plot[,2])
colnames(index_plot) <- c("Date","Confidence")
g2 <- ggplot(index_plot) 
g2 <- g2 + geom_line(aes(x=Date, y=Confidence, colour="Confidence"), size = 0.5)
g2 <- g2 + labs(color="Legend text")
g2 <- g2 + geom_rect(data=recessions.df, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=0), fill='grey', alpha=0.5)
g2 <- g2 + geom_rect(data=tp2, aes(xmin=Peak, xmax=Trough, ymin=0, ymax=+Inf), fill='blue', alpha=0.5)
g2 <- g2 + ylab("") + xlab("")
g2 <- g2 + ggtitle("Confidence") 
g2 <- g2 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g2 <- g2 + theme(legend.title=element_blank()) 
g2 <- g2 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"), expand=c(0,0),
                        limits = as.Date(c("1990-12-31", NA)))
g2 <- g2 + theme(legend.position="none")

index_plot <- conf_indices[,c(1,4)]
index_plot[,2] <- scale(index_plot[,2])
colnames(index_plot) <- c("Date","Confidence")
g3 <- ggplot(index_plot) 
g3 <- g3 + geom_line(aes(x=Date, y=Confidence, colour="Confidence"), size = 0.5)
g3 <- g3 + labs(color="Legend text")
g3 <- g3 + geom_rect(data=recessions.df, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=0), fill='grey', alpha=0.5)
g3 <- g3 + geom_rect(data=tp3, aes(xmin=Peak, xmax=Trough, ymin=0, ymax=+Inf), fill='blue', alpha=0.5)
g3 <- g3 + ylab("Indicator") + xlab("")
g3 <- g3 + ggtitle("BER BCI") 
g3 <- g3 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g3 <- g3 + theme(legend.title=element_blank()) 
g3 <- g3 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"), expand=c(0,0),
                        limits = as.Date(c("1990-12-31", NA)))
g3 <- g3 + theme(legend.position="none")

index_plot <- conf_indices[,c(1,5)]
index_plot[,2] <- scale(index_plot[,2])
colnames(index_plot) <- c("Date","Confidence")
g4 <- ggplot(index_plot) 
g4 <- g4 + geom_line(aes(x=Date, y=Confidence, colour="Confidence"), size = 0.5)
g4 <- g4 + labs(color="Legend text")
g4 <- g4 + geom_rect(data=recessions.df, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=0), fill='grey', alpha=0.5)
g4 <- g4 + geom_rect(data=tp4, aes(xmin=Peak, xmax=Trough, ymin=0, ymax=+Inf), fill='blue', alpha=0.5)
g4 <- g4 + ylab("") + xlab("")
g4 <- g4 + ggtitle("SACCI BCI") 
g4 <- g4 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g4 <- g4 + theme(legend.title=element_blank()) 
g4 <- g4 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"), expand=c(0,0),
                        limits = as.Date(c("1990-12-31", NA)))
g4 <- g4 + theme(legend.position="none")

library(gridExtra)
grid.arrange(g1, g2, g3, g4, ncol=2, nrow =2)



#------------------------------------------

#Leading and coincident
index_plot <- conf_indices[,c(1,7)]
index_plot[,2] <- scale(index_plot[,2])
colnames(index_plot) <- c("Date","Confidence")
g1 <- ggplot(index_plot) 
g1 <- g1 + geom_line(aes(x=Date, y=Confidence, colour="Confidence"), size = 0.5)
g1 <- g1 + labs(color="Legend text")
g1 <- g1 + geom_rect(data=recessions.df, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=0), fill='grey', alpha=0.5)
g1 <- g1 + geom_rect(data=tp6, aes(xmin=Peak, xmax=Trough, ymin=0, ymax=+Inf), fill='black', alpha=0.5)
g1 <- g1 + ylab("") + xlab("")
g1 <- g1 + ggtitle("Leading") 
g1 <- g1 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g1 <- g1 + theme(legend.title=element_blank()) 
g1 <- g1 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"), expand=c(0,0),
                        limits = as.Date(c("1990-12-31", NA)))
g1 <- g1 + theme(legend.position="none")

index_plot <- conf_indices[,c(1,4)]
index_plot[,2] <- scale(index_plot[,2])
colnames(index_plot) <- c("Date","Confidence")
g3 <- ggplot(index_plot) 
g3 <- g3 + geom_line(aes(x=Date, y=Confidence, colour="Confidence"), size = 0.5)
g3 <- g3 + labs(color="Legend text")
g3 <- g3 + geom_rect(data=recessions.df, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=0), fill='grey', alpha=0.5)
g3 <- g3 + geom_rect(data=tp3, aes(xmin=Peak, xmax=Trough, ymin=0, ymax=+Inf), fill='black', alpha=0.5)
g3 <- g3 + ylab("Indicator") + xlab("")
g3 <- g3 + ggtitle("BER BCI") 
g3 <- g3 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g3 <- g3 + theme(legend.title=element_blank()) 
g3 <- g3 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"), expand=c(0,0),
                        limits = as.Date(c("1990-12-31", NA)))
g3 <- g3 + theme(legend.position="none")

index_plot <- conf_indices[,c(1,8)]
index_plot[,2] <- scale(index_plot[,2])
colnames(index_plot) <- c("Date","Confidence")
g4 <- ggplot(index_plot) 
g4 <- g4 + geom_line(aes(x=Date, y=Confidence, colour="Confidence"), size = 0.5)
g4 <- g4 + labs(color="Legend text")
g4 <- g4 + geom_rect(data=recessions.df, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=0), fill='grey', alpha=0.5)
g4 <- g4 + geom_rect(data=tp7, aes(xmin=Peak, xmax=Trough, ymin=0, ymax=+Inf), fill='black', alpha=0.5)
g4 <- g4 + ylab("Indicator") + xlab("")
g4 <- g4 + ggtitle("Coincident") 
g4 <- g4 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g4 <- g4 + theme(legend.title=element_blank()) 
g4 <- g4 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"), expand=c(0,0),
                        limits = as.Date(c("1990-12-31", NA)))
g4 <- g4 + theme(legend.position="none")

library(gridExtra)
grid.arrange(g3, g1, g4, ncol=2, nrow =2)

#------------------------------------------
#Long term
suppressMessages(library(BCDating))

dat <- BBQ(ts(conf_indices[-1:-60,2],start =c(1975,1),end=c(2016,3),frequency=4), mincycle = 5, minphase = 2, name="Activity")
tp1 <- as.data.frame(show(dat))[,-3]
dat <- BBQ(ts(conf_indices[-1:-60,3],start =c(1975,1),end=c(2016,3),frequency=4), mincycle = 5, minphase = 2, name="Activity")
tp2 <- as.data.frame(show(dat))[,-3]
dat <- BBQ(ts(conf_indices[-1:-60,4],start =c(1975,1),end=c(2016,3),frequency=4), mincycle = 5, minphase = 2, name="Activity")
tp3 <- as.data.frame(show(dat))[,-3]
dat <- BBQ(ts(conf_indices[-1:-60,5],start =c(1975,1),end=c(2016,3),frequency=4), mincycle = 5, minphase = 2, name="Activity")
tp4 <- as.data.frame(show(dat))[,-3]

maak_datums <- function(data) {
    data$Peak <- as.Date(as.yearqtr(data[,1], format = "%YQ%q"), frac = 1)
    data$Trough <- as.Date(as.yearqtr(data[,2], format = "%YQ%q"), frac = 1)
    data$Peak[1] <- "1973-12-31"
    data$Trough[nrow(data)] <- "2016-12-31"
    return(data)
}

tp1 <- maak_datums(tp1)
tp2 <- maak_datums(tp2)
tp3 <- maak_datums(tp3)
tp4 <- maak_datums(tp4)
tp5 <- maak_datums(tp5)

detach("package:BCDating", unload=TRUE)

#----------------------------------

index_plot <- conf_indices[-1:-60,c(1,4)]
index_plot[,2] <- scale(index_plot[,2])
colnames(index_plot) <- c("Date","Confidence")
g1 <- ggplot(index_plot) 
g1 <- g1 + geom_line(aes(x=Date, y=Confidence, colour="Confidence"), size = 0.5)
g1 <- g1 + labs(color="Legend text")
g1 <- g1 + geom_rect(data=recessions.l, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=0), fill='grey', alpha=0.5)
g1 <- g1 + geom_rect(data=tp3, aes(xmin=Peak, xmax=Trough, ymin=0, ymax=+Inf), fill='black', alpha=0.5)
g1 <- g1 + ylab("") + xlab("")
g1 <- g1 + ggtitle("Leading") 
g1 <- g1 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g1 <- g1 + theme(legend.title=element_blank()) 
g1 <- g1 + scale_x_date(expand=c(0,0),limits = as.Date(c("1973-12-31", NA)))
g1 <- g1 + theme(legend.position="none")

index_plot <- conf_indices[-1:-60,c(1,5)]
index_plot[,2] <- scale(index_plot[,2])
colnames(index_plot) <- c("Date","Confidence")
g3 <- ggplot(index_plot) 
g3 <- g3 + geom_line(aes(x=Date, y=Confidence, colour="Confidence"), size = 0.5)
g3 <- g3 + labs(color="Legend text")
g3 <- g3 + geom_rect(data=recessions.l, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=0), fill='grey', alpha=0.5)
g3 <- g3 + geom_rect(data=tp4, aes(xmin=Peak, xmax=Trough, ymin=0, ymax=+Inf), fill='black', alpha=0.5)
g3 <- g3 + ylab("Indicator") + xlab("")
g3 <- g3 + ggtitle("BER BCI") 
g3 <- g3 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g3 <- g3 + theme(legend.title=element_blank()) 
g3 <- g3 + scale_x_date(expand=c(0,0),limits = as.Date(c("1973-12-31", NA)))
g3 <- g3 + theme(legend.position="none")

index_plot <- conf_indices[-1:-60,c(1,3)]
index_plot[,2] <- scale(index_plot[,2])
colnames(index_plot) <- c("Date","Confidence")
g4 <- ggplot(index_plot) 
g4 <- g4 + geom_line(aes(x=Date, y=Confidence, colour="Confidence"), size = 0.5)
g4 <- g4 + labs(color="Legend text")
g4 <- g4 + geom_rect(data=recessions.l, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=0), fill='grey', alpha=0.5)
g4 <- g4 + geom_rect(data=tp2, aes(xmin=Peak, xmax=Trough, ymin=0, ymax=+Inf), fill='black', alpha=0.5)
g4 <- g4 + ylab("Indicator") + xlab("")
g4 <- g4 + ggtitle("Coincident") 
g4 <- g4 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g4 <- g4 + theme(legend.title=element_blank()) 
g4 <- g4 + scale_x_date(expand=c(0,0),limits = as.Date(c("1973-12-31", NA)))
g4 <- g4 + theme(legend.position="none")

library(gridExtra)
grid.arrange(g3, g1, g4, ncol=2, nrow =2)
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

vardat <- cbind(BER_BCI,RGDP_Growth)
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

irf.y1 <- irf(var1,impulse = "BER_BCI", response = "RGDP_Growth", n.ahead = 12,runs = 1000, seed=12345) 
irf.y2 <- irf(var1,impulse = "RGDP_Growth", response = "BER_BCI", n.ahead = 12,runs = 1000, seed=12345)
par(mfrow=c(1,2),mar=c(7,5,7,2), cex=0.6, new=FALSE)
plot(irf.y1,plot.type = c("single"))
par(mfrow=c(1,2),mar=c(6,4,4.2,2), cex=0.6, new = TRUE)
plot(irf.y2,plot.type = c("single"))

par(mfrow=c(1,1), new=FALSE)
par(mfrow=c(1,2), new=TRUE)
p1 <- plot(irf.y1,plot.type = c("multiple"))
p2 <- plot(irf.y2,plot.type = c("multiple"))

par(mfrow=c(1,1), new=FALSE)
nf <- layout(matrix(c(1,2,1,2), 1, 2, byrow = TRUE))
layout.show(nf)
#par(cex=0.6)
plot(irf.y1,plot.type = c("single"), main="Response from BER_BCI")
par(new = TRUE)
plot(irf.y2,plot.type = c("single"), main="Response from RGDP Growth")

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

#---------------------------------------------------
#Longer term
detach("package:BCDating", unload=TRUE)

vardat <- cbind(BER_BCI,RGDP_Growth)[-1:-60,]
infocrit <- VARselect(vardat, lag.max = 12, type = "const")
k_aic <- infocrit$selection[1]
k_hq  <- infocrit$selection[2]
k_sic <- infocrit$selection[3]
k <- min(k_aic,k_sic,k_hq)
var1 <- VAR(vardat,p=k,type="const")

irf.y1 <- irf(var1,impulse = "Coincident", response = "RGDP_Growth", n.ahead = 12,runs = 1000, seed=12345) 
irf.y2 <- irf(var1,impulse = "RGDP_Growth", response = "Coincident", n.ahead = 12,runs = 1000, seed=12345)
par(mfrow=c(1,2),mar=c(7,5,7,2), cex=0.6, new=FALSE)
plot(irf.y1,plot.type = c("single"))
par(mfrow=c(1,2),mar=c(6,4,4.2,2), cex=0.6, new = TRUE)
plot(irf.y2,plot.type = c("single"))

par(mfrow=c(1,1), new=FALSE)
nf <- layout(matrix(c(1,2,1,2), 1, 2, byrow = TRUE))
layout.show(nf)
#par(cex=0.6)
plot(irf.y1,plot.type = c("single"), main="Response from Coincident")
par(new = TRUE)
plot(irf.y2,plot.type = c("single"), main="Response from RGDP Growth")

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
    



##========================##
## ---- UNCERTAINTY ------##
##========================##
se <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x))*(length(na.omit(x))-1)) #adjust for (n-1)
##------------##
## Dispersion                    
##------------##

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

#-------------------------------------------------
#PCA
#-------------------------------------------------
uncert_indices <- cbind(w.uncert_error,GDPdata$IMF_Uncertainty, GDPdata$SAVI,GDPgrowth4$RGDP)
colnames(uncert_indices) <-c("Date","Dispersion","Idiosyncratic","Aggregate","EPU","SAVI","RGDP_Growth")
uncert_indices[,-1] <- scale(uncert_indices[,-1])
un <- uncert_indices #un <- na.locf(uncert_indices)
un[is.na(un)] <- 0
#uncert_indices$Uncertainty <- rowMeans(uncert_indices[,c(2:6)],na.rm = TRUE)
uncert_indices$Uncertainty <- princomp(un[,c(2:6)])$scores[,1]
uncert_indices <- uncert_indices[,c(1:6,8,7)]

#fit <- princomp(un[,c(2:6)])
#print(fit)
#summary(fit) # print variance accounted for 
#loadings(fit) # pc loadings 
#plot(fit,type="lines") # scree plot 
#p <- fit$scores # the principal components


index_plot <- uncert_indices
g <- ggplot(index_plot) 
#g <- g + geom_line(aes(x=Date, y=Dispersion, colour="Dispersion"), size = 0.5)
#g <- g + geom_line(aes(x=Date, y=Aggregate, colour="Aggregate"), size = 0.5)
#g <- g + geom_line(aes(x=Date, y=Idiosyncratic, colour="Idiosyncratic"), size = 0.5)
g <- g + geom_line(aes(x=Date, y=EPU, colour="EPU"), size = 0.9)
g <- g + geom_line(aes(x=Date, y=SAVI, colour="SAVI"), size = 0.9)
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
RGDP_Growth <- conf_indices[,6]
#EPU <- p[,1]
    
par(mfrow=c(3,2))
ccf(Dispersion, RGDP_Growth, na.action = na.pass, ylim=c(-0.5, 0.5))
ccf(Idiosyncratic, RGDP_Growth, na.action = na.pass, ylim=c(-0.5, 0.5))
ccf(Aggregate, RGDP_Growth, na.action = na.pass, ylim=c(-0.5, 0.5))
#par(mfrow=c(2,2))
ccf(EPU, RGDP_Growth, na.action = na.pass, ylim=c(-0.5, 0.5))
ccf(SAVI, RGDP_Growth, na.action = na.pass, ylim=c(-0.5, 0.5))
ccf(Uncertainty, RGDP_Growth, na.action = na.pass, ylim=c(-0.5, 0.5))
#ccf(PCA, RGDP_Growth, na.action = na.pass)
#corstarsl(cbind(uncert_indices[,-1],PCA))

#-----------------------------------------------
#Sectoral Analysis
manufac <- cbind(w.uncert_error.M,w.uncertainty.M[,16], GDPgrowth4$Manufacturing)
colnames(manufac) <- c("Date","Idiosyncratic","Aggregate","Dispersion","RGDP_Growth")
manufac[,2:4] <- scale(manufac[,2:4])
#manufac$Uncertainty <- rowMeans(manufac[,c(2:4)],na.rm = TRUE)
manufac <- manufac[-95,]
manufac$Uncertainty <- princomp(na.locf(manufac[,2:4]))$scores[,1]
manufac <- manufac[,c(1:4,6,5)]

construct <- cbind(w.uncert_error.B,w.uncertainty.B[,16], GDPgrowth4$Construction)
colnames(construct) <- c("Date","Idiosyncratic","Aggregate","Dispersion","RGDP_Growth")
construct[,2:4] <- scale(construct[,2:4])
#construct$Uncertainty <- rowMeans(construct[,c(2:4)],na.rm = TRUE)
construct <- construct[c(-1:-5,-95),] #un <- na.locf(uncert_indices)
#un[is.na(un)] <- 0
construct$Uncertainty <- princomp(na.locf(construct[,2:4]))$scores[,1]
construct <- construct[,c(1:4,6,5)]

trade <- cbind(w.uncert_error.T,w.uncertainty.T[,16], GDPgrowth4$Trade)
colnames(trade) <- c("Date","Idiosyncratic","Aggregate","Dispersion","RGDP_Growth")
trade[,2:4] <- scale(trade[,2:4])
#trade$Uncertainty <- rowMeans(trade[,c(2:4)],na.rm = TRUE)
trade <- trade[c(-1,-95),] #un <- na.locf(uncert_indices)
#un[is.na(un)] <- 0
trade$Uncertainty <- princomp(na.locf(trade[,2:4]))$scores[,1]
trade <- trade[,c(1:4,6,5)]

services <- cbind(w.uncert_error.S,w.uncertainty.S[,16], GDPgrowth4$Services)
colnames(services) <- c("Date","Idiosyncratic","Aggregate","Dispersion","RGDP_Growth")
services[,2:4] <- scale(services[,2:4])
#services$Uncertainty <- rowMeans(services[,c(2:4)],na.rm = TRUE)
services <- services[c(-1:-53,-95),] #un <- na.locf(uncert_indices)
#un[is.na(un)] <- 0
services$Uncertainty <- princomp(na.locf(services[,2:4]))$scores[,1]
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
print(xt, "latex",comment=FALSE, caption.placement = getOption("xtable.caption.placement", "top"), scalebox = 0.7)
xt <- xtable(xt2)
print(xt, "latex",comment=FALSE, caption.placement = getOption("xtable.caption.placement", "top"), scalebox = 0.75)


calc_ccf <- function(data) {
    Idiosyncratic <- data[,2]
    Aggregate <- data[,3]
    Dispersion <- data[,4]
    Uncertainty <- data[,5]
    RGDP_Growth <- data[,6]
    
    par(mfrow=c(2,2))
    ccf(Idiosyncratic, RGDP_Growth, na.action = na.pass, ylim=c(-0.5, 0.5))
    ccf(Aggregate, RGDP_Growth, na.action = na.pass, ylim=c(-0.5, 0.5))
    ccf(Dispersion, RGDP_Growth, na.action = na.pass, ylim=c(-0.5, 0.5))
    ccf(Uncertainty, RGDP_Growth, na.action = na.pass, ylim=c(-0.5, 0.5))
}

calc_ccf(manufac)
calc_ccf(construct)
calc_ccf(trade)
calc_ccf(services)


##==========================
##------VAR Analysis--------
##==========================
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
var6 <- calc_var(cbind(Uncertainty, RGDP_Growth)[,])


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

G_manu <- calc_sectoralvar(manufac)
G_build <- calc_sectoralvar(construct)
G_trade <- calc_sectoralvar(trade)
G_serv <- calc_sectoralvar(services)

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
data <- manufac
#data <- construct
#data <- trade
#data <- services

Idiosyncratic <- data[,2]
Aggregate <- data[,3]
Dispersion <- data[,4]
Uncertainty <- data[,5]
RGDP_Growth <- data[,6]

vardat <- cbind(Uncertainty,RGDP_Growth)
infocrit <- VARselect(vardat, lag.max = 12, type = "const")
k_aic <- infocrit$selection[1]
k_hq  <- infocrit$selection[2]
k_sic <- infocrit$selection[3]
k <- min(k_aic,k_sic,k_hq)
varm <- VAR(vardat,p=k,type="const")

irf.y1 <- irf(varm,impulse = "Uncertainty", response = "RGDP_Growth", n.ahead = 12,runs = 1000, seed=12345) 
irf.y2 <- irf(varm,impulse = "RGDP_Growth", response = "Uncertainty", n.ahead = 12,runs = 1000, seed=12345)
par(mfrow=c(1,2),mar=c(7,5,7,2), cex=0.6, new=FALSE)
plot(irf.y1,plot.type = c("single"))
par(mfrow=c(1,2),mar=c(6,4,4.2,1), cex=0.6, new = TRUE)
plot(irf.y2,plot.type = c("single"))

source("plot_varfevd.R")
#dev.off()
par(mfrow=c(1,2), new = FALSE)
plot.varfevd(fevd(varm, n.ahead = 10 ),plot.type = "single")


#===========================================
#All three together
index_plot <- cbind(conf_indices[,c(1,2)],uncert_indices$Uncertainty) 
index_plot[,-1] <- scale(index_plot[,-1])
colnames(index_plot) <- c("Date","Activity","Uncertainty")
g <- ggplot(index_plot) 
g <- g + geom_line(aes(x=Date, y=Activity, colour="Activity"), size = 1)
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


write.csv2(conf_indices,"conf_indices.csv")
write.csv2(uncert_indices,"uncert_indices.csv")

conf_indices <- read.csv2("conf_indices.csv")[,-1]
uncert_indices <- read.csv2("uncert_indices.csv")[,-1]


#=================
#Expanded VAR
#=================
Activity <- conf_indices[,2]
Confidence <- conf_indices[,3]
BER_BCI <- conf_indices[,4]
SACCI_BCI <- conf_indices[,5] 
RGDP_Growth <- conf_indices[,6]

Dispersion <- uncert_indices[,2]
Idiosyncratic <- uncert_indices[,3]
Aggregate <- uncert_indices[,4]
EPU <- uncert_indices[,5] 
SAVI <- uncert_indices[,6] 
Uncertainty <- uncert_indices[,7]

#Three-variable VAR
vardat <- cbind(Activity,Uncertainty,RGDP_Growth)  
infocrit <- VARselect(vardat, lag.max = 16, type = "const")
k_aic <- infocrit$selection[1]
k_hq  <- infocrit$selection[2]
k_sic <- infocrit$selection[3]
k <- min(k_aic,k_sic,k_hq)
var_3 <- VAR(vardat,p=k,type="const")

irf.y1 <- irf(var_3,impulse = c("Activity","Uncertainty"), response = "RGDP_Growth", n.ahead = 12,runs = 1000, seed=12345) 
par(mfrow=c(1,2),mar=c(5,4,4,2), cex=0.5)
plot(irf.y1,plot.type = c("single"))

irf.y2 <- irf(var_3,impulse = "RGDP_Growth", response = c("Activity","Uncertainty"), n.ahead = 12,runs = 1000, seed=12345)
par(mfrow=c(1,2),mar=c(5,4,4,2), cex=0.5)
plot(irf.y2,plot.type = c("single"))

source("plot_varfevd.R")
#dev.off()
par(mfrow=c(1,3))
plot.varfevd(fevd(var_3, n.ahead = 10 ),plot.type = "single")

#---------------------------------
#In growth rates:
JSE <- GDPgrowth4$RJSE
Bond <- GDPdata$Bond2
TBill <- GDPdata$T.Bill
Spread <- Bond-TBill
Employment <- GDPgrowth4$Employ
Investment <- GDPgrowth4$Rinvestment 
Production <- GDPgrowth4$RProduction

vardat <- cbind(Activity,Uncertainty,JSE,Spread,RGDP_Growth,
                Production,Employment,Investment)  

vardat <- cbind(BER_BCI,JSE,Spread,RGDP_Growth,
                Production,Employment,Investment)  

infocrit <- VARselect(vardat, lag.max = 16, type = "const")
k_aic <- infocrit$selection[1]
k_hq  <- infocrit$selection[2]
k_sic <- infocrit$selection[3]
k <- min(k_aic,k_sic,k_hq)
vare <- VAR(vardat,p=2,type="const")

irf.y1 <- irf(vare,impulse = c("Activity","Uncertainty","JSE","Spread","Production","Employment","Investment"),
              response = c("RGDP_Growth"), n.ahead = 12,runs = 1000, seed=12345) 

irf.y1 <- irf(vare,impulse = c("Activity","Uncertainty"),
              response = c("RGDP_Growth"), n.ahead = 12,runs = 1000, seed=12345) 

irf.y1 <- irf(vare,impulse = c("Activity","Uncertainty"),
              response = c("RGDP_Growth","Production","Investment"), n.ahead = 12,runs = 1000, seed=12345) 

irf.y1 <- irf(vare,impulse = c("BER_BCI"),
              response = c("RGDP_Growth","Production","Investment"), n.ahead = 12,runs = 1000, seed=12345) 

plot(irf.y1,plot.type = c("multiple"))

irf.y2 <- irf(vare,impulse = "RGDP_Growth", response = c("Activity","Uncertainty"), n.ahead = 12,runs = 1000, seed=12345)
plot(irf.y2,plot.type = c("multiple"))

par(mfrow=c(2,3),mar=c(3,4,2,1), cex=0.6)
plot(irf.y1,plot.type = c("single"), main="")

par(mfrow=c(1,3))
plot(irf.y1,plot.type = c("single"), main="Response from BER BCI")

source("plot_varfevd.R")
#dev.off()
par(mfrow=c(1,1))
plot.varfevd(fevd(vare, n.ahead = 10 ),plot.type = "single")

plot(fevd(vare, n.ahead = 10 ))
#-------------------------------
#In levels:

JSE <- log(realGDP$RJSE[-1:-4])
Investment <- log(realGDP$Rinvestment[-1:-4])
Production <- log(realGDP$RProduction[-1:-4])
RGDP <- log(realGDP$RGDP[-1:-4])

vardat <- cbind(Activity,Uncertainty,JSE,Spread,RGDP,
                Production,Employment,Investment)  

infocrit <- VARselect(vardat, lag.max = 16, type = "const")
k_aic <- infocrit$selection[1]
k_hq  <- infocrit$selection[2]
k_sic <- infocrit$selection[3]
k <- min(k_aic,k_sic,k_hq)
var1 <- VAR(vardat,p=2,type="const")

irf.y1 <- irf(var1,impulse = c("Activity","Uncertainty"),
              response = c("RGDP","Production","Investment"), n.ahead = 12,runs = 1000, seed=12345) 

par(mfrow=c(2,3),mar=c(3,4,2,1), cex=0.6)
plot(irf.y1,plot.type = c("single"), main="")

plot(irf.y1,plot.type = c("multiple"))

irf.y2 <- irf(var1,impulse = "RGDP", response = c("Activity","Uncertainty","JSE","Bond","Spread","Production","Employment","Investment"), n.ahead = 12,runs = 1000, seed=12345)
plot(irf.y2,plot.type = c("multiple"))

par(mfrow=c(1,2),mar=c(5,4,4,2), cex=0.5)
plot(irf.y1,plot.type = c("single"))

par(mfrow=c(2,3))
plot.varfevd(fevd(vare, n.ahead = 10 ),plot.type = "single")

#---------------------------------
#Var Diagnostics
library(fUnitRoots)

summary(ur.df(Employment, c("none"), selectlags = c("AIC")))
summary(ur.df(Employment, c("drift"), selectlags = c("AIC")))
summary(ur.df(Employment, c("trend"), selectlags = c("AIC")))

adf.test(Employment)
adfTest(Spread)

vardat <- cbind(Activity,Uncertainty,RGDP_Growth)

infocrit <- VARselect(vardat, lag.max = 16, type = "const")
k_aic <- infocrit$selection[1]
k_hq  <- infocrit$selection[2]
k_sic <- infocrit$selection[3]
k <- min(k_aic,k_sic,k_hq)
var1 <- VAR(vardat,p=2,type="const")

serialT1 <- serial.test(var1, lags.bg=2, type="BG")
serialT2 <- serial.test(var1, lags.pt=2, type="PT.adjusted")
serialT3 <- serial.test(var1, lags.pt=2, type="PT.asymptotic")
archT <- arch.test(var1, lags.single=1, lags.multi = 1, multivariate.only = F)
normT <- normality.test(var1, multivariate.only = F)


plot(var1,plot.type = c("multiple"))
plot(serialT1,plot.type = c("multiple"))
methods(plot)
getAnywhere(plot.varest)
plot(serialT1,ylim.hist=c(0,2))
plot(serialT1,ylim.hist=c(0,1))
plot(normT)

# Assessment of residuals - marginal-marginal
par(din=c(5,2.5))
layout(matrix(c(1,2),nrow=1,ncol=2,byrow = T))
resids <- cbind(var1[[1]][[1]][[2]], var1[[1]][[2]][[2]])
plot(resids, ylim=c(-1.6,1.6), xlim=c(-0.8,0.8), xlab="Log Price (M)", ylab="Log Quantity (M)", main="Marginal versus marginal")
ellipse(mu=c(0,0), sigma=cov(resids), alpha=0.1,npoints=300,col="red")

# Assessment of residuals - conditional-marginal
resid2 <- cbind(marg$residuals, cond$residuals)
plot(resid2, ylim=c(-1.6,1.6), xlim=c(-0.8,0.8), xlab="Log Price (M)", ylab="Log Quantity (C)", main="Conditional versus marginal")
ellipse(mu=c(0,0), sigma=cov(resid2), alpha=0.1,npoints=300,col="red")





#--------------------------------
#Compare turning points graphically
#separate geom_rect met ymin en ymax adjusted
ggplot(dfm) +
    geom_rect(data = subset(Grunfeld, dummy == 0), 
              aes(ymin = -Inf, ymax = Inf, xmin = year-0.5, xmax = year+0.5), 
              alpha = 0.2)+
    geom_line(aes(x=year, y=value, linetype=variable, colour=variable)) +
    theme_bw() +
    facet_wrap( ~ firm)

#=================




#Principle components

# Load data
data(iris)
head(iris, 3)

# log transform 
log.ir <- log(iris[, 1:4])
ir.species <- iris[, 5]

# apply PCA - scale. = TRUE is highly 
# advisable, but default is FALSE. 
un <- uncert_indices[,2:6]

un[is.na(un)] <- 0
un <- na.locf(un,fromLast = TRUE)
un <- na.locf(un)

fit <- princomp(un,center = TRUE,scale = TRUE,cor=TRUE,na.rm=TRUE)
print(fit)
summary(fit) # print variance accounted for 
loadings(fit) # pc loadings 
plot(fit,type="lines") # scree plot 
p <- fit$scores # the principal components
biplot(fit)
screeplot(fit)


#PCA on caret package to correct skewness and scale variables
require(caret)
trans <- preProcess(un,method=c("BoxCox", "center","scale", "pca"))
PC <- predict(trans, un[,1:5])


# Varimax Rotated Principal Components
# retaining 5 components 
library(psych)
fit <- principal(mydata, nfactors=5, rotate="varimax")
fit # print results


#---------
p <- princomp(survey)
summary(p)
plot(p)
biplot(p)
p$loadings
p$scores

s <- factanal(un, factors = 2, rotation = "varimax",scores = "regression",
              na.action=na.omit)
s
f <- s$scores

####### Calculating Principal component of returns of S&P CNX 500 companies ########
## Access the relevant file ##
returns <- read.csv("Returns_CNX_500.csv")

## Dealing with missing values in the returns data for companies 
for(i in 2:ncol(returns))
{
    returns1[, i] <- approx(returns$Year, returns1[ ,i], returns$Year)$y  
## approx function basically fits the value of linear approximate between the missing data points and the column $y stores the approximated values.
}    

## Convert the data into matrix ##
ret <- as.matrix(returns1, nrow = dim(returns1)[1], ncol = dim(returns1)[2])

##Computing the principal component using eigenvalue decomposition ##
princ.return <- princomp(ret) ## This is it.!!

## Identifying what components to be used ##
barplot(height=fit$sdev[1:10]/fit$sdev[1])   
# I am plotting the standard deviation of the PC's divided by standard deviation of PC 1, this can help us decide on a benchmark that we can use to select the relevant components.

## To get the first principal component in a variable ##
load <- loadings(fit)[,1]   
## loadings() gives the linear combination by which our input variables will be linearly weighted to compute the components, and this command gives us the loading for 1st PC.

u <- as.matrix(un)
pr.cp <- u %*% load  
## Matrix multiplication of the input data with the loading for the 1st PC gives us the 1st PC in matrix form. 

pr <- as.numeric(pr.cp) ## Gives us the 1st PC in numeric form in pr.



# load spam data
data(spam)
# perform PCA on dataset
prComp <- prcomp(log10(spam[,-58]+1))
# print out the eigenvector/rotations first 5 rows and PCs
head(prComp$rotation[, 1:5], 5)
# create new variable that marks spam as 2 and nospam as 1
typeColor <- ((spam$type=="spam")*1 + 1)
# plot the first two principal components
plot(prComp$x[,1],prComp$x[,2],col=typeColor,xlab="PC1",ylab="PC2")




library(caret)
# create train and test sets
inTrain <- createDataPartition(y=spam$type,p=0.75, list=FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
# create preprocess object
preProc <- preProcess(log10(training[,-58]+1),method="pca",pcaComp=2)
# calculate PCs for training data
trainPC <- predict(preProc,log10(training[,-58]+1))
# run model on outcome and principle components
modelFit <- train(training$type ~ .,method="glm",data=trainPC)
# calculate PCs for test data
testPC <- predict(preProc,log10(testing[,-58]+1))
# compare results
confusionMatrix(testing$type,predict(modelFit,testPC))
# construct model
modelFit <- train(training$type ~ .,method="glm",preProcess="pca",data=training)
# print results of model
confusionMatrix(testing$type,predict(modelFit,testing))






