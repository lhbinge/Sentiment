##========================================================================================##
## -------------------------------- BUILDING ---------------------------------------------##
##========================================================================================##
setwd("C:\\Users\\Laurie\\OneDrive\\Documents\\BING\\BER Confidence Surveys\\Sentiment")
#change the working directory

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

GDPdata <- read.csv("GDP Data.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)
GDPdata$X <- as.Date(GDPdata$X, format = "%Y/%m/%d")

#library(xlsx)
#write.xlsx(BER.B, "check.xlsx")

##=============================
BER.B <- rbind.fill(read.csv("Building.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE),
                    read.csv("Building_pre2001.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE))
BER.B <- BER.B[BER.B$Latecomer == FALSE | is.na(BER.B$Latecomer),]

colnames(BER.B)[1:6] <- c("region","id","sector","weight","factor","surveyQ")

BER.B$surveyQ <- toupper(BER.B$surveyQ)

BER.B$region <- factor(BER.B$region)
BER.B$sector <- factor(BER.B$sector) #could include labels
BER.B$id <- factor(BER.B$id)
BER.B$surveyQ <- factor(BER.B$surveyQ)

# replace 1,2,3 (Up, Same, Down) responses with 1,0,-1
for(i in 7:16) {
    BER.B[,i] <- replace(BER.B[,i], BER.B[,i]==2, 0)
    BER.B[,i] <- replace(BER.B[,i], BER.B[,i]==3,-1)
}

BER.B$Q6 <- replace(BER.B$Q6, BER.B$Q6==-1,0) # replace -1 (Less keen) responses with 0

for(i in 17:20) {
    BER.B[,i] <- replace(BER.B[,i], BER.B[,i]==1, 1)
    BER.B[,i] <- replace(BER.B[,i], BER.B[,i]==2, 0.5)
    BER.B[,i] <- replace(BER.B[,i], BER.B[,i]==3, 0)
}

##================================##
## CALCULATE INDICATORS: BUILDING ##
##================================##

building <- BER.B[BER.B$sector=="5000" | BER.B$sector=="6000",]
res <- aggregate(building[,(match("surveyQ",colnames(building))+1):ncol(building)], by=list(building$surveyQ), FUN=mean, na.rm=TRUE)
colnames(res) <- paste("res",colnames(res),sep=".")

building <- BER.B[BER.B$sector=="5010" | BER.B$sector=="6010",]
nonres <- aggregate(building[,(match("surveyQ",colnames(building))+1):ncol(building)], by=list(building$surveyQ), FUN=mean, na.rm=TRUE)
colnames(nonres) <- paste("nonres",colnames(nonres),sep=".")

building <- BER.B[BER.B$sector=="5000" | BER.B$sector=="6000" | BER.B$sector=="5010" | BER.B$sector=="6010",]
total <- aggregate(building[,(match("surveyQ",colnames(building))+1):ncol(building)], by=list(building$surveyQ), FUN=mean, na.rm=TRUE)
colnames(total) <- paste("total",colnames(total),sep=".")

building <- BER.B[BER.B$sector=="5000",]
con_res <- aggregate(building[,(match("surveyQ",colnames(building))+1):ncol(building)], by=list(building$surveyQ), FUN=mean, na.rm=TRUE)
colnames(con_res) <- paste("con_res",colnames(con_res),sep=".")

building <- BER.B[BER.B$sector=="5010",]
con_nonres <- aggregate(building[,(match("surveyQ",colnames(building))+1):ncol(building)], by=list(building$surveyQ), FUN=mean, na.rm=TRUE)
colnames(con_nonres) <- paste("con_nonres",colnames(con_nonres),sep=".")

building <- BER.B[BER.B$sector=="5000" | BER.B$sector=="5010",]
con_tot <- aggregate(building[,(match("surveyQ",colnames(building))+1):ncol(building)], by=list(building$surveyQ), FUN=mean, na.rm=TRUE)
colnames(con_tot) <- paste("con_tot",colnames(con_tot),sep=".")

building <- BER.B[BER.B$sector=="6000",]
subcon_res <- aggregate(building[,(match("surveyQ",colnames(building))+1):ncol(building)], by=list(building$surveyQ), FUN=mean, na.rm=TRUE)
colnames(subcon_res) <- paste("subcon_res",colnames(subcon_res),sep=".")

building <- BER.B[BER.B$sector=="6010",]
subcon_nonres <- aggregate(building[,(match("surveyQ",colnames(building))+1):ncol(building)], by=list(building$surveyQ), FUN=mean, na.rm=TRUE)
colnames(subcon_nonres) <- paste("subcon_nonres",colnames(subcon_nonres),sep=".")

building <- BER.B[BER.B$sector=="6000" | BER.B$sector=="6010",]
subcon_tot <- aggregate(building[,(match("surveyQ",colnames(building))+1):ncol(building)], by=list(building$surveyQ), FUN=mean, na.rm=TRUE)
colnames(subcon_tot) <- paste("subcon_tot",colnames(subcon_tot),sep=".")

building <- BER.B[BER.B$sector=="5000" | BER.B$sector=="6000" | BER.B$sector=="5010" | BER.B$sector=="6010",]
wc <- building[building$region=="1",]
wc <- aggregate(wc[,(match("surveyQ",colnames(wc))+1):ncol(wc)], by=list(wc$surveyQ), FUN=mean, na.rm=TRUE)
colnames(wc) <- paste("wc",colnames(wc),sep=".")

kzn <- building[building$region=="5",]
kzn <- aggregate(kzn[,(match("surveyQ",colnames(kzn))+1):ncol(kzn)], by=list(kzn$surveyQ), FUN=mean, na.rm=TRUE)
colnames(kzn) <- paste("kzn",colnames(kzn),sep=".")

gp <- building[building$region=="6",]
gp <- aggregate(gp[,(match("surveyQ",colnames(gp))+1):ncol(gp)], by=list(gp$surveyQ), FUN=mean, na.rm=TRUE)
colnames(gp) <- paste("gp",colnames(gp),sep=".")


building <- cbind(res[,2:17],nonres[,2:17],total[,2:17],
                  con_res[,2:17],con_nonres[,2:17],con_tot[,2:17],
                  subcon_res[,2:17],subcon_nonres[,2:17],subcon_tot[,2:17],
                  wc[,2:17],kzn[,2:17],gp[,2:17], deparse.level = 1)
building <- building * 100
building <- cbind(Date=res$res.Group.1,building)

indicators <- read.csv("indicators_B.csv", header=TRUE, sep=";",na.strings = "", skipNul = TRUE)
indicators <- indicators[,1:2]
building <- merge(indicators,building, by.x="Date", by.y="Date", all=TRUE)[,-2]
building$Date <- GDPdata$X[-1:-5]

indicators_alt <- read.csv("indicators_B.csv", header=TRUE, sep=";",na.strings = "", skipNul = TRUE)
indicators_alt <- indicators_alt[,1:2]
indicators_alt <- merge(indicators_alt,building, by.x="Date", by.y="date", all=TRUE)[,-2]
indicators_alt$Date <- GDPdata$X[-1:-5]



indicator_plot <- indicators[,c("Date","res","nonres","total")]
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


indicator_plot <- indicators[,c("Date","con_res","con_nonres","con_tot")]
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

indicator_plot <- indicators[,c("Date","subcon_res","subcon_nonres","subcon_tot")]
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

indicator_plot <- indicators[,c("Date","WC","KZN","GP")]
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

#Plot Data
BERplot <- aggregate(BER.B$id, by=list(BER.B$surveyQ,BER.B$region), FUN = length)
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

indicator_plot <- cbind(indicators[,c("Date","total")],indicators_alt[,"total"])
colnames(indicator_plot) <- c("Date","total (incl. latecomers)","total (excl. latecomers)")
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


indicator_plot <- cbind(indicators[,c("Date","res")],indicators_alt[,"res"])
colnames(indicator_plot) <- c("Date","res (incl. latecomers)","res (excl. latecomers)")
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

indicator_plot <- cbind(indicators[,c("Date","nonres")],indicators_alt[,"nonres"])
colnames(indicator_plot) <- c("Date","nonres (incl. latecomers)","nonres (excl. latecomers)")
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


pub <- read.csv("Building_BER_Published.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)

indicator_plot <- cbind(building[,c("Date","con_tot.Q1")],pub[,c("con_totalQ1")])
colnames(indicator_plot) <- c("Date","con_tot.Q1:Microdata","con_tot.Q1:Published")
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


indicator_plot <- cbind(building[,c("Date","con_tot.Q2A")],pub[,c("con_totalQ2A")])
colnames(indicator_plot) <- c("Date","con_tot.Q2A:Microdata","con_tot.Q2A:Published")
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


indicator_plot <- cbind(building[,c("Date","con_tot.Q7")],pub[,c("con_totalQ7")])
colnames(indicator_plot) <- c("Date","con_tot.Q2A:Microdata","con_tot.Q2A:Published")
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


##=============================


bou <- function(BER.B=BER.B,question=Q1,late="Out") {
    if(late=="Out") { BER.B <- BER.B[BER.B$Latecomer == FALSE | is.na(BER.B$Latecomer),] }
    
    
    
    
}





