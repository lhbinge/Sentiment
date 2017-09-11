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

datums <- read.csv("dates.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)[-1:-4,]
datums$Datum <- as.Date(datums$Datum, format = "%Y/%m/%d")

pub <- read.csv("Building_BER_Published.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)

ref <- read.csv("Ref Series.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)

##=============================
BER.B <- read.csv("Building_93Q2-17Q2.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)
#BER.B <- read.csv("Building_full.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)
colnames(BER.B)[1:6] <- c("region","id","sector","weight","factor","surveyQ")
BER.B$surveyQ <- toupper(BER.B$surveyQ)
BER.B$sector <- factor(BER.B$sector) #could include labels
BER.B$id <- factor(BER.B$id)
BER.B$region <- factor(BER.B$region)

#==============================
#Clean SurveyQ variable
BER.B$temp <- NULL
for(i in 1:nrow(BER.B)) {
    ifelse(substr(BER.B$surveyQ[i], 1, 1)==9, 
           BER.B$temp[i] <- paste0("19",BER.B$surveyQ[i],sep=""),
           BER.B$temp[i] <- paste0("20",BER.B$surveyQ[i],sep=""))
}
BER.B$surveyQ <- BER.B$temp
BER.B <- BER.B[,-ncol(BER.B)]
BER.B$surveyQ <- factor(BER.B$surveyQ)

#==============================
#Clean regions

#foute <- c("1993Q2","1993Q3","1994Q1","1994Q2","1994Q3","1994Q4")
#BER.B$region[(BER.B$surveyQ %in% foute) & (BER.B$region == 2)] <- 3.1
#BER.B$region[(BER.B$surveyQ %in% foute) & (BER.B$region == 3)] <- 4.1
#BER.B$region[(BER.B$surveyQ %in% foute) & (BER.B$region == 4)] <- 2.1
#BER.B$region[(BER.B$surveyQ %in% foute) & (BER.B$region == 6)] <- 8.1
#BER.B$region[(BER.B$surveyQ %in% foute) & (BER.B$region == 7)] <- 9.1
#BER.B$region[(BER.B$surveyQ %in% foute) & (BER.B$region == 8)] <- 6.1
#BER.B$region[(BER.B$surveyQ %in% foute) & (BER.B$region == 9)] <- 6.1
#BER.B$region[BER.B$surveyQ %in% foute] <- round(BER.B$region[BER.B$surveyQ %in% foute])

#Mode <- function(x) {
#    ux <- na.remove(unique(x))
#    ux[which.max(tabulate(match(x, ux)))]
#}

#fout <- c("1995Q1")
#for(i in levels(BER.B$id)) {
#    BER.B$region[(BER.B$surveyQ %in% fout) & (BER.B$id == i)] <- Mode(BER.B$region[(BER.B$id == i)]) 
#    
#}

#==============================
#Exlcude old questions
BER.B <- BER.B[,!grepl("X",colnames(BER.B))]    

#Replace Values
for(i in 7:16) {
    BER.B[,i] <- replace(BER.B[,i], BER.B[,i]==2, 0)
    BER.B[,i] <- replace(BER.B[,i], BER.B[,i]==3,-1)
}

for(i in 17:20) {
    BER.B[,i] <- replace(BER.B[,i], BER.B[,i]==1, 1)
    BER.B[,i] <- replace(BER.B[,i], BER.B[,i]==2, 0.5)
    BER.B[,i] <- replace(BER.B[,i], BER.B[,i]==3, 0)
}

#Impute Latecomers
#Dit lyk asof die Latecomers klaar gedupliseer is en gemerk is as Imputed

#Eclude Latecomers
BER.B <- BER.B[BER.B$Latecomer == FALSE | is.na(BER.B$Latecomer),]

BER.B <- BER.B[,1:22]

##================================##
## CALCULATE INDICATORS: BUILDING ##
##================================##
residential <- c(5000,6000)
non_residential <- c(5010,6010)
all <- c(5000,5010,6000,6010)
contractor_res <- 5000
contractor_nonres <- 5010
contractor <- c(5000,5010)
subcon_res <- 6000
subcon_nonres <- 6010
subcon <- c(6000,6010)

streke <- unique(BER.B$region)


ongeweeg <- function(sektor=all,streek=streke) {
    build <- BER.B[BER.B$sector %in% sektor & BER.B$region %in% streek,]
    sector <- aggregate(build[,(match("surveyQ",colnames(build))+1):ncol(build)], by=list(build$surveyQ), FUN=mean, na.rm=TRUE)
    sector$Obs <- aggregate(build$Q1, by=list(build$surveyQ), FUN=length)[,2]
    sector <- merge(datums,sector, by.x="Date", by.y="Group.1", all=TRUE)
    sector[,3:16] <- na.approx(sector[,3:16]*100)
    return(sector)
}

geweeg <- function(sektor=all,streek=streke) {
    
    weeg <- function(temp) {  #calculate weighted mean for each quarter for all columns
        temp <- cbind(factor=temp$factor,temp$factor*temp[(match("surveyQ",colnames(temp))+1):ncol(temp)])
        #temp <- colSums(temp, na.rm=TRUE, dims = 1)/sum(temp$factor, na.rm=TRUE)
        #calculate the sum(wi*xi)/sum(wi)
        temp <- colSums(temp, na.rm=TRUE, dims = 1)/    
            sapply(colnames(temp), function(x) sum(temp$factor[!is.na(temp[colnames(temp) == x])]))
        #weight only by those that responded to a specific question
        return(temp)
    }
    
    BER.B$factor <- BER.B$weight
    build <- BER.B[BER.B$sector %in% sektor & BER.B$region %in% streek,]
    sector <- data.frame()
    for(kwartaal in levels(build$surveyQ)) {
        sector <- rbind(sector,weeg(build[build$surveyQ==kwartaal,]))
    }
    sector <- sector *100
    sector[,1] <- levels(build$surveyQ)
    colnames(sector) <- colnames(build)[-1:-5]
    sector <- merge(datums,sector, by.x="Date", by.y="surveyQ", all=TRUE)
    #sector <- as.data.frame(t(sapply(levels(man$surveyQ), function(kwartaal) weeg(man[man$surveyQ==kwartaal,]))))
    sector[,3:16] <- na.approx(sector[,3:16])
    return(sector)
}

Building <- ongeweeg(all)
Building_w <- geweeg(all)

Residential <- ongeweeg(residential)
Non_residential <- ongeweeg(non_residential)
Contractor_res <- ongeweeg(contractor_res)
Contractor_nonres <- ongeweeg(contractor_nonres)
Contractor <- ongeweeg(contractor)
Subcon_res <- ongeweeg(subcon_res)
Subcon_nonres <- ongeweeg(subcon_nonres)
Subcon <- ongeweeg(subcon)

WC <- ongeweeg(all,1)
KZN <- ongeweeg(all,5)
GP <- ongeweeg(all,6)

#Interpolasie:
Contractor[c(3,22,29,51),c(3,4,6,8,10,12)] <- pub[c(3,22,29,51),2:7]
Contractor_res[c(3,22,29,51),c(3,4,6,8,10,12)] <- pub[c(3,22,29,51),11:16]
Contractor_nonres[c(3,22,29,51),c(3,4,6,8,10,12)] <- pub[c(3,22,29,51),20:25]
Subcon[c(3,22,29,51),c(3,4,6,8,10,12)] <- pub[c(3,22,29,51),29:34]
Subcon_res[c(3,22,29,51),c(3,4,6,8,10,12)] <- pub[c(3,22,29,51),38:43]
Subcon_nonres[c(3,22,29,51),c(3,4,6,8,10,12)] <- pub[c(3,22,29,51),47:52]


##=============================
indicator_plot <- cbind(Building[,c("Datum","Q1")],Residential[,"Q1"],Non_residential[,"Q1"]) 
colnames(indicator_plot) <- c("Date","Building","Residential","Non-residential")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_line()
g <- g + ylab("Indicator") + xlab("")
g <- g + ggtitle("New Building Categories")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) 
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g <- g + theme(legend.position="bottom")
g


indicator_plot <- cbind(Building[,c("Datum","Q1")],Building_w[,c("Q1")])
colnames(indicator_plot) <- c("Date","Unweighted","Weighted")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_line()
g <- g + ylab("Indicator") + xlab("")
g <- g + ggtitle("Q1: Building")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) 
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g <- g + theme(legend.position="bottom")
g

#Regions

BERplot <- aggregate(BER.B$id, by=list(BER.B$surveyQ,BER.B$region), FUN = length)
BERplot$Group.1 <- as.Date(as.yearqtr(BERplot$Group.1, format = "%YQ%q"))
g <- ggplot(BERplot, aes(x=Group.1, y=x,fill=Group.2))
g <- g + geom_bar(stat="identity")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + scale_fill_discrete(name="Region")
g <- g + scale_y_continuous(labels=comma)
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g <- g + ylab("Number of Respondents")
g <- g + xlab("Date")
g


#Published series
indicator_plot <- cbind(WC[,c("Datum","Q1")],KZN[,"Q1"],GP[,"Q1"])
colnames(indicator_plot) <- c("Date","WC","KZN","GP")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_line()
g <- g + ylab("Indicator") + xlab("")
g <- g + ggtitle("Q1: Provinces")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) 
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g <- g + theme(legend.position="bottom")
g

##=============================
#Published series
indicator_plot <- cbind(Contractor[,c("Datum","Q1")],pub[,"con_totalQ1"])
colnames(indicator_plot) <- c("Date","Microdata","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g1 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g1 <- g1 + geom_line()
g1 <- g1 + ylab("Indicator") + xlab("")
g1 <- g1 + theme(legend.title=element_blank())
g1 <- g1 + ggtitle("Q1: Contractors") 
g1 <- g1 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g1 <- g1 + theme(legend.title=element_blank()) 
g1 <- g1 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g1 <- g1 + theme(legend.position="none")

indicator_plot <- cbind(Contractor[,c("Datum","Q2A")],pub[,c("con_totalQ2A")])
colnames(indicator_plot) <- c("Date","Microdata","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g2 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g2 <- g2 + geom_line()
g2 <- g2 + ylab("") + xlab("")
g2 <- g2 + theme(legend.title=element_blank())
g2 <- g2 + ggtitle("Q2A: Contractors") 
g2 <- g2 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g2 <- g2 + theme(legend.title=element_blank()) 
g2 <- g2 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g2 <- g2 + theme(legend.position="none")

indicator_plot <- cbind(Subcon[,c("Datum","Q1")],pub[,c("subcon_totalQ1")])
colnames(indicator_plot) <- c("Date","Microdata","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g3 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g3 <- g3 + geom_line()
g3 <- g3 + ylab("Indicator") + xlab("")
g3 <- g3 + theme(legend.title=element_blank())
g3 <- g3 + ggtitle("Q1: Subcontractors") 
g3 <- g3 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g3 <- g3 + theme(legend.title=element_blank()) 
g3 <- g3 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g3 <- g3 + theme(legend.position="bottom",plot.margin=unit(c(-0.5,0.4,0,0.4), "cm"))

indicator_plot <- cbind(Subcon[,c("Datum","Q8")],pub[,c("subcon_totalQ8")])
colnames(indicator_plot) <- c("Date","Microdata","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g4 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g4 <- g4 + geom_line()
g4 <- g4 + ylab("") + xlab("")
g4 <- g4 + theme(legend.title=element_blank())
g4 <- g4 + ggtitle("Q8: Subcontractors") 
g4 <- g4 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g4 <- g4 + theme(legend.title=element_blank()) 
g4 <- g4 + theme(legend.position="bottom",plot.margin=unit(c(-0.5,0.4,0,0.4), "cm"))
g4 <- g4 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

library(gridExtra)
grid.arrange(g1, g2, g3, g4, ncol=2, nrow =2)


#Published series
indicator_plot <- cbind(Contractor_res[,c("Datum","Q3A")],pub[,"con_resQ3A"])
colnames(indicator_plot) <- c("Date","Microdata","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g1 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g1 <- g1 + geom_line()
g1 <- g1 + ylab("Indicator") + xlab("")
g1 <- g1 + theme(legend.title=element_blank())
g1 <- g1 + ggtitle("Q3A: Contractors (Residential)") 
g1 <- g1 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g1 <- g1 + theme(legend.title=element_blank()) 
g1 <- g1 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g1 <- g1 + theme(legend.position="none")

indicator_plot <- cbind(Contractor_nonres[,c("Datum","Q4A")],pub[,c("con_nonresQ4A")])
colnames(indicator_plot) <- c("Date","Microdata","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g2 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g2 <- g2 + geom_line()
g2 <- g2 + ylab("") + xlab("")
g2 <- g2 + theme(legend.title=element_blank())
g2 <- g2 + ggtitle("Q4A: Contractors (Non-Residential)") 
g2 <- g2 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g2 <- g2 + theme(legend.title=element_blank()) 
g2 <- g2 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g2 <- g2 + theme(legend.position="none")

indicator_plot <- cbind(Subcon_res[,c("Datum","Q5A")],pub[,c("subcon_resQ5A")])
colnames(indicator_plot) <- c("Date","Microdata","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g3 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g3 <- g3 + geom_line()
g3 <- g3 + ylab("Indicator") + xlab("")
g3 <- g3 + theme(legend.title=element_blank())
g3 <- g3 + ggtitle("Q5A: Subcontractors (Residential)") 
g3 <- g3 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g3 <- g3 + theme(legend.title=element_blank()) 
g3 <- g3 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g3 <- g3 + theme(legend.position="bottom",plot.margin=unit(c(-0.5,0.4,0,0.4), "cm"))

indicator_plot <- cbind(Subcon_nonres[,c("Datum","Q7")],pub[,c("subcon_nonresQ7")])
colnames(indicator_plot) <- c("Date","Microdata","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g4 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g4 <- g4 + geom_line()
g4 <- g4 + ylab("") + xlab("")
g4 <- g4 + theme(legend.title=element_blank())
g4 <- g4 + ggtitle("Q6: Subcontractors (Non-Residential)") 
g4 <- g4 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g4 <- g4 + theme(legend.title=element_blank()) 
g4 <- g4 + theme(legend.position="bottom",plot.margin=unit(c(-0.5,0.4,0,0.4), "cm"))
g4 <- g4 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

library(gridExtra)
grid.arrange(g1, g2, g3, g4, ncol=2, nrow =2)


##=============================
#Compare to reference series

build <- cbind(Residential[,c("Datum","Q2A","Q3A")],ref[,c("GFCF.Residential","Residential.building")])
colnames(build) <- c("Date","Q2A","Q3A","GFCF.Residential","Residential.building")

build[,2:5] <- scale(build[,2:5])
plot <- melt(build, id="Date")  # convert to long format
g <- ggplot(data=plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_line()
g <- g + ylab("Indicator")+ xlab("")
g <- g + ggtitle("Residential")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) 
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g <- g + theme(legend.position="bottom")
g

source("corstarsl.R")
xt <- xtable(corstarsl(build[,-1])[3:4,1:2])
print(xt, "latex",comment=FALSE,scalebox = 0.8)


Q2A <- build[,2]
Q3A <- build[,3] 
GFCF <- build[,4]
Build <- build[,5]
    
par(mfrow=c(2,2))
ccf(Q2A, GFCF, na.action = na.pass, ylim=c(-0.2, 0.8))
ccf(Q2A, Build, na.action = na.pass, ylim=c(-0.2, 0.8))
ccf(Q3A, GFCF, na.action = na.pass, ylim=c(-0.2, 0.8))
ccf(Q3A, Build, na.action = na.pass, ylim=c(-0.2, 0.8))



build <- cbind(Non_residential[,c("Datum","Q2A","Q3A")],ref[,c("GFCF.Non.residential","Non.residential.building")])
colnames(build) <- c("Date","Q2A","Q3A","GFCF.Non-residential","Non.residential.building")

build[,2:5] <- scale(build[,2:5])
plot <- melt(build, id="Date")  # convert to long format
g <- ggplot(data=plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_line()
g <- g + ylab("Indicator")+ xlab("")
g <- g + ggtitle("Non-Residential")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) 
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g <- g + theme(legend.position="bottom")
g

xt <- xtable(corstarsl(build[,-1])[3:4,1:2])
print(xt, "latex",comment=FALSE,scalebox = 0.9)

Q2A <- build[,2]
Q3A <- build[,3] 
GFCF <- build[,4]
Build <- build[,5]

par(mfrow=c(2,2))
ccf(Q2A, GFCF, na.action = na.pass, ylim=c(-0.2, 0.6))
ccf(Q2A, Build, na.action = na.pass, ylim=c(-0.2, 0.6))
ccf(Q3A, GFCF, na.action = na.pass, ylim=c(-0.2, 0.6))
ccf(Q3A, Build, na.action = na.pass, ylim=c(-0.2, 0.6))




build <- cbind(Building[,c("Datum","Q2A","Q3A")],ref[,c("GFCF.Res_Non.res","GFCF.Total","Total.building")])
colnames(build) <- c("Date","Q2A","Q3A","GFCF.Res & Non.res","GFCF.Total","Total.building")

build[,2:6] <- scale(build[,2:6])
plot <- melt(build, id="Date")  # convert to long format
g <- ggplot(data=plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_line()
g <- g + ylab("Indicator")+ xlab("")
g <- g + ggtitle("Total Building")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) 
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g <- g + theme(legend.position="bottom")
g

xt <- xtable(corstarsl(build[,-1])[3:5,1:2])
print(xt, "latex",comment=FALSE,scalebox = 0.8)

Q2A <- build[,2]
Q3A <- build[,3] 
GFCF <- build[,4]
Build <- build[,6]

par(mfrow=c(2,2))
ccf(Q2A, GFCF, na.action = na.pass, ylim=c(-0.2, 0.8))
ccf(Q2A, Build, na.action = na.pass, ylim=c(-0.2, 0.8))
ccf(Q3A, GFCF, na.action = na.pass, ylim=c(-0.2, 0.8))
ccf(Q3A, Build, na.action = na.pass, ylim=c(-0.2, 0.8))



build <- cbind(WC[,c("Datum","Q2A","Q3A")],KZN[,c("Q2A","Q3A")],GP[,c("Q2A","Q3A")],ref[,c("Building_WC","Building_KZN","Building_GP")])
colnames(build) <- c("Date","WC.Q2A","WC.Q3A","KZN.Q2A","KZN.Q3A","GP.Q2A","GP.Q3A","Building_WC","Building_KZN","Building_GP")

#Published series
indicator_plot <- cbind(build[,c("Date","WC.Q2A","WC.Q3A","Building_WC")])
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g1 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g1 <- g1 + geom_line()
g1 <- g1 + ylab("Indicator") + xlab("")
g1 <- g1 + theme(legend.title=element_blank())
g1 <- g1 + ggtitle("Western Cape") 
g1 <- g1 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g1 <- g1 + theme(legend.title=element_blank()) 
g1 <- g1 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g1 <- g1 + theme(legend.position="none")

indicator_plot <- cbind(build[,c("Date","KZN.Q2A","KZN.Q3A","Building_KZN")])
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g2 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g2 <- g2 + geom_line()
g2 <- g2 + ylab("") + xlab("")
g2 <- g2 + theme(legend.title=element_blank())
g2 <- g2 + ggtitle("KZN") 
g2 <- g2 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g2 <- g2 + theme(legend.title=element_blank()) 
g2 <- g2 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g2 <- g2 + theme(legend.position="none")

indicator_plot <- cbind(build[,c("Date","GP.Q2A","GP.Q3A","Building_GP")])
colnames(indicator_plot) <- c("Date","Q2A","Q3A","Building")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g3 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g3 <- g3 + geom_line()
g3 <- g3 + ylab("Indicator") + xlab("")
g3 <- g3 + theme(legend.title=element_blank())
g3 <- g3 + ggtitle("Gauteng") 
g3 <- g3 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g3 <- g3 + theme(legend.title=element_blank()) 
g3 <- g3 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g3 <- g3 + theme(legend.position="bottom",plot.margin=unit(c(-0.5,0.4,0,0.4), "cm"))

library(gridExtra)
grid.arrange(g1, g2, g3, ncol=2, nrow =2)

tafel <- cbind(corstarsl(build[,-1])[7,1:2],corstarsl(build[,-1])[8,3:4],
               corstarsl(build[,-1])[9,5:6])
row.names(tafel) <- "Building"
tafel <- t(tafel)
xt <- xtable(tafel)
print(xt, "latex",comment=FALSE,scalebox = 0.8)


WC.Q3A <- build[,3] 
WC.Build <- build[,8]
KZN.Q3A <- build[,5] 
KZN.Build <- build[,9]
GP.Q3A <- build[,7] 
GP.Build <- build[,10]

par(mfrow=c(2,2))
ccf(WC.Q3A, WC.Build, na.action = na.pass, ylim=c(-0.2, 0.4))
ccf(KZN.Q3A, KZN.Build, na.action = na.pass, ylim=c(-0.2, 0.4))
ccf(GP.Q3A, GP.Build, na.action = na.pass, ylim=c(-0.2, 0.6))



#=========================================
#Architects, Civils, QSs
#=========================================
skoon <- function(data) {
    colnames(data)[1:6] <- c("region","id","sector","weight","factor","surveyQ")
    data$surveyQ <- toupper(data$surveyQ)
    data$sector <- factor(data$sector) #could include labels
    data$id <- factor(data$id)
    data$region <- factor(data$region)
    
    data$temp <- NULL
    for(i in 1:nrow(data)) {
        data$temp[i] <- paste0("20",data$surveyQ[i],sep="")
    }
    data$surveyQ <- data$temp
    data <- data[,-ncol(data)]
    data$surveyQ <- factor(data$surveyQ)
    
    for(i in 7:(ncol(data)-6)) {
        data[,i] <- replace(data[,i], data[,i]==2, 0)
        data[,i] <- replace(data[,i], data[,i]==3,-1)
    }
    
    if(ncol(data)>23) {
        for(i in 17:20) {
            data[,i] <- replace(data[,i], data[,i]==0, 0.5)
            data[,i] <- replace(data[,i], data[,i]==-1, 0)
        }
    }
    
    data <- data[data$Latecomer == FALSE | is.na(data$Latecomer),]
    data <- data[,1:(ncol(data)-6)]
    return(data)
}
    
arc <- skoon(read.csv("Argitekte.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE))
civil <- skoon(read.csv("Civils.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE))
qs <- skoon(read.csv("QS.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE))

ongeweeg <- function(data=arc,uit=1) {
    build <- data#[data$sector %in% sektor & data$region %in% streek,]
    sector <- aggregate(build[,(match("surveyQ",colnames(build))+1):ncol(build)], by=list(build$surveyQ), FUN=mean, na.rm=TRUE)
    sector$Obs <- aggregate(build$Q1, by=list(build$surveyQ), FUN=length)[,2]
    sector <- merge(datums[-1:-32,],sector, by.x="Date", by.y="Group.1", all=TRUE)
    sector[,3:(ncol(sector)-uit)] <- na.approx(sector[,3:(ncol(sector)-uit)]*100)
    return(sector)
}

architects <- ongeweeg(arc,1)
qss <- ongeweeg(qs,1)
civils <- ongeweeg(civil,3)

#---------------------------------
pub <- read.csv("Arc_Published.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)

#Interpolasie:
architects[c(19),c(3,4,6,8,10,12)] <- pub[c(19),2:7]
qss[c(11,19),c(3,4,6,8,10,12)] <- pub[c(11,19),8:13]
civils[c(19),c(3,4,6,8,10,12:15)] <- pub[c(19),14:22]


#Published series
indicator_plot <- cbind(architects[,c("Datum","Q1")],pub[,"Arc_Q1"])
colnames(indicator_plot) <- c("Date","Microdata","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g1 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g1 <- g1 + geom_line()
g1 <- g1 + ylab("Indicator") + xlab("")
g1 <- g1 + theme(legend.title=element_blank())
g1 <- g1 + ggtitle("Q1: Architects") 
g1 <- g1 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g1 <- g1 + theme(legend.title=element_blank()) 
g1 <- g1 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g1 <- g1 + theme(legend.position="none")

indicator_plot <- cbind(architects[,c("Datum","Q2A")],pub[,c("Arc_Q2A")])
colnames(indicator_plot) <- c("Date","Microdata","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g2 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g2 <- g2 + geom_line()
g2 <- g2 + ylab("") + xlab("")
g2 <- g2 + theme(legend.title=element_blank())
g2 <- g2 + ggtitle("Q2A: Architects") 
g2 <- g2 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g2 <- g2 + theme(legend.title=element_blank()) 
g2 <- g2 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g2 <- g2 + theme(legend.position="none")

indicator_plot <- cbind(architects[,c("Datum","Q4A")],pub[,c("Arc_Q4A")])
colnames(indicator_plot) <- c("Date","Microdata","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g3 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g3 <- g3 + geom_line()
g3 <- g3 + ylab("Indicator") + xlab("")
g3 <- g3 + theme(legend.title=element_blank())
g3 <- g3 + ggtitle("Q4A: Architects") 
g3 <- g3 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g3 <- g3 + theme(legend.title=element_blank()) 
g3 <- g3 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g3 <- g3 + theme(legend.position="bottom",plot.margin=unit(c(-0.5,0.4,0,0.4), "cm"))

indicator_plot <- cbind(architects[,c("Datum","Q6A")],pub[,c("Arc_Q6A")])
colnames(indicator_plot) <- c("Date","Microdata","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g4 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g4 <- g4 + geom_line()
g4 <- g4 + ylab("") + xlab("")
g4 <- g4 + theme(legend.title=element_blank())
g4 <- g4 + ggtitle("Q6A: Architects") 
g4 <- g4 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g4 <- g4 + theme(legend.title=element_blank()) 
g4 <- g4 + theme(legend.position="bottom",plot.margin=unit(c(-0.5,0.4,0,0.4), "cm"))
g4 <- g4 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

library(gridExtra)
grid.arrange(g1, g2, g3, g4, ncol=2, nrow =2)



#Published series
indicator_plot <- cbind(qss[,c("Datum","Q1")],pub[,"QS_Q1"])
colnames(indicator_plot) <- c("Date","Microdata","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g1 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g1 <- g1 + geom_line()
g1 <- g1 + ylab("Indicator") + xlab("")
g1 <- g1 + theme(legend.title=element_blank())
g1 <- g1 + ggtitle("Q1: QSs") 
g1 <- g1 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g1 <- g1 + theme(legend.title=element_blank()) 
g1 <- g1 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g1 <- g1 + theme(legend.position="none")

indicator_plot <- cbind(qss[,c("Datum","Q2A")],pub[,c("QS_Q2A")])
colnames(indicator_plot) <- c("Date","Microdata","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g2 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g2 <- g2 + geom_line()
g2 <- g2 + ylab("") + xlab("")
g2 <- g2 + theme(legend.title=element_blank())
g2 <- g2 + ggtitle("Q2A: QSs") 
g2 <- g2 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g2 <- g2 + theme(legend.title=element_blank()) 
g2 <- g2 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g2 <- g2 + theme(legend.position="none")

indicator_plot <- cbind(qss[,c("Datum","Q4A")],pub[,c("QS_Q4A")])
colnames(indicator_plot) <- c("Date","Microdata","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g3 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g3 <- g3 + geom_line()
g3 <- g3 + ylab("Indicator") + xlab("")
g3 <- g3 + theme(legend.title=element_blank())
g3 <- g3 + ggtitle("Q4A: QSs") 
g3 <- g3 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g3 <- g3 + theme(legend.title=element_blank()) 
g3 <- g3 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g3 <- g3 + theme(legend.position="bottom",plot.margin=unit(c(-0.5,0.4,0,0.4), "cm"))

indicator_plot <- cbind(qss[,c("Datum","Q6A")],pub[,c("QS_Q6A")])
colnames(indicator_plot) <- c("Date","Microdata","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g4 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g4 <- g4 + geom_line()
g4 <- g4 + ylab("") + xlab("")
g4 <- g4 + theme(legend.title=element_blank())
g4 <- g4 + ggtitle("Q6A: QSs") 
g4 <- g4 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g4 <- g4 + theme(legend.title=element_blank()) 
g4 <- g4 + theme(legend.position="bottom",plot.margin=unit(c(-0.5,0.4,0,0.4), "cm"))
g4 <- g4 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

library(gridExtra)
grid.arrange(g1, g2, g3, g4, ncol=2, nrow =2)


#Published series
indicator_plot <- cbind(civils[,c("Datum","Q1")],pub[,"CE_Q1"])
colnames(indicator_plot) <- c("Date","Microdata","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g1 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g1 <- g1 + geom_line()
g1 <- g1 + ylab("Indicator") + xlab("")
g1 <- g1 + theme(legend.title=element_blank())
g1 <- g1 + ggtitle("Q1: CEs") 
g1 <- g1 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g1 <- g1 + theme(legend.title=element_blank()) 
g1 <- g1 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g1 <- g1 + theme(legend.position="none")

indicator_plot <- cbind(civils[,c("Datum","Q2A")],pub[,c("CE_Q2A")])
colnames(indicator_plot) <- c("Date","Microdata","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g2 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g2 <- g2 + geom_line()
g2 <- g2 + ylab("") + xlab("")
g2 <- g2 + theme(legend.title=element_blank())
g2 <- g2 + ggtitle("Q2A: CEs") 
g2 <- g2 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g2 <- g2 + theme(legend.title=element_blank()) 
g2 <- g2 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g2 <- g2 + theme(legend.position="none")

indicator_plot <- cbind(civils[,c("Datum","Q4A")],pub[,c("CE_Q4A")])
colnames(indicator_plot) <- c("Date","Microdata","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g3 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g3 <- g3 + geom_line()
g3 <- g3 + ylab("Indicator") + xlab("")
g3 <- g3 + theme(legend.title=element_blank())
g3 <- g3 + ggtitle("Q4A: CEs") 
g3 <- g3 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g3 <- g3 + theme(legend.title=element_blank()) 
g3 <- g3 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g3 <- g3 + theme(legend.position="bottom",plot.margin=unit(c(-0.5,0.4,0,0.4), "cm"))

indicator_plot <- cbind(civils[,c("Datum","Q6")],pub[,c("CE_Q6")])
colnames(indicator_plot) <- c("Date","Microdata","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g4 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g4 <- g4 + geom_line()
g4 <- g4 + ylab("") + xlab("")
g4 <- g4 + theme(legend.title=element_blank())
g4 <- g4 + ggtitle("Q6: CEs") 
g4 <- g4 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g4 <- g4 + theme(legend.title=element_blank()) 
g4 <- g4 + theme(legend.position="bottom",plot.margin=unit(c(-0.5,0.4,0,0.4), "cm"))
g4 <- g4 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

library(gridExtra)
grid.arrange(g1, g2, g3, g4, ncol=2, nrow =2)

###-------------------
#Final interpolated series
#indicators_final <- cbind(datums,na.approx(indicators[,2:ncol(indicators)]))
#write.csv(indicators_final,"Building_Indicators.csv")
Building1 <- Building[,c(1,19,3:16)]
Building1 <- cbind(Building1,NA,Residential[,c(1,19,3:16)])
Building1 <- cbind(Building1,NA,Non_residential[,c(1,19,3:16)])
Building1 <- cbind(Building1,NA,WC[,c(1,19,3:16)])
Building1 <- cbind(Building1,NA,KZN[,c(1,19,3:16)])
Building1 <- cbind(Building1,NA,GP[,c(1,19,3:16)])
colnames(Building1)[17] <- " "
colnames(Building1)[34] <- " "
colnames(Building1)[51] <- " "
colnames(Building1)[68] <- " "
colnames(Building1)[85] <- " "

Building1[,c(1,18,35,52,69,86)] <- lapply(Building1[,c(1,18,35,52,69,86)], as.character)
Building1[,c(-1:-2,-17:-19,-34:-36,-51:-53,-68:-70,-85:-87)] <- round(Building1[,c(-1:-2,-17:-19,-34:-36,-51:-53,-68:-70,-85:-87)],2)
Building1 <- rbind(colnames(Building1),Building1)
colnames(Building1)[1:16] <- "Total"
colnames(Building1)[18:33] <- "Residential"
colnames(Building1)[35:50] <- "Non-Residential"
colnames(Building1)[52:67] <- "WC"
colnames(Building1)[69:84] <- "KZN"
colnames(Building1)[86:101] <- "GP"


Contractor1 <- Contractor[,c(1,19,3:16)]
Contractor1 <- cbind(Contractor1,NA,Contractor_res[,c(1,19,3:16)])
Contractor1 <- cbind(Contractor1,NA,Contractor_nonres[,c(1,19,3:16)])
colnames(Contractor1)[17] <- " "
colnames(Contractor1)[34] <- " "
Contractor1[,c(1,18,35)] <- lapply(Contractor1[,c(1,18,35)], as.character)
Contractor1[,c(-1:-2,-17:-19,-34:-36)] <- round(Contractor1[,c(-1:-2,-17:-19,-34:-36)],2)
Contractor1 <- rbind(colnames(Contractor1),Contractor1)
colnames(Contractor1)[1:16] <- "Total"
colnames(Contractor1)[18:33] <- "Residential"
colnames(Contractor1)[35:50] <- "Non-Residential"


Subcon1 <- Subcon[,c(1,19,3:16)]
Subcon1 <- cbind(Subcon1,NA,Subcon_res[,c(1,19,3:16)])
Subcon1 <- cbind(Subcon1,NA,Subcon_nonres[,c(1,19,3:16)])
colnames(Subcon1)[17] <- " "
colnames(Subcon1)[34] <- " "
Subcon1[,c(1,18,35)] <- lapply(Subcon1[,c(1,18,35)], as.character)
Subcon1[,c(-1:-2,-17:-19,-34:-36)] <- round(Subcon1[,c(-1:-2,-17:-19,-34:-36)],2)
Subcon1 <- rbind(colnames(Subcon1),Subcon1)
colnames(Subcon1)[1:16] <- "Total"
colnames(Subcon1)[18:33] <- "Residential"
colnames(Subcon1)[35:50] <- "Non-Residential"

architects1 <- architects[,c(1,14,3:13)]
architects1[,c(1)] <- as.character(architects1[,c(1)])
architects1[,c(-1:-2)] <- round(architects1[,c(-1:-2)],2)
architects1 <- rbind(colnames(architects1),architects1)
colnames(architects1)[1:13] <- "Total"

qss1 <- qss[,c(1,14,3:13)]
qss1[,c(1)] <- as.character(qss1[,c(1)])
qss1[,c(-1:-2)] <- round(qss1[,c(-1:-2)],2)
qss1 <- rbind(colnames(qss1),qss1)
colnames(qss1)[1:13] <- "Total"

civils1 <- civils[,c(1,19,3:16)]
civils1[,c(1)] <- as.character(civils1[,c(1)])
civils1[,c(-1:-2)] <- round(civils1[,c(-1:-2)],2)
civils1 <- rbind(colnames(civils1),civils1)
colnames(civils1)[1:16] <- "Total"

library(xlsx)
skryf <- function(data,naam) {
    write.xlsx2(data, "Building_Indicators.xlsx", sheetName = naam, 
                col.names=TRUE, row.names=FALSE, append=TRUE, showNA=FALSE)
}
skryf(Building1,"BUILD")
skryf(Contractor1,"BUILD-Con")
skryf(Subcon1,"BUILD-Sub")
skryf(architects1,"ARC")
skryf(qss1,"QS")
skryf(civils1,"CE")



skryf <- function(data,naam) {
    data[,-1:-2] <- round(data[,-1:-2],2)
    data[,2] <- as.character(data$Datum)
    write.xlsx2(data, "Building_Indicators.xlsx", sheetName = naam, 
                col.names=TRUE, row.names=FALSE, append=TRUE, showNA=FALSE)
}
skryf(Residential,"Residential")
skryf(Non_residential,"Non_residential")
skryf(Contractor,"Contractor")
skryf(Contractor_res,"Contractor_res")
skryf(Contractor_nonres,"Contractor_nonres")
skryf(Subcon,"Subcon")
skryf(Subcon_res,"Subcon_res")
skryf(Subcon_nonres,"Subcon_nonres")
skryf(WC,"WC")
skryf(KZN,"KZN")
skryf(GP,"GP")






