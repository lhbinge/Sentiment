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

#library(xlsx)
#write.xlsx(BER.B, "check.xlsx")

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


###-------------------
#Final interpolated series
#indicators_final <- cbind(datums,na.approx(indicators[,2:ncol(indicators)]))
#write.csv(indicators_final,"Building_Indicators.csv")

library(xlsx)
skryf <- function(data,naam) {
    data[,-1:-2] <- round(data[,-1:-2],2)
    data[,2] <- as.character(data$Datum)
    write.xlsx2(data, "Building_Indicators.xlsx", sheetName = naam, 
                col.names=TRUE, row.names=FALSE, append=TRUE, showNA=FALSE)
}

skryf(Building,"Building")
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




#==============================
#Old function
bou <- function(BER.B=BER.B, datums=datums, laat="uit", alt=TRUE) {
    
    # replace 1,2,3 (Up, Same, Down) responses with 1,0,-1
    for(i in 7:16) {
        BER.B[,i] <- replace(BER.B[,i], BER.B[,i]==2, 0)
        BER.B[,i] <- replace(BER.B[,i], BER.B[,i]==3,-1)
    }
    
    if(alt==FALSE) {
        BER.B$Q6 <- replace(BER.B$Q6, BER.B$Q6==-1,0) # replace -1 (Less keen) responses with 0
        
        for(i in 17:20) {  #weighted
            BER.B[,i] <- replace(BER.B[,i], BER.B[,i]==1, 0.67)  
            BER.B[,i] <- replace(BER.B[,i], BER.B[,i]==2, 0.33)
            BER.B[,i] <- replace(BER.B[,i], BER.B[,i]==3, 0)
        }
    }
    
    if(alt==TRUE) {
        for(i in 17:20) {
            BER.B[,i] <- replace(BER.B[,i], BER.B[,i]==1, 1)
            BER.B[,i] <- replace(BER.B[,i], BER.B[,i]==2, 0.5)
            BER.B[,i] <- replace(BER.B[,i], BER.B[,i]==3, 0)
        }
    }
    if(laat=="uit") { BER.B <- BER.B[BER.B$Latecomer == FALSE | is.na(BER.B$Latecomer),]}
    
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
    
    
    building <- cbind(res[,2:16],nonres[,2:16],total[,2:16],
                      con_res[,2:16],con_nonres[,2:16],con_tot[,2:16],
                      subcon_res[,2:16],subcon_nonres[,2:16],subcon_tot[,2:16],
                      wc[,2:16],kzn[,2:16],gp[,2:16], deparse.level = 1)
    building <- building * 100
    building <- cbind(Date=res$res.Group.1,building)
    
    building <- merge(datums,building, by.x="Date", by.y="Date", all=TRUE)[,-2]
    building$Date <- datums$Datum
    return(building)
}    


indicator_plot <- indicators[,c("Date","con_res.Q1","con_nonres.Q1","con_tot.Q1")]
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

indicator_plot <- indicators[,c("Date","subcon_res.Q1","subcon_nonres.Q1","subcon_tot.Q1")]
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

indicator_plot <- indicators[,c("Date","wc.Q1","kzn.Q1","gp.Q1")]
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
g <- g + scale_fill_discrete(name="Region")
g <- g + scale_y_continuous(labels=comma)
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g <- g + ylab("Number of Respondents")
g <- g + xlab("Date")
g

##=============================

indicators_late <- bou(BER.B, datums, "in", FALSE)

indicator_plot <- cbind(indicators[,c("Date","total.Q1")],indicators_late[,"total.Q1"])
colnames(indicator_plot) <- c("Date","total (excl. latecomers)","total (incl. latecomers)")
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


indicator_plot <- cbind(indicators[,c("Date","res.Q1")],indicators_late[,"res.Q1"])
colnames(indicator_plot) <- c("Date","res (excl. latecomers)","res (incl. latecomers)")
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

indicator_plot <- cbind(indicators[,c("Date","nonres.Q1")],indicators_late[,"nonres.Q1"])
colnames(indicator_plot) <- c("Date","nonres (excl. latecomers)","nonres (incl. latecomers)")
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


#------------------------------
#Interoplasie (net linear vs use published value)

indicator_plot <- cbind(indicators[,c("Date","con_tot.Q1")],pub[,c("con_totalQ1")])
indicator_plot[,4] <- na.approx(indicator_plot[,2])
indicator_plot[is.na(indicator_plot[,2]),2] <- indicator_plot[is.na(indicator_plot[,2]),3]
indicator_plot <- indicator_plot[,-3]
colnames(indicator_plot) <- c("Date","Microdata: Substituted","Microdata: Interpolated")
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


indicator_plot <- cbind(indicators[,c("Date","subcon_tot.Q2A")],pub[,c("subcon_totalQ2A")])
indicator_plot[,4] <- na.approx(indicator_plot[,2])
indicator_plot[is.na(indicator_plot[,2]),2] <- indicator_plot[is.na(indicator_plot[,2]),3]
indicator_plot <- indicator_plot[,-3]
colnames(indicator_plot) <- c("Date","Microdata: Substituted","Microdata: Interpolated")
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


indicator_plot <- cbind(Residential[,c("Datum","Q3A")],ref[,c("GFCF.Residential","Residential.building","Res..excl..h.c.a.","Res..excl..h.c.","Res..excl..a.")])
indicator_plot[,2:7] <- scale(indicator_plot[,2:7])
colnames(indicator_plot) <- c("Date","res.Q3A","GFCF.Residential","Residential.building","Res.excl.h.c.a.","Res.excl.h.c.","Res.excl.a.")
corstarsl(indicator_plot[,-1])


indicator_plot <- cbind(Non_residential[,c("Datum","Q3A")],ref[,c("GFCF.Non.residential","Non.residential.building","Non.res..excl..h.c.a.","Non.res..excl..h.c.","Non.res..excl..a.")])
indicator_plot[,2:7] <- scale(indicator_plot[,2:7])
colnames(indicator_plot) <- c("Date","non_res.Q3A","GFCF.Non-residential","Non.residential.building","Non.res.excl.h.c.a.","Non.res.excl.h.c.","Non.res.excl.a.")
corstarsl(indicator_plot[,-1])

indicator_plot <- cbind(Building[,c("Datum","Q3A")],ref[,c("GFCF.Res_Non.res","GFCF.Total","Total.building","Total..excl..h.c.a.","Total..excl..h.c.","Total..incl..all.")])
indicator_plot[,2:8] <- scale(indicator_plot[,2:8])
colnames(indicator_plot) <- c("Date","total.Q3A","GFCF.Res & Non.res","GFCF.Total","Total.building","Total.excl.h.c.a.","Total.excl.h.c.","Total..incl.all")
corstarsl(indicator_plot[,-1])


indicator_plot <- cbind(WC[,c("Datum","Q3A")],KZN[,"Q3A"],GP[,"Q3A"],ref[,c("Building_WC","Building_KN","Building_GP")])
indicator_plot[,2:7] <- scale(indicator_plot[,2:7])
colnames(indicator_plot) <- c("Date","wc.Q3A","kzn.Q3A","gp.Q3A","Building_WC","Building_KN","Building_GP")
corstarsl(indicator_plot[,-1])


indicator_plot <- cbind(Contractor[,c("Datum","Q9")],pub[,"con_totalQ9"])
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
g1

indicator_plot <- cbind(Building[,c("Datum","Q9")],Building_w[,"Q9"])
indicator_plot$Datum <- as.Date(indicator_plot$Datum)
colnames(indicator_plot) <- c("Date","Unweighted","Weighted")
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
g1




