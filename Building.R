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

datums <- read.csv("dates.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)
datums$Datum <- as.Date(datums$Datum, format = "%Y/%m/%d")

#library(xlsx)
#write.xlsx(BER.B, "check.xlsx")

##=============================
BER.B <- read.csv("Building_full.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)

colnames(BER.B)[1:6] <- c("region","id","sector","weight","factor","surveyQ")
BER.B$surveyQ <- toupper(BER.B$surveyQ)
BER.B$sector <- factor(BER.B$sector) #could include labels
BER.B$id <- factor(BER.B$id)
BER.B$surveyQ <- factor(BER.B$surveyQ)

##=============================
#Clean regions

foute <- c("1993Q2","1993Q3","1994Q1","1994Q2","1994Q3","1994Q4")
BER.B$region[(BER.B$surveyQ %in% foute) & (BER.B$region == 2)] <- 3.1
BER.B$region[(BER.B$surveyQ %in% foute) & (BER.B$region == 3)] <- 4.1
BER.B$region[(BER.B$surveyQ %in% foute) & (BER.B$region == 4)] <- 2.1
BER.B$region[(BER.B$surveyQ %in% foute) & (BER.B$region == 6)] <- 8.1
BER.B$region[(BER.B$surveyQ %in% foute) & (BER.B$region == 7)] <- 9.1
BER.B$region[(BER.B$surveyQ %in% foute) & (BER.B$region == 8)] <- 6.1
BER.B$region[(BER.B$surveyQ %in% foute) & (BER.B$region == 9)] <- 6.1
BER.B$region[BER.B$surveyQ %in% foute] <- round(BER.B$region[BER.B$surveyQ %in% foute])

Mode <- function(x) {
    ux <- na.remove(unique(x))
    ux[which.max(tabulate(match(x, ux)))]
}

fout <- c("1995Q1")
for(i in levels(BER.B$id)) {
    BER.B$region[(BER.B$surveyQ %in% fout) & (BER.B$id == i)] <- Mode(BER.B$region[(BER.B$id == i)]) 
    
}
BER.B$region <- factor(BER.B$region)

#==============================

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

##=============================

indicators <- bou(BER.B, datums, "uit", TRUE)
    

indicator_plot <- indicators[,c("Date","res.Q1","nonres.Q1","total.Q1")]
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_line()
g <- g + ylab("Indicator") + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) 
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g <- g + theme(legend.position="bottom")
g


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

##=============================

pub <- read.csv("Building_BER_Published.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)

indicator_plot <- cbind(indicators[,c("Date","con_tot.Q1")],pub[,c("con_totalQ1")])
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


indicator_plot <- cbind(indicators[,c("Date","con_tot.Q2A")],pub[,c("con_totalQ2A")])
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



##=============================
#Compare to reference series
ref <- read.csv("Ref Series.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)

source("corstarsl.R")

indicator_plot <- cbind(indicators[,c("Date","res.Q3A")],ref[,c("GFCF.Residential","Residential.building","Res..excl..h.c.a.","Res..excl..h.c.","Res..excl..a.")])
indicator_plot[,2:7] <- scale(indicator_plot[,2:7])
colnames(indicator_plot) <- c("Date","res.Q3A","GFCF.Residential","Residential.building","Res.excl.h.c.a.","Res.excl.h.c.","Res.excl.a.")
corstarsl(indicator_plot[,-1])

plot <- melt(indicator_plot[,c(1:4)], id="Date")  # convert to long format
g <- ggplot(data=plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_line()
g <- g + ylab("Indicator")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) 
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g <- g + theme(legend.position="bottom")
g

plot <- melt(indicator_plot[,c(1,2,5,6,7)], id="Date")  # convert to long format
g <- ggplot(data=plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_line()
g <- g + ylab("Indicator")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) 
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g <- g + theme(legend.position="bottom")
g


indicator_plot <- cbind(indicators[,c("Date","nonres.Q3A")],ref[,c("GFCF.Non.residential","Non.residential.building","Non.res..excl..h.c.a.","Non.res..excl..h.c.","Non.res..excl..a.")])
indicator_plot[,2:7] <- scale(indicator_plot[,2:7])
colnames(indicator_plot) <- c("Date","non_res.Q3A","GFCF.Non-residential","Non.residential.building","Non.res.excl.h.c.a.","Non.res.excl.h.c.","Non.res.excl.a.")
corstarsl(indicator_plot[,-1])

plot <- melt(indicator_plot[,c(1:4)], id="Date")  # convert to long format
g <- ggplot(data=plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_line()
g <- g + ylab("Indicator")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) 
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g <- g + theme(legend.position="bottom")
g

plot <- melt(indicator_plot[,c(1,2,5,6,7)], id="Date")  # convert to long format
g <- ggplot(data=plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_line()
g <- g + ylab("Indicator")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) 
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g <- g + theme(legend.position="bottom")
g




indicator_plot <- cbind(indicators[,c("Date","total.Q3A")],ref[,c("GFCF.Res_Non.res","GFCF.Total","Total.building","Total..excl..h.c.a.","Total..excl..h.c.","Total..incl..all.")])
indicator_plot[,2:8] <- scale(indicator_plot[,2:8])
colnames(indicator_plot) <- c("Date","total.Q3A","GFCF.Res & Non.res","GFCF.Total","Total.building","Total.excl.h.c.a.","Total.excl.h.c.","Total..incl.all")
corstarsl(indicator_plot[,-1])

plot <- melt(indicator_plot[,c(1:5)], id="Date")  # convert to long format
g <- ggplot(data=plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_line()
g <- g + ylab("Indicator")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) 
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g <- g + theme(legend.position="bottom")
g

plot <- melt(indicator_plot[,c(1,2,6,7,8)], id="Date")  # convert to long format
g <- ggplot(data=plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_line()
g <- g + ylab("Indicator")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) 
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g <- g + theme(legend.position="bottom")
g



indicator_plot <- cbind(indicators[,c("Date","wc.Q3A","kzn.Q3A","gp.Q3A")],ref[,c("Building_WC","Building_KN","Building_GP")])
indicator_plot[,2:7] <- scale(indicator_plot[,2:7])
colnames(indicator_plot) <- c("Date","wc.Q3A","kzn.Q3A","gp.Q3A","Building_WC","Building_KN","Building_GP")
corstarsl(indicator_plot[,-1])

plot <- melt(indicator_plot, id="Date")  # convert to long format
g <- ggplot(data=plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_line()
g <- g + ylab("Indicator")
g <- g + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) 
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g <- g + theme(legend.position="bottom")
g


###-------------------
#final interpolated series

indicators_final <- cbind(datums,na.approx(indicators[,2:ncol(indicators)]))

write.csv(indicators_final,"Building_Indicators.csv")



