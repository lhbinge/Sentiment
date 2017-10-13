##================================================================================##
## ----------------------- MANUFACTURING -----------------------------------------##
##================================================================================##
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

datums <- read.csv("dates3.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)
datums$Datum <- as.Date(datums$Datum, format = "%Y/%m/%d")

pub <- read.csv("Manufacturing_BER_Published.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)
ref <- read.csv("Ref Series_2.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)

#library(xlsx)
#write.xlsx(BER.M, "check.xlsx")

##=============================
#Correcting the region kodes
#BER.M <- read.csv("Manufacturing_full.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)

#colnames(BER.M)[1:7] <- c("region","id","sector","weight","turnover","factor","surveyQ")
#BER.M$surveyQ <- toupper(BER.M$surveyQ)
#BER.M$sector <- factor(BER.M$sector) #could include labels
#BER.M$id <- factor(BER.M$id)
#BER.M$surveyQ <- factor(BER.M$surveyQ)

##=============================
#Clean regions
#Mode <- function(x) {
#    ux <- na.remove(unique(x))
#    ux[which.max(tabulate(match(x, ux)))]
#}

#foute <- c("2000Q2","2000Q3","2000Q4","2001Q1")
#korrek <- c("1999Q1","1999Q2","1999Q3","1999Q4","2001Q2","2001Q3","2001Q4",
#            "2002Q1","2002Q2","2002Q3","2002Q4","2003Q1")

#for(i in levels(BER.M$id)) {
#    if(!is.na(Mode(BER.M$region[(BER.M$surveyQ %in% korrek) & (BER.M$id == i)]))) {
#        BER.M$region[(BER.M$surveyQ %in% foute) & (BER.M$id == i)] <- Mode(BER.M$region[(BER.M$surveyQ %in% korrek) & (BER.M$id == i)]) 
#    }
#}
#BER.M$region <- factor(BER.M$region)

#write.csv(man, "check2.csv")


##=============================
#Correcting the faktor variable 
BER.M <- read.csv("Manufacturing_current faktor.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)

colnames(BER.M)[1:7] <- c("region","id","sector","weight","turnover","factor","surveyQ")
BER.M$surveyQ <- toupper(BER.M$surveyQ)
BER.M$sector <- factor(BER.M$sector) #could include labels
BER.M$id <- factor(BER.M$id)
BER.M$surveyQ <- factor(BER.M$surveyQ)

##=============================
#Clean faktor
Mode <- function(x) {
    ux <- na.remove(unique(x))
    ux[which.max(tabulate(match(x, ux)))]
}

foute <- c("2000Q2","2000Q3","2000Q4","2001Q1")
korrek <- c("1999Q1","1999Q2","1999Q3","1999Q4",
            "2001Q2","2001Q3","2001Q4","2002Q1","2002Q2","2002Q3","2002Q4",
            "2003Q1","2003Q2","2003Q3","2003Q4")#,"2004Q1","2004Q2","2004Q3","2004Q4",
            #"2005Q1","2005Q2","2005Q3","2006Q1","2006Q2","2006Q3","2006Q4")

for(i in levels(BER.M$id)) {
    if(!is.na(Mode(BER.M$factor[(BER.M$surveyQ %in% korrek) & (BER.M$id == i)]))) {
        BER.M$factor[(BER.M$surveyQ %in% foute) & (BER.M$id == i)] <- Mode(BER.M$factor[(BER.M$surveyQ %in% korrek) & (BER.M$id == i)]) 
    }
}

#write.csv(BER.M, "check2.csv")

##=============================
#BER.M <- read.csv("Manufacturing_corrected.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)

#colnames(BER.M)[1:7] <- c("region","id","sector","weight","turnover","factor","surveyQ")
#BER.M$surveyQ <- toupper(BER.M$surveyQ)
#BER.M$sector <- factor(BER.M$sector) #could include labels
#BER.M$id <- factor(BER.M$id)
#BER.M$surveyQ <- factor(BER.M$surveyQ)

#Exlcude Latecomers and old questions
BER.M <- BER.M[BER.M$Latecomer == FALSE | is.na(BER.M$Latecomer),]
BER.M <- BER.M[,!grepl("X",colnames(BER.M))]    
BER.M <- BER.M[,1:64]
#==============================
# replace 1,2,3 (Up, Same, Down) responses with 1,0,-1
for(i in 8:64) {
    BER.M[,i] <- replace(BER.M[,i], BER.M[,i]==2, 0)
    BER.M[,i] <- replace(BER.M[,i], BER.M[,i]==3,-1)
}

for(i in 43:49) {
    BER.M[,i] <- replace(BER.M[,i], BER.M[,i]==0, 0.5)
    BER.M[,i] <- replace(BER.M[,i], BER.M[,i]==-1, 0)
}

##======================##
## CALCULATE INDICATORS ##
##======================##
food <- c(1010, 1011,1013,1019,1020,1021)
textiles <- c(1120,1040,1049,1042,1060,1070)
wood <- c(1109,1110,1080,1081)
chemicals <- c(1130,1140,1149,1219)
glass <- c(1153,1159)
metals <- c(1160,1161,1170,1179,1181,1182,1189)
electrical <- c(1190,1191,1199,1192,1194)
transport <- c(1200,1201,1209)
furniture <- c(1090,1099)
all <- as.numeric(as.character(unique(BER.M$sector)))


consumer <- c(1010,1011,1019,1049,1060,1070,1090,1099,1110,1120,1149,1189,1192,1020,1021)
intermediate <- c(1013,1040,1042,1080,1081,1109,1130,1140,1153,1159,1160,1161,
                  1179,1191,1199,1219)
capital <- c(1170,1181,1182,1189,1190,1194,1200,1201,1209)
             
streke <- unique(BER.M$region)


ongeweeg <- function(sektor=all,streek=streke) {
    man <- BER.M[BER.M$sector %in% sektor & BER.M$region %in% streek,]
    sector <- aggregate(man[,(match("surveyQ",colnames(man))+1):ncol(man)], by=list(man$surveyQ), FUN=mean, na.rm=TRUE)
    sector <- merge(datums,sector, by.x="Date", by.y="Group.1", all=TRUE)
    sector[,-1:-2] <- na.approx(sector[,-1:-2]*100)
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
    
    man <- BER.M[BER.M$sector %in% sektor & BER.M$region %in% streek,]
    sector <- data.frame()
    for(kwartaal in levels(man$surveyQ)) {
        sector <- rbind(sector,weeg(man[man$surveyQ==kwartaal,]))
    }
    sector <- sector *100
    sector[,1] <- levels(man$surveyQ)
    colnames(sector) <- colnames(man)[-1:-6]
    sector <- merge(datums,sector, by.x="Date", by.y="surveyQ", all=TRUE)
    #sector <- as.data.frame(t(sapply(levels(man$surveyQ), function(kwartaal) weeg(man[man$surveyQ==kwartaal,]))))
    sector[,-1:-2] <- na.approx(sector[,-1:-2])
    return(sector)
}

Manufacturing <- geweeg(all)
#Interpolasie:
Manufacturing[24,c(seq(3,31,by=2),33:48)] <- pub[24,2:32]
Manufacturing[33,c(seq(3,31,by=2),33:48)] <- pub[33,2:32]
Manufacturing[56,c(seq(3,31,by=2),33:48)] <- pub[56,2:32]

Food <- geweeg(food)
Textiles <- geweeg(textiles)
Wood <- geweeg(wood)
Chemicals <- geweeg(chemicals)
Glass <- geweeg(glass)
Metals <- geweeg(metals)
Electrical <- geweeg(electrical)
Radio <- geweeg(radio)
Elec_radio <- geweeg(c(electrical,radio))
Transport <- geweeg(transport)
Furniture <- geweeg(furniture)

Consumer <- geweeg(consumer)
Intermediate <- geweeg(intermediate)
Capital <- geweeg(capital)

WC <- geweeg(all,1)
KZN <- geweeg(all,5)
GP <- geweeg(all,6)

#----------------------------------
#Unweighted
Manufacturing_u <- ongeweeg(all)
Manufacturing_u[24,c(seq(3,31,by=2),33:48)] <- pub[24,2:32]
Manufacturing_u[33,c(seq(3,31,by=2),33:48)] <- pub[33,2:32]
Manufacturing_u[56,c(seq(3,31,by=2),33:48)] <- pub[56,2:32]

Food_u <- ongeweeg(food)
Textiles_u <- ongeweeg(textiles)
Wood_u <- ongeweeg(wood)
Chemicals_u <- ongeweeg(chemicals)
Glass_u <- ongeweeg(glass)
Metals_u <- ongeweeg(metals)
Electrical_u <- ongeweeg(electrical)
Radio_u <- ongeweeg(radio)
Elec_radio_u <- ongeweeg(c(electrical,radio))
Transport_u <- ongeweeg(transport)
Furniture_u <- ongeweeg(furniture)

Consumer_u <- ongeweeg(consumer)
Intermediate_u <- ongeweeg(intermediate)
Capital_u <- ongeweeg(capital)

WC_u <- ongeweeg(all,1)
KZN_u <- ongeweeg(all,5)
GP_u <- ongeweeg(all,6)

##=============================
indicator_plot <- cbind(Manufacturing[,c("Datum","Q20")],Manufacturing_u[,c("Q20")],
                        Manufacturing_n[,c("Q20")])
colnames(indicator_plot) <- c("Date","Weighted","Unweighted","Weighted_new")    
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_line()
g <- g + ylab("Indicator") + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) 
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g <- g + theme(legend.position="bottom")
g

indicator_plot <- cbind(Manufacturing[,c("Datum","Q20")],pub[,c("Total_Q20")])
colnames(indicator_plot) <- c("Date","Q4:Microdata","Q4:Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_line()
g <- g + ylab("Indicator") + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) 
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g <- g + theme(legend.position="bottom")
g


#Published series
indicator_plot <- cbind(Manufacturing[,c("Datum","Q1A")],pub[,c("Total_Q1A")])
colnames(indicator_plot) <- c("Date","Microdata","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g1 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g1 <- g1 + geom_line()
g1 <- g1 + ylab("Indicator") + xlab("")
g1 <- g1 + theme(legend.title=element_blank())
g1 <- g1 + ggtitle("Q1A") 
g1 <- g1 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g1 <- g1 + theme(legend.title=element_blank()) 
g1 <- g1 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g1 <- g1 + theme(legend.position="none")

indicator_plot <- cbind(Manufacturing[,c("Datum","Q2A")],pub[,c("Total_Q2A")])
colnames(indicator_plot) <- c("Date","Microdata","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g2 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g2 <- g2 + geom_line()
g2 <- g2 + ylab("") + xlab("")
g2 <- g2 + theme(legend.title=element_blank())
g2 <- g2 + ggtitle("Q2A") 
g2 <- g2 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g2 <- g2 + theme(legend.title=element_blank()) 
g2 <- g2 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g2 <- g2 + theme(legend.position="none")

indicator_plot <- cbind(Manufacturing[,c("Datum","Q3A")],pub[,c("Total_Q3A")])
colnames(indicator_plot) <- c("Date","Microdata","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g3 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g3 <- g3 + geom_line()
g3 <- g3 + ylab("Indicator") + xlab("")
g3 <- g3 + theme(legend.title=element_blank())
g3 <- g3 + ggtitle("Q3A") 
g3 <- g3 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g3 <- g3 + theme(legend.title=element_blank()) 
g3 <- g3 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g3 <- g3 + theme(legend.position="bottom",plot.margin=unit(c(-0.5,0.4,0,0.4), "cm"))

indicator_plot <- cbind(Manufacturing[,c("Datum","Q6A")],pub[,c("Total_Q6A")])
colnames(indicator_plot) <- c("Date","Microdata","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g4 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g4 <- g4 + geom_line()
g4 <- g4 + ylab("") + xlab("")
g4 <- g4 + theme(legend.title=element_blank())
g4 <- g4 + ggtitle("Q6A") 
g4 <- g4 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g4 <- g4 + theme(legend.title=element_blank()) 
g4 <- g4 + theme(legend.position="bottom",plot.margin=unit(c(-0.5,0.4,0,0.4), "cm"))
g4 <- g4 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

library(gridExtra)
grid.arrange(g1, g2, g3, g4, ncol=2, nrow =2)


#Published series (constraints)
indicator_plot <- cbind(Manufacturing[,c("Datum","Q21")],pub[,c("Total_Q21")])
colnames(indicator_plot) <- c("Date","Microdata","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g1 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g1 <- g1 + geom_line()
g1 <- g1 + ylab("Indicator") + xlab("")
g1 <- g1 + theme(legend.title=element_blank())
g1 <- g1 + ggtitle("Q21") 
g1 <- g1 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g1 <- g1 + theme(legend.title=element_blank()) 
g1 <- g1 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g1 <- g1 + theme(legend.position="none")

indicator_plot <- cbind(Manufacturing[,c("Datum","Q22")],pub[,c("Total_Q22")])
colnames(indicator_plot) <- c("Date","Microdata","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g2 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g2 <- g2 + geom_line()
g2 <- g2 + ylab("") + xlab("")
g2 <- g2 + theme(legend.title=element_blank())
g2 <- g2 + ggtitle("Q22") 
g2 <- g2 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g2 <- g2 + theme(legend.title=element_blank()) 
g2 <- g2 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g2 <- g2 + theme(legend.position="none")

indicator_plot <- cbind(Manufacturing[,c("Datum","Q25")],pub[,c("Total_Q25")])
colnames(indicator_plot) <- c("Date","Microdata","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g3 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g3 <- g3 + geom_line()
g3 <- g3 + ylab("Indicator") + xlab("")
g3 <- g3 + theme(legend.title=element_blank())
g3 <- g3 + ggtitle("Q25") 
g3 <- g3 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g3 <- g3 + theme(legend.title=element_blank()) 
g3 <- g3 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g3 <- g3 + theme(legend.position="bottom",plot.margin=unit(c(-0.5,0.4,0,0.4), "cm"))

indicator_plot <- cbind(Manufacturing[,c("Datum","Q27")],pub[,c("Total_Q27")])
colnames(indicator_plot) <- c("Date","Microdata","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g4 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g4 <- g4 + geom_line()
g4 <- g4 + ylab("") + xlab("")
g4 <- g4 + theme(legend.title=element_blank())
g4 <- g4 + ggtitle("Q27") 
g4 <- g4 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g4 <- g4 + theme(legend.title=element_blank()) 
g4 <- g4 + theme(legend.position="bottom",plot.margin=unit(c(-0.5,0.4,0,0.4), "cm"))
g4 <- g4 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

library(gridExtra)
grid.arrange(g1, g2, g3, g4, ncol=2, nrow =2)



##=============================
#New faktor variable 
BER.M <- read.csv("Manufacturing_new faktor.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)
#BER.M$Faktor <- BER.M$Firmw * BER.M$Sectorw

colnames(BER.M)[1:9] <- c("region","id","sector","weight","turnover","firmw","sectorw","factor","surveyQ")
BER.M$surveyQ <- toupper(BER.M$surveyQ)
BER.M$sector <- factor(BER.M$sector) #could include labels
BER.M$id <- factor(BER.M$id)
BER.M$surveyQ <- factor(BER.M$surveyQ)

#Exlcude Latecomers and old questions
BER.M <- BER.M[BER.M$Latecomer == FALSE | is.na(BER.M$Latecomer),]
BER.M <- BER.M[,!grepl("X",colnames(BER.M))]    
BER.M <- BER.M[,1:66]
#==============================
# replace 1,2,3 (Up, Same, Down) responses with 1,0,-1
for(i in 10:66) {
    BER.M[,i] <- replace(BER.M[,i], BER.M[,i]==2, 0)
    BER.M[,i] <- replace(BER.M[,i], BER.M[,i]==3,-1)
}

for(i in 45:51) {
    BER.M[,i] <- replace(BER.M[,i], BER.M[,i]==0, 0.5)
    BER.M[,i] <- replace(BER.M[,i], BER.M[,i]==-1, 0)
}


BER.M$Sector[BER.M$sector %in% food] <- "Food" 
BER.M$Sector[BER.M$sector %in% textiles] <- "Textiles" 
BER.M$Sector[BER.M$sector %in% wood] <- "Wood" 
BER.M$Sector[BER.M$sector %in% chemicals] <- "Chemicals" 
BER.M$Sector[BER.M$sector %in% glass] <- "Glass" 
BER.M$Sector[BER.M$sector %in% metals] <- "Metals" 
BER.M$Sector[BER.M$sector %in% electrical] <- "Eelectrical" 
BER.M$Sector[BER.M$sector %in% transport] <- "Transport" 
BER.M$Sector[BER.M$sector %in% furniture] <- "Furniture" 


#BER.M$sectorw <- BER.M$factor/BER.M$weight
BERplot <- aggregate(BER.M$sectorw, by=list(BER.M$surveyQ,BER.M$sector,BER.M$Sector), FUN = mean, na.rm=TRUE)
colnames(BERplot) <- c("surveyQ","sector","Sector","weight")
BERplot1 <- dcast(BERplot, formula = surveyQ ~ Sector + sector)
BERplot1 <- merge(datums,BERplot1,by.x="Date",by.y ="surveyQ")
BERplot1[,-1:-2] <- na.locf(BERplot1[,-1:-2],fromLast = TRUE)
BERplot1[,-1:-2] <- na.locf(BERplot1[,-1:-2])

BERplot <- BERplot1
BERplot$Food <- rowMeans(BERplot1[,12:17])
BERplot$Textiles <- rowMeans(BERplot1[,29:34])
BERplot$Wood <- rowMeans(BERplot1[,38:41])
BERplot$Chemicals <- rowMeans(BERplot1[,3:6])
BERplot$Glass <- rowMeans(BERplot1[,20:21])
BERplot$Metals <- rowMeans(BERplot1[,22:28])
BERplot$Eelectrical <- rowMeans(BERplot1[,7:11])
BERplot$Transport <- rowMeans(BERplot1[,35:37])
BERplot$Furniture <- rowMeans(BERplot1[,18:19])
BERplot <- BERplot[,c(1,42:50)]
BERplot$Total <- rowSums(BERplot[,2:10])
BERplot[,-1] <- BERplot[,-1]/BERplot[,11]


BERplot$Date <- as.Date(as.yearqtr(BERplot$Date, format = "%YQ%q"), frac = 1)
BERplot <- BERplot[,c(1,5,8,2,10,6,7,3,9,4,11)]
index_plot <- melt(BERplot[,-11], id="Date")
g <- ggplot(index_plot, aes(x=Date,y=value,group=variable,fill=variable)) 
g <- g + geom_bar(stat="identity")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + scale_fill_discrete(name="Sub-sector")
g <- g + scale_y_continuous(labels=comma)
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g <- g + ylab("Weights") + xlab("Date")
g <- g + guides(fill = guide_legend(reverse = TRUE))
g


BERplot <- aggregate(BER.M$factor, by=list(BER.M$surveyQ,BER.M$Sector), FUN = sum, na.rm=TRUE)#[-1:-5,]
BERplot1 <- aggregate(BER.M$factor, by=list(BER.M$surveyQ), FUN = sum, na.rm=TRUE)
BERplot$y <- BERplot$x/BERplot1$x
#BERplot <- aggregate(BER.M$factor, by=list(BER.M$surveyQ,BER.M$Sector), FUN = mean)
BERplot$Group.1 <- as.Date(as.yearqtr(BERplot$Group.1, format = "%YQ%q"), frac = 1)
g <- ggplot(BERplot, aes(x=Group.1, y=y,fill=Group.2))
g <- g + geom_bar(stat="identity")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + scale_fill_discrete(name="Sub-sector")
g <- g + scale_y_continuous(labels=comma)
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g <- g + ylab("Weights") + xlab("Date")
g <- g + guides(fill = guide_legend(reverse = TRUE))
g


BER.M <- BER.M[,-6:-7]

Manufacturing_n <- geweeg(all)
Manufacturing_n[24,c(seq(3,31,by=2),33:48)] <- pub[24,2:32]
Manufacturing_n[33,c(seq(3,31,by=2),33:48)] <- pub[33,2:32]
Manufacturing_n[56,c(seq(3,31,by=2),33:48)] <- pub[56,2:32]

Food_n <- geweeg(food)
Textiles_n <- geweeg(textiles)
Wood_n <- geweeg(wood)
Chemicals_n <- geweeg(chemicals)
Glass_n <- geweeg(glass)
Metals_n <- geweeg(metals)
Electrical_n <- geweeg(electrical)
Radio_n <- geweeg(radio)
Elec_radio_n <- geweeg(c(electrical,radio))
Transport_n <- geweeg(transport)
Furniture_n <- geweeg(furniture)

Consumer_n <- geweeg(consumer)
Intermediate_n <- geweeg(intermediate)
Capital_n <- geweeg(capital)

WC_n <- geweeg(all,1)
KZN_n <- geweeg(all,5)
GP_n <- geweeg(all,6)


##=============================
#New 2-step weighting 
BER.M <- read.csv("Manufacturing_new faktor.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)

colnames(BER.M)[1:9] <- c("region","id","sector","weight","turnover","firmw","sectorw","factor","surveyQ")
BER.M$surveyQ <- toupper(BER.M$surveyQ)
BER.M$sector <- factor(BER.M$sector) #could include labels
BER.M$id <- factor(BER.M$id)
BER.M$surveyQ <- factor(BER.M$surveyQ)
BER.M$factor <- as.numeric(as.character(BER.M$factor))

#BER.M$factor <- BER.M$firmw * BER.M$sectorw

#Exlcude Latecomers and old questions
BER.M <- BER.M[BER.M$Latecomer == FALSE | is.na(BER.M$Latecomer),]
BER.M <- BER.M[,!grepl("X",colnames(BER.M))]    
BER.M <- BER.M[,1:66]
#==============================
#Maak factor reg
Ej <- aggregate(BER.M$firmw, by=list(BER.M$surveyQ,BER.M$sector), FUN=sum)
BER.M <-  merge(BER.M, Ej, by.x=c("surveyQ","sector"), by.y=c("Group.1","Group.2"),all = TRUE)
BER.M$factor <- BER.M$factor/BER.M$x
BER.M <- BER.M[,c(3,4,2,5:9,1,10:66)]
#==============================

# replace 1,2,3 (Up, Same, Down) responses with 1,0,-1
for(i in 10:66) {
    BER.M[,i] <- replace(BER.M[,i], BER.M[,i]==2, 0)
    BER.M[,i] <- replace(BER.M[,i], BER.M[,i]==3,-1)
}

for(i in 45:51) {
    BER.M[,i] <- replace(BER.M[,i], BER.M[,i]==0, 0.5)
    BER.M[,i] <- replace(BER.M[,i], BER.M[,i]==-1, 0)
}

geweeg2 <- function(sektor=all,streek=streke) {
    
    weeg2 <- function(temp) {  #calculate weighted mean for each quarter for all columns
        sectorw=temp$sectorw[1]
        temp <- cbind(firmw=temp$firmw,temp$firmw*temp[(match("surveyQ",colnames(temp))+1):ncol(temp)])
        #temp <- colSums(temp, na.rm=TRUE, dims = 1)/sum(temp$factor, na.rm=TRUE)
        #calculate the sum(wi*xi)/sum(wi)
        temp <- colSums(temp, na.rm=TRUE, dims = 1)/    
            sapply(colnames(temp), function(x) sum(temp$firmw[!is.na(temp[colnames(temp) == x])]))
        temp <- c(sectorw,temp)
        #weight only by those that responded to a specific question
        return(temp)
    }
    
    man <- BER.M[BER.M$sector %in% sektor & BER.M$region %in% streek,]
    sector <- data.frame()
    for(kwartaal in levels(man$surveyQ)) {
        sector <- rbind(sector,weeg2(man[man$surveyQ==kwartaal,]))
    }
    
    sector <- sector *100
    sector[,2] <- levels(man$surveyQ)
    colnames(sector) <- colnames(man)[c(7,9:ncol(man))]
    sector <- merge(datums,sector, by.x="Date", by.y="surveyQ", all=TRUE)
    #sector <- as.data.frame(t(sapply(levels(man$surveyQ), function(kwartaal) weeg(man[man$surveyQ==kwartaal,]))))
    sector[,3:58] <- na.approx(sector[,3:58],na.rm = FALSE)
    return(sector)
    
}



maak.index <- function(sektor=all,streek=streke) {
    lys <- list()
    t <- 0
    for(k in sektor) {
        t <- t+1
        lys[[t]] <- geweeg2(k)
    }
    saam <- lys[[1]]
    for(j in 4:60) {
        for(i in 1:100) {
            x <- NULL
            w <- NULL
            for(t in 1:length(sektor)) {
                x <- c(x,lys[[t]][i,j])
                w <- c(w,lys[[t]][i,3])
            }
            saam[i,j] <- weighted.mean(x,w,na.rm=TRUE) 
        }
    }
    saam <- saam[,-3]
    return(saam)
}



Manufacturing_2 <- maak.index(all)
Manufacturing_2[24,c(seq(3,31,by=2),33:48)] <- pub[24,2:32]
Manufacturing_2[33,c(seq(3,31,by=2),33:48)] <- pub[33,2:32]
Manufacturing_2[56,c(seq(3,31,by=2),33:48)] <- pub[56,2:32]

Food_2 <- maak.index(food)
Textiles_2 <- maak.index(textiles)
Wood_2 <- maak.index(wood)
Chemicals_2 <- maak.index(chemicals)
Glass_2 <- maak.index(glass)
Metals_2 <- maak.index(metals)
Electrical_2 <- maak.index(electrical)
Radio_2 <- maak.index(radio)
Elec_radio_2 <- maak.index(c(electrical,radio))
Transport_2 <- maak.index(transport)
Furniture_2 <- maak.index(furniture)

Consumer_2 <- maak.index(consumer)
Intermediate_2 <- maak.index(intermediate)
Capital_2 <- maak.index(capital)

WC_2 <- maak.index(all,1)
KZN_2 <- maak.index(all,5)
GP_2 <- maak.index(all,6)

BER.M <- BER.M[,-6:-7]
Manufacturing_n2 <- geweeg(all)

#---------------------------------------------------
#New faktor
indicator_plot <- cbind(Manufacturing[,c("Datum","Q20")],Manufacturing_n2[,c("Q20")],
                        Manufacturing_n[,c("Q20")],Manufacturing_2[,c("Q20")])
colnames(indicator_plot) <- c("Date","Q20:Microdata","Q20:New_2","Q20:New","Q20:2-step")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_line()
g <- g + ylab("Indicator") + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) 
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g <- g + theme(legend.position="bottom")
g



indicator_plot <- cbind(Manufacturing_n2[,c("Datum","Q20")],Manufacturing_2[,c("Q20")])
colnames(indicator_plot) <- c("Date","Q20:New_2","Q20:2-step")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_line()
g <- g + ylab("Indicator") + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) 
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g <- g + theme(legend.position="bottom")
g



indicator_plot <- cbind(Manufacturing[,c("Datum","Q1A")],
                        Manufacturing_n[,c("Q1A")],Manufacturing_2[,c("Q1A")])
colnames(indicator_plot) <- c("Date","Microdata","New","2-step")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g1 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g1 <- g1 + geom_line()
g1 <- g1 + ylab("Indicator") + xlab("")
g1 <- g1 + theme(legend.title=element_blank())
g1 <- g1 + ggtitle("Q1A") 
g1 <- g1 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g1 <- g1 + theme(legend.title=element_blank()) 
g1 <- g1 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g1 <- g1 + theme(legend.position="none")

indicator_plot <- cbind(Manufacturing[,c("Datum","Q2A")],
                        Manufacturing_n[,c("Q2A")],Manufacturing_2[,c("Q2A")])
colnames(indicator_plot) <- c("Date","Microdata","New","2-step")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g2 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g2 <- g2 + geom_line()
g2 <- g2 + ylab("") + xlab("")
g2 <- g2 + theme(legend.title=element_blank())
g2 <- g2 + ggtitle("Q2A") 
g2 <- g2 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g2 <- g2 + theme(legend.title=element_blank()) 
g2 <- g2 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g2 <- g2 + theme(legend.position="none")

indicator_plot <- cbind(Manufacturing[,c("Datum","Q3A")],
                        Manufacturing_n[,c("Q3A")],Manufacturing_2[,c("Q3A")])
colnames(indicator_plot) <- c("Date","Microdata","New","2-step")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g3 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g3 <- g3 + geom_line()
g3 <- g3 + ylab("Indicator") + xlab("")
g3 <- g3 + theme(legend.title=element_blank())
g3 <- g3 + ggtitle("Q3A") 
g3 <- g3 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g3 <- g3 + theme(legend.title=element_blank()) 
g3 <- g3 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g3 <- g3 + theme(legend.position="bottom",plot.margin=unit(c(-0.5,0.4,0,0.4), "cm"))

indicator_plot <- cbind(Manufacturing[,c("Datum","Q6A")],
                        Manufacturing_n[,c("Q6A")],Manufacturing_2[,c("Q6A")])
colnames(indicator_plot) <- c("Date","Microdata","New","2-step")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g4 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g4 <- g4 + geom_line()
g4 <- g4 + ylab("") + xlab("")
g4 <- g4 + theme(legend.title=element_blank())
g4 <- g4 + ggtitle("Q6A") 
g4 <- g4 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g4 <- g4 + theme(legend.title=element_blank()) 
g4 <- g4 + theme(legend.position="bottom",plot.margin=unit(c(-0.5,0.4,0,0.4), "cm"))
g4 <- g4 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

library(gridExtra)
grid.arrange(g1, g2, g3, g4, ncol=2, nrow =2)


indicator_plot <- cbind(Manufacturing[,c("Datum","Q21")],
                        Manufacturing_n[,c("Q21")],Manufacturing_2[,c("Q21")])
colnames(indicator_plot) <- c("Date","Microdata","New","2-step")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g1 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g1 <- g1 + geom_line()
g1 <- g1 + ylab("Indicator") + xlab("")
g1 <- g1 + theme(legend.title=element_blank())
g1 <- g1 + ggtitle("Q21") 
g1 <- g1 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g1 <- g1 + theme(legend.title=element_blank()) 
g1 <- g1 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g1 <- g1 + theme(legend.position="none")

indicator_plot <- cbind(Manufacturing[,c("Datum","Q22")],
                        Manufacturing_n[,c("Q22")],Manufacturing_2[,c("Q22")])
colnames(indicator_plot) <- c("Date","Microdata","New","2-step")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g2 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g2 <- g2 + geom_line()
g2 <- g2 + ylab("") + xlab("")
g2 <- g2 + theme(legend.title=element_blank())
g2 <- g2 + ggtitle("Q22") 
g2 <- g2 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g2 <- g2 + theme(legend.title=element_blank()) 
g2 <- g2 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g2 <- g2 + theme(legend.position="none")

indicator_plot <- cbind(Manufacturing[,c("Datum","Q25")],
                        Manufacturing_n[,c("Q25")],Manufacturing_2[,c("Q25")])
colnames(indicator_plot) <- c("Date","Microdata","New","2-step")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g3 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g3 <- g3 + geom_line()
g3 <- g3 + ylab("Indicator") + xlab("")
g3 <- g3 + theme(legend.title=element_blank())
g3 <- g3 + ggtitle("Q25") 
g3 <- g3 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g3 <- g3 + theme(legend.title=element_blank()) 
g3 <- g3 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g3 <- g3 + theme(legend.position="bottom",plot.margin=unit(c(-0.5,0.4,0,0.4), "cm"))

indicator_plot <- cbind(Manufacturing[,c("Datum","Q27")],
                        Manufacturing_n[,c("Q27")],Manufacturing_2[,c("Q27")])
colnames(indicator_plot) <- c("Date","Microdata","New","2-step")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g4 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g4 <- g4 + geom_line()
g4 <- g4 + ylab("") + xlab("")
g4 <- g4 + theme(legend.title=element_blank())
g4 <- g4 + ggtitle("Q27") 
g4 <- g4 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g4 <- g4 + theme(legend.title=element_blank()) 
g4 <- g4 + theme(legend.position="bottom",plot.margin=unit(c(-0.5,0.4,0,0.4), "cm"))
g4 <- g4 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

library(gridExtra)
grid.arrange(g1, g2, g3, g4, ncol=2, nrow =2)


#---------------------------------------------------
#New Sector Classifications
indicator_plot <- cbind(Food[,c("Datum","Q20")],
                        Food_n[,c("Q20")],Food_2[,c("Q20")])
colnames(indicator_plot) <- c("Date","Microdata","New","2-step")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g1 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g1 <- g1 + geom_line()
g1 <- g1 + ylab("Indicator") + xlab("")
g1 <- g1 + theme(legend.title=element_blank())
g1 <- g1 + ggtitle("Food") 
g1 <- g1 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g1 <- g1 + theme(legend.title=element_blank()) 
g1 <- g1 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g1 <- g1 + theme(legend.position="none")

indicator_plot <- cbind(Textiles[,c("Datum","Q20")],
                        Textiles_n[,c("Q20")],Textiles_2[,c("Q20")])
colnames(indicator_plot) <- c("Date","Microdata","New","2-step")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g2 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g2 <- g2 + geom_line()
g2 <- g2 + ylab("") + xlab("")
g2 <- g2 + theme(legend.title=element_blank())
g2 <- g2 + ggtitle("Textiles") 
g2 <- g2 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g2 <- g2 + theme(legend.title=element_blank()) 
g2 <- g2 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g2 <- g2 + theme(legend.position="none")

indicator_plot <- cbind(Wood[,c("Datum","Q20")],
                        Wood_n[,c("Q20")],Wood_2[,c("Q20")])
colnames(indicator_plot) <- c("Date","Microdata","New","2-step")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g3 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g3 <- g3 + geom_line()
g3 <- g3 + ylab("Indicator") + xlab("")
g3 <- g3 + theme(legend.title=element_blank())
g3 <- g3 + ggtitle("Wood") 
g3 <- g3 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g3 <- g3 + theme(legend.title=element_blank()) 
g3 <- g3 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g3 <- g3 + theme(legend.position="bottom",plot.margin=unit(c(-0.5,0.4,0,0.4), "cm"))

indicator_plot <- cbind(Chemicals[,c("Datum","Q20")],
                        Chemicals_n[,c("Q20")],Chemicals_2[,c("Q20")])
colnames(indicator_plot) <- c("Date","Microdata","New","2-step")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g4 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g4 <- g4 + geom_line()
g4 <- g4 + ylab("") + xlab("")
g4 <- g4 + theme(legend.title=element_blank())
g4 <- g4 + ggtitle("Chemicals") 
g4 <- g4 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g4 <- g4 + theme(legend.title=element_blank()) 
g4 <- g4 + theme(legend.position="bottom",plot.margin=unit(c(-0.5,0.4,0,0.4), "cm"))
g4 <- g4 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

library(gridExtra)
grid.arrange(g1, g2, g3, g4, ncol=2, nrow =2)


#New Sector Classifications
indicator_plot <- cbind(Glass[,c("Datum","Q20")],
                        Glass_n[,c("Q20")],Glass_2[,c("Q20")])
colnames(indicator_plot) <- c("Date","Microdata","New","2-step")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g1 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g1 <- g1 + geom_line()
g1 <- g1 + ylab("Indicator") + xlab("")
g1 <- g1 + theme(legend.title=element_blank())
g1 <- g1 + ggtitle("Glass") 
g1 <- g1 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g1 <- g1 + theme(legend.title=element_blank()) 
g1 <- g1 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g1 <- g1 + theme(legend.position="none")

indicator_plot <- cbind(Metals[,c("Datum","Q20")],
                        Metals_n[,c("Q20")],Metals_2[,c("Q20")])
colnames(indicator_plot) <- c("Date","Microdata","New","2-step")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g2 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g2 <- g2 + geom_line()
g2 <- g2 + ylab("") + xlab("")
g2 <- g2 + theme(legend.title=element_blank())
g2 <- g2 + ggtitle("Metals") 
g2 <- g2 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g2 <- g2 + theme(legend.title=element_blank()) 
g2 <- g2 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g2 <- g2 + theme(legend.position="none")

indicator_plot <- cbind(Elec_radio[,c("Datum","Q20")],
                        Elec_radio_n[,c("Q20")],Elec_radio_2[,c("Q20")])
colnames(indicator_plot) <- c("Date","Microdata","New","2-step")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g3 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g3 <- g3 + geom_line()
g3 <- g3 + ylab("Indicator") + xlab("")
g3 <- g3 + theme(legend.title=element_blank())
g3 <- g3 + ggtitle("Elec_radio") 
g3 <- g3 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g3 <- g3 + theme(legend.title=element_blank()) 
g3 <- g3 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g3 <- g3 + theme(legend.position="bottom",plot.margin=unit(c(-0.5,0.4,0,0.4), "cm"))

indicator_plot <- cbind(Transport[,c("Datum","Q20")],
                        Transport_n[,c("Q20")],Transport_2[,c("Q20")])
colnames(indicator_plot) <- c("Date","Microdata","New","2-step")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g4 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g4 <- g4 + geom_line()
g4 <- g4 + ylab("") + xlab("")
g4 <- g4 + theme(legend.title=element_blank())
g4 <- g4 + ggtitle("Transport") 
g4 <- g4 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g4 <- g4 + theme(legend.title=element_blank()) 
g4 <- g4 + theme(legend.position="bottom",plot.margin=unit(c(-0.5,0.4,0,0.4), "cm"))
g4 <- g4 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

library(gridExtra)
grid.arrange(g1, g2, g3, g4, ncol=2, nrow =2)


#New Sector Classifications
indicator_plot <- cbind(Furniture[,c("Datum","Q20")],
                        Furniture_n[,c("Q20")],Furniture_2[,c("Q20")])
colnames(indicator_plot) <- c("Date","Microdata","New","2-step")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g1 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g1 <- g1 + geom_line()
g1 <- g1 + ylab("Indicator") + xlab("")
g1 <- g1 + theme(legend.title=element_blank())
g1 <- g1 + ggtitle("Furniture") 
g1 <- g1 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g1 <- g1 + theme(legend.title=element_blank()) 
g1 <- g1 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g1 <- g1 + theme(legend.position="none")

indicator_plot <- cbind(Consumer[,c("Datum","Q20")],
                        Consumer_n[,c("Q20")],Consumer_2[,c("Q20")])
colnames(indicator_plot) <- c("Date","Microdata","New","2-step")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g2 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g2 <- g2 + geom_line()
g2 <- g2 + ylab("") + xlab("")
g2 <- g2 + theme(legend.title=element_blank())
g2 <- g2 + ggtitle("Consumer Goods") 
g2 <- g2 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g2 <- g2 + theme(legend.title=element_blank()) 
g2 <- g2 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g2 <- g2 + theme(legend.position="none")

indicator_plot <- cbind(Intermediate[,c("Datum","Q20")],
                        Intermediate_n[,c("Q20")],Intermediate_2[,c("Q20")])
colnames(indicator_plot) <- c("Date","Microdata","New","2-step")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g3 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g3 <- g3 + geom_line()
g3 <- g3 + ylab("Indicator") + xlab("")
g3 <- g3 + theme(legend.title=element_blank())
g3 <- g3 + ggtitle("Intermediate Goods") 
g3 <- g3 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g3 <- g3 + theme(legend.title=element_blank()) 
g3 <- g3 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g3 <- g3 + theme(legend.position="bottom",plot.margin=unit(c(-0.5,0.4,0,0.4), "cm"))

indicator_plot <- cbind(Capital[,c("Datum","Q20")],
                        Capital_n[,c("Q20")],Capital_2[,c("Q20")])
colnames(indicator_plot) <- c("Date","Microdata","New","2-step")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g4 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g4 <- g4 + geom_line()
g4 <- g4 + ylab("") + xlab("")
g4 <- g4 + theme(legend.title=element_blank())
g4 <- g4 + ggtitle("Capital Goods") 
g4 <- g4 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g4 <- g4 + theme(legend.title=element_blank()) 
g4 <- g4 + theme(legend.position="bottom",plot.margin=unit(c(-0.5,0.4,0,0.4), "cm"))
g4 <- g4 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

library(gridExtra)
grid.arrange(g1, g2, g3, g4, ncol=2, nrow =2)

#---------------------------------
#Regions

BERplot <- aggregate(BER.M$id, by=list(BER.M$surveyQ,BER.M$region), FUN = length)
BERplot$Group.1 <- as.Date(as.yearqtr(BERplot$Group.1, format = "%YQ%q"))
BERplot$Group.2 <- factor(BERplot$Group.2)
g <- ggplot(BERplot, aes(x=Group.1, y=x,fill=Group.2))
g <- g + geom_bar(stat="identity")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + scale_fill_discrete(name="Region")
g <- g + scale_y_continuous(labels=comma)
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g <- g + ylab("Number of Respondents")
g <- g + xlab("Date")
g



indicator_plot <- cbind(WC[,c("Datum","Q20")],
                        WC_n[,c("Q20")],WC_2[,c("Q20")])
colnames(indicator_plot) <- c("Date","Microdata","New","2-step")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g1 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g1 <- g1 + geom_line()
g1 <- g1 + ylab("Indicator") + xlab("")
g1 <- g1 + theme(legend.title=element_blank())
g1 <- g1 + ggtitle("WC") 
g1 <- g1 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g1 <- g1 + theme(legend.title=element_blank()) 
g1 <- g1 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g1 <- g1 + theme(legend.position="none")

indicator_plot <- cbind(KZN[,c("Datum","Q20")],
                        KZN_n[,c("Q20")],KZN_2[,c("Q20")])
colnames(indicator_plot) <- c("Date","Microdata","New","2-step")
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

indicator_plot <- cbind(GP[,c("Datum","Q20")],
                        GP_n[,c("Q20")],GP_2[,c("Q20")])
colnames(indicator_plot) <- c("Date","Microdata","New","2-step")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g3 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g3 <- g3 + geom_line()
g3 <- g3 + ylab("Indicator") + xlab("")
g3 <- g3 + theme(legend.title=element_blank())
g3 <- g3 + ggtitle("GP") 
g3 <- g3 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g3 <- g3 + theme(legend.title=element_blank()) 
g3 <- g3 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g3 <- g3 + theme(legend.position="bottom",plot.margin=unit(c(-0.5,0.4,0,0.4), "cm"))

indicator_plot <- cbind(WC[,c("Datum","Q20")],KZN[,c("Q20")],GP[,c("Q20")])
colnames(indicator_plot) <- c("Date","WC","KZN","GP") 
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g4 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g4 <- g4 + geom_line()
g4 <- g4 + ylab("") + xlab("")
g4 <- g4 + theme(legend.title=element_blank())
g4 <- g4 + ggtitle("Regions") 
g4 <- g4 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g4 <- g4 + theme(legend.title=element_blank()) 
g4 <- g4 + theme(legend.position="bottom",plot.margin=unit(c(-0.5,0.4,0,0.4), "cm"))
g4 <- g4 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

library(gridExtra)
grid.arrange(g1, g2, g3, g4, ncol=2, nrow =2)


#-------------------------------------
#Reference series
indicator_plot <- cbind(Manufacturing[,c("Datum","Q3A")],Manufacturing_n[,c("Q3A")],
                        ref[,c("RTotal_Sales.sa","Total_Vol.sa","Total.sa")])
colnames(indicator_plot) <- c("Date","Q3A:Micro","Q3A:New","SARB:Sales","SARB:Volume","StatsSA:Volume")
indicator_plot[,-1] <- scale(indicator_plot[,-1])
plot <- melt(indicator_plot, id="Date")  # convert to long format
g <- ggplot(data=plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_line()
g <- g + ylab("Indicator") + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) 
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g <- g + theme(legend.position="bottom")
g


#-----------------------------------
source("corstarsl.R")
Manu <- cbind(Manufacturing[,c("Q1A","Q3A")],Manufacturing_u[,c("Q1A","Q3A")],
              Manufacturing_n[,c("Q1A","Q3A")],Manufacturing_2[,c("Q1A","Q3A")],
              ref[,c("RTotal_Sales.sa","Total_Vol.sa","Total.sa")])
colnames(Manu) <- c("Q1A","Q3A","Q1A_u","Q3A_u","Q1A_new","Q3A_new","Q1A_2s","Q3A_2s","SARB:Sales","SARB:Volume","StatsSA:Volume")

xt <- xtable(corstarsl(Manu)[9:11,1:8])
print(xt, "latex",comment=FALSE,scalebox = 0.6)

V <- sapply(colnames(Manu),function(x) sd(Manu[,x], na.rm = TRUE))[1:8]
V <- t(as.data.frame(V))
row.names(V) <- "Vol"
xt <- xtable(V)
print(xt, "latex",comment=FALSE,scalebox = 0.6)

#ccf(Manufacturing_n[,c("Q1A")],ref[,c("Total.sa")], na.action = na.pass)


#---------------------------------------------
#Sectors
indicator_plot <- cbind(Food[,c("Datum","Q3A")],Food_n[,c("Q3A")],ref[,c("Food.sa")])
indicator_plot[,-1] <- scale(indicator_plot[,-1])
colnames(indicator_plot) <- c("Date","Microdata","New","StatsSA")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g1 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g1 <- g1 + geom_line()
g1 <- g1 + ylab("Indicator") + xlab("")
g1 <- g1 + theme(legend.title=element_blank())
g1 <- g1 + ggtitle("Food") 
g1 <- g1 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g1 <- g1 + theme(legend.title=element_blank()) 
g1 <- g1 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g1 <- g1 + theme(legend.position="none")

indicator_plot <- cbind(Textiles[,c("Datum","Q3A")],Textiles_n[,c("Q3A")],ref[,c("Textiles.sa")])
indicator_plot[,-1] <- scale(indicator_plot[,-1])
colnames(indicator_plot) <- c("Date","Microdata","New","StatsSA")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g2 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g2 <- g2 + geom_line()
g2 <- g2 + ylab("") + xlab("")
g2 <- g2 + theme(legend.title=element_blank())
g2 <- g2 + ggtitle("Textiles") 
g2 <- g2 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g2 <- g2 + theme(legend.title=element_blank()) 
g2 <- g2 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g2 <- g2 + theme(legend.position="none")

indicator_plot <- cbind(Wood[,c("Datum","Q3A")],Wood_n[,c("Q3A")],ref[,c("Wood.sa")])
indicator_plot[,-1] <- scale(indicator_plot[,-1])
colnames(indicator_plot) <- c("Date","Microdata","New","StatsSA")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g3 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g3 <- g3 + geom_line()
g3 <- g3 + ylab("Indicator") + xlab("")
g3 <- g3 + theme(legend.title=element_blank())
g3 <- g3 + ggtitle("Wood") 
g3 <- g3 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g3 <- g3 + theme(legend.title=element_blank()) 
g3 <- g3 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g3 <- g3 + theme(legend.position="bottom",plot.margin=unit(c(-0.5,0.4,0,0.4), "cm"))

indicator_plot <- cbind(Chemicals[,c("Datum","Q3A")],Chemicals_n[,c("Q3A")],ref[,c("Chemical")])
indicator_plot[,-1] <- scale(indicator_plot[,-1])
colnames(indicator_plot) <- c("Date","Microdata","New","StatsSA")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g4 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g4 <- g4 + geom_line()
g4 <- g4 + ylab("") + xlab("")
g4 <- g4 + theme(legend.title=element_blank())
g4 <- g4 + ggtitle("Chemicals") 
g4 <- g4 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g4 <- g4 + theme(legend.title=element_blank()) 
g4 <- g4 + theme(legend.position="bottom",plot.margin=unit(c(-0.5,0.4,0,0.4), "cm"))
g4 <- g4 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

library(gridExtra)
grid.arrange(g1, g2, g3, g4, ncol=2, nrow =2)



indicator_plot <- cbind(Glass[,c("Datum","Q3A")],Glass_n[,c("Q3A")],ref[,c("Glass.sa")])
indicator_plot[,-1] <- scale(indicator_plot[,-1])
colnames(indicator_plot) <- c("Date","Microdata","New","StatsSA")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g1 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g1 <- g1 + geom_line()
g1 <- g1 + ylab("Indicator") + xlab("")
g1 <- g1 + theme(legend.title=element_blank())
g1 <- g1 + ggtitle("Glass") 
g1 <- g1 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g1 <- g1 + theme(legend.title=element_blank()) 
g1 <- g1 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g1 <- g1 + theme(legend.position="none")

indicator_plot <- cbind(Metals[,c("Datum","Q3A")],Metals_n[,c("Q3A")],ref[,c("Metals.sa")])
indicator_plot[,-1] <- scale(indicator_plot[,-1])
colnames(indicator_plot) <- c("Date","Microdata","New","StatsSA")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g2 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g2 <- g2 + geom_line()
g2 <- g2 + ylab("") + xlab("")
g2 <- g2 + theme(legend.title=element_blank())
g2 <- g2 + ggtitle("Metals") 
g2 <- g2 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g2 <- g2 + theme(legend.title=element_blank()) 
g2 <- g2 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g2 <- g2 + theme(legend.position="none")


ref$Elec_Radio <- rowMeans(ref[,c("Electrical.sa","Radio.sa")])
indicator_plot <- cbind(Elec_radio[,c("Datum","Q3A")],Elec_radio_n[,c("Q3A")],ref[,c("Elec_Radio")])
indicator_plot[,-1] <- scale(indicator_plot[,-1])
colnames(indicator_plot) <- c("Date","Microdata","New","StatsSA")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g3 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g3 <- g3 + geom_line()
g3 <- g3 + ylab("Indicator") + xlab("")
g3 <- g3 + theme(legend.title=element_blank())
g3 <- g3 + ggtitle("Elec_Radio") 
g3 <- g3 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g3 <- g3 + theme(legend.title=element_blank()) 
g3 <- g3 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g3 <- g3 + theme(legend.position="bottom",plot.margin=unit(c(-0.5,0.4,0,0.4), "cm"))

indicator_plot <- cbind(Transport[,c("Datum","Q3A")],Transport_n[,c("Q3A")],ref[,c("Transport.sa")])
indicator_plot[,-1] <- scale(indicator_plot[,-1])
colnames(indicator_plot) <- c("Date","Microdata","New","StatsSA")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g4 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g4 <- g4 + geom_line()
g4 <- g4 + ylab("") + xlab("")
g4 <- g4 + theme(legend.title=element_blank())
g4 <- g4 + ggtitle("Transport") 
g4 <- g4 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g4 <- g4 + theme(legend.title=element_blank()) 
g4 <- g4 + theme(legend.position="bottom",plot.margin=unit(c(-0.5,0.4,0,0.4), "cm"))
g4 <- g4 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

library(gridExtra)
grid.arrange(g1, g2, g3, g4, ncol=2, nrow =2)


indicator_plot <- cbind(Furniture[,c("Datum","Q3A")],Furniture_n[,c("Q3A")],
                        ref[,c("Furniture.sa","Furniture_all.sa")])
colnames(indicator_plot) <- c("Date","Microdata","New","StatsSA","StatsSA (all)")
indicator_plot[,-1] <- scale(indicator_plot[,-1])
plot <- melt(indicator_plot, id="Date")  # convert to long format
g <- ggplot(data=plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_line()
g <- g + ggtitle("Funrniture") 
g <- g + ylab("Indicator") + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) 
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g <- g + theme(legend.position="bottom")
g



source("corstarsl.R")

x1 <- corstarsl(cbind(Food[,c("Datum","Q1A","Q3A")],ref[,c("Food.sa")])[,-1])
x2 <- corstarsl(cbind(Textiles[,c("Datum","Q1A","Q3A")],ref[,c("Textiles")])[,-1])
x3 <- corstarsl(cbind(Wood[,c("Datum","Q1A","Q3A")],ref[,c("Wood.sa")])[,-1])
x4 <- corstarsl(cbind(Chemicals[,c("Datum","Q1A","Q3A")],ref[,c("Chemical")])[,-1])
x5 <- corstarsl(cbind(Glass[,c("Datum","Q1A","Q3A")],ref[,c("Glass.sa")])[,-1])
x6 <- corstarsl(cbind(Metals[,c("Datum","Q1A","Q3A")],ref[,c("Metals.sa")])[,-1])
x7 <- corstarsl(cbind(Elec_radio[,c("Datum","Q1A","Q3A")],ref[,c("Elec_Radio")])[,-1])
x8 <- corstarsl(cbind(Transport[,c("Datum","Q1A","Q3A")],ref[,c("Transport.sa")])[,-1])
x9 <- corstarsl(cbind(Furniture[,c("Datum","Q1A","Q3A")],ref[,c("Furniture_all.sa")])[,-1])

x_1 <- rbind(x1[3,],x2[3,],x3[3,],x4[3,],x5[3,],x6[3,],x7[3,],x8[3,],x9[3,])
row.names(x_1) <- c("Food","Textiles","Wood","Chemicals","Glass","Metals",
                  "Elec_radio","Transport","Furniture")


x1 <- corstarsl(cbind(Food_n[,c("Datum","Q1A","Q3A")],ref[,c("Food.sa")])[,-1])
x2 <- corstarsl(cbind(Textiles_n[,c("Datum","Q1A","Q3A")],ref[,c("Textiles")])[,-1])
x3 <- corstarsl(cbind(Wood_n[,c("Datum","Q1A","Q3A")],ref[,c("Wood.sa")])[,-1])
x4 <- corstarsl(cbind(Chemicals_n[,c("Datum","Q1A","Q3A")],ref[,c("Chemical")])[,-1])
x5 <- corstarsl(cbind(Glass_n[,c("Datum","Q1A","Q3A")],ref[,c("Glass.sa")])[,-1])
x6 <- corstarsl(cbind(Metals_n[,c("Datum","Q1A","Q3A")],ref[,c("Metals.sa")])[,-1])
x7 <- corstarsl(cbind(Elec_radio_n[,c("Datum","Q1A","Q3A")],ref[,c("Elec_Radio")])[,-1])
x8 <- corstarsl(cbind(Transport_n[,c("Datum","Q1A","Q3A")],ref[,c("Transport.sa")])[,-1])
x9 <- corstarsl(cbind(Furniture_n[,c("Datum","Q1A","Q3A")],ref[,c("Furniture_all.sa")])[,-1])

x_2 <- rbind(x1[3,],x2[3,],x3[3,],x4[3,],x5[3,],x6[3,],x7[3,],x8[3,],x9[3,])


x1 <- corstarsl(cbind(Food_u[,c("Datum","Q1A","Q3A")],ref[,c("Food.sa")])[,-1])
x2 <- corstarsl(cbind(Textiles_u[,c("Datum","Q1A","Q3A")],ref[,c("Textiles")])[,-1])
x3 <- corstarsl(cbind(Wood_u[,c("Datum","Q1A","Q3A")],ref[,c("Wood.sa")])[,-1])
x4 <- corstarsl(cbind(Chemicals_u[,c("Datum","Q1A","Q3A")],ref[,c("Chemical")])[,-1])
x5 <- corstarsl(cbind(Glass_u[,c("Datum","Q1A","Q3A")],ref[,c("Glass.sa")])[,-1])
x6 <- corstarsl(cbind(Metals_u[,c("Datum","Q1A","Q3A")],ref[,c("Metals.sa")])[,-1])
x7 <- corstarsl(cbind(Elec_radio_u[,c("Datum","Q1A","Q3A")],ref[,c("Elec_Radio")])[,-1])
x8 <- corstarsl(cbind(Transport_u[,c("Datum","Q1A","Q3A")],ref[,c("Transport.sa")])[,-1])
x9 <- corstarsl(cbind(Furniture_u[,c("Datum","Q1A","Q3A")],ref[,c("Furniture_all.sa")])[,-1])

x_3 <- rbind(x1[3,],x2[3,],x3[3,],x4[3,],x5[3,],x6[3,],x7[3,],x8[3,],x9[3,])

x1 <- corstarsl(cbind(Food_2[,c("Datum","Q1A","Q3A")],ref[,c("Food.sa")])[,-1])
x2 <- corstarsl(cbind(Textiles_2[,c("Datum","Q1A","Q3A")],ref[,c("Textiles")])[,-1])
x3 <- corstarsl(cbind(Wood_2[,c("Datum","Q1A","Q3A")],ref[,c("Wood.sa")])[,-1])
x4 <- corstarsl(cbind(Chemicals_2[,c("Datum","Q1A","Q3A")],ref[,c("Chemical")])[,-1])
x5 <- corstarsl(cbind(Glass_2[,c("Datum","Q1A","Q3A")],ref[,c("Glass.sa")])[,-1])
x6 <- corstarsl(cbind(Metals_2[,c("Datum","Q1A","Q3A")],ref[,c("Metals.sa")])[,-1])
x7 <- corstarsl(cbind(Elec_radio_2[,c("Datum","Q1A","Q3A")],ref[,c("Elec_Radio")])[,-1])
x8 <- corstarsl(cbind(Transport_2[,c("Datum","Q1A","Q3A")],ref[,c("Transport.sa")])[,-1])
x9 <- corstarsl(cbind(Furniture_2[,c("Datum","Q1A","Q3A")],ref[,c("Furniture_all.sa")])[,-1])

x_4 <- rbind(x1[3,],x2[3,],x3[3,],x4[3,],x5[3,],x6[3,],x7[3,],x8[3,],x9[3,])


x <- cbind(x_1,x_3,x_2,x_4)
colnames(x) <- c("Q1A","Q3A","Q1A_U","Q3A_U","Q1A_New","Q3A_New","Q1A_2s","Q3A_2s")

xt <- xtable(x)
print(xt, "latex",comment=FALSE,scalebox = 0.6)




data <- cbind(Food[,c("Q1A","Q3A")],Food_u[,c("Q1A","Q3A")],
              Food_n[,c("Q1A","Q3A")],Food_2[,c("Q1A","Q3A")])
V <- sapply(1:8,function(x) sd(data[,x], na.rm = TRUE))[1:8]

data <- cbind(Textiles[,c("Q1A","Q3A")],Textiles_u[,c("Q1A","Q3A")],
              Textiles_n[,c("Q1A","Q3A")],Textiles_2[,c("Q1A","Q3A")])
V <- rbind(V,sapply(1:8,function(x) sd(data[,x], na.rm = TRUE))[1:8])

data <- cbind(Wood[,c("Q1A","Q3A")],Wood_u[,c("Q1A","Q3A")],
              Wood_n[,c("Q1A","Q3A")],Wood_2[,c("Q1A","Q3A")])
V <- rbind(V,sapply(1:8,function(x) sd(data[,x], na.rm = TRUE))[1:8])

data <- cbind(Chemicals[,c("Q1A","Q3A")],Chemicals_u[,c("Q1A","Q3A")],
              Chemicals_n[,c("Q1A","Q3A")],Chemicals_2[,c("Q1A","Q3A")])
V <- rbind(V,sapply(1:8,function(x) sd(data[,x], na.rm = TRUE))[1:8])

data <- cbind(Glass[,c("Q1A","Q3A")],Glass_u[,c("Q1A","Q3A")],
              Glass_n[,c("Q1A","Q3A")],Glass_2[,c("Q1A","Q3A")])
V <- rbind(V,sapply(1:8,function(x) sd(data[,x], na.rm = TRUE))[1:8])

data <- cbind(Metals[,c("Q1A","Q3A")],Metals_u[,c("Q1A","Q3A")],
              Metals_n[,c("Q1A","Q3A")],Metals_2[,c("Q1A","Q3A")])
V <- rbind(V,sapply(1:8,function(x) sd(data[,x], na.rm = TRUE))[1:8])

data <- cbind(Elec_radio[,c("Q1A","Q3A")],Elec_radio_u[,c("Q1A","Q3A")],
              Elec_radio_n[,c("Q1A","Q3A")],Elec_radio_2[,c("Q1A","Q3A")])
V <- rbind(V,sapply(1:8,function(x) sd(data[,x], na.rm = TRUE))[1:8])

data <- cbind(Transport[,c("Q1A","Q3A")],Transport_u[,c("Q1A","Q3A")],
              Transport_n[,c("Q1A","Q3A")],Transport_2[,c("Q1A","Q3A")])
V <- rbind(V,sapply(1:8,function(x) sd(data[,x], na.rm = TRUE))[1:8])

data <- cbind(Furniture[,c("Q1A","Q3A")],Furniture_u[,c("Q1A","Q3A")],
              Furniture_n[,c("Q1A","Q3A")],Furniture_2[,c("Q1A","Q3A")])
V <- rbind(V,sapply(1:8,function(x) sd(data[,x], na.rm = TRUE))[1:8])

colnames(V) <- c("Q1A","Q3A","Q1A_U","Q3A_U","Q1A_New","Q3A_New","Q1A_2s","Q3A_2s")
row.names(V) <- c("Food","Textiles","Wood","Chemicals","Glass","Metals","Elec_radio","Transport","Furniture")

xt <- xtable(V)
print(xt, "latex",comment=FALSE,scalebox = 0.6)


ccf(Food_u[,c("Q1A")],ref[,c("Food.sa")], na.action = na.pass)


#====================================================
#Moving averages
ma <- function(x,n=5) {filter(x,rep(1/n,n),method="convolution",sides=2)}

Manu <- cbind(Manufacturing[,c("Q1A","Q3A")],Manufacturing_u[,c("Q1A","Q3A")],
              ref[,c("RTotal_Sales.sa","Total_Vol.sa","Total.sa")])
colnames(Manu) <- c("Q1A","Q3A","Q1A_u","Q3A_u","SARB:Sales","SARB:Volume","StatsSA:Volume")

detach("package:dplyr", unload=TRUE)
manu <- as.data.frame(filter(as.ts(Manu),rep(1/4,4),method="convolution",sides=2))


indicator_plot <- cbind(Manufacturing[,c("Datum","Q3A")],manu)
colnames(indicator_plot)[1] <- c("Date")#,"Q3A:Micro","Q3A:New","SARB:Sales","SARB:Volume","StatsSA:Volume")
indicator_plot[,-1] <- scale(indicator_plot[,-1])
plot <- melt(indicator_plot, id="Date")  # convert to long format
g <- ggplot(data=plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_line()
g <- g + ylab("Indicator") + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) 
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g <- g + theme(legend.position="bottom")
g

corstarsl(indicator_plot[,-1])



lys <- list()
t <- 0
for(k in all) {
    t <- t+1
    lys[[t]] <- geweeg2(k)
}

saam <- sector
for(j in 4:60) {
    for(i in 1:100) {
        x <- NULL
        w <- NULL
        for(t in 1:39) {
            x <- c(x,lys[[t]][i,j])
            w <- c(w,lys[[t]][i,3])
        }
        saam[i,j] <- weighted.mean(x,w,na.rm=TRUE) 
    }
}

saam <- saam[,-3]
saam[24,c(seq(3,31,by=2),33:48)] <- pub[24,2:32]
saam[33,c(seq(3,31,by=2),33:48)] <- pub[33,2:32]
saam[56,c(seq(3,31,by=2),33:48)] <- pub[56,2:32]

#lys[[1:2]][i,j]

#"1010" <- geweeg2("1010")
#"1011" <- geweeg2("1011")
#saam <- sector

#for(j in 4:60) {
#    for(i in 1:100) {
#        saam[i,j] <- weighted.mean(c(`1010`[i,j],`1011`[i,j]), 
#                                   c(`1010`[i,3],`1011`[i,3]),na.rm=TRUE) 
#    }
#}


levels(BER.M$sector)
sektor <- food
streek <- streke
kwartaal <- "1992Q1"
temp <- man[man$surveyQ==kwartaal,]




write.csv(Manufacturing,"Indicators.csv")






