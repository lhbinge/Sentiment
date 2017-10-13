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

GDPdata <- read.csv("GDP Data2.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)
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

realGDP <- read.csv("RealGDP2.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)
realGDP$Date <- as.Date(realGDP$Date, format = "%Y/%m/%d")

GDPgrowth4 <- as.data.frame(sapply(log(realGDP[,-1]), diff, lag =4))
GDPgrowth1 <- as.data.frame(sapply(log(realGDP[,-1]), diff, lag =1))


##====================================##
## READING IN THE DATA ##
##====================================##
#BER.M <- rbind.fill(read.csv("Manufacturing.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE),
#                     read.csv("Manufacturing_pre2001.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE))
#BER.M <- BER.M[,1:62]
#colnames(BER.M)[1:7] <- c("region","id","sector","weight","turnover","factor","surveyQ")

BER.M <- read.csv("Manufacturing_new faktor.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)
BER.M$Faktor <- BER.M$Gewig * BER.M$Sectorw
BER.M <- BER.M[,c(1:5,8:75)]
colnames(BER.M)[1:7] <- c("region","id","sector","weight","turnover","factor","surveyQ")
exclude <- c("2016Q4","2017Q1","2017Q2")
BER.M <- subset(BER.M, !(surveyQ %in% exclude))
BER.M$factor <- as.numeric(as.character(BER.M$factor))

##===============================##
#BER.B <- read.csv("Building_corrected.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)
#BER.B <- BER.B[!(BER.B$surveyQ %in% c("2015Q4","2016Q1","2016Q2","2016Q3")),1:21]

BER.B <- read.csv("Building_93Q2-17Q2.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)
colnames(BER.B)[1:6] <- c("region","id","sector","weight","factor","surveyQ")

BER.B$factor[BER.B$sector ==5000] <- 0.30
BER.B$factor[BER.B$sector ==5010] <- 0.15
BER.B$factor[BER.B$sector ==6000] <- 0.10
BER.B$factor[BER.B$sector ==6010] <- 0.15
BER.B$factor <- BER.B$factor*BER.B$weight

#BER.B$factor <- BER.B$weight
BER.B$temp <- NULL
for(i in 1:nrow(BER.B)) {
    ifelse(substr(BER.B$surveyQ[i], 1, 1)==9, 
           BER.B$temp[i] <- paste0("19",BER.B$surveyQ[i],sep=""),
           BER.B$temp[i] <- paste0("20",BER.B$surveyQ[i],sep=""))
}
BER.B$surveyQ <- BER.B$temp
BER.B <- BER.B[,-ncol(BER.B)]
BER.B$surveyQ <- factor(BER.B$surveyQ)
BER.B <- subset(BER.B, !(surveyQ %in% exclude))

##===============================##
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
    
    data$weight <- replace(data$weight, data$weight==0, 1)
    data$factor <- data$weight*0.10
    
    #data <- data[data$Latecomer == FALSE | is.na(data$Latecomer),]
    data <- data[,1:(ncol(data)-6)]
    
    data <- subset(data, !(surveyQ %in% exclude))
    return(data)
}


arc <- skoon(read.csv("Argitekte.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE))
civil <- skoon(read.csv("Civils.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE))
qs <- skoon(read.csv("QS.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE))


##============================##
#BER.R <- read.csv("Retail.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)
#BER.W <- read.csv("Wholesale.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)
#BER.T <- rbind(BER.R,BER.W)
#BER.T <- rbind.fill(BER.T,read.csv("Trade_pre2001.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE))
#BER.T <- BER.T[,1:21]
#colnames(BER.T)[1:6] <- c("region","id","sector","weight","factor","surveyQ")
#BER.T$factor <- as.numeric(as.character(BER.T$factor))

#BER.V <- rbind.fill(read.csv("Motor.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE),
#                    read.csv("Motor_pre2001.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE))
#BER.V <- BER.V[,1:28]
#colnames(BER.V)[1:6] <- c("region","id","sector","weight","factor","surveyQ")
#BER.V$factor <- BER.V$weight*0.25

skoon <- function(data) {
    colnames(data)[1:7] <- c("region","id","sector","weight","sectorw","factor","surveyQ")
    data$surveyQ <- toupper(data$surveyQ)
    data$sector <- factor(data$sector) #could include labels
    data$id <- factor(data$id)
    data$region <- factor(data$region)
    
    data$temp <- NULL
    for(i in 1:nrow(data)) {
        ifelse(substr(data$surveyQ[i], 1, 1)==9, 
               data$temp[i] <- paste0("19",data$surveyQ[i],sep=""),
               data$temp[i] <- paste0("20",data$surveyQ[i],sep=""))
    }
    data$surveyQ <- data$temp
    data <- data[,-ncol(data)]
    data$surveyQ <- factor(data$surveyQ)
    
    data$sectorw <- as.numeric(as.character(data$sectorw))
    data$factor <- as.numeric(as.character(data$factor))
    data$factor <- data$weight*data$sectorw
    
    #data <- data[data$Latecomer == FALSE | is.na(data$Latecomer),]
    data <- data[,1:(ncol(data)-5)]
    
    data <- subset(data, !(surveyQ %in% exclude))
    return(data)
}

BER.R <- skoon(read.csv("Retail_92Q2-17Q2_sep.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE))
BER.W <- skoon(read.csv("Wholesale_92Q2-17Q2_sep.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE))
BER.V <- skoon(read.csv("Motor_92Q2-17Q2.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE))
BER.V$factor <- BER.V$weight*0.2

##===============================##
BER.S <- read.csv("Services_05Q2-17Q2.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)
colnames(BER.S)[1:6] <- c("region","id","sector","weight","factor","surveyQ")

BER.S$temp <- NULL
for(i in 1:nrow(BER.S)) {
    BER.S$temp[i] <- paste0("20",BER.S$surveyQ[i],sep="")
}
BER.S$surveyQ <- BER.S$temp
BER.S <- BER.S[,-ncol(BER.S)]
BER.S$surveyQ <- factor(BER.S$surveyQ)

BER.S$sector[BER.S$sector==6000] <- 6001
BER.S$sector[BER.S$sector==6010] <- 6011

catering <- c(6001,6020,6030,6011)
transport <- c(7020,7010,7070,7090,7080,7060,7000,7040,7100,7120,7110,7050)
realestate <- c(8000,8010)
business <- c(8040,8080,8070,8090,8020,8060,8050,8030)
other <- c(8150,8120,8210,8180,8140,8160,8190,8100,8200,8230,8130,8110,8170,8240,8220)
community <- c(9000,9010,9030,9050,9060,9020,9040)

BER.S$factor[BER.S$sector %in% catering] <- 0.15
BER.S$factor[BER.S$sector %in% transport] <- 0.15 
BER.S$factor[BER.S$sector %in% realestate] <- 0.15
BER.S$factor[BER.S$sector %in% business] <- 0.05 
BER.S$factor[BER.S$sector %in% other] <- 0.45 
BER.S$factor[BER.S$sector %in% community] <- 0.05 

#BER.S$factor[BER.S$sector %in% catering] <- 0.16/4
#BER.S$factor[BER.S$sector %in% transport] <- 0.24/12 
#BER.S$factor[BER.S$sector %in% realestate] <- 0.1/2
#BER.S$factor[BER.S$sector %in% business] <- 0.05/8 
#BER.S$factor[BER.S$sector %in% other] <- 0.4/15 
#BER.S$factor[BER.S$sector %in% community] <- 0.05/7 

BER.S$factor <- BER.S$factor*BER.S$weight
BER.S <- subset(BER.S, !(surveyQ %in% exclude))


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
tempBER.R <- cbind(BER.R[,c("id","sector","weight","factor","surveyQ", "Q1","Q2A","Q2P","Q3A","Q3P","Q5A","Q5P","Q8",       "Q4A","Q4P")],"Trade")
colnames(tempBER.R) <-    c("id","sector","weight","factor","surveyQ", "Q1","Q2A","Q2P","Q3A","Q3P","Q4A","Q4P","Q5A",      "Q6A","Q6P"  ,"Sector")
tempBER.R[,c("Q5P","Q6A","Q6P")] <- NA
tempBER.W <- cbind(BER.W[,c("id","sector","weight","factor","surveyQ", "Q1","Q2A","Q2P","Q3A","Q3P","Q5A","Q5P","Q8",       "Q4A","Q4P")],"Trade")
colnames(tempBER.W) <-    c("id","sector","weight","factor","surveyQ", "Q1","Q2A","Q2P","Q3A","Q3P","Q4A","Q4P","Q5A",      "Q6A","Q6P"  ,"Sector")
tempBER.W[,c("Q5P","Q6A","Q6P")] <- NA
tempBER.V <- cbind(BER.V[,c("id","sector","weight","factor","surveyQ", "Q1","Q2A","Q2P","Q3A","Q3P",                        "Q4A","Q4P")],"Trade")
colnames(tempBER.V) <-    c("id","sector","weight","factor","surveyQ", "Q1","Q2A","Q2P","Q3A","Q3P",                        "Q6A","Q6P"  ,"Sector")
tempBER.V[,c("Q4A","Q4P","Q5A","Q5P")] <- NA
tempBER.S <- cbind(BER.S[,c("id","sector","weight","factor","surveyQ", "Q1","Q2A","Q2P","Q3A","Q3P","Q4A","Q4P","Q5A","Q5P")],            "Services")
colnames(tempBER.S) <-    c("id","sector","weight","factor","surveyQ", "Q1","Q2A","Q2P","Q3A","Q3P","Q4A","Q4P","Q5A","Q5P",              "Sector")
tempBER.S[,c("Q6A","Q6P")] <- NA

temparc <- cbind(arc[,c("id","sector","weight","factor","surveyQ", "Q1","Q6A","Q6P","Q5A","Q5P","Q2A","Q2P",                "Q4A","Q4P")],"Construction")
colnames(temparc) <-    c("id","sector","weight","factor","surveyQ", "Q1","Q2A","Q2P","Q3A","Q3P","Q4A","Q4P",              "Q6A","Q6P","Sector")
temparc[,c("Q5A","Q5P")] <- NA
tempqs <- cbind(qs[,c("id","sector","weight","factor","surveyQ", "Q1","Q6A","Q6P","Q5A","Q5P","Q2A","Q2P",                "Q4A","Q4P")],"Construction")
colnames(tempqs) <-    c("id","sector","weight","factor","surveyQ", "Q1","Q2A","Q2P","Q3A","Q3P","Q4A","Q4P",              "Q6A","Q6P","Sector")
tempqs[,c("Q5A","Q5P")] <- NA
tempcivil <- cbind(civil[,c("id","sector","weight","factor","surveyQ", "Q1","Q2A","Q2P","Q3A","Q3P","Q4A","Q4P","Q5A","Q5P")],            "Construction")
colnames(tempcivil) <-    c("id","sector","weight","factor","surveyQ", "Q1","Q2A","Q2P","Q3A","Q3P","Q4A","Q4P","Q5A","Q5P",              "Sector")
tempcivil[,c("Q6A","Q6P")] <- NA

BER <- tempBER.M
BER <- rbind(BER,tempBER.B,tempBER.R,tempBER.W,tempBER.V,tempBER.S,temparc,tempqs,tempcivil)
BER <- BER[,c(15,1:12,16,17,13,14)]
rm(tempBER.M,tempBER.B,tempBER.R,tempBER.W,tempBER.V,tempBER.S,temparc,tempqs,tempcivil)
rm(BER.M,BER.B,BER.R,BER.W,BER.V,BER.S,arc,qs,civil)

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


#Maak factor reg
Ej <- aggregate(BER$weight, by=list(BER$surveyQ,BER$sector), FUN=sum)
BER <-  merge(BER, Ej, by.x=c("surveyQ","sector"), by.y=c("Group.1","Group.2"))
BER$factorn <- BER$factor/BER$x
BER <- BER[,c(3,4,2,5,6,19,1,7:17)]

#-----------------------------------------------------------------
tafel <- aggregate(BER$id, by=list(BER$surveyQ,BER$Sector), FUN = length)
tafel <- cbind(obs=aggregate(tafel$x, by=list(tafel$Group.2), FUN = sum ),ave=aggregate(tafel$x, by=list(tafel$Group.2), FUN = mean ))[,-3]
tafel$obs.Group.1 <- as.character(tafel$obs.Group.1)
tafel$Resp <- tafel$ave.x/c(1000,1400,1400,1000)
tafel$Sample <- c("1992Q1-2016Q3","1993Q2-2016Q3","1992Q2-2016Q3","2005Q2-2016Q3") 
tafel$Missing <- c("1997Q4,2000Q1,2005Q4","1993Q4,1998Q3,2000Q2,2005Q4","1992Q4,1993Q3,2005Q4","2005Q4") 
tafel <- tafel[,c(1,5,2:4,6)]
tafel <- rbind(tafel,c("Total","1992Q1-2016Q3",nrow(BER),mean(aggregate(BER$id, by=list(BER$surveyQ),FUN = length)[,2]),
                       mean(aggregate(BER$id, by=list(BER$surveyQ), FUN = length)[,2])/4800,"2005Q4"))
tafel[, c(4:5)] <- sapply(tafel[, c(4:5)], as.numeric)
colnames(tafel) <- c("Sector","Sample","Total Obs","Obs/Quarter", "Response Rate","Missing Quarters")     
xt <- xtable(tafel, caption="Sample characteristics", digits=c(2), align= c('r', "p{3cm}", rep('r',5) ) )
print(xt, "latex",comment=FALSE, caption.placement = getOption("xtable.caption.placement", "top"), scalebox = 0.8,
      include.rownames=FALSE)

#-----------------------------------------------------------------
BERplot <- aggregate(BER$id, by=list(BER$surveyQ,BER$Sector), FUN = length)
BERplot$Group.1 <- as.Date(as.yearqtr(BERplot$Group.1, format = "%YQ%q"), frac = 1)
g <- ggplot(BERplot, aes(x=Group.1, y=x,fill=Group.2))
g <- g + geom_bar(stat="identity")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + scale_fill_discrete(name="Sector")
g <- g + scale_y_continuous(labels=comma)
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g <- g + ylab("Number of Respondents") + xlab("Date")
g <- g + guides(fill = guide_legend(reverse = TRUE))
g

#==========================================
#Illustrating weights
BER.M <- BER[BER$Sector=="Manufacturing",]
BER.M$Sector <- as.character(BER.M$Sector)
manufacturings <- unique(BER.M$sector)

food <- c(1010, 1011,1013,1019,1020,1021)
textiles <- c(1120,1040,1049,1042,1060,1070)
wood <- c(1109,1110,1080,1081)
chemicals <- c(1130,1140,1149,1219)
glass <- c(1153,1159)
metals <- c(1160,1161,1170,1179,1181,1182,1189)
electrical <- c(1190,1191,1199,1192,1194)
transport <- c(1200,1201,1209)
furniture <- c(1090,1099)


BER.M$Sector[BER.M$sector %in% food] <- "Food" 
BER.M$Sector[BER.M$sector %in% textiles] <- "Textiles" 
BER.M$Sector[BER.M$sector %in% wood] <- "Wood" 
BER.M$Sector[BER.M$sector %in% chemicals] <- "Chemicals" 
BER.M$Sector[BER.M$sector %in% glass] <- "Glass" 
BER.M$Sector[BER.M$sector %in% metals] <- "Metals" 
BER.M$Sector[BER.M$sector %in% electrical] <- "Eelectrical" 
BER.M$Sector[BER.M$sector %in% transport] <- "Transport" 
BER.M$Sector[BER.M$sector %in% furniture] <- "Furniture" 


BER.M$sectorw <- BER.M$factor/BER.M$weight
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

#-----------------------------------------------------------
BER.B <- BER[BER$Sector=="Construction",]
BER.B$Sector <- as.character(BER.B$Sector)
constructs <- unique(BER.B$sector)

contractor_res <- 5000
contractor_nonres <- 5010
subcon_res <- 6000
subcon_nonres <- 6010
arc <- 99
qs <- 88
civils <- c(700,701,702,703)

BER.B$Sector[BER.B$sector %in% contractor_res] <- "Contractor (Residential)" 
BER.B$Sector[BER.B$sector %in% contractor_nonres] <- "Contractor (Non-Residential)" 
BER.B$Sector[BER.B$sector %in% subcon_res] <- "Sub-contractor (Residential)" 
BER.B$Sector[BER.B$sector %in% subcon_nonres] <- "Sub-contractor (Non-Residential)" 

BER.B$Sector[BER.B$sector %in% arc] <- "Architects" 
BER.B$Sector[BER.B$sector %in% qs] <- "Quantity Surveyors" 
BER.B$Sector[BER.B$sector %in% civils] <- "Civil Engineers" 

BER.B$sectorw <- BER.B$factor/BER.B$weight

BERplot <- aggregate(BER.B$sectorw, by=list(BER.B$surveyQ,BER.B$sector,BER.B$Sector), FUN = mean,na.rm=TRUE)
colnames(BERplot) <- c("surveyQ","sector","Sector","weight")
BERplot1 <- dcast(BERplot, formula = surveyQ ~ Sector + sector)
BERplot1 <- merge(datums,BERplot1,by.x="Date",by.y ="surveyQ")
#BERplot1[,-1:-2] <- na.locf(BERplot1[,-1:-2],fromLast = TRUE)
BERplot1[,-1:-2] <- na.locf(BERplot1[,-1:-2])

BERplot <- BERplot1
BERplot$Arc <- BERplot1[,3]
BERplot$Civils <- rowMeans(BERplot1[,4:7],na.rm=TRUE)
BERplot$Con_nonres <- BERplot1[,8]
BERplot$Con_res <- BERplot1[,9]
BERplot$QS <- BERplot1[,10]
BERplot$Sub_nonres <- BERplot1[,11]
BERplot$Sub_res <- BERplot1[,12]
BERplot <- BERplot[,c(1,13:19)]
BERplot$Total <- rowSums(BERplot[,2:8],na.rm=TRUE)
BERplot[,-1] <- BERplot[,-1]/BERplot[,9]
colnames(BERplot) <- c("Date","Architects","Civil Engineers","Contractor (Non-Residential)","Contractor (Residential)",
                       "Quantity Surveyors","Sub-contractor (Non-Residential)","Sub-contractor (Residential)")
#BERplot <- aggregate(BER.M$factor, by=list(BER.M$surveyQ,BER.M$Sector), FUN = sum, na.rm=TRUE)
#BERplot1 <- aggregate(BER.M$factor, by=list(BER.M$surveyQ), FUN = sum, na.rm=TRUE)
#BERplot$y <- BERplot$x/BERplot1$x

#BERplot <- merge(BERplot,BERplot1,by="Group.1")
#BERplot$y <- BERplot$x.x/BERplot$x.y
#BERplot <- BERplot[order(BERplot$Group.2),]
#BERplot <- aggregate(BER.M$factor, by=list(BER.M$surveyQ,BER.M$Sector), FUN = mean)
BERplot$Date <- as.Date(as.yearqtr(BERplot$Date, format = "%YQ%q"), frac = 1)
#BERplot <- BERplot[,c(1,5,4,8,7,2,3,6)]
index_plot <- melt(BERplot[,-9], id="Date")
g <- ggplot(index_plot, aes(x=Date,y=value,group=variable,fill=variable)) 
g <- g + geom_bar(stat="identity")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + scale_fill_discrete(name="Sub-sector")
g <- g + scale_y_continuous(labels=comma)
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g <- g + ylab("Weights") + xlab("Date")
g <- g + guides(fill = guide_legend(reverse = TRUE))
g


#BER.M$sectorw <- BER.M$factor/BER.M$weight
BERplot <- aggregate(BER.B$factor, by=list(BER.B$surveyQ,BER.B$Sector), FUN = sum, na.rm=TRUE)#[-1:-5,]
BERplot1 <- aggregate(BER.B$factor, by=list(BER.B$surveyQ), FUN = sum, na.rm=TRUE)
#BERplot$y <- BERplot$x/BERplot1$x
BERplot <- merge(BERplot,BERplot1,by="Group.1")
BERplot$y <- BERplot$x.x/BERplot$x.y
BERplot <- BERplot[order(BERplot$Group.2),]
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

BERplot <- aggregate(BER.B$factor, by=list(BER.B$surveyQ,BER.B$Sector), FUN = length)
#BERplot1 <- aggregate(BER.S$factor, by=list(BER.S$surveyQ), FUN = sum, na.rm=TRUE)
#BERplot$y <- BERplot$x/BERplot1$x
BERplot1 <- aggregate(BER.B$factor, by=list(BER.B$surveyQ), FUN = length)
BERplot <- merge(BERplot,BERplot1,by="Group.1")
BERplot$y <- BERplot$x.x/BERplot$x.y
BERplot <- BERplot[order(BERplot$Group.2),]
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

BERplot <- aggregate(BERplot$y, by=list(BERplot$Group.1,BERplot$Group.2), FUN = mean,na.rm=TRUE)

#------------------------------------------------------
BER.T <- BER[BER$Sector=="Trade",]
BER.T$Sector <- as.character(BER.T$Sector)
trades <- unique(BER.T$sector)

retailsd <- c(3110,3120,3160,3140,3130,3170,3150)
retailnd <- c(3230,3210,3240,3220)
retaild <- c(3330,3370,3310,3340,3350,3320,3380,3360)
retailo <- c(3410,3420,0,NA)
wholenc <- c(2120,2110,2130,2140)
wholec <- c(2250,2220,2230,2240,2210,2270,2260)
motor <- c(3510)

BER.T$Sector[BER.T$sector %in% retailsd] <- "Retail (semi-durable)" 
BER.T$Sector[BER.T$sector %in% retailnd] <- "Retail (non-durable)" 
BER.T$Sector[BER.T$sector %in% retaild] <- "Retail (durable)" 
BER.T$Sector[BER.T$sector %in% retailo] <- "Retail (other)" 
BER.T$Sector[BER.T$sector %in% wholenc] <- "Wholesale (non-consumer)" 
BER.T$Sector[BER.T$sector %in% wholec] <- "Wholesale (consumer)" 
BER.T$Sector[BER.T$sector %in% motor] <- "Motor Trade" 


BER.T$sectorw <- BER.T$factor/BER.T$weight
BERplot <- aggregate(BER.T$sectorw, by=list(BER.T$surveyQ,BER.T$sector,BER.T$Sector), FUN = mean, na.rm=TRUE)
colnames(BERplot) <- c("surveyQ","sector","Sector","weight")
BERplot1 <- dcast(BERplot, formula = surveyQ ~ Sector + sector)
BERplot1 <- merge(datums,BERplot1,by.x="Date",by.y ="surveyQ")
BERplot1[,-1:-2] <- na.locf(BERplot1[,-1:-2],fromLast = TRUE)
BERplot1[,-1:-2] <- na.locf(BERplot1[,-1:-2])
BERplot1[16,4] <- 0.04
BERplot1[16,18] <- 0.12

BERplot <- BERplot1
BERplot$retailsd <- rowMeans(BERplot1[,18:24])
BERplot$retailnd <- rowMeans(BERplot1[,12:15])
BERplot$retaild <- rowMeans(BERplot1[,4:11])
BERplot$retailo <- rowMeans(BERplot1[,16:17],na.rm = TRUE)
BERplot$wholenc <- rowMeans(BERplot1[,32:35])
BERplot$wholec <- rowMeans(BERplot1[,25:31])
BERplot$motor <- BERplot1[,3]
BERplot <- BERplot[,c(1,36:42)]
BERplot$Total <- rowSums(BERplot[,2:8])
BERplot[,-1] <- BERplot[,-1]/BERplot[,9]
colnames(BERplot) <- c("Date","Retail (semi-durable)","Retail (non-durable)","Retail (durable)","Retail (other)",
                       "Wholesale (non-consumer)","Wholesale (consumer)","Motor Trade")

#BERplot <- aggregate(BER.M$factor, by=list(BER.M$surveyQ,BER.M$Sector), FUN = sum, na.rm=TRUE)
#BERplot1 <- aggregate(BER.M$factor, by=list(BER.M$surveyQ), FUN = sum, na.rm=TRUE)
#BERplot$y <- BERplot$x/BERplot1$x

#BERplot <- merge(BERplot,BERplot1,by="Group.1")
#BERplot$y <- BERplot$x.x/BERplot$x.y
#BERplot <- BERplot[order(BERplot$Group.2),]
#BERplot <- aggregate(BER.M$factor, by=list(BER.M$surveyQ,BER.M$Sector), FUN = mean)
BERplot$Date <- as.Date(as.yearqtr(BERplot$Date, format = "%YQ%q"), frac = 1)
BERplot <- BERplot[,c(1,8,4,3,5,2,7,6,9)]
index_plot <- melt(BERplot[,-9], id="Date")
g <- ggplot(index_plot, aes(x=Date,y=value,group=variable,fill=variable)) 
g <- g + geom_bar(stat="identity")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + scale_fill_discrete(name="Sub-sector")
g <- g + scale_y_continuous(labels=comma)
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g <- g + ylab("Weights") + xlab("Date")
g <- g + guides(fill = guide_legend(reverse = TRUE))
g


BERplot <- aggregate(BER.T$factor, by=list(BER.T$surveyQ,BER.T$Sector), FUN = sum, na.rm=TRUE)
BERplot1 <- aggregate(BER.T$factor, by=list(BER.T$surveyQ), FUN = sum, na.rm=TRUE)
BERplot <- merge(BERplot,BERplot1,by="Group.1")
BERplot$y <- BERplot$x.x/BERplot$x.y
BERplot <- BERplot[order(BERplot$Group.2),]
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


BERplot <- aggregate(BER.T$factor, by=list(BER.T$surveyQ,BER.T$Sector), FUN = length)
#BERplot1 <- aggregate(BER.S$factor, by=list(BER.S$surveyQ), FUN = sum, na.rm=TRUE)
#BERplot$y <- BERplot$x/BERplot1$x
BERplot1 <- aggregate(BER.T$factor, by=list(BER.T$surveyQ), FUN = length)
BERplot <- merge(BERplot,BERplot1,by="Group.1")
BERplot$y <- BERplot$x.x/BERplot$x.y
BERplot <- BERplot[order(BERplot$Group.2),]
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

BERplot$year <- as.year(BERplot$Group.1)
BERplot <- aggregate(BERplot$y, by=list(BERplot$Group.1,BERplot$Group.2), FUN = mean,na.rm=TRUE)

#------------------------------------------------------
BER.S <- BER[BER$Sector=="Services",]
BER.S$Sector <- as.character(BER.S$Sector)
servicess <- unique(BER.S$sector)[!is.na(unique(BER.S$sector))]


#Illustrating weights
catering <- c(6001,6020,6030,6011,NA)
transport <- c(7020,7010,7070,7090,7080,7060,7000,7040,7100,7120,7110,7050)
realestate <- c(8000,8010,8020)
business <- c(8040,8080,8070,8090,8060,8050,8030)
other <- c(8150,8120,8210,8180,8140,8160,8190,8100,8200,8230,8130,8110,8170,8240,8220)
community <- c(9000,9010,9030,9050,9060,9020,9040)


BER.S$Sector[BER.S$sector %in% catering] <- "Catering and Accommodation" 
BER.S$Sector[BER.S$sector %in% transport] <- "Transport and Storage" 
BER.S$Sector[BER.S$sector %in% realestate] <- "Real Estate" 
BER.S$Sector[BER.S$sector %in% business] <- "Business Services" 
BER.S$Sector[BER.S$sector %in% other] <- "Other Business Activities" 
BER.S$Sector[BER.S$sector %in% community] <- "Community and Personal Services" 


BER.S$sectorw <- BER.S$factor/BER.S$weight
BERplot <- aggregate(BER.S$sectorw, by=list(BER.S$surveyQ,BER.S$sector,BER.S$Sector), FUN = mean, na.rm=TRUE)
colnames(BERplot) <- c("surveyQ","sector","Sector","weight")
BERplot1 <- dcast(BERplot, formula = surveyQ ~ Sector + sector)
BERplot1 <- merge(datums,BERplot1,by.x="Date",by.y ="surveyQ")
BERplot1[,-1:-2] <- na.locf(BERplot1[,-1:-2],fromLast = TRUE)
BERplot1[,-1:-2] <- na.locf(BERplot1[,-1:-2])

BERplot <- BERplot1
BERplot$catering <- rowMeans(BERplot1[,11:14])
BERplot$transport <- rowMeans(BERplot1[,39:50])
BERplot$realestate <- rowMeans(BERplot1[,37:38])
BERplot$business <- rowMeans(BERplot1[,3:10])
BERplot$other <- rowMeans(BERplot1[,22:36])
BERplot$community <- rowMeans(BERplot1[,15:21])
BERplot <- BERplot[,c(1,51:56)]
BERplot$Total <- rowSums(BERplot[,2:7])
BERplot[,-1] <- BERplot[,-1]/BERplot[,8]
colnames(BERplot) <- c("Date","Catering and Accommodation","Transport and Storage","Real Estate","Business Services",
                       "Other Business Activities","Community and Personal Services")

#BERplot <- aggregate(BER.M$factor, by=list(BER.M$surveyQ,BER.M$Sector), FUN = sum, na.rm=TRUE)
#BERplot1 <- aggregate(BER.M$factor, by=list(BER.M$surveyQ), FUN = sum, na.rm=TRUE)
#BERplot$y <- BERplot$x/BERplot1$x

#BERplot <- merge(BERplot,BERplot1,by="Group.1")
#BERplot$y <- BERplot$x.x/BERplot$x.y
#BERplot <- BERplot[order(BERplot$Group.2),]
#BERplot <- aggregate(BER.M$factor, by=list(BER.M$surveyQ,BER.M$Sector), FUN = mean)
BERplot$Date <- as.Date(as.yearqtr(BERplot$Date, format = "%YQ%q"), frac = 1)
BERplot <- BERplot[,c(1,5,2,7,6,4,3,8)]
index_plot <- melt(BERplot[,-8], id="Date")
g <- ggplot(index_plot, aes(x=Date,y=value,group=variable,fill=variable)) 
g <- g + geom_bar(stat="identity")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + scale_fill_discrete(name="Sub-sector")
g <- g + scale_y_continuous(labels=comma)
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g <- g + ylab("Weights") + xlab("Date")
g <- g + guides(fill = guide_legend(reverse = TRUE))
g


BERplot <- aggregate(BER.S$factor, by=list(BER.S$surveyQ,BER.S$Sector), FUN = sum, na.rm=TRUE)
BERplot1 <- aggregate(BER.S$factor, by=list(BER.S$surveyQ), FUN = sum, na.rm=TRUE)
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


BERplot <- aggregate(BER.S$factor, by=list(BER.S$surveyQ,BER.S$Sector), FUN = length)
#BERplot1 <- aggregate(BER.S$factor, by=list(BER.S$surveyQ), FUN = sum, na.rm=TRUE)
#BERplot$y <- BERplot$x/BERplot1$x
BERplot1 <- aggregate(BER.S$factor, by=list(BER.S$surveyQ), FUN = length)
BERplot <- merge(BERplot,BERplot1,by="Group.1")
BERplot$y <- BERplot$x.x/BERplot$x.y
BERplot <- BERplot[order(BERplot$Group.2),]
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


##========================##
##----- CONFIDENCE -------##
##========================##
#Use simple row means:
calc_conf <- function(data) {
    confidence <- aggregate(data[,(match("surveyQ",colnames(data))+1):ncol(data)], by=list(data$surveyQ), FUN=mean, na.rm=TRUE)
    confidence$Conf_cc <- rowMeans(confidence[,c("Q1","Q2A","Q3A","Q4A","Q6A")],na.rm = TRUE, dims = 1)
    confidence$Conf_fl <- rowMeans(confidence[,c("Q2P","Q3P","Q4P","Q6P")],na.rm = TRUE, dims = 1)
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
    w.confidence$Conf_cc <- rowMeans(w.confidence[,c("Q1","Q2A","Q3A","Q4A","Q6A")],na.rm = TRUE, dims = 1)
    w.confidence$Conf_fl <- rowMeans(w.confidence[,c("Q2P","Q3P","Q4P","Q6P")],na.rm = TRUE, dims = 1)
    #w.confidence$Conf_all <- rowMeans(w.confidence[,c("Q1","Q2A","Q3A","Q4A","Q5A","Q6A","Q2P","Q3P","Q4P","Q5P","Q6P")],na.rm = TRUE, dims = 1)
    w.confidence <- merge(datums,w.confidence,by.x="Date",by.y="row.names", all=TRUE)[,-3]
    w.confidence[,14:15] <- na.approx(w.confidence[,14:15],na.rm = FALSE)
    
    return(w.confidence)
}


calc_wconf <- function(data) {
    ##Weighted versions
    weeg <- function(temp) {  #calculate weighted mean for each quarter for all columns
        temp <- cbind(factor=temp$factorn,temp$factorn*temp[(match("surveyQ",colnames(temp))+1):ncol(temp)])
        #temp <- colSums(temp, na.rm=TRUE, dims = 1)/sum(temp$factor, na.rm=TRUE)
        #calculate the sum(wi*xi)/sum(wi)
        temp <- colSums(temp, na.rm=TRUE, dims = 1)/    
            sapply(colnames(temp), function(x) sum(temp$factor[!is.na(temp[colnames(temp) == x])]))
        #weight only by those that responded to a specific question
        return(temp)
    }
    
    w.confidence <- as.data.frame(t(sapply(levels(data$surveyQ), function(kwartaal) weeg(data[data$surveyQ==kwartaal,]))))
    w.confidence$Conf_cc <- rowMeans(w.confidence[,c("Q1","Q2A","Q3A","Q4A","Q6A")],na.rm = TRUE, dims = 1)
    w.confidence$Conf_fl <- rowMeans(w.confidence[,c("Q2P","Q3P","Q4P","Q6P")],na.rm = TRUE, dims = 1)
    #w.confidence$Conf_all <- rowMeans(w.confidence[,c("Q1","Q2A","Q3A","Q4A","Q5A","Q6A","Q2P","Q3P","Q4P","Q5P","Q6P")],na.rm = TRUE, dims = 1)
    w.confidence <- merge(datums,w.confidence,by.x="Date",by.y="row.names", all=TRUE)[,-3]
    w.confidence[,14:15] <- na.approx(w.confidence[,14:15],na.rm = FALSE)
    
    return(w.confidence)
}



#Use PCA:
#pca_conf <- function(data) {
#    confidence <- aggregate(data[,(match("surveyQ",colnames(data))+1):ncol(data)], by=list(data$surveyQ), FUN=mean, na.rm=TRUE)
#    concc <- scale(confidence[,c("Q1","Q2A","Q3A","Q4A","Q5A","Q6A")])
#    concc[is.na(concc)] <- 0
#    confidence$Conf_cc <- princomp(concc[,c(1:6)])$scores[,1]
#    concc <- scale(confidence[,c("Q2P","Q3P","Q4P","Q5P","Q6P")])
#    concc[is.na(concc)] <- 0
#    confidence$Conf_fl <- princomp(concc[,c(1:5)])$scores[,1]
#    confidence <- merge(datums,confidence,by.x="Date",by.y="Group.1", all=TRUE)
#    confidence[,14:15] <- na.approx(confidence[,14:15],na.rm = FALSE)
#    return(confidence)
#}


#pca_wconf <- function(data) {
    ##Weighted versions
#    weeg <- function(temp) {  #calculate weighted mean for each quarter for all columns
#        temp <- cbind(factor=temp$factor,temp$factor*temp[(match("surveyQ",colnames(temp))+1):ncol(temp)])
        #temp <- colSums(temp, na.rm=TRUE, dims = 1)/sum(temp$factor, na.rm=TRUE)
        #calculate the sum(wi*xi)/sum(wi)
#        temp <- colSums(temp, na.rm=TRUE, dims = 1)/    
#            sapply(colnames(temp), function(x) sum(temp$factor[!is.na(temp[colnames(temp) == x])]))
        #weight only by those that responded to a specific question
#        return(temp)
#    }
    
#    w.confidence <- as.data.frame(t(sapply(levels(data$surveyQ), function(kwartaal) weeg(data[data$surveyQ==kwartaal,]))))
#    concc <- scale(w.confidence[,c("Q1","Q2A","Q3A","Q4A","Q5A","Q6A")])
#    concc[is.na(concc)] <- 0
#    w.confidence$Conf_cc <- princomp(concc[,c(1:6)])$scores[,1]
#    concc <- scale(w.confidence[,c("Q2P","Q3P","Q4P","Q5P","Q6P")])
#    concc[is.na(concc)] <- 0
#    w.confidence$Conf_fl <- princomp(concc[,c(1:5)])$scores[,1]
#    w.confidence <- merge(datums,w.confidence,by.x="Date",by.y="row.names", all=TRUE)[,-3]
#    w.confidence[,14:15] <- na.approx(w.confidence[,14:15],na.rm = FALSE)
#    
#    return(w.confidence)
#}


geweeg2 <- function(sektor=all) {
    
    weeg2 <- function(temp) {  #calculate weighted mean for each quarter for all columns
        sectorw=temp$sectorw[1]
        temp <- cbind(firmw=temp$weight,temp$weight*temp[(match("surveyQ",colnames(temp))+1):ncol(temp)])
        #temp <- colSums(temp, na.rm=TRUE, dims = 1)/sum(temp$factor, na.rm=TRUE)
        #calculate the sum(wi*xi)/sum(wi)
        temp <- colSums(temp, na.rm=TRUE, dims = 1)/    
            sapply(colnames(temp), function(x) sum(temp$firmw[!is.na(temp[colnames(temp) == x])]))
        temp <- c(sectorw,temp)
        #weight only by those that responded to a specific question
        return(temp)
    }
    
    man <- BER[BER$sector %in% sektor,]
    sector <- data.frame()
    for(kwartaal in levels(man$surveyQ)) {
        sector <- rbind(sector,weeg2(man[man$surveyQ==kwartaal,]))
    }
    
    sector[,2] <- levels(man$surveyQ)
    colnames(sector) <- colnames(man)[c(2,8:ncol(man))]
    sector <- merge(datums,sector, by.x="Date", by.y="surveyQ", all=TRUE)
    #sector <- as.data.frame(t(sapply(levels(man$surveyQ), function(kwartaal) weeg(man[man$surveyQ==kwartaal,]))))
    #sector[,-1:-2] <- na.approx(sector[,-1:-2],na.rm = FALSE)
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
    for(j in 4:14) {
        for(i in 1:95) {
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
    saam$Conf_cc <- rowMeans(saam[,c("Q1","Q2A","Q3A","Q4A","Q5A","Q6A")],na.rm = TRUE, dims = 1)
    saam$Conf_fl <- rowMeans(saam[,c("Q2P","Q3P","Q4P","Q5P","Q6P")],na.rm = TRUE, dims = 1)
    #concc <- scale(saam[,c("Q1","Q2A","Q3A","Q4A","Q5A","Q6A")])
    #concc[is.na(concc)] <- 0
    #saam$Conf_cc <- -1*princomp(concc[,c(1:6)])$scores[,1]
    #concc <- scale(saam[,c("Q2P","Q3P","Q4P","Q5P","Q6P")])
    #concc[is.na(concc)] <- 0
    #saam$Conf_fl <- -1*princomp(concc[,c(1:5)])$scores[,1]    
    saam[,14:15] <- na.approx(saam[,14:15],na.rm = FALSE)
    
    return(saam)
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


BER$sectorw <- BER$factor/BER$weight
BER <- BER[,c(1:4,19,5:18)]
w.indicators.M_2 <- maak.index(manufacturings)
w.indicators.B_2 <- maak.index(constructs)
w.indicators.T_2 <- maak.index(trades)
w.indicators.S_2 <- maak.index(servicess)



#indicators.B1 <- pca_conf(BER[BER$Sector=="Construction",])
#w.indicators.B1 <- pca_wconf(BER[BER$Sector=="Construction",])

#index_plot <- cbind(w.indicators.M[,c(2,15)],w.indicators.M_2[,c(15)])#,(GDPdata$ManConf-50)/50, GDPgrowth4$Manufacturing)
index_plot <- cbind(w.indicators.B[,c(2,15)],w.indicators.B_2[,c(15)])#,(GDPdata$BuildConf-50)/50, GDPgrowth4$Construction)
index_plot <- cbind(w.indicators.T[,c(2,15)],w.indicators.T_2[,c(15)])#,(GDPdata$RetailConf-50)/50, GDPgrowth4$Trade)
index_plot <- cbind(w.indicators.S[,c(2,15)],w.indicators.S_2[,c(15)])#,(GDPdata$BER_BCI-50)/50, GDPgrowth4$Services)
colnames(index_plot) <- c("Date","1-step","2-step")#,"BER","RGDP")
#index_plot[,-1] <- scale(index_plot[,-1])
index_plot <- melt(index_plot, id="Date")  # convert to long format
g <- ggplot(index_plot, aes(x=Date,y=value,group=variable,colour=variable)) 
g <- g + geom_line()
g <- g + theme(legend.title=element_blank())
g <- g + ggtitle("Manufacturing") 
g <- g + ylab("Indicator") + xlab("")
g <- g + theme(legend.position="bottom")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g

cor(index_plot[,-1])
##=================================##
seisoen <- function(product) {
    for(i in 2:ncol(product)) {
        product[,i] <- na.locf(product[,i], na.rm=FALSE, fromLast=TRUE)
        m <- ts(product[,i],start=c(1992,01), end= c(2016,3), frequency = 4)
        dec <- decompose(m, "additive")
        product[,i] <- as.numeric(m - dec$seasonal)
    }
    return(product)
}

##Weighted versions
weights <- GDPdata[,c(1:4,6)]
activity <- cbind(w.indicators.M[,c(2,14)],w.indicators.B[,14],w.indicators.T[,14],w.indicators.S[,14])
colnames(activity) <- c("Date","Manufacturing","Construction","Trade","Services")
conf <- cbind(w.indicators.M[,c(2,15)],w.indicators.B[,15],w.indicators.T[,15],w.indicators.S[,15])
colnames(conf) <- c("Date","Manufacturing","Construction","Trade","Services")

activity <- seisoen(activity)
conf <- seisoen(conf)
activity[1:5,3] <- NA
conf[1:5,3] <- NA
activity[1,4] <- NA
conf[1,4] <- NA
activity[1:53,5] <- NA
conf[1:53,5] <- NA

#create weighted means by GDP share
activity$Activity <- sapply(activity$Date, function(x) weighted.mean(activity[which(activity$Date==x),c(2:5)], 
                                                                     weights[weights$Date==x,-1],na.rm=TRUE))
conf$Confidence <- sapply(conf$Date, function(x) weighted.mean(conf[which(conf$Date==x),c(2:5)], 
                                                               weights[weights$Date==x,-1],na.rm=TRUE))
w.indicators <- cbind(activity[,c(1,6)],conf[,6])
#w.indicators <- seisoen(w.indicators)
colnames(w.indicators) <- c("Date","Activity","Confidence")
#w.indicators$C <- -1*princomp(w.indicators[,2:3])$scores[,1]



weights <- GDPdata[,c(1:4,6)]
index_plot <- weights
index_plot[,-1] <- na.locf(index_plot[,-1],fromLast = TRUE)
index_plot[,2:5] <- index_plot[,2:5]/rowSums(index_plot[,2:5],na.rm=TRUE)
index_plot <- melt(index_plot, id="Date")
g <- ggplot(index_plot, aes(x=Date,y=value,group=variable,fill=variable)) 
g <- g + geom_bar(stat="identity")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + scale_fill_discrete(name="Sub-sector")
g <- g + scale_y_continuous(labels=comma)
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g <- g + ylab("Weights") + xlab("Date")
g <- g + guides(fill = guide_legend(reverse = TRUE))
g


#weights <- GDPdata[,c(1:4,6)]
#activity_2 <- cbind(w.indicators.M_2[,c(2,14)],w.indicators.B_2[,14],w.indicators.T_2[,14],w.indicators.S_2[,14])
#colnames(activity_2) <- c("Date","Manufacturing","Construction","Trade","Services")
#conf_2 <- cbind(w.indicators.M_2[,c(2,15)],w.indicators.B_2[,15],w.indicators.T_2[,15],w.indicators.S_2[,15])
#colnames(conf_2) <- c("Date","Manufacturing","Construction","Trade","Services")

#create weighted means by GDP share
#activity_2$Activity <- sapply(activity_2$Date, function(x) weighted.mean(activity_2[which(activity_2$Date==x),c(2:5)], 
#                                                                     weights[weights$Date==x,-1],na.rm=TRUE))
#conf_2$Confidence <- sapply(conf_2$Date, function(x) weighted.mean(conf_2[which(conf_2$Date==x),c(2:5)], 
#                                                               weights[weights$Date==x,-1],na.rm=TRUE))
#w.indicators_2 <- cbind(activity_2[,c(1,6)],conf_2[,6])
#colnames(w.indicators_2) <- c("Date","Activity","Confidence")


#w.indicators[,-1] <- scale(w.indicators[,-1])
#un <- w.indicators #un <- na.locf(uncert_indices)
#un[is.na(un)] <- 0
#w.indicators$CC <- rowMeans(w.indicators[,c(2:3)],na.rm = TRUE)
#w.indicators$CC <- princomp(un[,c(2:3)])$scores[,1]*-1


index_plot <- cbind(activity[,1:2],conf[,2])
#index_plot <- w.indicators.M[,c(2,14:15)]
#colnames(index_plot) <- c("Date","Activity","Confidence")
colnames(index_plot) <- c("Date","Confidence_Current","Confidence_Expected")
index_plot <- melt(index_plot, id="Date")  # convert to long format
g1 <- ggplot(index_plot, aes(x=Date,y=value,group=variable,colour=variable)) 
g1 <- g1 + geom_line()
g1 <- g1 + theme(legend.title=element_blank())
g1 <- g1 + ggtitle("Manufacturing") 
g1 <- g1 + ylab("Indicator") + xlab("")
g1 <- g1 + theme(legend.position="none")
g1 <- g1 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g1 <- g1 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

index_plot <- cbind(activity[,c(1,3)],conf[,3])
#index_plot <- w.indicators.B[,c(2,14:15)]
#colnames(index_plot) <- c("Date","Activity","Confidence")
colnames(index_plot) <- c("Date","Confidence_Current","Confidence_Expected")
index_plot <- melt(index_plot, id="Date")  # convert to long format
g2 <- ggplot(index_plot, aes(x=Date,y=value,group=variable,colour=variable)) 
g2 <- g2 + geom_line()
g2 <- g2 + theme(legend.title=element_blank())
g2 <- g2 + ggtitle("Construction") 
g2 <- g2 + ylab("") + xlab("")
g2 <- g2 + theme(legend.position="none")
g2 <- g2 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g2 <- g2 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

index_plot <- cbind(activity[,c(1,4)],conf[,4])
#index_plot <- w.indicators.T[,c(2,14:15)]
#colnames(index_plot) <- c("Date","Activity","Confidence")
colnames(index_plot) <- c("Date","Confidence_Current","Confidence_Expected")
index_plot <- melt(index_plot, id="Date")  # convert to long format
g3 <- ggplot(index_plot, aes(x=Date,y=value,group=variable,colour=variable)) 
g3 <- g3 + geom_line()
g3 <- g3 + theme(legend.title=element_blank())
g3 <- g3 + ggtitle("Trade") 
g3 <- g3 + ylab("Indicator") + xlab("")
g3 <- g3 + theme(legend.position="bottom",plot.margin=unit(c(-0.5,0.4,0,0.4), "cm"))
g3 <- g3 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g3 <- g3 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

index_plot <- cbind(activity[,c(1,5)],conf[,5])
#index_plot <- w.indicators.S[-1:-52,c(2,14:15)]
#colnames(index_plot) <- c("Date","Activity","Confidence")
colnames(index_plot) <- c("Date","Confidence_Current","Confidence_Expected")
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
#colnames(index_plot) <- c("Date","Activity","Confidence")
colnames(index_plot) <- c("Date","Confidence_Current","Confidence_Expected")
g <- ggplot(index_plot) 
g <- g + geom_line(aes(x=Date, y=Confidence_Current, colour="Confidence_Current"), size = 1)
g <- g + geom_line(aes(x=Date, y=Confidence_Expected, colour="Confidence_Expected"), size = 1)
#g <- g + geom_line(aes(x=Date, y=C, colour="C"), size = 1)
g <- g + labs(color="Legend text")
g <- g + geom_rect(data=recessions.df, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='grey', alpha=0.5)
g <- g + ylab("Indicator") + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) 
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"), expand=c(0,0), 
                      limits = as.Date(c("1990-12-31", NA)))
g <- g + theme(legend.position="bottom")
g


conf_indices <- cbind(w.indicators,GDPdata$BER_BCI, GDPgrowth4$SACCI,GDPgrowth4$RGDP)
#colnames(conf_indices) <- c("Date","Activity","Confidence","BER_BCI","SACCI_Growth","RGDP_Growth")
colnames(conf_indices) <- c("Date","Confidence_Current","Confidence_Expected","BER_BCI","SACCI_Growth","RGDP_Growth")

conf_indices1 <- cbind(w.indicators,GDPdata$BER_BCI, GDPdata$SACCI,GDPgrowth4$RGDP)
#colnames(conf_indices1) <- c("Date","Activity","Confidence","BER_BCI","SACCI_BCI","RGDP_Growth")
colnames(conf_indices1) <- c("Date","Confidence_Current","Confidence_Expected","BER_BCI","SACCI_BCI","RGDP_Growth")

#conf_indices <- cbind(w.indicators_2,GDPdata$BER_BCI, GDPdata$SACCI_G,GDPgrowth4$RGDP)
#colnames(conf_indices) <- c("Date","Activity","Confidence","BER_BCI","SACCI_Growth","RGDP_Growth")

index_plot <- conf_indices1
index_plot[,2:6] <- scale(index_plot[,2:6])
g <- ggplot(index_plot) 
g <- g + geom_line(aes(x=Date, y=Confidence_Current, colour="Confidence_Current", linetype="Confidence_Current", size = "Confidence_Current"))
g <- g + geom_line(aes(x=Date, y=Confidence_Expected, colour="Confidence_Expected", linetype="Confidence_Expected", size = "Confidence_Expected"))
g <- g + geom_line(aes(x=Date, y=BER_BCI, colour="BER_BCI", linetype="BER_BCI", size = "BER_BCI"))
g <- g + geom_line(aes(x=Date, y=SACCI_BCI, colour="SACCI_BCI", linetype="SACCI_BCI", size = "SACCI_BCI"))
g <- g + geom_line(aes(x=Date, y=RGDP_Growth, colour="RGDP_Growth", linetype="RGDP_Growth", size = "RGDP_Growth"))
g <- g + scale_linetype_manual(values=c("twodash","solid","solid","dashed","longdash"))
g <- g + scale_size_manual(values=c(0.71,1.1,1.1,1.1,0.71))
g <- g + scale_colour_manual(values=c("#A3A500","#F8766D","#00BFC4","#E76BF3","#00BF7D"))
g <- g + labs(color="Legend text", linetype="Legend text", size="Legend text")
g <- g + geom_rect(data=recessions.df, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='grey', alpha=0.5)
g <- g + ylab("Standardised Indicator") + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) 
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"), expand=c(0,0),limits = as.Date(c("1990-12-31", NA)))
g <- g + theme(legend.position="bottom")
g


index_plot <- conf_indices1
index_plot[,2:6] <- scale(index_plot[,2:6])
g <- ggplot(index_plot) 
g <- g + geom_line(aes(x=Date, y=Current, colour="Current", linetype="Current", size = "Current"))
g <- g + geom_line(aes(x=Date, y=Expected, colour="Expected", linetype="Expected", size = "Expected"))
g <- g + geom_line(aes(x=Date, y=BER_BCI, colour="BER_BCI", linetype="BER_BCI", size = "BER_BCI"))
g <- g + geom_line(aes(x=Date, y=SACCI_BCI, colour="SACCI_BCI", linetype="SACCI_BCI", size = "SACCI_BCI"))
g <- g + geom_line(aes(x=Date, y=RGDP_Growth, colour="RGDP_Growth", linetype="RGDP_Growth", size = "RGDP_Growth"))
g <- g + scale_linetype_manual(values=c("twodash","solid","solid","dashed","longdash"))
g <- g + scale_size_manual(values=c(0.71,1.1,1.1,1.1,0.71))
g <- g + labs(color="Legend text", linetype="Legend text", size="Legend text")
g <- g + geom_rect(data=recessions.df, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='grey', alpha=0.5)
g <- g + ylab("Indicator") + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) 
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"), expand=c(0,0),limits = as.Date(c("1990-12-31", NA)))
g <- g + theme(legend.position="bottom")
g

source("corstarsl.R")
xt <- xtable(corstarsl(conf_indices[,-1]), caption="Correlations of confidence indicators and real GDP growth")
print(xt, "latex",comment=FALSE, caption.placement = getOption("xtable.caption.placement", "top"), scalebox = 0.8)


Confidence_Current <- conf_indices[,2]
Confidence_Expected <- conf_indices[,3]
BER_BCI <- conf_indices[,4]
SACCI_Growth <- conf_indices[,5] 
SACCI_Growth <- na.locf(SACCI_Growth,fromLast = TRUE)
RGDP_Growth <- conf_indices[,6]

par(mfrow=c(2,2))
ccf(Confidence_Current, RGDP_Growth, na.action = na.pass, ylim=c(-0.3, 0.8), 
    ylab = "Correlation", xlab = "Number of Lags")
ccf(Confidence_Expected, RGDP_Growth, na.action = na.pass, ylim=c(-0.3, 0.8), 
    ylab = "Correlation", xlab = "Number of Lags")
ccf(BER_BCI, RGDP_Growth, na.action = na.pass, ylim=c(-0.3, 0.8), 
    ylab = "Correlation", xlab = "Number of Lags")
ccf(SACCI_Growth, RGDP_Growth, na.action = na.pass, ylim=c(-0.3, 0.8), 
    ylab = "Correlation", xlab = "Number of Lags")


manufac <- cbind(activity[,c(1,2)],conf[,2],(GDPdata$ManuConf-50)/50, GDPgrowth4$Manufacturing)
#manufac <- cbind(indicators.M[,c(2,14:15)],(GDPdata$ManuConf-50)/50, GDPgrowth4$Manufacturing)
#colnames(manufac) <- c("Date","Activity","Confidence","BER_BCI","RGDP")
colnames(manufac) <- c("Date","Confidence_Current","Confidence_Expected","BER_BCI","RGDP_Growth")

construct <- cbind(activity[,c(1,3)],conf[,3],(GDPdata$BuildingConf-50)/50, GDPgrowth4$Construction)
#construct <- cbind(indicators.B[,c(2,14:15)],(GDPdata$BuildingConf-50)/50, GDPgrowth4$Construction)
#colnames(construct) <- c("Date","Activity","Confidence","BER_BCI","RGDP_Growth")
colnames(construct) <- c("Date","Confidence_Current","Confidence_Expected","BER_BCI","RGDP_Growth")

GDPdata$TradeConf <- rowMeans(GDPdata[,c(14,15,17)])
trade <- cbind(activity[,c(1,4)],conf[,4],(GDPdata$TradeConf-50)/50, GDPgrowth4$Trade)
#trade <- cbind(indicators.T[,c(2,14:15)],(GDPdata$TradeConf-50)/50, GDPgrowth4$Trade)
#colnames(trade) <- c("Date","Activity","Confidence","BER_BCI","RGDP")
colnames(trade) <- c("Date","Confidence_Current","Confidence_Expected","BER_BCI","RGDP_Growth")

services <- cbind(activity[,c(1,5)],conf[,5], GDPgrowth4$Services)
#services <- cbind(indicators.S[,c(2,14:15)], GDPgrowth4$Services)
#colnames(services) <- c("Date","Activity","Confidence","RGDP")
colnames(services) <- c("Date","Confidence_Current","Confidence_Expected","RGDP_Growth")
services$BER_BCI <- 1
services <- services[,c(1,2,3,5,4)]


#manufac <- cbind(w.indicators.M_2[,c(2,14:15)],(GDPdata$ManuConf-50)/50, GDPgrowth4$Manufacturing)
#colnames(manufac) <- c("Date","Activity","Confidence","BER_BCI","RGDP")

#construct <- cbind(w.indicators.B_2[,c(2,14:15)],(GDPdata$BuildingConf-50)/50, GDPgrowth4$Construction)
#colnames(construct) <- c("Date","Activity","Confidence","BER_BCI","RGDP_Growth")

#GDPdata$TradeConf <- rowMeans(GDPdata[,c(14,15,17)])
#trade <- cbind(w.indicators.T_2[,c(2,14:15)],(GDPdata$TradeConf-50)/50, GDPgrowth4$Trade)
#colnames(trade) <- c("Date","Activity","Confidence","BER_BCI","RGDP")

#services <- cbind(w.indicators.S_2[,c(2,14:15)], GDPgrowth4$Services)
#colnames(services) <- c("Date","Activity","Confidence","RGDP")
#services$BER_BCI <- 1
#services <- services[,c(1,2,3,5,4)]

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
g3 <- g3 + theme(legend.position="bottom")
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
g4 <- g4 + theme(legend.position="bottom")
g4 <- g4 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g4 <- g4 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

library(gridExtra)

grid_arrange_shared_legend <- function(...) {
    plots <- list(...)
    g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
    legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
    lheight <- sum(legend$height)
    grid.arrange(
        do.call(arrangeGrob, lapply(plots, function(x)
            x + theme(legend.position="none"))),
        legend,
        ncol = 1,
        heights = unit.c(unit(1, "npc") - lheight, lheight))
}

#grid_arrange_shared_legend(p1, p2, p3, p4)
grid_arrange_shared_legend(g1, g2, g3, g4, ncol=2, nrow =2)


source("corstarsl.R")
xt1 <- cbind(corstarsl(manufac[,-1]),corstarsl(construct[,-1]))
xt2 <- cbind(corstarsl(trade[,-1]),corstarsl(services[,-1]))
xt1 <- sapply(xt1,as.character)
xt2 <- sapply(xt2,as.character)
xt2[3,4:5] <- ""
xt2[4,6] <- ""
xt1[1,] <- c("Current","Expected","BER_BCI","Current","Expected","BER_BCI")
xt2[1,] <- c("Current","Expected","BER_BCI","Current","Expected","BER_BCI")
colnames(xt1) <- c(" ","Manufacturing"," "," ","Construction"," ")
colnames(xt2) <- c(" ","Trade"," "," ","Services"," ")
row.names(xt1) <- c(" ","Expected","BER_BCI","RGDP_Growth")
row.names(xt2) <- c(" ","Expected","BER_BCI","RGDP_Growth")

xt <- xtable(xt1, caption="Correlations of sectoral confidence and growth")
print(xt, "latex",comment=FALSE, caption.placement = getOption("xtable.caption.placement", "top"), scalebox = 0.8)
xt <- xtable(xt2)
print(xt, "latex",comment=FALSE, caption.placement = getOption("xtable.caption.placement", "top"), scalebox = 0.85)


calc_ccf <- function(data, serv=0) {
    Confidence_Current <- data[,2]
    Confidence_Expected <- data[,3]
    BER_BCI <- data[,4]
    RGDP_Growth <- data[,5]
    
    par(mfrow=c(2,2))
    ccf(Confidence_Current, RGDP_Growth, na.action = na.omit, ylim=c(-0.3, 0.8), 
        ylab = "Correlation", xlab = "Number of Lags")
    ccf(Confidence_Expected, RGDP_Growth, na.action = na.omit, ylim=c(-0.3, 0.8), 
        ylab = "Correlation", xlab = "Number of Lags")
    if(serv==0) { ccf(BER_BCI, RGDP_Growth, na.action = na.pass, ylim=c(-0.3, 0.8), 
                      ylab = "Correlation", xlab = "Number of Lags") }
}

calc_ccf(manufac)
#calc_ccf(construct)
#calc_ccf(trade)
#calc_ccf(services, 1)


#Turning Points
suppressMessages(library(BCDating))

#conf_indices[67,3] <- -0.15

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
#dat <- BBQ(ts(conf_indices[,5],start =c(1992,1),end=c(2015,3),frequency=4), mincycle = 4, minphase = 2, name="Activity")
#tp4 <- as.data.frame(show(dat))[,-3]
dat <- BBQ(ts(GDPdata$SACCI_BCI,start =c(1992,1),end=c(2015,3),frequency=4), mincycle = 5, minphase = 2, name="Activity")
tp4 <- as.data.frame(show(dat))[,-3]
dat <- BBQ(ts(conf_indices[,6],start =c(1992,1),end=c(2015,3),frequency=4), mincycle = 5, minphase = 2, name="Activity")
tp5 <- as.data.frame(show(dat))[,-3]

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


index_plot <- conf_indices[,c(1,2)]
index_plot[,2] <- scale(index_plot[,2])
colnames(index_plot) <- c("Date","Confidence")
g1 <- ggplot(index_plot) 
g1 <- g1 + geom_line(aes(x=Date, y=Confidence, colour="Confidence"), size = 0.5)
g1 <- g1 + labs(color="Legend text")
g1 <- g1 + geom_rect(data=recessions.df, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=0), fill='grey', alpha=0.5)
g1 <- g1 + geom_rect(data=tp1, aes(xmin=Peak, xmax=Trough, ymin=0, ymax=+Inf), fill='blue', alpha=0.5)
g1 <- g1 + ylab("Indicator") + xlab("")
g1 <- g1 + ggtitle("Confidence_Current") 
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
g2 <- g2 + ggtitle("Confidence_Expected") 
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

index_plot <- GDPdata[,c("Date","SACCI_BCI")]
#index_plot <- conf_indices[,c(1,5)]
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


#---------------------------------------
#Concordance
S <- conf_indices
d <- NULL
for(i in 1:nrow(tp1)) {
    d <- c(d,seq(tp1[i,3], tp1[i,4], by="day")[-1]) 
}
S[,2] <- 1
S[S$Date %in% d,2] <- 0

d <- NULL
for(i in 1:nrow(tp2)) {
    d <- c(d,seq(tp2[i,3], tp2[i,4], by="day")[-1]) 
}
S[,3] <- 1
S[S$Date %in% d,3] <- 0

d <- NULL
for(i in 1:nrow(tp3)) {
    d <- c(d,seq(tp3[i,3], tp3[i,4], by="day")[-1]) 
}
S[,4] <- 1
S[S$Date %in% d,4] <- 0

d <- NULL
for(i in 1:nrow(tp4)) {
    d <- c(d,seq(tp4[i,3], tp4[i,4], by="day")[-1]) 
}
S[,5] <- 1
S[S$Date %in% d,5] <- 0

d <- NULL
for(i in 1:nrow(tp5)) {
    d <- c(d,seq(tp5[i,3], tp5[i,4], by="day")[-1]) 
}
S[,6] <- 1
S[S$Date %in% d,6] <- 0


d <- NULL
for(i in 1:nrow(recessions.df)) {
    d <- c(d,seq(recessions.df[i,1], recessions.df[i,2], by="day")[-1]) 
}
S$SARB <- 1
S[S$Date %in% d,7] <- 0

#S[1:60,2:7] <- NA
#S[1:127,6] <- NA

Concord_1<-NULL
Concord_2<-NULL
p_1<-NULL
p_2<-NULL

for(k in 0:3) {
    Concord1<-NULL
    Concord2<-NULL
    p.1<-NULL
    p.2<-NULL
    
    for(i in 2:5) {
        Concord1[i-1] <- (sum(S[1:(95-k),i]*S[(1+k):95,7], na.rm = TRUE)+sum((1-S[1:(95-k),i])*(1-S[(1+k):95,7]),na.rm = TRUE))/sum(!is.na(S[,i]))
        Concord2[i-1] <- (sum(S[1:(95-k),i]*S[(1+k):95,6], na.rm = TRUE)+sum((1-S[1:(95-k),i])*(1-S[(1+k):95,6]),na.rm = TRUE))/sum(!is.na(S[,i]))
        
        s_x <- S[1:(95-k),i]/sqrt(var(S[1:(95-k),i], na.rm = TRUE))/sqrt(var(S[(1+k):95,7], na.rm = TRUE))
        s_y1 <- S[(1+k):95,7]/sqrt(var(S[(1+k):95,7], na.rm = TRUE))/sqrt(var(S[1:(95-k),i], na.rm = TRUE))
        s_y2 <- S[(1+k):95,6]/sqrt(var(S[(1+k):95,6], na.rm = TRUE))/sqrt(var(S[1:(95-k),i], na.rm = TRUE))
        
        m <- lm(s_x ~ s_y1, na.action=na.exclude)
        p1 <- coeftest(m,vcov. = NeweyWest)[2,4]
        m <- lm(s_x ~ s_y1, na.action=na.exclude)
        p2 <- coeftest(m,vcov. = NeweyWest)[2,4]
        
        p.1 <- rbind(p.1,p1)
        p.2 <- rbind(p.2,p2)
    }
    Concord_1 <- cbind(Concord_1,Concord1)
    Concord_2 <- cbind(Concord_2,Concord2)
    
    p_1 <- cbind(p_1,p.1)
    p_2 <- cbind(p_2,p.2)
}

#Concord2[i-2] <- (sum(S[,i]*S[,2])+sum((1-S[,i])*(1-S[,2])))/nrow(S)    

Concord_1 <- t(round(Concord_1, digits = 4))
Concord_2 <- t(round(Concord_2, digits = 4))

p_1 <- t(round(p_1, digits = 6))
p_2 <- t(round(p_2, digits = 6))

mystars <- ifelse(p_1 < .01, "***", ifelse(p_1 < .05, "** ", ifelse(p_1 < .1, "* ", " ")))
Concord_1 <- matrix(paste(Concord_1, mystars, sep=""), ncol=4)
Concord_1 <- rbind(c("Confidence_Current","Confidence_Expected","BER_BCI","SACCI_BCI"),Concord_1)
row.names(Concord_1) <- c(" ","lead=0","lead=1","lead=2","lead=3")

mystars <- ifelse(p_2 < .01, "***", ifelse(p_2 < .05, "** ", ifelse(p_2 < .1, "* ", " ")))
Concord_2 <- matrix(paste(Concord_2, mystars, sep=""), ncol=4)
Concord_2 <- rbind(c("Confidence_Current","Confidence_Expected","BER_BCI","SACCI_BCI"),Concord_2)
row.names(Concord_2) <- c(" ","lead=0","lead=1","lead=2","lead=3")

#Concord_1 <- cbind(Concord_1,Concord_2)

xt <- xtable(Concord_1, caption="Concordance statistics")
#addtorow <- list()
#addtorow$pos <- list(0)
#addtorow$command <- paste0(paste0('& \\multicolumn{4}{c}{  SARB Cycle  } & \\multicolumn{4}{c}{ RGDP Growth cycle }', collapse=''), '\\\\')

print(xt, "latex",comment=FALSE, add.to.row=addtorow, include.rownames=TRUE, include.colnames=F,
      caption.placement = getOption("xtable.caption.placement", "top"), scalebox = 0.8)



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
    w.uncertainty$Uncert_cc <- rowMeans(w.uncertainty[,c("Q2A","Q3A","Q4A","Q5A","Q6A")],na.rm = TRUE, dims = 1)
    w.uncertainty$Uncert_fl <- rowMeans(w.uncertainty[,c("Q2P","Q3P","Q4P","Q5P","Q6P")],na.rm = TRUE, dims = 1)
    w.uncertainty <- merge(datums,w.uncertainty,by.x="Date",by.y="row.names", all=TRUE)[,-3]
    w.uncertainty[,14:15] <- na.approx(w.uncertainty[,14:15],na.rm = FALSE)
    for(t in 2:nrow(w.uncertainty)) { w.uncertainty$Disp[t-1] <- w.uncertainty$Uncert_fl[t-1]/w.uncertainty$Uncert_cc[t] }
    w.uncertainty$Disp[t] <- NA
    return(w.uncertainty)
}


calc_wuncert <- function(data) {
    ##Weighted versions
    weeg.2 <- function(temp) {  #calculate weighted standard deviation for each quarter for all columns
        temp <- cbind(factor=temp$factorn,temp$factorn*temp[(match("surveyQ",colnames(temp))+1):ncol(temp)])
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
    w.uncertainty$Uncert_cc <- rowMeans(w.uncertainty[,c("Q2A","Q3A","Q4A","Q5A","Q6A")],na.rm = TRUE, dims = 1)
    w.uncertainty$Uncert_fl <- rowMeans(w.uncertainty[,c("Q2P","Q3P","Q4P","Q5P","Q6P")],na.rm = TRUE, dims = 1)
    w.uncertainty <- merge(datums,w.uncertainty,by.x="Date",by.y="row.names", all=TRUE)[,-3]
    w.uncertainty[,14:15] <- na.approx(w.uncertainty[,14:15],na.rm = FALSE)
    for(t in 2:nrow(w.uncertainty)) { w.uncertainty$Disp[t-1] <- w.uncertainty$Uncert_fl[t-1]/w.uncertainty$Uncert_cc[t] }
    w.uncertainty$Disp[t] <- NA
    return(w.uncertainty)
}



#PCA versions:
pca_uncert <- function(data) {
    uncertainty <- aggregate(data[,(match("surveyQ",colnames(data))+1):ncol(data)], by=list(data$surveyQ), FUN=se)
    unc <- scale(uncertainty[,c("Q2A","Q3A","Q4A","Q5A","Q6A")])
    unc[is.na(unc)] <- 0
    uncertainty$Uncert_cc <- princomp(unc[,c(1:5)])$scores[,1]
    unc <- scale(uncertainty[,c("Q2P","Q3P","Q4P","Q5P","Q6P")])
    unc[is.na(unc)] <- 0
    uncertainty$Uncert_fl <- princomp(unc[,c(1:5)])$scores[,1]
    uncertainty <- merge(datums,uncertainty,by.x="Date",by.y="Group.1", all=TRUE)
    uncertainty[,14:15] <- na.approx(uncertainty[,14:15],na.rm = FALSE)
    for(t in 2:nrow(uncertainty)) { uncertainty$Disp[t-1] <- uncertainty$Uncert_fl[t-1]/uncertainty$Uncert_cc[t] }
    uncertainty$Disp[t] <- NA
    return(uncertainty)
}

pca_wuncert <- function(data) {
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
    unc <- scale(w.uncertainty[,c("Q2A","Q3A","Q4A","Q5A","Q6A")])
    unc[is.na(unc)] <- 0
    w.uncertainty$Uncert_cc <- princomp(unc[,c(1:5)])$scores[,1]
    unc <- scale(w.uncertainty[,c("Q2P","Q3P","Q4P","Q5P","Q6P")])
    unc[is.na(unc)] <- 0
    w.uncertainty$Uncert_fl <- princomp(unc[,c(1:5)])$scores[,1]
    w.uncertainty <- merge(datums,w.uncertainty,by.x="Date",by.y="row.names", all=TRUE)[,-3]
    w.uncertainty[,14:15] <- na.approx(w.uncertainty[,14:15],na.rm = FALSE)
    for(t in 2:nrow(w.uncertainty)) { w.uncertainty$Disp[t-1] <- w.uncertainty$Uncert_fl[t-1]/w.uncertainty$Uncert_cc[t] }
    w.uncertainty$Disp[t] <- NA
    return(w.uncertainty)
}


geweeg2 <- function(sektor=all) {
    
    weeg2 <- function(temp) {  #calculate weighted mean for each quarter for all columns
        sectorw=temp$sectorw[1]
        temp <- cbind(firmw=temp$weight,temp$weight*temp[(match("surveyQ",colnames(temp))+1):ncol(temp)])
        #calculate total that responded up (1) and down (-1) over sum(wi) = fractions up and down
        frac.up <- sapply(1:ncol(temp), function(x) sum(temp[which(temp[,x]>0),x],na.rm=TRUE))/
            sapply(colnames(temp), function(x) sum(temp$firmw[!is.na(temp[colnames(temp) == x])]))
        frac.dn <- sapply(1:ncol(temp), function(x) sum(temp[which(temp[,x]<0),x],na.rm=TRUE))/
            sapply(colnames(temp), function(x) sum(temp$firmw[!is.na(temp[colnames(temp) == x])]))
        #weight only by those that responded to a specific question 
        ind <- sqrt(frac.up-frac.dn-(frac.up+frac.dn)^2)        #this is the standard devation
        ind <- c(sectorw,ind)
        return(ind)
        
    }
    
    man <- BER[BER$sector %in% sektor,]
    sector <- data.frame()
    for(kwartaal in levels(man$surveyQ)) {
        sector <- rbind(sector,weeg2(man[man$surveyQ==kwartaal,]))
    }
    
    sector[,2] <- levels(man$surveyQ)
    colnames(sector) <- colnames(man)[c(2,7:ncol(man))]
    sector <- merge(datums,sector, by.x="Date", by.y="surveyQ", all=TRUE)
    #sector <- as.data.frame(t(sapply(levels(man$surveyQ), function(kwartaal) weeg(man[man$surveyQ==kwartaal,]))))
    #sector[,-1:-2] <- na.approx(sector[,-1:-2],na.rm = FALSE)
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
    for(j in 4:14) {
        for(i in 1:95) {
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
    saam$Uncert_cc <- rowMeans(saam[,c("Q2A","Q3A","Q4A","Q5A","Q6A")],na.rm = TRUE, dims = 1)
    saam$Uncert_fl <- rowMeans(saam[,c("Q2P","Q3P","Q4P","Q5P","Q6P")],na.rm = TRUE, dims = 1)
    saam[,14:15] <- na.approx(saam[,14:15],na.rm = FALSE)
    for(t in 2:nrow(saam)) { saam$Disp[t-1] <- saam$Uncert_fl[t-1]/saam$Uncert_cc[t] }
    saam$Disp[t] <- NA
    return(saam)
    
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


BER$sectorw <- BER$factor/BER$weight
BER <- BER[,c(1:4,18,5:17)]
w.uncertainty.M_2 <- maak.index(manufacturings)
w.uncertainty.B_2 <- maak.index(constructs)
w.uncertainty.T_2 <- maak.index(trades)
w.uncertainty.S_2 <- maak.index(servicess)



##Weighted versions
weights <- GDPdata[,c(1:4,6)]
w.uncertainty <- cbind(w.uncertainty.M[,c(2,16)],w.uncertainty.B[,16],w.uncertainty.T[,16],w.uncertainty.S[,16])
colnames(w.uncertainty) <- c("Date","Manufacturing","Construction","Trade","Services")
w.uncertainty[,-1] <- scale(w.uncertainty[,-1])

w.uncertainty$Dispersion <- sapply(w.uncertainty$Date,function(x) weighted.mean(w.uncertainty[which(w.uncertainty$Date==x),c(2:5)], 
                                                                                 weights[weights$Date==x,-1],na.rm=TRUE))

w.uncertainty <- seisoen(w.uncertainty)
w.uncertainty[1,3] <- NA
w.uncertainty[1:5,4] <- NA
w.uncertainty[1:53,5] <- NA

#uncertainty.M1 <- pca_uncert(BER[BER$Sector=="Manufacturing",])
#w.uncertainty.M1 <- pca_wuncert(BER[BER$Sector=="Manufacturing",])

#w.uncertainty.M1$Disp[c(23,32,55)] <- 0

index_plot <- cbind(w.uncertainty[,c(1,6)],uncertainty[,c(16)])
index_plot[,-1] <- scale(index_plot[,-1] )
colnames(index_plot) <- c("Date","disp","2-disp")
index_plot <- melt(index_plot, id="Date")  # convert to long format
g <- ggplot(index_plot, aes(x=Date,y=value,group=variable,colour=variable)) 
g <- g + geom_line()
g <- g + theme(legend.title=element_blank())
g <- g + ggtitle("Manufacturing") 
g <- g + ylab("Indicator") + xlab("")
g <- g + theme(legend.position="bottom")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g

cor(index_plot[-95,-1])

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
        error <- error[,c(2,8,20:24)]
        error <- error[rowSums(is.na(error[,3:7]))!=5, ]
        return(error)
    }
    
    errors <- data.frame()
    for(i in unique(data$id)){
        errors <- rbind(errors, exp.error(data[which(data$id==i),])) 
    }
    #errors <- rbind(errors,sapply(head(levels(data$id),1), function(i) exp.error(data[which(data$id==i),]) ) )
    return(errors)
}

#Save for speed
m_errors <- calc_errors(BER[BER$Sector=="Manufacturing",])
write.csv2(m_errors,"Manufacturing_errors3.csv")

b_errors <- calc_errors(BER[BER$Sector=="Construction",])
write.csv2(b_errors,"Building_errors3.csv")

t_errors <- calc_errors(BER[BER$Sector=="Trade",])
write.csv2(t_errors,"Trade_errors3.csv")

s_errors <- calc_errors(BER[BER$Sector=="Services",])
write.csv2(s_errors,"Services_errors3.csv")


#Read for speed
m_errors <- read.csv2("Manufacturing_errors3.csv", header=TRUE)[,-1]
m_errors$Datum <- as.Date(m_errors$Datum)
b_errors <- read.csv2("Building_errors3.csv", header=TRUE)[,-1]
b_errors$Datum <- as.Date(b_errors$Datum)
t_errors <- read.csv2("Trade_errors3.csv", header=TRUE)[,-1]
t_errors$Datum <- as.Date(t_errors$Datum)
s_errors <- read.csv2("Services_errors3.csv", header=TRUE)[,-1]
s_errors$Datum <- as.Date(s_errors$Datum)

calc_uncert.ee <- function(data) {
    idio.errors <- aggregate(data[,-1:-2], by=list(data$Datum), FUN=sd,na.rm = TRUE)
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
    #exp.errors[,3] <- c(NA,exp.errors[-99,3])
    #exp.errors[,4] <- c(NA,exp.errors[-99,4])
    return(exp.errors)
}

calc_wuncert.ee <- function(data) {
    ##Weighted versions
    weeg.3 <- function(data) {  #calculate weighted standard deviation for each quarter for all columns
        temp <- cbind(factor=data$factorn,data$factorn*data[,3:ncol(data)])
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
    #w.errors[,2] <- c(NA,w.errors[-99,2])
    #w.errors[,3] <- c(NA,w.errors[-99,3])
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

errors <- rbind(m_errors,b_errors,t_errors,s_errors)
uncert_error <- calc_uncert.ee(errors)

t_errors$total <- rowMeans(t_errors[,3:7],na.rm=TRUE)
BERplot <- aggregate(t_errors$total, by=list(t_errors$Datum), FUN = sd, na.rm=TRUE)#[-1:-5,]
#BERplot1 <- aggregate(BER.M$factor, by=list(BER.M$surveyQ), FUN = sum, na.rm=TRUE)
#BERplot$y <- BERplot$x/BERplot1$x
#BERplot <- aggregate(BER.M$factor, by=list(BER.M$surveyQ,BER.M$Sector), FUN = mean)
BERplot$Group.1 <- as.Date(as.yearqtr(BERplot$Group.1, format = "%YQ%q"), frac = 1)
g <- ggplot(BERplot, aes(x=Group.1, y=x))
g <- g + geom_line()
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + scale_fill_discrete(name="Sub-sector")
g <- g + scale_y_continuous(labels=comma)
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g <- g + ylab("Weights") + xlab("Date")
g <- g + guides(fill = guide_legend(reverse = TRUE))
g

#PCA Versions
pca_uncert.ee <- function(data) {
    idio.errors <- aggregate(data[,-1:-2], by=list(data$Datum), FUN=se)
    unc <- scale(idio.errors[,2:6])
    unc[is.na(unc)] <- 0
    idio.errors$idio <- princomp(unc[,c(1:5)])$scores[,1]
    idio.errors$idio <- na.approx(idio.errors$idio,na.rm=FALSE)
    
    agg.errors <- aggregate(data[,-1:-2], by=list(data$Datum), FUN= function(x) {mean(x, na.rm = TRUE)^2})
    unc <- scale(agg.errors[,2:6])
    unc[is.na(unc)] <- 0
    agg.errors$aggregate  <- princomp(unc[,c(1:5)])$scores[,1]
    agg.errors$aggregate <- na.approx(agg.errors$aggregate,na.rm=FALSE)
    
    exp.errors <- cbind(idio.errors[,c(1,7)],agg.errors[,7])
    colnames(exp.errors) <- c("Date","Uncert_Idiosyncratic","Uncert_Aggregate")
    exp.errors$Date <- as.Date(exp.errors$Date)
    exp.errors <- merge(datums,exp.errors,by.x="Datum",by.y="Date", all=TRUE)
    return(exp.errors)
}



pca_wuncert.ee <- function(data) {
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
    w.errors[,-1][is.na(w.errors[,-1])] <- 0
    w.errors$Idio  <- princomp(w.errors[,3:7])$scores[,1]
    w.errors$Aggr  <- princomp(w.errors[,9:13])$scores[,1]
    w.errors[,14:15] <- na.approx(w.errors[,14:15],na.rm=FALSE)
    w.errors <- w.errors[,c(1,14:15)]
    return(w.errors)
}


uncert_error.M1 <- pca_uncert.ee(m_errors)
w.uncert_error.M1 <- pca_wuncert.ee(m_errors)



geweeg.1 <- function(sektor=all) {
    
    weeg2 <- function(temp) {  #calculate weighted mean for each quarter for all columns
        sectorw=temp$sectorw[1]
        temp <- cbind(firmw=temp$weight,temp$weight*temp[(match("surveyQ",colnames(temp))+1):ncol(temp)])
        xbar <- colSums(temp, na.rm=TRUE, dims = 1)/
            sapply(colnames(temp), function(x) sum(temp$firmw[!is.na(temp[colnames(temp) == x])]))
        #this is the weighted standard devation: sum[wi*(xi-xbar)^2]/sum(wi) 
        #idio <- sqrt(sapply(colnames(temp), function(x) sum((temp[,x]-xbar[x])*(temp[,x]-xbar[x])*temp$firmw,na.rm=TRUE))/
        #                 sapply(colnames(temp), function(x) sum(temp$firmw[!is.na(temp[colnames(temp) == x])],na.rm=TRUE))) 
        aggr <- sapply(colnames(temp), function(x) xbar[x]*xbar[x]) 
        ind <- c(sectorw,aggr)
        return(ind)
    }
    
    man <- BER[BER$sector %in% sektor,]
    sector <- data.frame()
    for(kwartaal in levels(man$surveyQ)) {
        sector <- rbind(sector,weeg2(man[man$surveyQ==kwartaal,]))
    }
    
    sector[,2] <- levels(man$surveyQ)
    colnames(sector) <- colnames(man)[c(2,7:ncol(man))]
    sector <- merge(datums,sector, by.x="Date", by.y="surveyQ", all=TRUE)
    #sector <- as.data.frame(t(sapply(levels(man$surveyQ), function(kwartaal) weeg(man[man$surveyQ==kwartaal,]))))
    #sector[,-1:-2] <- na.approx(sector[,-1:-2],na.rm = FALSE)
    return(sector)
    
}




maak.index <- function(sektor=all,streek=streke) {
    lys <- list()
    t <- 0
    for(k in sektor) {
        t <- t+1
        lys[[t]] <- geweeg.1(k)
    }
    saam <- lys[[1]]
    for(j in 4:14) {
        for(i in 1:95) {
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
    saam$Aggr <- rowMeans(saam[,c("Q1","Q2A","Q3A","Q4A","Q5A","Q6A")],na.rm = TRUE, dims = 1)
    #saam$Uncert_fl <- rowMeans(saam[,c("Q2P","Q3P","Q4P","Q5P","Q6P")],na.rm = TRUE, dims = 1)
    saam[,14] <- na.approx(saam[,14],na.rm = FALSE)
    return(saam)
    
}


geweeg.2 <- function(sektor=all) {
    
    weeg2 <- function(temp) {  #calculate weighted mean for each quarter for all columns
        sectorw=temp$sectorw[1]
        temp <- cbind(firmw=temp$weight,temp$weight*temp[(match("surveyQ",colnames(temp))+1):ncol(temp)])
        xbar <- colSums(temp, na.rm=TRUE, dims = 1)/
            sapply(colnames(temp), function(x) sum(temp$firmw[!is.na(temp[colnames(temp) == x])]))
        #this is the weighted standard devation: sum[wi*(xi-xbar)^2]/sum(wi) 
        idio <- sqrt(sapply(colnames(temp), function(x) sum((temp[,x]-xbar[x])*(temp[,x]-xbar[x])*temp$firmw,na.rm=TRUE))/
                         sapply(colnames(temp), function(x) sum(temp$firmw[!is.na(temp[colnames(temp) == x])],na.rm=TRUE))) 
        #aggr <- sapply(colnames(temp), function(x) xbar[x]*xbar[x]) 
        ind <- c(sectorw,idio)
        return(ind)
    }
    
    man <- BER[BER$sector %in% sektor,]
    sector <- data.frame()
    for(kwartaal in levels(man$surveyQ)) {
        sector <- rbind(sector,weeg2(man[man$surveyQ==kwartaal,]))
    }
    
    sector[,2] <- levels(man$surveyQ)
    colnames(sector) <- colnames(man)[c(2,7:ncol(man))]
    sector <- merge(datums,sector, by.x="Date", by.y="surveyQ", all=TRUE)
    #sector <- as.data.frame(t(sapply(levels(man$surveyQ), function(kwartaal) weeg(man[man$surveyQ==kwartaal,]))))
    #sector[,-1:-2] <- na.approx(sector[,-1:-2],na.rm = FALSE)
    return(sector)
    
}


maak.index <- function(sektor=all,streek=streke) {
    lys <- list()
    t <- 0
    for(k in sektor) {
        t <- t+1
        lys[[t]] <- geweeg.2(k)
    }
    saam <- lys[[1]]
    for(j in 4:14) {
        for(i in 1:95) {
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
    saam$Aggr <- rowMeans(saam[,c("Q1","Q2A","Q3A","Q4A","Q5A","Q6A")],na.rm = TRUE, dims = 1)
    #saam$Uncert_fl <- rowMeans(saam[,c("Q2P","Q3P","Q4P","Q5P","Q6P")],na.rm = TRUE, dims = 1)
    saam[,14] <- na.approx(saam[,14],na.rm = FALSE)
    return(saam)
    
}



w.agg.M_2 <- maak.index(manufacturings)
w.idio.M_2 <- maak.index(manufacturings)


index_plot <- cbind(w.uncert_error.M[,c(1,2)],uncert_error.M[,c(3)])
index_plot[,-1] <- scale(index_plot[,-1] )
colnames(index_plot) <- c("Date","cc","fl")#,"pcacc","pcafl")
index_plot <- melt(index_plot, id="Date")  # convert to long format
g <- ggplot(index_plot, aes(x=Date,y=value,group=variable,colour=variable)) 
g <- g + geom_line()
g <- g + theme(legend.title=element_blank())
g <- g + ggtitle("Manufacturing") 
g <- g + ylab("Indicator") + xlab("")
g <- g + theme(legend.position="bottom")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g

cor(index_plot[complete.cases(index_plot),][,2:3])

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


aggr.errors <- seisoen(aggr.errors)
idio.errors <- seisoen(idio.errors)
aggr.errors[1:5,3] <- NA
aggr.errors[1,4] <- NA
aggr.errors[1:53,5] <- NA
idio.errors[1:5,3] <- NA
idio.errors[1,4] <- NA
idio.errors[1:53,5] <- NA

w.uncert_error <- cbind(w.uncertainty[,c(1,6)],idio.errors[,6],aggr.errors[,6])
colnames(w.uncert_error) <- c("Date","Dispersion","Idiosyncratic_error","Aggregate_error")

uncert_error <- cbind(uncertainty[,c(2,16)],uncert_error[,3:4])
colnames(uncert_error) <- c("Date","Dispersion","Idiosyncratic","Aggregate")
uncert_error[-99,-1] <- na.approx(uncert_error[-99,-1])
uncert_error[,-1] <- scale(uncert_error[,-1])


index_plot <- cbind(w.uncertainty[,c(1,2)])
index_plot[,-1] <- scale(index_plot[,-1])
colnames(index_plot) <- c("Date","Dispersion")
g1 <- ggplot(index_plot, aes(x=Date,y=Dispersion,group=1))
g1 <- g1 + geom_line(colour="#7CAE00")
g1 <- g1 + theme(legend.title=element_blank())
g1 <- g1 + ggtitle("Manufacturing") 
g1 <- g1 + ylab("Indicator") + xlab("")
g1 <- g1 + theme(legend.position="none")
g1 <- g1 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g1 <- g1 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

index_plot <- w.uncertainty[,c(1,3)]
index_plot[,-1] <- scale(index_plot[,-1])
colnames(index_plot) <- c("Date","Dispersion")
g2 <- ggplot(index_plot, aes(x=Date,y=Dispersion,group=1))
g2 <- g2 + geom_line(colour="#7CAE00")
g2 <- g2 + theme(legend.title=element_blank())
g2 <- g2 + ggtitle("Construction") 
g2 <- g2 + ylab("") + xlab("")
g2 <- g2 + theme(legend.position="none")
g2 <- g2 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g2 <- g2 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

index_plot <- cbind(w.uncertainty[,c(1,4)])
index_plot[,-1] <- scale(index_plot[,-1])
colnames(index_plot) <- c("Date","Dispersion")
g3 <- ggplot(index_plot, aes(x=Date,y=Dispersion,group=1))
g3 <- g3 + geom_line(colour="#7CAE00")
g3 <- g3 + theme(legend.title=element_blank())
g3 <- g3 + ggtitle("Trade") 
g3 <- g3 + ylab("Indicator") + xlab("")
g3 <- g3 + theme(legend.position="none")#,plot.margin=unit(c(-0.5,0.4,0,0.4), "cm"))
g3 <- g3 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g3 <- g3 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

index_plot <- cbind(w.uncertainty[,c(1,5)])
index_plot[,-1] <- scale(index_plot[,-1])
colnames(index_plot) <- c("Date","Dispersion")
g4 <- ggplot(index_plot, aes(x=Date,y=Dispersion,group=1))
g4 <- g4 + geom_line(colour="#7CAE00")
g4 <- g4 + theme(legend.title=element_blank())
g4 <- g4 + ggtitle("Services") 
g4 <- g4 + ylab("") + xlab("")
g4 <- g4 + theme(legend.position="none")#,plot.margin=unit(c(-0.5,0.4,0,0.4), "cm"))
g4 <- g4 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g4 <- g4 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

library(gridExtra)
grid.arrange(g1, g2, g3, g4, ncol=2, nrow =2)



index_plot <- cbind(aggr.errors[,c(1,2)])
index_plot[,-1] <- scale(index_plot[,-1])
colnames(index_plot) <- c("Date","Aggregate")
g1 <- ggplot(index_plot, aes(x=Date,y=Aggregate,group=1))
g1 <- g1 + geom_line(colour="#F8766D")
#index_plot <- melt(index_plot, id="Date")  # convert to long format
#g1 <- ggplot(index_plot, aes(x=Date,y=value,group=variable,colour="#F8766D")) 
#g1 <- g1 + geom_line()
g1 <- g1 + theme(legend.title=element_blank())
g1 <- g1 + ggtitle("Manufacturing") 
g1 <- g1 + ylab("Indicator") + xlab("")
g1 <- g1 + theme(legend.position="none")
g1 <- g1 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g1 <- g1 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

index_plot <- cbind(aggr.errors[,c(1,3)])
index_plot[,-1] <- scale(index_plot[,-1])
colnames(index_plot) <- c("Date","Aggregate")
g2 <- ggplot(index_plot, aes(x=Date,y=Aggregate,group=1))
g2 <- g2 + geom_line(colour="#F8766D")
g2 <- g2 + theme(legend.title=element_blank())
g2 <- g2 + ggtitle("Construction") 
g2 <- g2 + ylab("") + xlab("")
g2 <- g2 + theme(legend.position="none")
g2 <- g2 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g2 <- g2 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

index_plot <- cbind(aggr.errors[,c(1,4)])
index_plot[,-1] <- scale(index_plot[,-1])
colnames(index_plot) <- c("Date","Aggregate")
g3 <- ggplot(index_plot, aes(x=Date,y=Aggregate,group=1))
g3 <- g3 + geom_line(colour="#F8766D")
g3 <- g3 + theme(legend.title=element_blank())
g3 <- g3 + ggtitle("Trade") 
g3 <- g3 + ylab("Indicator") + xlab("")
g3 <- g3 + theme(legend.position="none")#,plot.margin=unit(c(-0.5,0.4,0,0.4), "cm"))
g3 <- g3 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g3 <- g3 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

index_plot <- cbind(aggr.errors[,c(1,5)])
index_plot[,-1] <- scale(index_plot[,-1])
colnames(index_plot) <- c("Date","Aggregate")
g4 <- ggplot(index_plot, aes(x=Date,y=Aggregate,group=1))
g4 <- g4 + geom_line(colour="#F8766D")
g4 <- g4 + theme(legend.title=element_blank())
g4 <- g4 + ggtitle("Services") 
g4 <- g4 + ylab("") + xlab("")
g4 <- g4 + theme(legend.position="none")#,plot.margin=unit(c(-0.5,0.4,0,0.4), "cm"))
g4 <- g4 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g4 <- g4 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

library(gridExtra)
grid.arrange(g1, g2, g3, g4, ncol=2, nrow =2)


index_plot <- cbind(idio.errors[,1:2])
index_plot[,-1] <- scale(index_plot[,-1])
colnames(index_plot) <- c("Date","Idiosyncratic")
g1 <- ggplot(index_plot, aes(x=Date,y=Idiosyncratic,group=1))
g1 <- g1 + geom_line(colour="#00BFC4")
g1 <- g1 + theme(legend.title=element_blank())
g1 <- g1 + ggtitle("Manufacturing") 
g1 <- g1 + ylab("Indicator") + xlab("")
g1 <- g1 + theme(legend.position="none")
g1 <- g1 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g1 <- g1 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

index_plot <- cbind(idio.errors[,c(1,3)])
index_plot[,-1] <- scale(index_plot[,-1])
colnames(index_plot) <- c("Date","Idiosyncratic")
g2 <- ggplot(index_plot, aes(x=Date,y=Idiosyncratic,group=1))
g2 <- g2 + geom_line(colour="#00BFC4")
g2 <- g2 + theme(legend.title=element_blank())
g2 <- g2 + ggtitle("Construction") 
g2 <- g2 + ylab("") + xlab("")
g2 <- g2 + theme(legend.position="none")
g2 <- g2 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g2 <- g2 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

index_plot <- cbind(idio.errors[,c(1,4)])
index_plot[,-1] <- scale(index_plot[,-1])
colnames(index_plot) <- c("Date","Idiosyncratic")
g3 <- ggplot(index_plot, aes(x=Date,y=Idiosyncratic,group=1))
g3 <- g3 + geom_line(colour="#00BFC4")
g3 <- g3 + theme(legend.title=element_blank())
g3 <- g3 + ggtitle("Trade") 
g3 <- g3 + ylab("Indicator") + xlab("")
g3 <- g3 + theme(legend.position="none")#,plot.margin=unit(c(-0.5,0.4,0,0.4), "cm"))
g3 <- g3 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g3 <- g3 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

index_plot <- cbind(idio.errors[,c(1,5)])
index_plot[,-1] <- scale(index_plot[,-1])
colnames(index_plot) <- c("Date","Idiosyncratic")
g4 <- ggplot(index_plot, aes(x=Date,y=Idiosyncratic,group=1))
g4 <- g4 + geom_line(colour="#00BFC4")
g4 <- g4 + theme(legend.title=element_blank())
g4 <- g4 + ggtitle("Services") 
g4 <- g4 + ylab("") + xlab("")
g4 <- g4 + theme(legend.position="none")#,plot.margin=unit(c(-0.5,0.4,0,0.4), "cm"))
g4 <- g4 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g4 <- g4 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

library(gridExtra)
grid.arrange(g1, g2, g3, g4, ncol=2, nrow =2)


index_plot <- w.uncert_error 
#index_plot$Date <- conf_indices$Date
#colnames(index_plot) <- c()
index_plot[,-1] <- scale(index_plot[,-1])
g <- ggplot(index_plot) 
g <- g + geom_line(aes(x=Date, y=Aggregate_error, colour="Aggregate_error"), size = 1)
g <- g + geom_line(aes(x=Date, y=Idiosyncratic_error, colour="Idiosyncratic_error"), size = 1)
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



uncert_indices <- cbind(w.uncert_error,GDPdata$IMF_Uncertainty, GDPdata$SAVI,GDPgrowth4$RGDP)
colnames(uncert_indices) <-c("Date","Dispersion","Idiosyncratic_error","Aggregate_error","EPU","SAVI","RGDP_Growth")
uncert_indices[,-1] <- scale(uncert_indices[,-1])
un <- uncert_indices #un <- na.locf(uncert_indices)
un[is.na(un)] <- 0
#uncert_indices$Uncertainty <- rowMeans(uncert_indices[,c(2:6)],na.rm = TRUE)
uncert_indices$Uncertainty_Combined <- princomp(un[,c(2:6)])$scores[,1]
uncert_indices <- uncert_indices[,c(1:6,8,7)]


index_plot <- uncert_indices
g <- ggplot(index_plot) 
g <- g + geom_line(aes(x=Date, y=EPU, colour="EPU"), size = 0.8)
g <- g + geom_line(aes(x=Date, y=SAVI, colour="SAVI"), size = 0.8)
g <- g + geom_line(aes(x=Date, y=Uncertainty_Combined, colour="Uncertainty_Combined"), size = 1)
g <- g + labs(color="Legend text")
g <- g + geom_rect(data=recessions.df, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='grey', alpha=0.5)
g <- g + ylab("Indicator") + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) 
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"), expand=c(0,0),
                      limits = as.Date(c("1990-12-31", NA)))
g <- g + theme(legend.position="bottom")
g


source("corstarsl.R")
colnames(uncert_indices)[7] <- "Uncertainty"
xt <- xtable(corstarsl(uncert_indices[,-1]), caption="Correlations between the uncertainty indicators")
print(xt, "latex",comment=FALSE, caption.placement = getOption("xtable.caption.placement", "top"), scalebox = 0.9)


Dispersion <- uncert_indices[,2]
Idiosyncratic_error <- uncert_indices[,3]
Aggregate_error <- uncert_indices[,4]
EPU <- uncert_indices[,5] 
SAVI <- uncert_indices[,6] 
Uncertainty_Combined <- uncert_indices[,7]
RGDP_Growth <- conf_indices[,6]

par(mfrow=c(3,2))
ccf(Dispersion, RGDP_Growth, na.action = na.pass, ylim=c(-0.5, 0.5), 
    ylab = "Correlation", xlab = "Number of Lags")
ccf(Idiosyncratic_error, RGDP_Growth, na.action = na.pass, ylim=c(-0.5, 0.5), 
    ylab = "Correlation", xlab = "Number of Lags")
ccf(Aggregate_error, RGDP_Growth, na.action = na.pass, ylim=c(-0.5, 0.5), 
    ylab = "Correlation", xlab = "Number of Lags")
#par(mfrow=c(2,2))
ccf(EPU, RGDP_Growth, na.action = na.pass, ylim=c(-0.5, 0.5), 
    ylab = "Correlation", xlab = "Number of Lags")
ccf(SAVI, RGDP_Growth, na.action = na.pass, ylim=c(-0.5, 0.5), 
    ylab = "Correlation", xlab = "Number of Lags")
ccf(Uncertainty_Combined, RGDP_Growth, na.action = na.pass, ylim=c(-0.5, 0.5), 
    ylab = "Correlation", xlab = "Number of Lags")



#Sectoral Analysis
manufac2 <- cbind(w.uncertainty[,c(1,2)],aggr.errors[,2],idio.errors[,2], GDPgrowth4$Manufacturing)
colnames(manufac2) <- c("Date","Dispersion","Aggregate","Idiosyncratic","RGDP_Growth")
manufac2[,2:4] <- scale(manufac2[,2:4])
#manufac2$Uncertainty <- rowMeans(manufac2[,c(2:4)],na.rm = TRUE)
manufac2 <- manufac2[-99,]
manufac2$Uncertainty <- princomp(na.locf(manufac2[,2:4]))$scores[,1]
manufac2 <- manufac2[,c(1:4,6,5)]

construct2 <- cbind(w.uncertainty[,c(1,3)],aggr.errors[,3],idio.errors[,3], GDPgrowth4$Construction)
colnames(construct2) <- c("Date","Dispersion","Aggregate","Idiosyncratic","RGDP_Growth")
construct2[,2:4] <- scale(construct2[,2:4])
#construct2$Uncertainty <- rowMeans(construct2[,c(2:4)],na.rm = TRUE)
construct2 <- construct2[c(-1:-5,-99),] #un <- na.locf(uncert_indices)
construct2[is.na(construct2)] <- 0
construct2$Uncertainty <- -1*princomp(na.locf(construct2[,2:4]))$scores[,1]
construct2 <- construct2[,c(1:4,6,5)]

trade2 <- cbind(w.uncertainty[,c(1,4)],aggr.errors[,4],idio.errors[,4], GDPgrowth4$Trade)
colnames(trade2) <- c("Date","Dispersion","Aggregate","Idiosyncratic","RGDP_Growth")
trade2[,2:4] <- scale(trade2[,2:4])
#trade2$Uncertainty <- rowMeans(trade2[,c(2:4)],na.rm = TRUE)
trade2 <- trade2[c(-1,-99),] #un <- na.locf(uncert_indices)
trade2[is.na(trade2)] <- 0
trade2$Uncertainty <- princomp(na.locf(trade2[,2:4]))$scores[,1]
trade2 <- trade2[,c(1:4,6,5)]

services2 <- cbind(w.uncertainty[,c(1,5)],aggr.errors[,5],idio.errors[,5], GDPgrowth4$Services)
colnames(services2) <- c("Date","Dispersion","Aggregate","Idiosyncratic","RGDP_Growth")
services2[,2:4] <- scale(services2[,2:4])
#services2$Uncertainty <- rowMeans(services2[,c(2:4)],na.rm = TRUE)
services2 <- services2[c(-1:-53,-99),] #un <- na.locf(uncert_indices)
services2[is.na(services2)] <- 0
services2$Uncertainty <- -1*princomp(na.locf(services2[,2:4]))$scores[,1]
services2 <- services2[,c(1:4,6,5)]

#Check correlations
source("corstarsl.R")
xt1 <- cbind(corstarsl(manufac2[,-1]),corstarsl(construct2[,-1]))
xt2 <- cbind(corstarsl(trade2[,-1]),corstarsl(services2[,-1]))
xt1 <- sapply(xt1,as.character)
xt2 <- sapply(xt2,as.character)
xt1[1,] <- c("Dispersion","Aggregate","Idiosyncratic","Combined","Dispersion","Aggregate","Idiosyncratic","Combined")
xt2[1,] <- c("Dispersion","Aggregate","Idiosyncratic","Combined","Dispersion","Aggregate","Idiosyncratic","Combined")
colnames(xt1) <- c(" "," ","Manufacturing"," "," "," ","Construction"," ")
colnames(xt2) <- c(" "," ","Trade"," "," "," ","Services"," ")
row.names(xt1) <- c(" ","Aggregate","Idiosyncratic","Combined","RGDP")
row.names(xt2) <- c(" ","Aggregate","Idiosyncratic","Combined","RGDP")

xt <- xtable(xt1, caption="Correlations of uncertainty indicators and growth")
print(xt, "latex",comment=FALSE, caption.placement = getOption("xtable.caption.placement", "top"), scalebox = 0.7)
xt <- xtable(xt2)
print(xt, "latex",comment=FALSE, caption.placement = getOption("xtable.caption.placement", "top"), scalebox = 0.75)


calc_ccf <- function(data) {
    Dispersion <- data[,2]
    Aggregate_error <- data[,3]
    Idiosyncratic_error <- data[,4]
    Uncertainty_Combined <- data[,5]
    RGDP_Growth <- data[,6]
    
    par(mfrow=c(2,2))
    ccf(Dispersion, RGDP_Growth, na.action = na.omit, ylim=c(-0.5, 0.5), 
        ylab = "Correlation", xlab = "Number of Lags")
    ccf(Aggregate_error, RGDP_Growth, na.action = na.omit, ylim=c(-0.5, 0.5), 
        ylab = "Correlation", xlab = "Number of Lags")
    ccf(Idiosyncratic_error, RGDP_Growth, na.action = na.omit, ylim=c(-0.5, 0.5), 
        ylab = "Correlation", xlab = "Number of Lags")
    ccf(Uncertainty_Combined, RGDP_Growth, na.action = na.omit, ylim=c(-0.5, 0.5), 
        ylab = "Correlation", xlab = "Number of Lags")
}

calc_ccf(manufac2)
#calc_ccf(construct2)
#calc_ccf(trade2)
#calc_ccf(services2)


##=================================================
##-------------VAR Analysis------------------------
##=================================================
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

var1 <- calc_var(cbind(Confidence_Current,RGDP_Growth))
var2 <- calc_var(cbind(Confidence_Expected, RGDP_Growth))
var3 <- calc_var(cbind(BER_BCI, RGDP_Growth))
var4 <- calc_var(cbind(SACCI_Growth, RGDP_Growth))

G <- data.frame()
G[1,1] <- causality(var1,cause = "Confidence_Current")$Granger[4]
G[1,2] <- as.numeric(as.character(causality(var1,cause = "Confidence_Current")$Granger[1]))
G[1,3] <- as.numeric(as.character(causality(var1,cause = "Confidence_Current")$Granger[3]))
G[2,1] <- causality(var1,cause = "RGDP_Growth")$Granger[4]
G[2,2] <- as.numeric(as.character(causality(var1,cause = "RGDP_Growth")$Granger[1]))
G[2,3] <- as.numeric(as.character(causality(var1,cause = "RGDP_Growth")$Granger[3]))

G[3,1] <- causality(var2,cause = "Confidence_Expected")$Granger[4]
G[3,2] <- as.numeric(as.character(causality(var2,cause = "Confidence_Expected")$Granger[1]))
G[3,3] <- as.numeric(as.character(causality(var2,cause = "Confidence_Expected")$Granger[3]))
G[4,1] <- causality(var2,cause = "RGDP_Growth")$Granger[4]
G[4,2] <- as.numeric(as.character(causality(var2,cause = "RGDP_Growth")$Granger[1]))
G[4,3] <- as.numeric(as.character(causality(var2,cause = "RGDP_Growth")$Granger[3]))

G[5,1] <- causality(var3,cause = "BER_BCI")$Granger[4]
G[5,2] <- as.numeric(as.character(causality(var3,cause = "BER_BCI")$Granger[1]))
G[5,3] <- as.numeric(as.character(causality(var3,cause = "BER_BCI")$Granger[3]))
G[6,1] <- causality(var3,cause = "RGDP_Growth")$Granger[4]
G[6,2] <- as.numeric(as.character(causality(var3,cause = "RGDP_Growth")$Granger[1]))
G[6,3] <- as.numeric(as.character(causality(var3,cause = "RGDP_Growth")$Granger[3]))

G[7,1] <- causality(var4,cause = "SACCI_Growth")$Granger[4]
G[7,2] <- as.numeric(as.character(causality(var4,cause = "SACCI_Growth")$Granger[1]))
G[7,3] <- as.numeric(as.character(causality(var4,cause = "SACCI_Growth")$Granger[3]))
G[8,1] <- causality(var4,cause = "RGDP_Growth")$Granger[4]
G[8,2] <- as.numeric(as.character(causality(var4,cause = "RGDP_Growth")$Granger[1]))
G[8,3] <- as.numeric(as.character(causality(var4,cause = "RGDP_Growth")$Granger[3]))

G[,2:3] <- round(G[,2:3],3)
mystars <- ifelse(G[,3] < .01, "***", ifelse(G[,3] < .05, "** ", ifelse(G[,3] < .1, "* ", " ")))
Gnew <- matrix(paste(G[,2], mystars, sep=""), ncol=1) 
G[,1] <- sub(".*: ", "", G[,1])
G[,2] <- Gnew
colnames(G) <- c("Granger causality H0:","statistic","p-value")

xt <- xtable(G, caption="Granger causality tests: aggregate confidence")
print(xt, "latex", include.rownames=FALSE,comment=FALSE, caption.placement = getOption("xtable.caption.placement", "top"), 
      scalebox = 0.8)


calc_sectoralvar <- function(data, serv=0) {
    Confidence_Current <- data[,2]
    Confidence_Expected <- data[,3]
    BER_BCI <- data[,4]
    RGDP_Growth <- data[,5]
    
    var1 <- calc_var(cbind(Confidence_Current,RGDP_Growth))
    var2 <- calc_var(cbind(Confidence_Expected, RGDP_Growth))
    var3 <- calc_var(cbind(BER_BCI, RGDP_Growth))
    
    ##Granger causality tests
    G <- data.frame()
    G[1,1] <- causality(var1,cause = "Confidence_Current")$Granger[4]
    G[1,2] <- as.numeric(as.character(causality(var1,cause = "Confidence_Current")$Granger[1]))
    G[1,3] <- as.numeric(as.character(causality(var1,cause = "Confidence_Current")$Granger[3]))
    G[2,1] <- causality(var1,cause = "RGDP_Growth")$Granger[4]
    G[2,2] <- as.numeric(as.character(causality(var1,cause = "RGDP_Growth")$Granger[1]))
    G[2,3] <- as.numeric(as.character(causality(var1,cause = "RGDP_Growth")$Granger[3]))
    
    G[3,1] <- causality(var2,cause = "Confidence_Expected")$Granger[4]
    G[3,2] <- as.numeric(as.character(causality(var2,cause = "Confidence_Expected")$Granger[1]))
    G[3,3] <- as.numeric(as.character(causality(var2,cause = "Confidence_Expected")$Granger[3]))
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

xt <- xtable(G_sector, caption="Granger causality tests: sectoral confidence")
print(xt, "latex", include.rownames=FALSE,comment=FALSE, caption.placement = getOption("xtable.caption.placement", "top"), 
      scalebox = 0.8)


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

var1 <- calc_var(cbind(Dispersion,RGDP_Growth)[-99,])
var2 <- calc_var(cbind(Aggregate_error, RGDP_Growth)[-99,])
var3 <- calc_var(cbind(Idiosyncratic_error, RGDP_Growth)[-99,])
var4 <- calc_var(cbind(EPU, RGDP_Growth)[-98:-99,])
var5 <- calc_var(cbind(SAVI,RGDP_Growth)[-1:-14,])
var6 <- calc_var(cbind(Uncertainty_Combined, RGDP_Growth))


##Granger causality tests
G <- data.frame()
G[1,1] <- causality(var1,cause = "Dispersion")$Granger[4]
G[1,2] <- as.numeric(as.character(causality(var1,cause = "Dispersion")$Granger[1]))
G[1,3] <- as.numeric(as.character(causality(var1,cause = "Dispersion")$Granger[3]))
G[2,1] <- causality(var1,cause = "RGDP_Growth")$Granger[4]
G[2,2] <- as.numeric(as.character(causality(var1,cause = "RGDP_Growth")$Granger[1]))
G[2,3] <- as.numeric(as.character(causality(var1,cause = "RGDP_Growth")$Granger[3]))

G[3,1] <- causality(var2,cause = "Aggregate_error")$Granger[4]
G[3,2] <- as.numeric(as.character(causality(var2,cause = "Aggregate_error")$Granger[1]))
G[3,3] <- as.numeric(as.character(causality(var2,cause = "Aggregate_error")$Granger[3]))
G[4,1] <- causality(var2,cause = "RGDP_Growth")$Granger[4]
G[4,2] <- as.numeric(as.character(causality(var2,cause = "RGDP_Growth")$Granger[1]))
G[4,3] <- as.numeric(as.character(causality(var2,cause = "RGDP_Growth")$Granger[3]))

G[5,1] <- causality(var3,cause = "Idiosyncratic_error")$Granger[4]
G[5,2] <- as.numeric(as.character(causality(var3,cause = "Idiosyncratic_error")$Granger[1]))
G[5,3] <- as.numeric(as.character(causality(var3,cause = "Idiosyncratic_error")$Granger[3]))
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

G[11,1] <- causality(var6,cause = "Uncertainty_Combined")$Granger[4]
G[11,2] <- as.numeric(as.character(causality(var6,cause = "Uncertainty_Combined")$Granger[1]))
G[11,3] <- as.numeric(as.character(causality(var6,cause = "Uncertainty_Combined")$Granger[3]))
G[12,1] <- causality(var6,cause = "RGDP_Growth")$Granger[4]
G[12,2] <- as.numeric(as.character(causality(var6,cause = "RGDP_Growth")$Granger[1]))
G[12,3] <- as.numeric(as.character(causality(var6,cause = "RGDP_Growth")$Granger[3]))

G[,2:3] <- round(G[,2:3],3)
mystars <- ifelse(G[,3] < .01, "***", ifelse(G[,3] < .05, "** ", ifelse(G[,3] < .1, "* ", " ")))
Gnew <- matrix(paste(G[,2], mystars, sep=""), ncol=1) 
G[,1] <- sub(".*: ", "", G[,1])
G[,2] <- Gnew
colnames(G) <- c("Granger causality H0:","statistic","p-value")

xt <- xtable(G, caption="Granger causality tests: uncertainty")
print(xt, "latex", include.rownames=FALSE,comment=FALSE, caption.placement = getOption("xtable.caption.placement", "top"), 
      scalebox = 0.8)



calc_sectoralvar <- function(data) {
    Dispersion <- data[,2]
    Aggregate <- data[,3]
    Idiosyncratic <- data[,4]
    Uncertainty <- data[,5]
    RGDP_Growth <- data[,6]
    
    var1 <- calc_var(cbind(Dispersion, RGDP_Growth))
    var2 <- calc_var(cbind(Aggregate, RGDP_Growth))
    var3 <- calc_var(cbind(Idiosyncratic,RGDP_Growth))
    var4 <- calc_var(cbind(Uncertainty, RGDP_Growth))
    
    ##Granger causality tests
    G <- data.frame()
    G[1,1] <- causality(var1,cause = "Dispersion")$Granger[4]
    G[1,2] <- as.numeric(as.character(causality(var1,cause = "Dispersion")$Granger[1]))
    G[1,3] <- as.numeric(as.character(causality(var1,cause = "Dispersion")$Granger[3]))
    G[2,1] <- causality(var1,cause = "RGDP_Growth")$Granger[4]
    G[2,2] <- as.numeric(as.character(causality(var1,cause = "RGDP_Growth")$Granger[1]))
    G[2,3] <- as.numeric(as.character(causality(var1,cause = "RGDP_Growth")$Granger[3]))
    
    G[3,1] <- causality(var2,cause = "Aggregate")$Granger[4]
    G[3,2] <- as.numeric(as.character(causality(var2,cause = "Aggregate")$Granger[1]))
    G[3,3] <- as.numeric(as.character(causality(var2,cause = "Aggregate")$Granger[3]))
    G[4,1] <- causality(var2,cause = "RGDP_Growth")$Granger[4]
    G[4,2] <- as.numeric(as.character(causality(var2,cause = "RGDP_Growth")$Granger[1]))
    G[4,3] <- as.numeric(as.character(causality(var2,cause = "RGDP_Growth")$Granger[3]))
    
    G[5,1] <- causality(var3,cause = "Idiosyncratic")$Granger[4]
    G[5,2] <- as.numeric(as.character(causality(var3,cause = "Idiosyncratic")$Granger[1]))
    G[5,3] <- as.numeric(as.character(causality(var3,cause = "Idiosyncratic")$Granger[3]))
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

G_manu <- calc_sectoralvar(manufac2)
G_build <- calc_sectoralvar(construct2)
G_trade <- calc_sectoralvar(trade2)
G_serv <- calc_sectoralvar(services2)

G_sector <- cbind(G_manu[,1:2],G_build[,2],G_trade[,2],G_serv[,2])
colnames(G_sector)[2:5] <- c("Manufacturing", "Construction","Trade","Services")

xt <- xtable(G_sector, caption="Granger causality tests: sectoral uncertainty")
print(xt, "latex", include.rownames=FALSE,comment=FALSE, caption.placement = getOption("xtable.caption.placement", "top"), 
      scalebox = 0.8)


vardat <- cbind(conf_indices[,2],conf_indices[,6])
colnames(vardat) <- c("Confidence_Current","RGDP_Growth")
infocrit <- VARselect(vardat, lag.max = 12, type = "const")
k_aic <- infocrit$selection[1]
k_hq  <- infocrit$selection[2]
k_sic <- infocrit$selection[3]
k <- min(k_aic,k_sic,k_hq)
var1 <- VAR(vardat,p=k,type="const")

irf.y1 <- irf(var1,impulse = "Confidence_Current", response = "RGDP_Growth", n.ahead = 12,runs = 1000, seed=12345) 
irf.y2 <- irf(var1,impulse = "RGDP_Growth", response = "Confidence_Current", n.ahead = 12,runs = 1000, seed=12345)

detach("package:BCDating", unload=TRUE)

par(mfrow=c(1,1), new=FALSE)
nf <- layout(matrix(c(1,2,1,2), 1, 2, byrow = TRUE))
#layout.show(nf)
#par(cex=1)
plot(irf.y1,plot.type = c("single"), main="Response from Confidence", xlab="Horizon in quarters")
par(new = TRUE)
plot(irf.y2,plot.type = c("single"), main="Response from RGDP Growth", xlab="Horizon in quarters")


source("plot_varfevd.R")
par(mfrow=c(1,1), new=FALSE)
par(mfrow=c(1,2))
plot.varfevd(fevd(var1, n.ahead = 10 ),plot.type = "single", xlab="Horizon in quarters", 
             ylab="Proportion of variance explained")

vardat <- cbind(manufac[,2],manufac[,5])
vardat <- cbind(construct[-1:-5,2],construct[-1:-5,5])
vardat <- cbind(trade[-1,2],trade[-1,5])
vardat <- cbind(services[-1:-53,2],services[-1:-53,5])

colnames(vardat) <- c("Confidence_Current","RGDP_Growth")
infocrit <- VARselect(vardat, lag.max = 12, type = "const")
k_aic <- infocrit$selection[1]
k_hq  <- infocrit$selection[2]
k_sic <- infocrit$selection[3]
k <- min(k_aic,k_sic,k_hq)
varm <- VAR(vardat,p=k,type="const")

irf.y1 <- irf(varm,impulse = "Confidence_Current", response = "RGDP_Growth", n.ahead = 12,runs = 1000, seed=12345) 
irf.y2 <- irf(varm,impulse = "RGDP_Growth", response = "Confidence_Current", n.ahead = 12,runs = 1000, seed=12345)

detach("package:BCDating", unload=TRUE)

par(mfrow=c(1,1), new=FALSE)
nf <- layout(matrix(c(1,2,1,2), 1, 2, byrow = TRUE))
#layout.show(nf)
#par(cex=1)
plot(irf.y1,plot.type = c("single"), main="Response from Confidence", xlab="Horizon in quarters")
par(new = TRUE)
plot(irf.y2,plot.type = c("single"), main="Response from RGDP Growth", xlab="Horizon in quarters")

source("plot_varfevd.R")
par(mfrow=c(1,2))
plot.varfevd(fevd(varm, n.ahead = 10 ),plot.type = "single")

vardat <- cbind(uncert_indices[,2],conf_indices[,6])[-99,]
colnames(vardat) <- c("Dispersion","RGDP_Growth")
infocrit <- VARselect(vardat, lag.max = 12, type = "const")
k_aic <- infocrit$selection[1]
k_hq  <- infocrit$selection[2]
k_sic <- infocrit$selection[3]
k <- min(k_aic,k_sic,k_hq)
var1 <- VAR(vardat,p=k,type="const")

irf.y1 <- irf(var1,impulse = "Dispersion", response = "RGDP_Growth", n.ahead = 12,runs = 1000, seed=12345) 
irf.y2 <- irf(var1,impulse = "RGDP_Growth", response = "Dispersion", n.ahead = 12,runs = 1000, seed=12345)

par(mfrow=c(1,1), new=FALSE)
nf <- layout(matrix(c(1,2,1,2), 1, 2, byrow = TRUE))
#layout.show(nf)
#par(cex=1)
plot(irf.y1,plot.type = c("single"), main="Response from Dispersion", xlab="Horizon in quarters")
par(new = TRUE)
plot(irf.y2,plot.type = c("single"), main="Response from RGDP Growth", xlab="Horizon in quarters")


vardat <- cbind(uncert_indices[,7],conf_indices[,6])
colnames(vardat) <- c("Uncertainty","RGDP_Growth")
infocrit <- VARselect(vardat, lag.max = 12, type = "const")
k_aic <- infocrit$selection[1]
k_hq  <- infocrit$selection[2]
k_sic <- infocrit$selection[3]
k <- min(k_aic,k_sic,k_hq)
var6 <- VAR(vardat,p=k,type="const")

irf.y1 <- irf(var6,impulse = "Uncertainty", response = "RGDP_Growth", n.ahead = 12,runs = 1000, seed=12345) 
irf.y2 <- irf(var6,impulse = "RGDP_Growth", response = "Uncertainty", n.ahead = 12,runs = 1000, seed=12345)

par(mfrow=c(1,1), new=FALSE)
nf <- layout(matrix(c(1,2,1,2), 1, 2, byrow = TRUE))
#layout.show(nf)
#par(cex=1)
plot(irf.y1,plot.type = c("single"), main="Response from Uncertainty", xlab="Horizon in quarters")
par(new = TRUE)
plot(irf.y2,plot.type = c("single"), main="Response from RGDP Growth", xlab="Horizon in quarters")


source("plot_varfevd.R")
par(mfrow=c(1,2))
plot.varfevd(fevd(var6, n.ahead = 10 ),plot.type = "single")


vardat <- cbind(manufac2[,5],manufac2[,6])
vardat <- cbind(construct2[,5],construct2[,6])
vardat <- cbind(trade2[,5],trade2[,6])
vardat <- cbind(services2[,5],services2[,6])

colnames(vardat) <- c("Uncertainty","RGDP_Growth")
infocrit <- VARselect(vardat, lag.max = 12, type = "const")
k_aic <- infocrit$selection[1]
k_hq  <- infocrit$selection[2]
k_sic <- infocrit$selection[3]
k <- min(k_aic,k_sic,k_hq)
varm <- VAR(vardat,p=k,type="const")

irf.y1 <- irf(varm,impulse = "Uncertainty", response = "RGDP_Growth", n.ahead = 12,runs = 1000, seed=12345) 
irf.y2 <- irf(varm,impulse = "RGDP_Growth", response = "Uncertainty", n.ahead = 12,runs = 1000, seed=12345)

par(mfrow=c(1,1), new=FALSE)
nf <- layout(matrix(c(1,2,1,2), 1, 2, byrow = TRUE))
#layout.show(nf)
#par(cex=1)
plot(irf.y1,plot.type = c("single"), main="Response from Uncertainty", xlab="Horizon in quarters")
par(new = TRUE)
plot(irf.y2,plot.type = c("single"), main="Response from RGDP Growth", xlab="Horizon in quarters")


source("plot_varfevd.R")
par(mfrow=c(1,2), new = FALSE)
plot.varfevd(fevd(varm, n.ahead = 10 ),plot.type = "single")



#Check net ander se IRFs
vardat <- cbind(conf_indices[,5],conf_indices[,6])[-1:-3,]
colnames(vardat) <- c("Uncertainty","RGDP_Growth")
infocrit <- VARselect(vardat, lag.max = 12, type = "const")
k_aic <- infocrit$selection[1]
k_hq  <- infocrit$selection[2]
k_sic <- infocrit$selection[3]
k <- min(k_aic,k_sic,k_hq)
var4 <- VAR(vardat,p=k,type="const")

irf.y1 <- irf(var4,impulse = "Uncertainty", response = "RGDP_Growth", n.ahead = 12,runs = 1000, seed=12345) 
irf.y2 <- irf(var4,impulse = "RGDP_Growth", response = "Uncertainty", n.ahead = 12,runs = 1000, seed=12345)

par(mfrow=c(1,1), new=FALSE)
nf <- layout(matrix(c(1,2,1,2), 1, 2, byrow = TRUE))
#layout.show(nf)
#par(cex=1)
plot(irf.y1,plot.type = c("single"), main="Response from Uncertainty", xlab="Horizon in quarters")
par(new = TRUE)
plot(irf.y2,plot.type = c("single"), main="Response from RGDP Growth", xlab="Horizon in quarters")

source("plot_varfevd.R")
par(mfrow=c(1,2))
plot.varfevd(fevd(var4, n.ahead = 10 ),plot.type = "single")



index_plot <- cbind(conf_indices[,c(1,2)],uncert_indices$Uncertainty) 
index_plot[,-1] <- scale(index_plot[,-1])
colnames(index_plot) <- c("Date","Confidence_Current","Uncertainty_Combined")
g <- ggplot(index_plot) 
g <- g + geom_line(aes(x=Date, y=Confidence_Current, colour="Confidence_Current"), size = 1)
g <- g + geom_line(aes(x=Date, y=Uncertainty_Combined, colour="Uncertainty_Combined"), size = 1)
g <- g + labs(color="Legend text")
g <- g + geom_rect(data=recessions.df, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='grey', alpha=0.5)
g <- g + ylab("Indicator") + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) 
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"), expand=c(0,0),
                      limits = as.Date(c("1990-12-31", NA)))
g <- g + theme(legend.position="bottom")
g



Confidence_Current <- conf_indices[,2]
Uncertainty_Combined <- uncert_indices$Uncertainty
RGDP_Growth <- conf_indices[,6]

vardat <- cbind(Confidence_Current,Uncertainty_Combined,RGDP_Growth)  
infocrit <- VARselect(vardat, lag.max = 16, type = "const")
k_aic <- infocrit$selection[1]
k_hq  <- infocrit$selection[2]
k_sic <- infocrit$selection[3]
k <- min(k_aic,k_sic,k_hq)
var_3 <- VAR(vardat,p=4,type="const")


irf.y1 <- irf(var_3,impulse = c("Confidence_Current","Uncertainty_Combined"), response = "RGDP_Growth", n.ahead = 12,runs = 1000, seed=12345) 
par(mfrow=c(1,2),mar=c(5,4,4,2), cex=0.6)
plot(irf.y1,plot.type = c("single"))


source("plot_varfevd.R")
par(mfrow=c(1,3))
plot.varfevd(fevd(var_3, n.ahead = 10 ),plot.type = "single")


JSE <- GDPgrowth4$RJSE
Bond <- GDPdata$Bond2
TBill <- GDPdata$T.Bill
Spread <- Bond-TBill
Employment <- GDPgrowth4$Employ
Investment <- GDPgrowth4$Rinvestment 
Production <- GDPgrowth4$RProduction

vardat <- cbind(Confidence_Current,Uncertainty_Combined,JSE,Spread,RGDP_Growth,
                Production,Employment,Investment)  
vare <- VAR(vardat,p=2,type="const")

irf.y1 <- irf(vare,impulse = c("Confidence_Current","Uncertainty_Combined"),
              response = c("RGDP_Growth","Production","Investment"), n.ahead = 12,runs = 1000, seed=12345) 
par(mfrow=c(2,3),mar=c(3,4,2,1), cex=0.6)
plot(irf.y1,plot.type = c("single"), main=c(" "))


source("plot_varfevd.R")
par(mfrow=c(1,3))
plot.varfevd(fevd(vare, n.ahead = 10 ),plot.type = "multiple")

par(mfrow=c(1,1))
plot.varfevd(fevd(vare, n.ahead = 10 ),plot.type = "single")


irf.y1 <- irf(vare,impulse = c("Activity"),
              response = c("RGDP_Growth"), n.ahead = 12,runs = 1000, seed=12345) 
irf.y2 <- irf(vare,impulse = c("Activity"),
              response = c("Production"), n.ahead = 12,runs = 1000, seed=12345) 
irf.y3 <- irf(vare,impulse = c("Activity"),
              response = c("Investment"), n.ahead = 12,runs = 1000, seed=12345) 
irf.y4 <- irf(vare,impulse = c("Uncertainty"),
              response = c("RGDP_Growth"), n.ahead = 12,runs = 1000, seed=12345) 
irf.y5 <- irf(vare,impulse = c("Uncertainty"),
              response = c("Production"), n.ahead = 12,runs = 1000, seed=12345) 
irf.y6 <- irf(vare,impulse = c("Uncertainty"),
              response = c("Investment"), n.ahead = 12,runs = 1000, seed=12345) 
#par(mfrow=c(2,3),mar=c(3,4,2,1), cex=0.6)


par(mfrow=c(1,1), new=T)
nf <- layout(matrix(c(1,2,3,4,5,6), 2, 3, byrow = TRUE))
#layout.show(nf)
#par(cex=1)
plot(irf.y1,plot.type = c("single"), main=c("IRF from Activity"))
par(new = T)
plot(irf.y2,plot.type = c("single"), main=c("IRF from Activity"))
par(new = T)
plot(irf.y3,plot.type = c("single"), main=c("IRF from Activity"))
par(new = T)
plot(irf.y4,plot.type = c("single"), main=c("IRF from Uncertainty"))
par(new = T)
plot(irf.y5,plot.type = c("single"), main=c("IRF from Uncertainty"))
par(new = T)
plot(irf.y6,plot.type = c("single"), main=c("IRF from Uncertainty"))
par(new = T)



write.csv2(conf_indices,"conf_indices.csv")
write.csv2(uncert_indices,"uncert_indices.csv")

setwd("C:\\Users\\Laurie\\OneDrive\\Documents\\BING\\BER Confidence Surveys\\Sentiment")
suppressMessages(library(xtable))

conf_indices <- read.csv2("conf_indices.csv")[,-1]
uncert_indices <- read.csv2("uncert_indices.csv")[,-1]

colnames(conf_indices)[2:3] <- c("Confidence_Current","Confidence_Expected")
colnames(uncert_indices)[7] <- c("Uncertainty_Combined")

all_indices <- cbind(conf_indices[,c(1:3)],uncert_indices[,c(2:4,7)])
xt <- xtable(all_indices[1:48,], caption="Sentiment Indicators")
print(xt, "latex",comment=FALSE, include.rownames=FALSE,
      caption.placement = getOption("xtable.caption.placement", "top"),scalebox=0.6)


setwd("C:\\Users\\Laurie\\OneDrive\\Documents\\BING\\BER Confidence Surveys\\Sentiment")
suppressMessages(library(xtable))

conf_indices <- read.csv2("conf_indices.csv")[,-1]
uncert_indices <- read.csv2("uncert_indices.csv")[,-1]

colnames(conf_indices)[2:3] <- c("Confidence_Current","Confidence_Expected")
colnames(uncert_indices)[7] <- c("Uncertainty_Combined")

all_indices <- cbind(conf_indices[,c(1:3)],uncert_indices[,c(2:4,7)])
xt <- xtable(all_indices[48:95,])
print(xt, "latex",comment=FALSE, include.rownames=FALSE,
      scalebox=0.6)




#Pffaff diagnostics
plot(p1ct, names = "e")

ser11 <- serial.test(p1ct, lags.pt = 16, type = "PT.asymptotic")
ser11$serial

norm1 <- normality.test(p1ct)
orm1$jb.mul

arch1 <- arch.test(p1ct, lags.multi = 5)
arch1$arch.mul

plot(arch1, names = "e")
plot(stability(p1ct), nc = 2)














