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
suppressMessages(library(sandwich))

GDPdata <- read.csv("GDP Data2.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)
GDPdata$Date <- as.Date(GDPdata$Date, format = "%Y/%m/%d")

datumso <- read.csv("dates2.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)
datumso$Datum <- as.Date(datumso$Datum, format = "%Y/%m/%d")

datums <- read.csv("dates4.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)
datums$Datum <- as.Date(datums$Datum, format = "%Y/%m/%d")

##For Grpahing Business cycles
recessions.df = read.table(textConnection(
    "Peak, Trough
    1990-12-31, 1993-05-30
    1996-11-30, 1999-08-31
    2007-11-30, 2009-08-31
    2013-11-30, 2016-12-31"), sep=',',
    colClasses=c('Date', 'Date'), header=TRUE)


recessions.plot = read.table(textConnection(
    "Peak, Trough
    1990-12-31, 1993-03-01
    1996-09-01, 1999-06-01
    2007-09-01, 2009-06-01
    2013-09-01, 2016-12-31"), sep=',',
    colClasses=c('Date', 'Date'), header=TRUE)


realGDP <- read.csv("RealGDP2.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)
realGDP$Date <- as.Date(realGDP$Date, format = "%Y/%m/%d")

GDPgrowth4 <- as.data.frame(sapply(log(realGDP[,-1]), diff, lag =4))
GDPgrowth1 <- as.data.frame(sapply(log(realGDP[,-1]), diff, lag =1))


##====================================##
## READING IN THE DATA ##
##====================================##
BER.M <- read.csv("Manufacturing_new faktor_sep.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)
BER.M$Faktor <- BER.M$Gewig * BER.M$Sectorw
BER.M <- BER.M[,c(1:5,8:75)]
colnames(BER.M)[1:7] <- c("region","id","sector","weight","turnover","factor","surveyQ")
exclude <- c("2016Q4","2017Q1","2017Q2")
BER.M <- subset(BER.M, !(surveyQ %in% exclude))
BER.M$factor <- as.numeric(as.character(BER.M$factor))

##===============================##
BER.B <- read.csv("Building_93Q2-17Q2.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)
colnames(BER.B)[1:6] <- c("region","id","sector","weight","factor","surveyQ")

BER.B$factor[BER.B$sector ==5000] <- 0.30
BER.B$factor[BER.B$sector ==5010] <- 0.15
BER.B$factor[BER.B$sector ==6000] <- 0.10
BER.B$factor[BER.B$sector ==6010] <- 0.15
BER.B$factor <- BER.B$factor*BER.B$weight

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
BER.V <- skoon(read.csv("Motor_92Q2-17Q2_sep.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE))
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


#----------------------
#Adjust Building Responses Rate
sub_build <- BER.B[BER.B$sector %in% c(99,88,700,701,702,703),] 
terug <- aggregate(sub_build$id, by=list(sub_build$surveyQ,sub_build$Sector), FUN = length)
terug <- cbind(obs=aggregate(terug$x, by=list(terug$Group.2), FUN = sum ),ave=aggregate(terug$x, by=list(terug$Group.2), FUN = mean ))[,-3]
sum(terug$ave.x)

#-----------------------

tafel <- aggregate(BER$id, by=list(BER$surveyQ,BER$Sector), FUN = length)
tafel <- cbind(obs=aggregate(tafel$x, by=list(tafel$Group.2), FUN = sum ),ave=aggregate(tafel$x, by=list(tafel$Group.2), FUN = mean ))[,-3]
tafel$obs.Group.1 <- as.character(tafel$obs.Group.1)
tafel$Resp <- tafel$ave.x/c(1000,1400,1400,1000)
tafel$Resp[2] <- (tafel$ave.x[2]+145)/1400
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


w <- tafel$obs.x
w[2] <- w[2]+37*145
weighted.mean(tafel$Resp,w)

BERplot <- aggregate(BER$id, by=list(BER$surveyQ,BER$Sector), FUN = length)
BERplot$Group.1 <- as.Date(as.yearqtr(BERplot$Group.1, format = "%YQ%q"), frac = 1)
g <- ggplot(BERplot, aes(x=Group.1, y=x,fill=Group.2))
g <- g + geom_bar(stat="identity")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + scale_fill_discrete(name="Sector")
g <- g + scale_y_continuous(labels=comma)
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g <- g + ylab("Number of Respondents") + xlab(" ")
g


#------------------------------------
#Illustrate responses rates
tafel <- aggregate(BER$id, by=list(BER$surveyQ,BER$Sector), FUN = length)
tafel$Group.1 <- as.Date(as.yearqtr(tafel$Group.1, format = "%YQ%q"), frac = 1)
for(i in 1:nrow(tafel)) {
    if(tafel$Group.2[i]=="Construction") { if(tafel$Group.1[i]<"2001-06-30") {
        tafel$x[i] <- (tafel$x[i]+145)} }
}

w <- dcast(tafel,Group.1 ~ Group.2, val.var=x)
w$Total <- rowSums(w[,-1],na.rm=TRUE)

for(i in 1:nrow(tafel)) {
    if(tafel$Group.2[i]=="Manufacturing") {tafel$x[i] <- tafel$x[i]/1000}
    if(tafel$Group.2[i]=="Construction") { tafel$x[i] <- (tafel$x[i])/1400}
    if(tafel$Group.2[i]=="Trade") {tafel$x[i] <- tafel$x[i]/1400}
    if(tafel$Group.2[i]=="Services") {tafel$x[i] <- tafel$x[i]/1000}
}

tydreeks <- dcast(tafel,Group.1 ~ Group.2, val.var=x)
tydreeks$Total <- rowMeans(tydreeks[,2:5],na.rm = TRUE)
for(i in 1:98) {
    tydreeks$Total[i] <- weighted.mean(tydreeks[i,2:5],w[i,2:5],na.rm = T)
}

BERplot <- melt(tydreeks,id="Group.1")
g <- ggplot(BERplot, aes(x=Group.1, y=value,colour=variable))
g <- g + geom_line(size=1)
g <- g + geom_point(size=0.5)
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + scale_fill_discrete(name="Sector")
g <- g + scale_y_continuous(labels=comma)
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g <- g + ylab("Response Rate") + xlab(" ")
g <- g + guides(fill = guide_legend(reverse = TRUE))
g <- g + theme(legend.title=element_blank()) 
g <- g + theme(legend.position="bottom")
g


#tafel <- aggregate(BER$id, by=list(BER$surveyQ,BER$Sector), FUN = length)
#tafel$Group.1 <- as.Date(as.yearqtr(tafel$Group.1, format = "%YQ%q"), frac = 1)


source("corstarsl.R")
#c <- corstarsl(cbind(tydreeks[,-1],GDPgrowth4$Manufacturing[c(-56)],GDPgrowth4$Construction[c(-56)],
#                GDPgrowth4$Trade[c(-56)],GDPgrowth4$Services[c(-56)],GDPgrowth4$RGDP[c(-56)]))
corstarsl(cbind(tydreeks[,-1],GDPgrowth4$RGDP[c(-56)]))[6,]


#Of op responses:
BERplot <- aggregate(BER$id, by=list(BER$surveyQ,BER$Sector), FUN = length)
BERplot$Group.1 <- as.Date(as.yearqtr(BERplot$Group.1, format = "%YQ%q"), frac = 1)

for(i in 1:nrow(BERplot)) {
    if(BERplot$Group.2[i]=="Construction") { if(BERplot$Group.1[i]<"2001-06-30") {
        BERplot$x[i] <- (BERplot$x[i]+145)} }
}

tydreeks <- dcast(BERplot,Group.1 ~ Group.2, val.var=x)
tydreeks$Total <- rowSums(tydreeks[,2:5],na.rm = TRUE)

source("corstarsl.R")
c <- corstarsl(cbind(tydreeks[,-1],GDPgrowth4$Manufacturing[c(-56)],GDPgrowth4$Construction[c(-56)],
               GDPgrowth4$Trade[c(-56)],GDPgrowth4$Services[c(-56)],GDPgrowth4$RGDP[c(-56)]))
c <- corstarsl(cbind(tydreeks[-1:-53,-1],GDPgrowth4$Manufacturing[c(-1:-53,-56)],GDPgrowth4$Construction[c(-1:-53,-56)],
                     GDPgrowth4$Trade[c(-1:-53,-56)],GDPgrowth4$Services[c(-1:-53,-56)],GDPgrowth4$RGDP[c(-1:-53,-56)]))
corstarsl(cbind(tydreeks[,-1],GDPgrowth4$RGDP[c(-56)]))[6,]

#Of net deel van stable samples

tafel <- aggregate(BERplot$id, by=list(BERplot$surveyQ,BERplot$Sector), FUN = length)
tafel$Group.1 <- as.Date(as.yearqtr(tafel$Group.1, format = "%YQ%q"), frac = 1)
for(i in 1:nrow(tafel)) {
    if(tafel$Group.2[i]=="Construction") { if(tafel$Group.1[i]<"2001-06-30") {
        tafel$x[i] <- (tafel$x[i]+145)} }
}

tafel1 <- aggregate(BERplot$id, by=list(BERplot$Sector), FUN = length)
tafel$Group.1 <- as.Date(as.yearqtr(tafel$Group.1, format = "%YQ%q"), frac = 1)

w <- dcast(tafel,Group.1 ~ Group.2, val.var=x)
w$Total <- rowSums(w[,-1],na.rm=TRUE)

for(i in 1:nrow(tafel)) {
    if(tafel$Group.2[i]=="Manufacturing") {tafel$x[i] <- tafel$x[i]/1000}
    if(tafel$Group.2[i]=="Construction") { tafel$x[i] <- (tafel$x[i])/1400}
    if(tafel$Group.2[i]=="Trade") {tafel$x[i] <- tafel$x[i]/1400}
    if(tafel$Group.2[i]=="Services") {tafel$x[i] <- tafel$x[i]/1000}
}

tydreeks <- dcast(tafel,Group.1 ~ Group.2, val.var=x)
tydreeks$Total <- rowMeans(tydreeks[,2:5],na.rm = TRUE)
for(i in 1:98) {
    tydreeks$Total[i] <- weighted.mean(tydreeks[i,2:5],w[i,2:5],na.rm = T)
}

plot <- melt(tydreeks,id="Group.1")
g <- ggplot(plot, aes(x=Group.1, y=value,colour=variable))
g <- g + geom_line(size=1)
g <- g + geom_point(size=0.5)
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + scale_fill_discrete(name="Sector")
g <- g + scale_y_continuous(labels=comma)
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g <- g + ylab("Response Rate") + xlab(" ")
g <- g + guides(fill = guide_legend(reverse = TRUE))
g <- g + theme(legend.title=element_blank()) 
g <- g + theme(legend.position="bottom")
g



#======================================
#Illustrate forecast error sample sizes
m_errors$Sector <- "Manufacturing"
b_errors$Sector <- "Construction"
t_errors$Sector <- "Trade"
s_errors$Sector <- "Services"

errors <- rbind(m_errors,b_errors,t_errors,s_errors)
    
BERplot <- aggregate(errors$Sector, by=list(errors$Datum,errors$Sector), FUN = length)
g <- ggplot(BERplot, aes(x=Group.1, y=x,fill=Group.2))
g <- g + geom_bar(stat="identity")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + scale_fill_discrete(name="Sector")
g <- g + scale_y_continuous(labels=comma)
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g <- g + ylab("Number of Respondents") + xlab(" ")
g

BERplot <- aggregate(errors$Sector, by=list(errors$Datum), FUN = length)
mean(BERplot$x)



#=========================
#Illustrating weights
#-------------------------
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
g <- g + ylab("Weights") + xlab(" ")
g <- g + guides(fill = guide_legend(reverse = TRUE))
g


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

BERplot$Date <- as.Date(as.yearqtr(BERplot$Date, format = "%YQ%q"), frac = 1)
#BERplot <- BERplot[,c(1,5,4,8,7,2,3,6)]
index_plot <- melt(BERplot[,-9], id="Date")
g <- ggplot(index_plot, aes(x=Date,y=value,group=variable,fill=variable)) 
g <- g + geom_bar(stat="identity")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + scale_fill_discrete(name="Sub-sector")
g <- g + scale_y_continuous(labels=comma)
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g <- g + ylab("Weights") + xlab(" ")
g <- g + guides(fill = guide_legend(reverse = TRUE))
g


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

BERplot$Date <- as.Date(as.yearqtr(BERplot$Date, format = "%YQ%q"), frac = 1)
BERplot <- BERplot[,c(1,8,4,3,5,2,7,6,9)]
index_plot <- melt(BERplot[,-9], id="Date")
g <- ggplot(index_plot, aes(x=Date,y=value,group=variable,fill=variable)) 
g <- g + geom_bar(stat="identity")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + scale_fill_discrete(name="Sub-sector")
g <- g + scale_y_continuous(labels=comma)
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g <- g + ylab("Weights") + xlab(" ")
g <- g + guides(fill = guide_legend(reverse = TRUE))
g


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

BERplot$Date <- as.Date(as.yearqtr(BERplot$Date, format = "%YQ%q"), frac = 1)
BERplot <- BERplot[,c(1,5,2,7,6,4,3,8)]
index_plot <- melt(BERplot[,-8], id="Date")
g <- ggplot(index_plot, aes(x=Date,y=value,group=variable,fill=variable)) 
g <- g + geom_bar(stat="identity")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + scale_fill_discrete(name="Sub-sector")
g <- g + scale_y_continuous(labels=comma)
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g <- g + ylab("Weights") + xlab(" ")
g <- g + guides(fill = guide_legend(reverse = TRUE))
g


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
g <- g + ylab("Weights") + xlab(" ")
g <- g + guides(fill = guide_legend(reverse = TRUE))
g


#==========================
#-------CONFIDENCE---------
#==========================
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
    w.confidence <- merge(datumso,w.confidence,by.x="Date",by.y="row.names", all=TRUE)[,-3]
    w.confidence[,14:15] <- na.approx(w.confidence[,14:15],na.rm = FALSE)
    
    return(w.confidence)
}

w.indicators.M <- calc_wconf(BER[BER$Sector=="Manufacturing",])
w.indicators.B <- calc_wconf(BER[BER$Sector=="Construction",])
w.indicators.T <- calc_wconf(BER[BER$Sector=="Trade",])
w.indicators.S <- calc_wconf(BER[BER$Sector=="Services",])

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


conf_indices <- cbind(w.indicators,GDPdata$BER_BCI, GDPgrowth4$SACCI,GDPgrowth4$RGDP)
#colnames(conf_indices) <- c("Date","Activity","Confidence","BER_BCI","SACCI_Growth","RGDP_Growth")
colnames(conf_indices) <- c("Date","Confidence_Current","Confidence_Expected","BER_BCI","SACCI_Growth","RGDP_Growth")


index_plot <- cbind(activity[,1:2],conf[,2])
index_plot$Date <- datums$Datum
#colnames(index_plot) <- c("Date","Activity","Confidence")
colnames(index_plot) <- c("Date","Confidence (Current)","Confidence (Expected)")
index_plot <- melt(index_plot, id="Date")  # convert to long format
g1 <- ggplot(index_plot, aes(x=Date,y=value,group=variable,colour=variable)) 
g1 <- g1 + geom_line()
g1 <- g1 + theme(legend.title=element_blank())
g1 <- g1 + ggtitle("Manufacturing") + theme(plot.title = element_text(hjust = 0.5))
g1 <- g1 + ylab("Indicator") + xlab("")
g1 <- g1 + theme(legend.position="none")
g1 <- g1 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g1 <- g1 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

index_plot <- cbind(activity[,c(1,3)],conf[,3])
index_plot$Date <- datums$Datum
#colnames(index_plot) <- c("Date","Activity","Confidence")
colnames(index_plot) <- c("Date","Confidence (Current)","Confidence (Expected)")
index_plot <- melt(index_plot, id="Date")  # convert to long format
g2 <- ggplot(index_plot, aes(x=Date,y=value,group=variable,colour=variable)) 
g2 <- g2 + geom_line()
g2 <- g2 + theme(legend.title=element_blank())
g2 <- g2 + ggtitle("Construction") + theme(plot.title = element_text(hjust = 0.5))
g2 <- g2 + ylab("") + xlab("")
g2 <- g2 + theme(legend.position="none")
g2 <- g2 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g2 <- g2 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

index_plot <- cbind(activity[,c(1,4)],conf[,4])
index_plot$Date <- datums$Datum
#colnames(index_plot) <- c("Date","Activity","Confidence")
colnames(index_plot) <- c("Date","Confidence (Current)","Confidence (Expected)")
index_plot <- melt(index_plot, id="Date")  # convert to long format
g3 <- ggplot(index_plot, aes(x=Date,y=value,group=variable,colour=variable)) 
g3 <- g3 + geom_line()
g3 <- g3 + theme(legend.title=element_blank())
g3 <- g3 + ggtitle("Trade") + theme(plot.title = element_text(hjust = 0.5))
g3 <- g3 + ylab("Indicator") + xlab("")
g3 <- g3 + theme(legend.position="bottom")
g3 <- g3 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g3 <- g3 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

index_plot <- cbind(activity[,c(1,5)],conf[,5])
index_plot$Date <- datums$Datum
#colnames(index_plot) <- c("Date","Activity","Confidence")
colnames(index_plot) <- c("Date","Confidence (Current)","Confidence (Expected)")
index_plot <- melt(index_plot, id="Date")  # convert to long format
g4 <- ggplot(index_plot, aes(x=Date,y=value,group=variable,colour=variable)) 
g4 <- g4 + geom_line()
g4 <- g4 + theme(legend.title=element_blank())
g4 <- g4 + ggtitle("Services") + theme(plot.title = element_text(hjust = 0.5))
g4 <- g4 + ylab("") + xlab("")
g4 <- g4 + theme(legend.position="bottom")
g4 <- g4 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g4 <- g4 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

library(gridExtra)
library(grid)

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




index_plot <- conf_indices
index_plot$Date <- datums$Datum
colnames(index_plot) <- c("Date","Confidence (Current)","Confidence (Expected)")
g <- ggplot(index_plot) 
g <- g + geom_line(aes(x=Date, y=`Confidence (Current)`, colour="Confidence (Current)"), size = 1)
g <- g + geom_line(aes(x=Date, y=`Confidence (Expected)`, colour="Confidence (Expected)"), size = 1)
g <- g + labs(color="Legend text")
g <- g + geom_rect(data=recessions.plot, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='grey', alpha=0.5)
g <- g + ylab("Indicator") + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) 
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"), expand=c(0,0), 
                      limits = as.Date(c("1990-12-31", NA)))
g <- g + theme(legend.position="bottom")
g





conf_indices1 <- cbind(w.indicators,GDPdata$BER_BCI, GDPdata$SACCI,GDPgrowth4$RGDP)
#colnames(conf_indices1) <- c("Date","Activity","Confidence","BER_BCI","SACCI_BCI","RGDP_Growth")
colnames(conf_indices1) <- c("Date","Confidence (Current)", "Confidence (Expected)", "BER BCI", "SACCI BCI", "RGDP Growth")

#conf_indices <- cbind(w.indicators_2,GDPdata$BER_BCI, GDPdata$SACCI_G,GDPgrowth4$RGDP)
#colnames(conf_indices) <- c("Date","Activity","Confidence","BER_BCI","SACCI_Growth","RGDP_Growth")

index_plot <- conf_indices1
index_plot$Date <- datums$Datum
index_plot[,2:6] <- scale(index_plot[,2:6])
g <- ggplot(index_plot) 
g <- g + geom_line(aes(x=Date, y=`Confidence (Current)`, colour="Confidence (Current)", linetype="Confidence (Current)", size = "Confidence (Current)"))
g <- g + geom_line(aes(x=Date, y=`Confidence (Expected)`, colour="Confidence (Expected)", linetype="Confidence (Expected)", size = "Confidence (Expected)"))
g <- g + geom_line(aes(x=Date, y=`BER BCI`, colour="BER BCI", linetype="BER BCI", size = "BER BCI"))
g <- g + geom_line(aes(x=Date, y=`SACCI BCI`, colour="SACCI BCI", linetype="SACCI BCI", size = "SACCI BCI"))
g <- g + geom_line(aes(x=Date, y=`RGDP Growth`, colour="RGDP Growth", linetype="RGDP Growth", size = "RGDP Growth"))
g <- g + scale_linetype_manual(values=c("twodash","solid","solid","dashed","longdash"))
g <- g + scale_size_manual(values=c(0.71,1.1,1.1,1.1,0.71))
g <- g + scale_colour_manual(values=c("#A3A500","#F8766D","#00BFC4","#E76BF3","#00BF7D"))
g <- g + labs(color="Legend text", linetype="Legend text", size="Legend text")
g <- g + geom_rect(data=recessions.plot, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='grey', alpha=0.5)
g <- g + ylab("Standardised Indicator") + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) 
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"), expand=c(0,0),limits = as.Date(c("1990-12-31", NA)))
g <- g + theme(legend.position="bottom")
g



source("corstarsl.R")
xt <- conf_indices[,-1]
colnames(xt) <- c("Confidence (Current)","Confidence (Expected)","BER BCI","SACCI Growth","Real GDP Growth")
xt <- xtable(corstarsl(xt), caption="Correlations between confidence indicators and real GDP growth")
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

index_plot <- manufac
colnames(index_plot) <- c("Date","Confidence (Current)", "Confidence (Expected)","BER BCI", "RGDP Growth")
index_plot$Date <- datums$Datum
index_plot[,-1] <- scale(index_plot[,-1])
index_plot <- melt(index_plot, id="Date")  # convert to long format
g1 <- ggplot(index_plot, aes(x=Date,y=value,group=variable,colour=variable,linetype=variable)) 
g1 <- g1 + scale_linetype_manual(values=c("solid","solid","dashed","twodash"))
g1 <- g1 + geom_line()
g1 <- g1 + theme(legend.title=element_blank())
g1 <- g1 + ggtitle("Manufacturing") + theme(plot.title = element_text(hjust = 0.5))
g1 <- g1 + ylab("Indicator") + xlab("")
g1 <- g1 + theme(legend.position="none")
g1 <- g1 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g1 <- g1 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

index_plot <- construct
colnames(index_plot) <- c("Date","Confidence (Current)", "Confidence (Expected)","BER BCI", "RGDP Growth")
index_plot$Date <- datums$Datum
index_plot[,-1] <- scale(index_plot[,-1])
index_plot <- melt(index_plot, id="Date")  # convert to long format
g2 <- ggplot(index_plot, aes(x=Date,y=value,group=variable,colour=variable,linetype=variable))
g2 <- g2 + scale_linetype_manual(values=c("solid","solid","dashed","twodash"))
g2 <- g2 + geom_line()
g2 <- g2 + theme(legend.title=element_blank())
g2 <- g2 + ggtitle("Construction") + theme(plot.title = element_text(hjust = 0.5))
g2 <- g2 + ylab("") + xlab("")
g2 <- g2 + theme(legend.position="none")
g2 <- g2 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g2 <- g2 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

index_plot <- trade
colnames(index_plot) <- c("Date","Confidence (Current)", "Confidence (Expected)","BER BCI", "RGDP Growth")
index_plot$Date <- datums$Datum
index_plot[,-1] <- scale(index_plot[,-1])
index_plot <- melt(index_plot, id="Date")  # convert to long format
g3 <- ggplot(index_plot, aes(x=Date,y=value,group=variable,colour=variable,linetype=variable)) 
g3 <- g3 + scale_linetype_manual(values=c("solid","solid","dashed","twodash"))
g3 <- g3 + geom_line()
g3 <- g3 + theme(legend.title=element_blank())
g3 <- g3 + ggtitle("Trade") + theme(plot.title = element_text(hjust = 0.5))
g3 <- g3 + ylab("Indicator") + xlab("")
g3 <- g3 + theme(legend.position="bottom")
g3 <- g3 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g3 <- g3 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

index_plot <- services
colnames(index_plot) <- c("Date","Confidence (Current)", "Confidence (Expected)","BER BCI", "RGDP Growth")
index_plot$Date <- datums$Datum
index_plot[,-1] <- scale(index_plot[,-1])
index_plot <- melt(index_plot, id="Date")  # convert to long format
g4 <- ggplot(index_plot, aes(x=Date,y=value,group=variable,colour=variable,linetype=variable)) 
g4 <- g4 + scale_linetype_manual(values=c("solid","solid","dashed","twodash"))
g4 <- g4 + geom_line()
g4 <- g4 + theme(legend.title=element_blank())
g4 <- g4 + ggtitle("Services") + theme(plot.title = element_text(hjust = 0.5))
g4 <- g4 + ylab("") + xlab("")
g4 <- g4 + theme(legend.position="bottom")
g4 <- g4 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g4 <- g4 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

library(gridExtra)
library(grid)

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
xt1[1,] <- c("Confidence (Cur)","Confidence (Exp)","BER BCI","Confidence (Cur)","Confidence (Exp)","BER BCI")
xt2[1,] <- c("Confidence (Cur)","Confidence (Exp)","BER BCI","Confidence (Cur)","Confidence (Exp)","BER BCI")
colnames(xt1) <- c(" ","Manufacturing"," "," ","Construction"," ")
colnames(xt2) <- c(" ","Trade"," "," ","Services"," ")
row.names(xt1) <- c(" ","Confidence (Exp)","BER BCI","RGDP Growth")
row.names(xt2) <- c(" ","Confidence (Exp)","BER BCI","RGDP Growth")

xt <- xtable(xt1, caption="Correlations between sectoral confidence and real sectoral GDP growth")
print(xt, "latex",comment=FALSE, caption.placement = getOption("xtable.caption.placement", "top"), scalebox = 0.8)
xt <- xtable(xt2)
print(xt, "latex",comment=FALSE, caption.placement = getOption("xtable.caption.placement", "top"), scalebox = 0.8)



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

#------------------------------------------
#Include confidence bands
#sigma^2/n

calc_sd <- function(data) {
    sd <- aggregate(data[,(match("surveyQ",colnames(data))+1):ncol(data)], by=list(data$surveyQ), FUN=sd, na.rm=TRUE)
    n <- aggregate(data[,(match("surveyQ",colnames(data))+1):ncol(data)], by=list(data$surveyQ), FUN=function(x) sum(!is.na(x)))
    sdn <- sd[,-1]/n[,-1]
    sdn <- cbind(sd[,1:2],sdn)[,-2]
    sdn$Conf_cc <- rowMeans(sdn[,c("Q1","Q2A","Q3A","Q4A","Q5A","Q6A")],na.rm = TRUE, dims = 1)
    sdn$Conf_fl <- rowMeans(sdn[,c("Q2P","Q3P","Q4P","Q5P","Q6P")],na.rm = TRUE, dims = 1)
    #Row means for simple composite indicators (the question is which questions to include)
    sdn <- merge(datums,sdn,by.x="Date",by.y="Group.1", all=TRUE)
    sdn[,14:15] <- na.approx(sdn[,14:15],na.rm = FALSE)
    return(sdn)
}

sd.M <- calc_sd(BER[BER$Sector=="Manufacturing",])
sd.B <- calc_sd(BER[BER$Sector=="Construction",])
sd.T <- calc_sd(BER[BER$Sector=="Trade",])
sd.S <- calc_sd(BER[BER$Sector=="Services",])

calc_wsd <- function(data) {
    ##Weighted versions
    weeg <- function(data) {  #calculate weighted standard deviation for each quarter for all columns
        n <- nrow(data)
        temp <- cbind(factor=data$factorn,data$factorn*data[,8:ncol(data)])
        xbar <- colSums(temp, na.rm=TRUE, dims = 1)/sapply(colnames(temp), function(x) sum(temp$factor[!is.na(temp[colnames(temp) == x])]))
        #temp <- data[,-1:-7]
        #this is the weighted standard devation: sum[wi*(xi-xbar)^2]/sum(wi) 
        idio <- sqrt(sapply(colnames(temp), function(x) sum((temp[,x]-xbar[x])*(temp[,x]-xbar[x])*temp$factor,na.rm=TRUE))/
            sapply(colnames(temp), function(x) sum(temp$factor[!is.na(temp[colnames(temp) == x])],na.rm=TRUE))) 
        idio[-1] <- idio[-1]/sqrt(n)
        return(idio)
    }
    w.errors <- as.data.frame(t(sapply(levels(data$surveyQ), function(kwartaal) weeg(data[data$surveyQ==kwartaal,]))))
    #w.errors <- as.data.frame(t(sapply(datums$Datum, function(kwartaal) weeg.3(data[data$Datum==kwartaal,]))))
    w.errors <- merge(datums,w.errors,by.x="Date",by.y="row.names", all=TRUE)[,-1]
    #colnames(w.errors) <- c("Date","Idio.factor","Idio.Q2","Idio.Q3","Idio.Q4","Idio.Q5","Idio.Q6")
    w.errors$Conf_cc <- rowMeans(w.errors[,c("Q1","Q2A","Q3A","Q4A","Q6A")],na.rm = TRUE, dims = 1)
    w.errors$Conf_fl <- rowMeans(w.errors[,c("Q2P","Q3P","Q4P","Q6P")],na.rm = TRUE, dims = 1)
    #w.errors[,14:15] <- na.approx(w.errors[,14:15],na.rm=FALSE)
    #w.errors <- w.errors[,c(1,14:15)]
    #w.errors[,2] <- c(NA,w.errors[-99,2])
    #w.errors[,3] <- c(NA,w.errors[-99,3])
    return(w.errors)
}

wsd.M <- calc_wsd(BER[BER$Sector=="Manufacturing",])
wsd.B <- calc_wsd(BER[BER$Sector=="Construction",])
wsd.T <- calc_wsd(BER[BER$Sector=="Trade",])
wsd.S <- calc_wsd(BER[BER$Sector=="Services",])



weights <- GDPdata[,c(1:4,6)]
sdact <- cbind(wsd.M[,c(1,14)],wsd.B[,14],wsd.T[,14],wsd.S[,14])
colnames(sdact) <- c("Date","Manufacturing","Construction","Trade","Services")
sdconf <- cbind(wsd.M[,c(1,15)],wsd.B[,15],wsd.T[,15],wsd.S[,15])
colnames(sdconf) <- c("Date","Manufacturing","Construction","Trade","Services")


#create weighted means by GDP share
sdact$Activity <- sapply(sdact$Date, function(x) weighted.mean(sdact[which(sdact$Date==x),c(2:5)], 
                                                                     weights[weights$Date %in% x,-1],na.rm=TRUE))
sdconf$Confidence <- sapply(sdconf$Date, function(x) weighted.mean(sdconf[which(sdconf$Date==x),c(2:5)], 
                                                               weights[weights$Date==x,-1],na.rm=TRUE))
wsd <- cbind(sdact[,c(1,6)],sdconf[,6])
#w.indicators <- seisoen(w.indicators)
colnames(wsd) <- c("Date","Activity","Confidence")
#w.indicators$C <- -1*princomp(w.indicators[,2:3])$scores[,1]





index_plot <- w.indicators
index_plot$actU <- index_plot$Activity+2*wsd$Activity
index_plot$actL <- index_plot$Activity-2*wsd$Activity
index_plot$conU <- index_plot$Confidence+2*wsd$Confidence
index_plot$conL <- index_plot$Confidence-2*wsd$Confidence

#colnames(index_plot) <- c("Date","Activity","Confidence")
#colnames(index_plot) <- c("Date","Confidence (Current)","Confidence (Expected)",
#                          "actU","actL","conU","conL")
g1 <- ggplot(index_plot) 
g1 <- g1 + geom_line(aes(x=Date, y=Activity, colour="Activity", linetype="Activity", size = "Activity)"))
g1 <- g1 + geom_line(aes(x=Date, y=actU, colour="actU", linetype="actU", size = "actU"))
g1 <- g1 + geom_line(aes(x=Date, y=actL, colour="actL", linetype="actL", size = "actL"))
g1 <- g1 + ylab("Indicator") + xlab("")
g1 <- g1 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g1 <- g1 + theme(legend.title=element_blank()) 
g1 <- g1 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"), expand=c(0,0), 
                      limits = as.Date(c("1990-12-31", NA)))
g1 <- g1 + scale_linetype_manual(values=c("solid","dashed","dashed"))
g1 <- g1 + scale_size_manual(values=c(1,0.7,0.7))
g1 <- g1 + scale_colour_manual(values=c("#F8766D","blue","blue"))
g1 <- g1 + labs(color="Legend text", linetype="Legend text", size="Legend text")
g1 <- g1 + theme(legend.position="none")
g1 <- g1 + ggtitle("Confidence (Current)") + theme(plot.title = element_text(hjust = 0.5))


g2 <- ggplot(index_plot) 
g2 <- g2 + geom_line(aes(x=Date, y=Confidence, colour="Confidence", linetype="Confidence", size = "Confidence"))
g2 <- g2 + geom_line(aes(x=Date, y=conU, colour="conU", linetype="conU", size = "conU"))
g2 <- g2 + geom_line(aes(x=Date, y=conL, colour="conL", linetype="conL", size = "conL"))
g2 <- g2 + ylab("Indicator") + xlab("")
g2 <- g2 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g2 <- g2 + theme(legend.title=element_blank()) 
g2 <- g2 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"), expand=c(0,0), 
                      limits = as.Date(c("1990-12-31", NA)))
g2 <- g2 + scale_linetype_manual(values=c("solid","dashed","dashed"))
g2 <- g2 + scale_size_manual(values=c(1,0.7,0.7))
g2 <- g2 + scale_colour_manual(values=c("#00BFC4","red","red"))
g2 <- g2 + labs(color="Legend text", linetype="Legend text", size="Legend text")
g2 <- g2 + theme(legend.position="none")
g2 <- g2 + ggtitle("Confidence (Expected)") + theme(plot.title = element_text(hjust = 0.5))

library(gridExtra)
library(grid)
grid.arrange(g1,g2,nrow=1,ncol=2)



index_plot <- w.indicators.M[,c(2,14)]
index_plot$actU <- index_plot$Conf_cc+2*wsd.M$Conf_cc
index_plot$actL <- index_plot$Conf_cc-2*wsd.M$Conf_cc
colnames(index_plot)[1:2] <- c("Date","Activity")
g1 <- ggplot(index_plot) 
g1 <- g1 + geom_line(aes(x=Date, y=Activity, colour="Activity", linetype="Activity", size = "Activity)"))
g1 <- g1 + geom_line(aes(x=Date, y=actU, colour="actU", linetype="actU", size = "actU"))
g1 <- g1 + geom_line(aes(x=Date, y=actL, colour="actL", linetype="actL", size = "actL"))
g1 <- g1 + ylab("Indicator") + xlab("")
g1 <- g1 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g1 <- g1 + theme(legend.title=element_blank()) 
g1 <- g1 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"), expand=c(0,0), 
                        limits = as.Date(c("1990-12-31", NA)))
g1 <- g1 + scale_linetype_manual(values=c("solid","dashed","dashed"))
g1 <- g1 + scale_size_manual(values=c(1,0.7,0.7))
g1 <- g1 + scale_colour_manual(values=c("#F8766D","blue","blue"))
g1 <- g1 + labs(color="Legend text", linetype="Legend text", size="Legend text")
g1 <- g1 + theme(legend.position="none")
g1 <- g1 + ggtitle("Manufacturing") + theme(plot.title = element_text(hjust = 0.5))

index_plot <- w.indicators.B[,c(2,14)]
index_plot$actU <- index_plot$Conf_cc+2*wsd.B$Conf_cc
index_plot$actL <- index_plot$Conf_cc-2*wsd.B$Conf_cc
colnames(index_plot)[1:2] <- c("Date","Activity")
g2 <- ggplot(index_plot) 
g2 <- g2 + geom_line(aes(x=Date, y=Activity, colour="Activity", linetype="Activity", size = "Activity)"))
g2 <- g2 + geom_line(aes(x=Date, y=actU, colour="actU", linetype="actU", size = "actU"))
g2 <- g2 + geom_line(aes(x=Date, y=actL, colour="actL", linetype="actL", size = "actL"))
g2 <- g2 + ylab("Indicator") + xlab("")
g2 <- g2 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g2 <- g2 + theme(legend.title=element_blank()) 
g2 <- g2 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"), expand=c(0,0), 
                       limits = as.Date(c("1990-12-31", NA)))
g2 <- g2 + scale_linetype_manual(values=c("solid","dashed","dashed"))
g2 <- g2 + scale_size_manual(values=c(1,0.7,0.7))
g2 <- g2 + scale_colour_manual(values=c("#F8766D","blue","blue"))
g2 <- g2 + labs(color="Legend text", linetype="Legend text", size="Legend text")
g2 <- g2 + theme(legend.position="none")
g2 <- g2 + ggtitle("Construction") + theme(plot.title = element_text(hjust = 0.5))

index_plot <- w.indicators.T[,c(2,14)]
index_plot$actU <- index_plot$Conf_cc+2*wsd.T$Conf_cc
index_plot$actL <- index_plot$Conf_cc-2*wsd.T$Conf_cc
colnames(index_plot)[1:2] <- c("Date","Activity")
g3 <- ggplot(index_plot) 
g3 <- g3 + geom_line(aes(x=Date, y=Activity, colour="Activity", linetype="Activity", size = "Activity)"))
g3 <- g3 + geom_line(aes(x=Date, y=actU, colour="actU", linetype="actU", size = "actU"))
g3 <- g3 + geom_line(aes(x=Date, y=actL, colour="actL", linetype="actL", size = "actL"))
g3 <- g3 + ylab("Indicator") + xlab("")
g3 <- g3 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g3 <- g3 + theme(legend.title=element_blank()) 
g3 <- g3 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"), expand=c(0,0), 
                        limits = as.Date(c("1990-12-31", NA)))
g3 <- g3 + scale_linetype_manual(values=c("solid","dashed","dashed"))
g3 <- g3 + scale_size_manual(values=c(1,0.7,0.7))
g3 <- g3 + scale_colour_manual(values=c("#F8766D","blue","blue"))
g3 <- g3 + labs(color="Legend text", linetype="Legend text", size="Legend text")
g3 <- g3 + theme(legend.position="none")
g3 <- g3 + ggtitle("Trade") + theme(plot.title = element_text(hjust = 0.5))

index_plot <- w.indicators.S[,c(2,14)]
index_plot$actU <- index_plot$Conf_cc+2*wsd.S$Conf_cc
index_plot$actL <- index_plot$Conf_cc-2*wsd.S$Conf_cc
colnames(index_plot)[1:2] <- c("Date","Activity")
g4 <- ggplot(index_plot) 
g4 <- g4 + geom_line(aes(x=Date, y=Activity, colour="Activity", linetype="Activity", size = "Activity)"))
g4 <- g4 + geom_line(aes(x=Date, y=actU, colour="actU", linetype="actU", size = "actU"))
g4 <- g4 + geom_line(aes(x=Date, y=actL, colour="actL", linetype="actL", size = "actL"))
g4 <- g4 + ylab("Indicator") + xlab("")
g4 <- g4 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g4 <- g4 + theme(legend.title=element_blank()) 
g4 <- g4 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"), expand=c(0,0), 
                        limits = as.Date(c("1990-12-31", NA)))
g4 <- g4 + scale_linetype_manual(values=c("solid","dashed","dashed"))
g4 <- g4 + scale_size_manual(values=c(1,0.7,0.7))
g4 <- g4 + scale_colour_manual(values=c("#F8766D","blue","blue"))
g4 <- g4 + labs(color="Legend text", linetype="Legend text", size="Legend text")
g4 <- g4 + theme(legend.position="none")
g4 <- g4 + ggtitle("Services") + theme(plot.title = element_text(hjust = 0.5))

library(gridExtra)
library(grid)

grid.arrange(g1, g2, g3, g4, ncol=2, nrow =2)





library(e1071)   
y <- uncert_indices[,2] # load e1071 
moment(y, order=4, center=TRUE, na.rm=T)/length(y) - sd(y,na.rm=T)^4 *(length(y)-3)/(length(y)*(length(y)-1))


calc_wsdsd <- function(data) {
    ##Weighted versions
    weeg <- function(data) {  #calculate weighted standard deviation for each quarter for all columns
        n <- nrow(data)
        temp <- cbind(factor=data$factorn,data$factorn*data[,8:ncol(data)])
        xbar <- colSums(temp, na.rm=TRUE, dims = 1)/sapply(colnames(temp), function(x) sum(temp$factor[!is.na(temp[colnames(temp) == x])]))
        #temp <- data[,-1:-7]
        #this is the weighted standard devation: sum[wi*(xi-xbar)^2]/sum(wi) 
        idio <- sqrt(sapply(colnames(temp), function(x) sum((temp[,x]-xbar[x])*(temp[,x]-xbar[x])*temp$factor,na.rm=TRUE))/
                         sapply(colnames(temp), function(x) sum(temp$factor[!is.na(temp[colnames(temp) == x])],na.rm=TRUE)))^4*(n-3)/(n*(n-1)) 
        vierde <- sapply(colnames(temp), function(x) sum((temp[,x]-xbar[x])*(temp[,x]-xbar[x])*(temp[,x]-xbar[x])*(temp[,x]-xbar[x])*temp$factor,na.rm=TRUE))/
                         sapply(colnames(temp), function(x) sum(temp$factor[!is.na(temp[colnames(temp) == x])],na.rm=TRUE))/n
        vierde <- vierde - idio
        return(vierde)
    }
    w.errors <- as.data.frame(t(sapply(levels(data$surveyQ), function(kwartaal) weeg(data[data$surveyQ==kwartaal,]))))
    #w.errors <- as.data.frame(t(sapply(datums$Datum, function(kwartaal) weeg.3(data[data$Datum==kwartaal,]))))
    w.errors <- merge(datums,w.errors,by.x="Date",by.y="row.names", all=TRUE)[,-1]
    #colnames(w.errors) <- c("Date","Idio.factor","Idio.Q2","Idio.Q3","Idio.Q4","Idio.Q5","Idio.Q6")
    w.errors$Conf_cc <- rowMeans(w.errors[,c("Q1","Q2A","Q3A","Q4A","Q6A")],na.rm = TRUE, dims = 1)
    w.errors$Conf_fl <- rowMeans(w.errors[,c("Q2P","Q3P","Q4P","Q6P")],na.rm = TRUE, dims = 1)
    #w.errors <- w.errors[,c(1,14:15)]
    #w.errors[,2] <- c(NA,w.errors[-99,2])
    #w.errors[,3] <- c(NA,w.errors[-99,3])
    return(w.errors)
}



wsdsd.M <- calc_wsdsd(BER[BER$Sector=="Manufacturing",])
for(t in 2:nrow(wsdsd.M)) { wsdsd.M$Disp[t-1] <- sqrt((w.uncertainty.M$Uncert_cc[t]^2*wsdsd.M$Conf_fl[t-1]^2 + w.uncertainty.M$Uncert_fl[t-1]^2*wsdsd.M$Conf_cc[t]^2)/w.uncertainty.M$Uncert_fl[t-1]^4 )}

wsdsd.B <- calc_wsdsd(BER[BER$Sector=="Construction",])
for(t in 6:nrow(wsdsd.B)) { wsdsd.B$Disp[t-1] <- sqrt((w.uncertainty.B$Uncert_cc[t]^2*wsdsd.B$Conf_fl[t-1]^2 + w.uncertainty.B$Uncert_fl[t-1]^2*wsdsd.B$Conf_cc[t]^2)/w.uncertainty.B$Uncert_fl[t-1]^4 )}

wsd.T <- calc_wsd(BER[BER$Sector=="Trade",])
wsd.S <- calc_wsd(BER[BER$Sector=="Services",])



#=============================================
#Alternative turning points
#Confdence_Current
x <- conf_indices[,c(1,2)]
x[,2] <- scale(x[,2])

peaks <- NULL
troughs <- NULL
for(i in 3:96) {
    if((x[i-2,2]<0|x[i-1,2]<0) & (x[i,2]<0) & (x[i+1,2]>0) & (x[i+2,2]>0)) {
        troughs[i] <- as.character(x[i,1])
    }   
    if((x[i-2,2]>0|x[i-1,2]>0) & (x[i,2]>0) & (x[i+1,2]<0) & (x[i+2,2]<0)) {
        peaks[i] <- as.character(x[i,1])
    }  
}
troughs[99] <- "2016-12-31"
troughs <- as.Date(troughs[!is.na(troughs)], format = "%Y-%m-%d")
peaks[1] <- "1990-12-31"
peaks <- as.Date(peaks[!is.na(peaks)], format = "%Y-%m-%d")
tp1 <- cbind(as.data.frame(peaks),as.data.frame(troughs))
colnames(tp1) <- c("Peak","Trough")


#Confdence_Expected
x <- conf_indices[,c(1,3)]
x[,2] <- scale(x[,2])

peaks <- NULL
troughs <- NULL
for(i in 3:96) {
    if((x[i-2,2]<0|x[i-1,2]<0) & (x[i,2]<0) & (x[i+1,2]>0) & (x[i+2,2]>0)) {
        troughs[i] <- as.character(x[i,1])
    }   
    if((x[i-2,2]>0) & (x[i-1,2]>0) & (x[i,2]>0) & (x[i+1,2]<0) & (x[i+2,2]<0|x[i+3,2]<0)) {
        peaks[i] <- as.character(x[i,1])
    }  
}
troughs[99] <- "2016-12-31"
troughs <- as.Date(troughs[!is.na(troughs)], format = "%Y-%m-%d")
peaks[1] <- "1990-12-31"
peaks <- as.Date(peaks[!is.na(peaks)], format = "%Y-%m-%d")
tp2 <- cbind(as.data.frame(peaks),as.data.frame(troughs))
colnames(tp2) <- c("Peak","Trough")



#BER BCI
x <- conf_indices[,c(1,4)]
#x[,2] <- x[,2]-50
x[,2] <- scale(x[,2])

peaks <- NULL
troughs <- NULL
for(i in 3:96) {
    if((x[i-2,2]<0|x[i-1,2]<0) & (x[i,2]<0) & (x[i+1,2]>0) & (x[i+2,2]>0)) {
        troughs[i] <- as.character(x[i,1])
    }   
    if((x[i-2,2]>0|x[i-1,2]>0) & (x[i,2]>0) & (x[i+1,2]<0) & (x[i+2,2]<0|x[i+3,2]<0)) {
        peaks[i] <- as.character(x[i,1])
    }  
}
troughs[99] <- "2016-12-31"
troughs <- as.Date(troughs[!is.na(troughs)], format = "%Y-%m-%d")
peaks[1] <- "1990-12-31"
peaks <- as.Date(peaks[!is.na(peaks)], format = "%Y-%m-%d")
tp3 <- cbind(as.data.frame(peaks),as.data.frame(troughs))
colnames(tp3) <- c("Peak","Trough")


#SACCI BCI
x <- conf_indices[,c(1,5)]
#x[,2] <- scale(x[,2])

peaks <- NULL
troughs <- NULL
for(i in 7:96) {
    if((x[i-2,2]<0) & (x[i-1,2]<0) & (x[i,2]<0) & (x[i+1,2]>0) & (x[i+2,2]>0)) {
        troughs[i] <- as.character(x[i,1])
    }   
    if((x[i-2,2]>0|x[i-1,2]>0) & (x[i,2]>0) & (x[i+1,2]<0) & (x[i+2,2]<0)) {
        peaks[i] <- as.character(x[i,1])
    }  
}
troughs[99] <- "2016-12-31"
troughs <- as.Date(troughs[!is.na(troughs)], format = "%Y-%m-%d")
peaks[1] <- "1990-12-31"
peaks <- as.Date(peaks[!is.na(peaks)], format = "%Y-%m-%d")
tp4 <- cbind(as.data.frame(peaks),as.data.frame(troughs))
colnames(tp4) <- c("Peak","Trough")



#RGDP GRowth
x <- conf_indices[,c(1,6)]
#x[,2] <- scale(x[,2])

peaks <- NULL
troughs <- NULL
for(i in 3:96) {
    if((x[i-2,2]<0) & (x[i-1,2]<0) & (x[i,2]<0) & (x[i+1,2]>0) & (x[i+2,2]>0)) {
        troughs[i] <- as.character(x[i,1])
    }   
    if((x[i-2,2]>0|x[i-1,2]>0) & (x[i,2]>0) & (x[i+1,2]<0) & (x[i+2,2]<0)) {
        peaks[i] <- as.character(x[i,1])
    }  
}
#troughs[99] <- "2016-12-31"
troughs <- as.Date(troughs[!is.na(troughs)], format = "%Y-%m-%d")
peaks[1] <- "1990-12-31"
peaks <- as.Date(peaks[!is.na(peaks)], format = "%Y-%m-%d")
tp5 <- cbind(as.data.frame(peaks),as.data.frame(troughs))
colnames(tp5) <- c("Peak","Trough")



index_plot <- conf_indices[,c(1,2)]
index_plot[,2] <- scale(index_plot[,2])
index_plot$Date <- datums$Datum
index_plot[,2] <- as.numeric(index_plot[,2])
colnames(index_plot) <- c("Date","Confidence")
g1 <- ggplot(index_plot) 
g1 <- g1 + geom_line(data=index_plot,aes(x=Date, y=Confidence,colour="Confidence"), size = 0.5)
#g1 <- g1 + labs(color="Legend text")
g1 <- g1 + geom_rect(data=recessions.df, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=0), fill='grey', alpha=0.5)
g1 <- g1 + geom_rect(data=tp1, aes(xmin=Peak, xmax=Trough, ymin=0, ymax=+Inf), fill='blue', alpha=0.5)
g1 <- g1 + ylab("Standardised Indicator") + xlab("")
g1 <- g1 + ggtitle("Confidence (Current)") + theme(plot.title = element_text(hjust = 0.5))
g1 <- g1 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g1 <- g1 + theme(legend.title=element_blank()) 
g1 <- g1 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"), expand=c(0,0),
                        limits = as.Date(c("1990-12-31", NA)))
g1 <- g1 + theme(legend.position="none")

index_plot <- conf_indices[,c(1,3)]
index_plot[,2] <- scale(index_plot[,2])
index_plot$Date <- datums$Datum
index_plot[,2] <- as.numeric(index_plot[,2])
colnames(index_plot) <- c("Date","Confidence")
g2 <- ggplot(index_plot) 
g2 <- g2 + geom_line(aes(x=Date, y=Confidence, colour="Confidence"), size = 0.5)
g2 <- g2 + labs(color="Legend text")
g2 <- g2 + geom_rect(data=recessions.df, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=0), fill='grey', alpha=0.5)
g2 <- g2 + geom_rect(data=tp2, aes(xmin=Peak, xmax=Trough, ymin=0, ymax=+Inf), fill='blue', alpha=0.5)
g2 <- g2 + ylab("Standardised Indicator") + xlab("")
g2 <- g2 + ggtitle("Confidence (Expected)") + theme(plot.title = element_text(hjust = 0.5))
g2 <- g2 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g2 <- g2 + theme(legend.title=element_blank()) 
g2 <- g2 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"), expand=c(0,0),
                        limits = as.Date(c("1990-12-31", NA)))
g2 <- g2 + theme(legend.position="none")


index_plot <- conf_indices[,c(1,4)]
index_plot[,2] <- scale(index_plot[,2])
index_plot$Date <- datums$Datum
index_plot[,2] <- as.numeric(index_plot[,2])
colnames(index_plot) <- c("Date","Confidence")
g3 <- ggplot(index_plot) 
g3 <- g3 + geom_line(aes(x=Date, y=Confidence, colour="Confidence"), size = 0.5)
g3 <- g3 + labs(color="Legend text")
g3 <- g3 + geom_rect(data=recessions.df, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=0), fill='grey', alpha=0.5)
g3 <- g3 + geom_rect(data=tp3, aes(xmin=Peak, xmax=Trough, ymin=0, ymax=+Inf), fill='blue', alpha=0.5)
g3 <- g3 + ylab("Standardised Indicator") + xlab("")
g3 <- g3 + ggtitle("BER BCI") + theme(plot.title = element_text(hjust = 0.5))
g3 <- g3 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g3 <- g3 + theme(legend.title=element_blank()) 
g3 <- g3 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"), expand=c(0,0),
                        limits = as.Date(c("1990-12-31", NA)))
g3 <- g3 + theme(legend.position="none")


#index_plot <- GDPdata[,c("Date","SACCI_BCI")]
index_plot <- conf_indices[,c(1,5)]
#index_plot[,2] <- scale(index_plot[,2])
index_plot$Date <- datums$Datum
index_plot[,2] <- as.numeric(index_plot[,2])
colnames(index_plot) <- c("Date","Confidence")
g4 <- ggplot(index_plot) 
g4 <- g4 + geom_line(aes(x=Date, y=Confidence, colour="Confidence"), size = 0.5)
g4 <- g4 + labs(color="Legend text")
g4 <- g4 + geom_rect(data=recessions.df, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=0), fill='grey', alpha=0.5)
g4 <- g4 + geom_rect(data=tp4, aes(xmin=Peak, xmax=Trough, ymin=0, ymax=+Inf), fill='blue', alpha=0.5)
g4 <- g4 + ylab("Growth Rate") + xlab("")
g4 <- g4 + ggtitle("SACCI BCI Growth") + theme(plot.title = element_text(hjust = 0.5))
g4 <- g4 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g4 <- g4 + theme(legend.title=element_blank()) 
g4 <- g4 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"), expand=c(0,0),
                        limits = as.Date(c("1990-12-31", NA)))
g4 <- g4 + theme(legend.position="none")

library(gridExtra)
grid.arrange(g1, g2, g3, g4, ncol=2, nrow =2)


png(filename = "turn.png", width = 600, height = 360)
grid.arrange(g1, g2, g3, g4, ncol=2, nrow =2)
dev.off()



#Concordance
S <- conf_indices
d <- NULL
for(i in 1:nrow(tp1)) {
    d <- c(d,seq(tp1[i,1], tp1[i,2], by="day")[-1]) 
}
S[,2] <- 1
S[S$Date %in% d,2] <- 0

d <- NULL
for(i in 1:nrow(tp2)) {
    d <- c(d,seq(tp2[i,1], tp2[i,2], by="day")[-1]) 
}
S[,3] <- 1
S[S$Date %in% d,3] <- 0

d <- NULL
for(i in 1:nrow(tp3)) {
    d <- c(d,seq(tp3[i,1], tp3[i,2], by="day")[-1]) 
}
S[,4] <- 1
S[S$Date %in% d,4] <- 0

d <- NULL
for(i in 1:nrow(tp4)) {
    d <- c(d,seq(tp4[i,1], tp4[i,2], by="day")[-1]) 
}
S[,5] <- 1
S[S$Date %in% d,5] <- 0

d <- NULL
for(i in 1:nrow(tp5)) {
    d <- c(d,seq(tp5[i,1], tp5[i,2], by="day")[-1]) 
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
        Concord2[i-1] <- (sum(S[1:(95-k),7]*S[(1+k):95,i], na.rm = TRUE)+sum((1-S[1:(95-k),7])*(1-S[(1+k):95,i]),na.rm = TRUE))/sum(!is.na(S[,7]))
        
        s_x1 <- S[1:(95-k),i]/sqrt(var(S[1:(95-k),i], na.rm = TRUE))
        s_x2 <- S[1:(95-k),7]/sqrt(var(S[1:(95-k),7], na.rm = TRUE))
        s_y1 <- S[(1+k):95,7]/sqrt(var(S[(1+k):95,7], na.rm = TRUE))
        s_y2 <- S[(1+k):95,i]/sqrt(var(S[(1+k):95,i], na.rm = TRUE))
        
        m <- lm(s_y1 ~ s_x1, na.action=na.exclude)
        p1 <- coeftest(m,vcov. = NeweyWest)[2,4]

        m <- lm(s_y2 ~ s_x2, na.action=na.exclude)
        p2 <- coeftest(m, vcov=NeweyWest(m,lag=0, prewhite=FALSE))[2,4]
        
        p.1 <- rbind(p.1,p1)
        p.2 <- rbind(p.2,p2)
    }
    Concord_1 <- cbind(Concord_1,Concord1)
    Concord_2 <- cbind(Concord_2,Concord2)
    
    p_1 <- cbind(p_1,p.1)
    p_2 <- cbind(p_2,p.2)
}


Concord_1 <- t(round(Concord_1, digits = 2))
Concord_2 <- t(round(Concord_2, digits = 2))

p_1 <- t(round(p_1, digits = 6))
p_2 <- t(round(p_2, digits = 6))

mystars <- ifelse(p_1 < .01, "***", ifelse(p_1 < .05, "** ", ifelse(p_1 < .1, "* ", " ")))
Concord_1 <- matrix(paste(Concord_1, mystars, sep=""), ncol=4)
colnames(Concord_1) <- colnames(S)[2:5]
row.names(Concord_1) <- c("lead=0","lead=1","lead=2","lead=3")

mystars <- ifelse(p_2 < .01, "***", ifelse(p_2 < .05, "** ", ifelse(p_2 < .1, "* ", " ")))
Concord_2 <- matrix(paste(Concord_2, mystars, sep=""), ncol=4)
colnames(Concord_2) <- colnames(S)[2:5]
row.names(Concord_2) <- c("lag=0","lag=1","lag=2","lag=3")

Concord_1<- Concord_1[seq(dim(Concord_1)[1],1),]
Concord_1 <- rbind(Concord_1,Concord_2[-1,])

xt <- xtable(Concord_1, caption="Concordance statistics with the SARB business cycle")
print(xt, "latex",comment=FALSE, include.rownames=TRUE, include.colnames=TRUE,
      caption.placement = getOption("xtable.caption.placement", "top"), scalebox = 0.8)



(recessions.df-tp1)/30/4

q<- as.yearqtr(recessions.df[,1])-as.yearqtr(tp1[,1])
q*4
q<- as.yearqtr(recessions.df[,2])-as.yearqtr(tp1[,2])
q*4

q<- as.yearqtr(recessions.df[,1])-as.yearqtr(tp2[-3,1])
q*4
q<- as.yearqtr(recessions.df[,2])-as.yearqtr(tp2[-3,2])
q*4

q<- as.yearqtr(recessions.df[,1])-as.yearqtr(tp3[-5:-6,1])
q*4
q<- as.yearqtr(recessions.df[,2])-as.yearqtr(tp3[-5:-6,2])
q*4

q<- as.yearqtr(recessions.df[,1])-as.yearqtr(tp4[-3,1])
q*4
q<- as.yearqtr(recessions.df[,2])-as.yearqtr(tp4[-3,2])
q*4


#-----------------
#Old backups
S <- conf_indices
#Besluit of mens standardised values wil gebruik
#S[,-1] <- scale(S[,-1])

S[,-1] <- replace(S[,-1],S[,-1]<0,0)
S[,-1] <- replace(S[,-1],S[,-1]>0,1)

#Besluit of mens cencoring rules wil gebruik
S[,-1] <- replace(S[,-1],S[,-1]<0,0)
S[,-1] <- replace(S[,-1],S[,-1]>0,1)

#Add official turning points
d <- NULL
for(i in 1:nrow(recessions.df)) {
    d <- c(d,seq(recessions.df[i,1], recessions.df[i,2], by="day")[-1]) 
}
S$SARB <- 1
S[S$Date %in% d,7] <- 0

#Datums bymekaar
peaks <- NULL
troughs <- NULL
for(i in 2:99) {
    if((S[i,2]==1) & (S[i-1,2]==0))   
        troughs[i] <- as.character(S[i,1])
    if((S[i,2]==0) & (S[i-1,2]==1))   
        peaks[i] <- as.character(S[i,1])
}
troughs[99] <- "2016-12-31"
troughs <- as.Date(troughs[!is.na(troughs)], format = "%Y-%m-%d")
peaks[1] <- "1990-12-31"
peaks <- as.Date(peaks[!is.na(peaks)], format = "%Y-%m-%d")
tp1 <- cbind(as.data.frame(peaks),as.data.frame(troughs))
colnames(tp1) <- c("Peak","Trough")



#Datums bymekaar
peaks <- NULL
troughs <- NULL
for(i in 3:97) {
    if((S[i-2,2]<0) & (S[i-2,2]<0) & (S[i,2]<0) & (S[i+1,2]>0) & (S[i+2,2]>0)) {
        troughs[i] <- as.character(S[i,1])
    }   
    if((S[i-2,2]>0) & (S[i-2,2]>0) & (S[i,2]>0) & (S[i+1,2]<0) & (S[i+2,2]<0)) {
        peaks[i] <- as.character(S[i,1])
    }  
}
troughs[99] <- "2016-12-31"
troughs <- as.Date(troughs[!is.na(troughs)], format = "%Y-%m-%d")
peaks[1] <- "1990-12-31"
peaks <- as.Date(peaks[!is.na(peaks)], format = "%Y-%m-%d")
tp1 <- cbind(as.data.frame(peaks),as.data.frame(troughs))
colnames(tp1) <- c("Peak","Trough")


#Die beste een
#Datums bymekaar
peaks <- NULL
troughs <- NULL
for(i in 3:97) {
    if((S[i-2,2]<0) & (S[i-2,2]<0) & (S[i,2]<0) & (S[i+1,2]>0) & (S[i+2,2]>0)) {
        troughs[i] <- as.character(S[i,1])
    }   
    if((S[i-2,2]>0) & (S[i-2,2]>0) & (S[i,2]>0) & (S[i+1,2]<0) & (S[i+2,2]<0)) {
        peaks[i] <- as.character(S[i,1])
    }  
}
troughs[99] <- "2016-12-31"
troughs <- as.Date(troughs[!is.na(troughs)], format = "%Y-%m-%d")
peaks[1] <- "1990-12-31"
peaks <- as.Date(peaks[!is.na(peaks)], format = "%Y-%m-%d")
tp1 <- cbind(as.data.frame(peaks),as.data.frame(troughs))
colnames(tp1) <- c("Peak","Trough")



draaipunte <- function(x) {
    peaks <- NULL
    troughs <- NULL
    for(i in 3:96) {
        if((x[i-2,2]<0|x[i-1,2]<0) & (x[i,2]<0) & (x[i+1,2]>0) & (x[i+2,2]>0|x[i+3,2]>0)) {
            troughs[i] <- as.character(x[i,1])
        }   
        if((x[i-2,2]>0|x[i-1,2]>0) & (x[i,2]>0) & (x[i+1,2]<0) & (x[i+2,2]<0|x[i+3,2]<0)) {
            peaks[i] <- as.character(x[i,1])
        }  
    }
    troughs[99] <- "2016-12-31"
    troughs <- as.Date(troughs[!is.na(troughs)], format = "%Y-%m-%d")
    peaks[1] <- "1990-12-31"
    peaks <- as.Date(peaks[!is.na(peaks)], format = "%Y-%m-%d")
    tp <- cbind(as.data.frame(peaks),as.data.frame(troughs))
    colnames(tp) <- c("Peak","Trough")
    return(tp)
}


if ((i > k.peak) & (i <= n - l.peak)) 
    peaks[i] <- all(z_p[i - LB_p + 1] >= z_p)

y <- S[,2]
n<-length(y)
k.peak <- 2
k.trough <- 2
l.peak <- 2
l.trough <- 2

peaks <- rep(NA, n)
troughs <- rep(NA, n)
for (i in 1:n) {
    LB_p <- max(1, i - k.peak)
    LB_t <- max(1, i - k.trough)
    z_p <- y[LB_p:min(n, i + l.peak)]
    z_t <- y[LB_t:min(n, i + l.trough)]
    if ((i > k.peak) & (i <= n - l.peak)) 
        peaks[i] <- all(z_p[i - LB_p + 1] >= z_p)
    if ((i > k.trough) & (i <= n - l.trough)) 
        troughs[i] <- CTS(z_t, i - LB_t + 1)
}




#=============================================
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

index_plot <- conf_indices[,c(1,3)]
index_plot[,2] <- scale(index_plot[,2])
index_plot[,1] <- as.Date(index_plot[,1], format = "%Y-%m-%d")
index_plot[,2] <- as.numeric(index_plot[,2])
colnames(index_plot) <- c("Date","Confidence")
g1 <- ggplot(index_plot) 
g1 <- g1 + geom_line(data=index_plot,aes(x=Date, y=Confidence,colour="Confidence"), size = 0.5)
#g1 <- g1 + labs(color="Legend text")
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

Concord_1 <- t(round(Concord_1, digits = 3))
Concord_2 <- t(round(Concord_2, digits = 3))

p_1 <- t(round(p_1, digits = 6))
p_2 <- t(round(p_2, digits = 6))

mystars <- ifelse(p_1 < .01, "***", ifelse(p_1 < .05, "** ", ifelse(p_1 < .1, "* ", " ")))
Concord_1 <- matrix(paste(Concord_1, mystars, sep=""), ncol=4)
Concord_1 <- rbind(c("Confidence_Current","Confidence_Expected","BER_BCI","SACCI_BCI"),Concord_1)
row.names(Concord_1) <- c(" ","lag=0","lag=1","lag=2","lag=3")

mystars <- ifelse(p_2 < .01, "***", ifelse(p_2 < .05, "** ", ifelse(p_2 < .1, "* ", " ")))
Concord_2 <- matrix(paste(Concord_2, mystars, sep=""), ncol=4)
Concord_2 <- rbind(c("Confidence_Current","Confidence_Expected","BER_BCI","SACCI_BCI"),Concord_2)
row.names(Concord_2) <- c(" ","lag=0","lag=1","lag=2","lag=3")

#Concord_1 <- cbind(Concord_1,Concord_2)

Concord_1 <- Concord_1[-1,]
colnames(Concord_1) <- c("Confidence (Current)","Confidence (Expected)","BER BCI","SACCI BCI")
xt <- xtable(Concord_1, caption="Concordance statistics with the SARB business cycle")
print(xt, "latex",comment=FALSE, include.rownames=TRUE, include.colnames=TRUE,
      caption.placement = getOption("xtable.caption.placement", "top"), scalebox = 0.8)



##===============##
##--UNCERTAINTY--##
##===============##
se <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x))*(length(na.omit(x))-1)) #adjust for (n-1)
##------------##
## Dispersion                    
##------------##
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
    w.uncertainty <- merge(datumso,w.uncertainty,by.x="Date",by.y="row.names", all=TRUE)[,-3]
    w.uncertainty[,14:15] <- na.approx(w.uncertainty[,14:15],na.rm = FALSE)
    for(t in 2:nrow(w.uncertainty)) { w.uncertainty$Disp[t-1] <- w.uncertainty$Uncert_fl[t-1]/w.uncertainty$Uncert_cc[t] }
    w.uncertainty$Disp[t] <- NA
    return(w.uncertainty)
}

w.uncertainty.M <- calc_wuncert(BER[BER$Sector=="Manufacturing",])
w.uncertainty.B <- calc_wuncert(BER[BER$Sector=="Construction",])
w.uncertainty.T <- calc_wuncert(BER[BER$Sector=="Trade",])
w.uncertainty.S <- calc_wuncert(BER[BER$Sector=="Services",])

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




#Read for speed
m_errors <- read.csv2("Manufacturing_errors3.csv", header=TRUE)[,-1]
m_errors$Datum <- as.Date(m_errors$Datum)
b_errors <- read.csv2("Building_errors3.csv", header=TRUE)[,-1]
b_errors$Datum <- as.Date(b_errors$Datum)
t_errors <- read.csv2("Trade_errors3.csv", header=TRUE)[,-1]
t_errors$Datum <- as.Date(t_errors$Datum)
s_errors <- read.csv2("Services_errors3.csv", header=TRUE)[,-1]
s_errors$Datum <- as.Date(s_errors$Datum)

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
    w.errors <- as.data.frame(t(sapply(datumso$Datum, function(kwartaal) weeg.3(data[data$Datum==kwartaal,]))))
    w.errors <- cbind(datumso$Datum,w.errors)
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

w.uncert_error.M <- calc_wuncert.ee(m_errors)
w.uncert_error.B <- calc_wuncert.ee(b_errors)
w.uncert_error.T <- calc_wuncert.ee(t_errors)
w.uncert_error.S <- calc_wuncert.ee(s_errors)

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



index_plot <- cbind(w.uncertainty[,c(1,2)])
index_plot[,-1] <- scale(index_plot[,-1])
index_plot[,2] <- as.numeric(index_plot[,2])
colnames(index_plot) <- c("Date","Dispersion")
index_plot$Date <- datums$Datum
g1 <- ggplot(index_plot, aes(x=Date,y=Dispersion,group=1))
g1 <- g1 + geom_line(colour="#7CAE00")
g1 <- g1 + theme(legend.title=element_blank())
g1 <- g1 + ggtitle("Manufacturing") + theme(plot.title = element_text(hjust = 0.5))
g1 <- g1 + ylab("Indicator") + xlab("")
g1 <- g1 + theme(legend.position="none")
g1 <- g1 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g1 <- g1 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

index_plot <- w.uncertainty[,c(1,3)]
index_plot[,-1] <- scale(index_plot[,-1])
index_plot[,2] <- as.numeric(index_plot[,2])
colnames(index_plot) <- c("Date","Dispersion")
index_plot$Date <- datums$Datum
g2 <- ggplot(index_plot, aes(x=Date,y=Dispersion,group=1))
g2 <- g2 + geom_line(colour="#7CAE00")
g2 <- g2 + theme(legend.title=element_blank())
g2 <- g2 + ggtitle("Construction") + theme(plot.title = element_text(hjust = 0.5))
g2 <- g2 + ylab("") + xlab("")
g2 <- g2 + theme(legend.position="none")
g2 <- g2 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g2 <- g2 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

index_plot <- cbind(w.uncertainty[,c(1,4)])
index_plot[,-1] <- scale(index_plot[,-1])
index_plot[,2] <- as.numeric(index_plot[,2])
colnames(index_plot) <- c("Date","Dispersion")
index_plot$Date <- datums$Datum
g3 <- ggplot(index_plot, aes(x=Date,y=Dispersion,group=1))
g3 <- g3 + geom_line(colour="#7CAE00")
g3 <- g3 + theme(legend.title=element_blank())
g3 <- g3 + ggtitle("Trade") + theme(plot.title = element_text(hjust = 0.5))
g3 <- g3 + ylab("Indicator") + xlab("")
g3 <- g3 + theme(legend.position="none")#,plot.margin=unit(c(-0.5,0.4,0,0.4), "cm"))
g3 <- g3 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g3 <- g3 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

index_plot <- cbind(w.uncertainty[,c(1,5)])
index_plot[,-1] <- scale(index_plot[,-1])
index_plot[,2] <- as.numeric(index_plot[,2])
colnames(index_plot) <- c("Date","Dispersion")
index_plot$Date <- datums$Datum
g4 <- ggplot(index_plot, aes(x=Date,y=Dispersion,group=1))
g4 <- g4 + geom_line(colour="#7CAE00")
g4 <- g4 + theme(legend.title=element_blank())
g4 <- g4 + ggtitle("Services") + theme(plot.title = element_text(hjust = 0.5))
g4 <- g4 + ylab("") + xlab("")
g4 <- g4 + theme(legend.position="none")#,plot.margin=unit(c(-0.5,0.4,0,0.4), "cm"))
g4 <- g4 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g4 <- g4 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

library(gridExtra)
grid.arrange(g1, g2, g3, g4, ncol=2, nrow =2)




index_plot <- cbind(aggr.errors[,c(1,2)])
index_plot[,-1] <- scale(index_plot[,-1])
index_plot[,2] <- as.numeric(index_plot[,2])
colnames(index_plot) <- c("Date","Aggregate")
index_plot$Date <- datums$Datum
g1 <- ggplot(index_plot, aes(x=Date,y=Aggregate,group=1))
g1 <- g1 + geom_line(colour="#F8766D")
g1 <- g1 + theme(legend.title=element_blank())
g1 <- g1 + ggtitle("Manufacturing") + theme(plot.title = element_text(hjust = 0.5))
g1 <- g1 + ylab("Indicator") + xlab("")
g1 <- g1 + theme(legend.position="none")
g1 <- g1 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g1 <- g1 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

index_plot <- cbind(aggr.errors[,c(1,3)])
index_plot[,-1] <- scale(index_plot[,-1])
index_plot[,2] <- as.numeric(index_plot[,2])
colnames(index_plot) <- c("Date","Aggregate")
index_plot$Date <- datums$Datum
g2 <- ggplot(index_plot, aes(x=Date,y=Aggregate,group=1))
g2 <- g2 + geom_line(colour="#F8766D")
g2 <- g2 + theme(legend.title=element_blank())
g2 <- g2 + ggtitle("Construction") + theme(plot.title = element_text(hjust = 0.5))
g2 <- g2 + ylab("") + xlab("")
g2 <- g2 + theme(legend.position="none")
g2 <- g2 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g2 <- g2 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

index_plot <- cbind(aggr.errors[,c(1,4)])
index_plot[,-1] <- scale(index_plot[,-1])
index_plot[,2] <- as.numeric(index_plot[,2])
colnames(index_plot) <- c("Date","Aggregate")
index_plot$Date <- datums$Datum
g3 <- ggplot(index_plot, aes(x=Date,y=Aggregate,group=1))
g3 <- g3 + geom_line(colour="#F8766D")
g3 <- g3 + theme(legend.title=element_blank())
g3 <- g3 + ggtitle("Trade") + theme(plot.title = element_text(hjust = 0.5))
g3 <- g3 + ylab("Indicator") + xlab("")
g3 <- g3 + theme(legend.position="none")#,plot.margin=unit(c(-0.5,0.4,0,0.4), "cm"))
g3 <- g3 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g3 <- g3 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

index_plot <- cbind(aggr.errors[,c(1,5)])
index_plot[,-1] <- scale(index_plot[,-1])
index_plot[,2] <- as.numeric(index_plot[,2])
colnames(index_plot) <- c("Date","Aggregate")
index_plot$Date <- datums$Datum
g4 <- ggplot(index_plot, aes(x=Date,y=Aggregate,group=1))
g4 <- g4 + geom_line(colour="#F8766D")
g4 <- g4 + theme(legend.title=element_blank())
g4 <- g4 + ggtitle("Services") + theme(plot.title = element_text(hjust = 0.5))
g4 <- g4 + ylab("") + xlab("")
g4 <- g4 + theme(legend.position="none")#,plot.margin=unit(c(-0.5,0.4,0,0.4), "cm"))
g4 <- g4 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g4 <- g4 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

library(gridExtra)
grid.arrange(g1, g2, g3, g4, ncol=2, nrow =2)




index_plot <- cbind(idio.errors[,1:2])
index_plot[,-1] <- scale(index_plot[,-1])
index_plot[,2] <- as.numeric(index_plot[,2])
colnames(index_plot) <- c("Date","Idiosyncratic")
index_plot$Date <- datums$Datum
g1 <- ggplot(index_plot, aes(x=Date,y=Idiosyncratic,group=1))
g1 <- g1 + geom_line(colour="#00BFC4")
g1 <- g1 + theme(legend.title=element_blank())
g1 <- g1 + ggtitle("Manufacturing") + theme(plot.title = element_text(hjust = 0.5))
g1 <- g1 + ylab("Indicator") + xlab("")
g1 <- g1 + theme(legend.position="none")
g1 <- g1 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g1 <- g1 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

index_plot <- cbind(idio.errors[,c(1,3)])
index_plot[,-1] <- scale(index_plot[,-1])
index_plot[,2] <- as.numeric(index_plot[,2])
colnames(index_plot) <- c("Date","Idiosyncratic")
index_plot$Date <- datums$Datum
g2 <- ggplot(index_plot, aes(x=Date,y=Idiosyncratic,group=1))
g2 <- g2 + geom_line(colour="#00BFC4")
g2 <- g2 + theme(legend.title=element_blank())
g2 <- g2 + ggtitle("Construction") + theme(plot.title = element_text(hjust = 0.5))
g2 <- g2 + ylab("") + xlab("")
g2 <- g2 + theme(legend.position="none")
g2 <- g2 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g2 <- g2 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

index_plot <- cbind(idio.errors[,c(1,4)])
index_plot[,-1] <- scale(index_plot[,-1])
index_plot[,2] <- as.numeric(index_plot[,2])
colnames(index_plot) <- c("Date","Idiosyncratic")
index_plot$Date <- datums$Datum
g3 <- ggplot(index_plot, aes(x=Date,y=Idiosyncratic,group=1))
g3 <- g3 + geom_line(colour="#00BFC4")
g3 <- g3 + theme(legend.title=element_blank())
g3 <- g3 + ggtitle("Trade") + theme(plot.title = element_text(hjust = 0.5))
g3 <- g3 + ylab("Indicator") + xlab("")
g3 <- g3 + theme(legend.position="none")#,plot.margin=unit(c(-0.5,0.4,0,0.4), "cm"))
g3 <- g3 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g3 <- g3 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

index_plot <- cbind(idio.errors[,c(1,5)])
index_plot[,-1] <- scale(index_plot[,-1])
index_plot[,2] <- as.numeric(index_plot[,2])
colnames(index_plot) <- c("Date","Idiosyncratic")
index_plot$Date <- datums$Datum
g4 <- ggplot(index_plot, aes(x=Date,y=Idiosyncratic,group=1))
g4 <- g4 + geom_line(colour="#00BFC4")
g4 <- g4 + theme(legend.title=element_blank())
g4 <- g4 + ggtitle("Services") + theme(plot.title = element_text(hjust = 0.5)) 
g4 <- g4 + ylab("") + xlab("")
g4 <- g4 + theme(legend.position="none")#,plot.margin=unit(c(-0.5,0.4,0,0.4), "cm"))
g4 <- g4 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g4 <- g4 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

library(gridExtra)
grid.arrange(g1, g2, g3, g4, ncol=2, nrow =2)



index_plot <- w.uncert_error 
index_plot[,-1] <- scale(index_plot[,-1])
index_plot$Date <- datums$Datum
g <- ggplot(index_plot) 
g <- g + geom_line(aes(x=Date, y=Aggregate_error, colour="Aggregate_error", linetype="Aggregate_error", size = "Aggregate_error"))
g <- g + geom_line(aes(x=Date, y=Idiosyncratic_error, colour="Idiosyncratic_error", linetype="Idiosyncratic_error", size = "Idiosyncratic_error"))
g <- g + geom_line(aes(x=Date, y=Dispersion, colour="Dispersion", linetype="Dispersion", size = "Dispersion"))
g <- g + scale_linetype_manual(values=c("twodash","solid","dotdash"))
g <- g + scale_size_manual(values=c(1,1,1))
g <- g + labs(color="Legend text", linetype="Legend text", size="Legend text")
g <- g + geom_rect(data=recessions.plot, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='grey', alpha=0.5)
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
uncert_indices$Uncertainty_Combined <- princomp(un[,c(2:6)])$scores[,1]
uncert_indices <- uncert_indices[,c(1:6,8,7)]




index_plot <- uncert_indices
index_plot$Date <- datums$Datum
g <- ggplot(index_plot) 
g <- g + geom_line(aes(x=Date, y=EPU, colour="EPU", linetype="EPU", size = "EPU"))
g <- g + geom_line(aes(x=Date, y=SAVI, colour="SAVI", linetype="SAVI", size = "SAVI"))
g <- g + geom_line(aes(x=Date, y=Uncertainty_Combined, colour="Uncertainty_Combined", linetype="Uncertainty_Combined", size = "Uncertainty_Combined"))
g <- g + scale_linetype_manual(values=c("twodash","dotdash","solid"))
g <- g + scale_size_manual(values=c(0.8,0.8,1.1))
g <- g + labs(color="Legend text", linetype="Legend text", size="Legend text")
g <- g + geom_rect(data=recessions.plot, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='grey', alpha=0.5)
g <- g + ylab("Indicator") + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) 
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"), expand=c(0,0),
                      limits = as.Date(c("1990-12-31", NA)))
g <- g + theme(legend.position="bottom")
g



source("corstarsl.R")
colnames(uncert_indices)[7] <- "Combined"
xt <- xtable(corstarsl(uncert_indices[,-1]), caption="Correlations between the uncertainty indicators")
print(xt, "latex",comment=FALSE, caption.placement = getOption("xtable.caption.placement", "top"), scalebox = 0.8)



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
services2$Uncertainty <- princomp(na.locf(services2[,2:4]))$scores[,1]
services2 <- services2[,c(1:4,6,5)]

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

xt <- xtable(xt1, caption="Correlations between the sectoral uncertainty indicators and real GDP growth")
print(xt, "latex",comment=FALSE, caption.placement = getOption("xtable.caption.placement", "top"), scalebox = 0.78)
xt <- xtable(xt2)
print(xt, "latex",comment=FALSE, caption.placement = getOption("xtable.caption.placement", "top"), scalebox = 0.8)



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



#======================
#----VAR ANALYSIS------
#======================
#--GRANGER CAUSALITY
#----------------------
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


xt <- xtable(G, caption="Granger causality tests: confidence")
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
      scalebox = 0.75)


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


#------------
#----VARs----
#------------
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

par(mfrow=c(1,1), new=FALSE)
nf <- layout(matrix(c(1,2,1,2), 1, 2, byrow = TRUE))
plot(irf.y1,plot.type = c("single"), main="Response from Confidence", xlab="Horizon in quarters", ylab="Real GDP Growth")
par(new = TRUE)
plot(irf.y2,plot.type = c("single"), main="Response from RGDP Growth", xlab="Horizon in quarters", ylab="Confidence (Current)")



png(filename = "fevd_var1.png", width = 600, height = 360)
source("plot_varfevd.R")
par(mfrow=c(1,1), new=FALSE)
par(mfrow=c(1,2))
plot.varfevd(fevd(var1, n.ahead = 10 ),plot.type = "single", xlab="Horizon in quarters", 
             ylab="Proportion of variance explained")
dev.off()

library(png)
library(grid)
grid.raster(readPNG("fevd_var1.png"))





vardat <- cbind(manufac[,2],manufac[,5])
colnames(vardat) <- c("Confidence_Current","RGDP_Growth")
infocrit <- VARselect(vardat, lag.max = 12, type = "const")
k_aic <- infocrit$selection[1]
k_hq  <- infocrit$selection[2]
k_sic <- infocrit$selection[3]
k <- min(k_aic,k_sic,k_hq)
varm <- VAR(vardat,p=k,type="const")

irf.y1 <- irf(varm,impulse = "Confidence_Current", response = "RGDP_Growth", n.ahead = 12,runs = 1000, seed=12345) 
irf.y2 <- irf(varm,impulse = "RGDP_Growth", response = "Confidence_Current", n.ahead = 12,runs = 1000, seed=12345)

par(mfrow=c(1,1), new=FALSE)
nf <- layout(matrix(c(1,2,1,2), 1, 2, byrow = TRUE))
#layout.show(nf)
#par(cex=1)
plot(irf.y1,plot.type = c("single"), main="Response from Confidence", xlab="Horizon in quarters")
par(new = TRUE)
plot(irf.y2,plot.type = c("single"), main="Response from RGDP Growth", xlab="Horizon in quarters")



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
plot(irf.y1,plot.type = c("single"), main="Response from Uncertainty", xlab="Horizon in quarters")
par(new = TRUE)
plot(irf.y2,plot.type = c("single"), main="Response from RGDP Growth", xlab="Horizon in quarters")



vardat <- cbind(manufac2[,5],manufac2[,6])
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
#par(cex=1)
plot(irf.y1,plot.type = c("single"), main="Response from Uncertainty", xlab="Horizon in quarters")
par(new = TRUE)
plot(irf.y2,plot.type = c("single"), main="Response from RGDP Growth", xlab="Horizon in quarters")




index_plot <- cbind(conf_indices[,c(1,2)],Uncertainty_Combined) 
index_plot[,-1] <- scale(index_plot[,-1])
colnames(index_plot) <- c("Date","Confidence_Current","Uncertainty_Combined")
index_plot$Date <- datums$Datum
g <- ggplot(index_plot) 
g <- g + geom_line(aes(x=Date, y=Confidence_Current, colour="Confidence_Current"), size = 1)
g <- g + geom_line(aes(x=Date, y=Uncertainty_Combined, colour="Uncertainty_Combined"), size = 1)
g <- g + labs(color="Legend text")
g <- g + geom_rect(data=recessions.plot, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='grey', alpha=0.5)
g <- g + ylab("Indicator") + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) 
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"), expand=c(0,0),
                      limits = as.Date(c("1990-12-31", NA)))
g <- g + theme(legend.position="bottom")
g




Confidence_Current <- conf_indices[,2]
Uncertainty_Combined <- uncert_indices[,7]
RGDP_Growth <- conf_indices[,6]

vardat <- cbind(Confidence_Current,Uncertainty_Combined,RGDP_Growth)  
infocrit <- VARselect(vardat, lag.max = 16, type = "const")
k_aic <- infocrit$selection[1]
k_hq  <- infocrit$selection[2]
k_sic <- infocrit$selection[3]
k <- min(k_aic,k_sic,k_hq)
var_3 <- VAR(vardat,p=2,type="const")

irf.y1 <- irf(var_3,impulse = c("Confidence_Current","Uncertainty_Combined"), response = "RGDP_Growth", n.ahead = 12,runs = 1000, seed=12345) 
par(mfrow=c(1,2),mar=c(5,4,4,2), cex=0.6)
plot(irf.y1,plot.type = c("single"), xlab="Horizon in quarters")





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


#----------
vardat <- cbind(conf_indices[,3],conf_indices[,6])
colnames(vardat) <- c("Confidence_Expected","RGDP_Growth")
infocrit <- VARselect(vardat, lag.max = 12, type = "const")
k_aic <- infocrit$selection[1]
k_hq  <- infocrit$selection[2]
k_sic <- infocrit$selection[3]
k <- min(k_aic,k_sic,k_hq)
var2 <- VAR(vardat,p=k,type="const")

irf.y1 <- irf(var2,impulse = "Confidence_Expected", response = "RGDP_Growth", n.ahead = 12,runs = 1000, seed=12345) 
irf.y2 <- irf(var2,impulse = "RGDP_Growth", response = "Confidence_Expected", n.ahead = 12,runs = 1000, seed=12345)

par(mfrow=c(1,1), new=FALSE)
nf <- layout(matrix(c(1,2,1,2), 1, 2, byrow = TRUE))
plot(irf.y1,plot.type = c("single"), main="Response from Confidence (Exp)", xlab="Horizon in quarters", ylab="Real GDP Growth")
par(new = TRUE)
plot(irf.y2,plot.type = c("single"), main="Response from RGDP Growth", xlab="Horizon in quarters", ylab="Confidence (Expected)")


png(filename = "fevd_var2.png", width = 600, height = 360)
source("plot_varfevd.R")
par(mfrow=c(1,1), new=FALSE)
par(mfrow=c(1,2))
plot.varfevd(fevd(var1, n.ahead = 10 ),plot.type = "single", xlab="Horizon in quarters", 
             ylab="Proportion of variance explained")
dev.off()

library(png)
library(grid)
grid.raster(readPNG("fevd_var2.png"))



vardat <- cbind(conf_indices[,4],conf_indices[,6])
colnames(vardat) <- c("BER_BCI","RGDP_Growth")
infocrit <- VARselect(vardat, lag.max = 12, type = "const")
k_aic <- infocrit$selection[1]
k_hq  <- infocrit$selection[2]
k_sic <- infocrit$selection[3]
k <- min(k_aic,k_sic,k_hq)
var3 <- VAR(vardat,p=k,type="const")

irf.y1 <- irf(var3,impulse = "BER_BCI", response = "RGDP_Growth", n.ahead = 12,runs = 1000, seed=12345) 
irf.y2 <- irf(var3,impulse = "RGDP_Growth", response = "BER_BCI", n.ahead = 12,runs = 1000, seed=12345)

par(mfrow=c(1,1), new=FALSE)
nf <- layout(matrix(c(1,2,1,2), 1, 2, byrow = TRUE))
plot(irf.y1,plot.type = c("single"), main="Response from BER BCI", xlab="Horizon in quarters", ylab="Real GDP Growth")
par(new = TRUE)
plot(irf.y2,plot.type = c("single"), main="Response from RGDP Growth", xlab="Horizon in quarters", ylab="BER BCI")


png(filename = "fevd_var3.png", width = 600, height = 360)
source("plot_varfevd.R")
par(mfrow=c(1,1), new=FALSE)
par(mfrow=c(1,2))
plot.varfevd(fevd(var3, n.ahead = 10 ),plot.type = "single", xlab="Horizon in quarters", 
             ylab="Proportion of variance explained")
dev.off()

library(png)
library(grid)
grid.raster(readPNG("fevd_var3.png"))



vardat <- cbind(conf_indices[-1:-3,5],conf_indices[-1:-3,6])
colnames(vardat) <- c("SACCI_BCI_Growth","RGDP_Growth")
infocrit <- VARselect(vardat, lag.max = 12, type = "const")
k_aic <- infocrit$selection[1]
k_hq  <- infocrit$selection[2]
k_sic <- infocrit$selection[3]
k <- min(k_aic,k_sic,k_hq)
var4 <- VAR(vardat,p=k,type="const")

irf.y1 <- irf(var4,impulse = "SACCI_BCI_Growth", response = "RGDP_Growth", n.ahead = 12,runs = 1000, seed=12345) 
irf.y2 <- irf(var4,impulse = "RGDP_Growth", response = "SACCI_BCI_Growth", n.ahead = 12,runs = 1000, seed=12345)

par(mfrow=c(1,1), new=FALSE)
nf <- layout(matrix(c(1,2,1,2), 1, 2, byrow = TRUE))
plot(irf.y1,plot.type = c("single"), main="Response from SACCI BCI Growth", xlab="Horizon in quarters", ylab="Real GDP Growth")
par(new = TRUE)
plot(irf.y2,plot.type = c("single"), main="Response from RGDP Growth", xlab="Horizon in quarters", ylab="SACCI BCI Growth")


png(filename = "fevd_var4.png", width = 600, height = 360)
source("plot_varfevd.R")
par(mfrow=c(1,1), new=FALSE)
par(mfrow=c(1,2))
plot.varfevd(fevd(var4, n.ahead = 10 ),plot.type = "single", xlab="Horizon in quarters", 
             ylab="Proportion of variance explained")
dev.off()

library(png)
library(grid)
grid.raster(readPNG("fevd_var4.png"))






BER_BCI <- conf_indices[,4]
Uncertainty_Combined <- uncert_indices[,7]
RGDP_Growth <- conf_indices[,6]

vardat <- cbind(BER_BCI,Uncertainty_Combined,RGDP_Growth)  
infocrit <- VARselect(vardat, lag.max = 16, type = "const")
k_aic <- infocrit$selection[1]
k_hq  <- infocrit$selection[2]
k_sic <- infocrit$selection[3]
k <- min(k_aic,k_sic,k_hq)
var_3 <- VAR(vardat,p=2,type="const")

irf.y1 <- irf(var_3,impulse = c("BER_BCI","Uncertainty_Combined"), response = "RGDP_Growth", n.ahead = 12,runs = 1000, seed=12345) 
par(mfrow=c(1,2),mar=c(5,4,4,2), cex=0.6)
plot(irf.y1,plot.type = c("single"), xlab="Horizon in quarters")





JSE <- GDPgrowth4$RJSE
Bond <- GDPdata$Bond2
TBill <- GDPdata$T.Bill
Spread <- Bond-TBill
Employment <- GDPgrowth4$Employ
Investment <- GDPgrowth4$Rinvestment 
Production <- GDPgrowth4$RProduction

vardat <- cbind(BER_BCI,Uncertainty_Combined,JSE,Spread,RGDP_Growth,
                Production,Employment,Investment)  
vare <- VAR(vardat,p=2,type="const")

irf.y1 <- irf(vare,impulse = c("BER_BCI","Uncertainty_Combined"),
              response = c("RGDP_Growth","Production","Investment"), n.ahead = 12,runs = 1000, seed=12345) 
par(mfrow=c(2,3),mar=c(3,4,2,1), cex=0.6)
plot(irf.y1,plot.type = c("single"), main=c(" "))


source("plot_varfevd.R")
par(mfrow=c(1,3))
plot.varfevd(fevd(vare, n.ahead = 10 ),plot.type = "multiple")

par(mfrow=c(1,1))
plot.varfevd(fevd(vare, n.ahead = 10 ),plot.type = "single")





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






#------------------
#Robustness tests
#------------------
#----------------------------------------------------------
#Only with firms that for part of the forecast error sample
#----------------------------------------------------------
#where any there for the whole sample?
c <- aggregate(BER$Q1, by=list(BER$surveyQ,BER$id), FUN=length)
c$Group.1 <- as.Date(as.yearqtr(c$Group.1, format = "%YQ%q"))
c <- c[c$Group.1>"2005-12-31",]
n <- length(unique(c$Group.1))
c <- aggregate(c$x, by=list(c$Group.2), FUN=sum)
c <- c[order(c[,2]),]

u <- n*0.75
m <- n*0.5
l <- n*0.25
r.75 <- sum(c$x>u)/length(c$x)
r.50 <- sum(c$x<u & c$x>m)/length(c$x)
r.25 <- sum(c$x<m & c$x>l)/length(c$x)
r.0  <- sum(c$x<l)/length(c$x)

ids <- c$Group.1[c$x>49]

calc_id <- function(data) {
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
        #error <- error[,c(2:8,20:24)]
        error <- error[rowSums(is.na(error[,20:24]))!=5, ]
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
m_errors <- calc_id(BER[BER$Sector=="Manufacturing",])
write.csv2(m_errors,"Man_errors4.csv")

b_errors <- calc_id(BER[BER$Sector=="Construction",])
write.csv2(b_errors,"Bui_errors4.csv")

t_errors <- calc_id(BER[BER$Sector=="Trade",])
write.csv2(t_errors,"Tra_errors4.csv")

s_errors <- calc_id(BER[BER$Sector=="Services",])
write.csv2(s_errors,"Ser_errors4.csv")


#Read for speed
m_errors <- read.csv2("Man_errors4.csv", header=TRUE)[,-1]
m_errors$Datum <- as.Date(m_errors$Datum)
b_errors <- read.csv2("Bui_errors4.csv", header=TRUE)[,-1]
b_errors$Datum <- as.Date(b_errors$Datum)
t_errors <- read.csv2("Tra_errors4.csv", header=TRUE)[,-1]
t_errors$Datum <- as.Date(t_errors$Datum)
s_errors <- read.csv2("Ser_errors4.csv", header=TRUE)[,-1]
s_errors$Datum <- as.Date(s_errors$Datum)

#Calculate indicators with smaller sample
errors <- rbind(m_errors,b_errors,t_errors,s_errors)

BERplot <- aggregate(errors$Sector, by=list(errors$Datum,errors$Sector), FUN = length)
g <- ggplot(BERplot, aes(x=Group.1, y=x,fill=Group.2))
g <- g + geom_bar(stat="identity")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + scale_fill_discrete(name="Sector")
g <- g + scale_y_continuous(labels=comma)
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g <- g + ylab("Number of Respondents") + xlab(" ")
g


#Make a table to compare characteristics
karak <- aggregate(errors$weight,by=list(errors$weight),FUN=length)
karak$y <- round(karak$x/sum(karak$x)*100,2)
karak1 <- aggregate(BER$weight,by=list(BER$weight),FUN=length)[-1,]
karak1$y <- round(karak1$x/sum(karak1$x)*100,2)

karak <- cbind(karak1,karak[,-1])
colnames(karak) <- c("Firm Size Category","Observations","Percentage of sample",
                     "Observations","Percentage of sample")


#Alternative confidence indicators
BERplot <- aggregate(errors$Sector, by=list(errors$Datum), FUN = length)
mean(BERplot$x)
#ds <- unique(errors$id)
#w.indicators.M <- calc_wconf(BER[BER$Sector=="Manufacturing" & BER$id %in% ids,])
#w.indicators.B <- calc_wconf(BER[BER$Sector=="Construction" & BER$id %in% ids,])
#w.indicators.T <- calc_wconf(BER[BER$Sector=="Trade" & BER$id %in% ids,])
#w.indicators.S <- calc_wconf(BER[BER$Sector=="Services" & BER$id %in% ids,])

#Include those in stable sample
colnames(errors)[1] <- "surveyQ"
errors <- errors[,colnames(BER)]
w.indicators.M <- calc_wconf(errors[errors$Sector=="Manufacturing",])
w.indicators.B <- calc_wconf(errors[errors$Sector=="Construction",])
w.indicators.T <- calc_wconf(errors[errors$Sector=="Trade",])
w.indicators.S <- calc_wconf(errors[errors$Sector=="Services",])


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

activity$Activity <- sapply(activity$Date, function(x) weighted.mean(activity[which(activity$Date==x),c(2:5)], 
                                                                     weights[weights$Date==x,-1],na.rm=TRUE))
conf$Confidence <- sapply(conf$Date, function(x) weighted.mean(conf[which(conf$Date==x),c(2:5)], 
                                                               weights[weights$Date==x,-1],na.rm=TRUE))
w.indicators <- cbind(activity[,c(1,6)],conf[,6])
colnames(w.indicators) <- c("Date","Activity","Confidence")



aconf_indices <- cbind(conf_indices[,-4:-5],w.indicators[,-1])
colnames(aconf_indices) <- c("Date","Conf (Cur): Full sample", "Conf (Exp): Full sample", 
                             "RGDP Growth","Conf (Cur): Stable sample","Conf (Exp): Stable sample")


#-----------
#UNCERTAINTY
w.uncertainty.M <- calc_wuncert(errors[errors$Sector=="Manufacturing",])
w.uncertainty.B <- calc_wuncert(errors[errors$Sector=="Construction",])
w.uncertainty.T <- calc_wuncert(errors[errors$Sector=="Trade",])
w.uncertainty.S <- calc_wuncert(errors[errors$Sector=="Services",])

weights <- GDPdata[,c(1:4,6)]
w.uncertainty <- cbind(w.uncertainty.M[,c(2,16)],w.uncertainty.B[,16],w.uncertainty.T[,16],w.uncertainty.S[,16])
colnames(w.uncertainty) <- c("Date","Manufacturing","Construction","Trade","Services")
w.uncertainty$Construction[c(28,89)] <- NA
w.uncertainty[,-1] <- scale(w.uncertainty[,-1])
w.uncertainty$Dispersion <- sapply(w.uncertainty$Date,function(x) weighted.mean(w.uncertainty[which(w.uncertainty$Date==x),c(2:5)], 
                                                                                weights[weights$Date==x,-1],na.rm=TRUE))
w.uncertainty <- seisoen(w.uncertainty)
w.uncertainty[1,3] <- NA
w.uncertainty[1:5,4] <- NA
w.uncertainty[1:53,5] <- NA


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

auncert_indices <- cbind(w.uncert_error,GDPdata$IMF_Uncertainty, GDPdata$SAVI,GDPgrowth4$RGDP)
colnames(auncert_indices) <-c("Date","Dispersion","Idiosyncratic_error","Aggregate_error","EPU","SAVI","RGDP_Growth")
auncert_indices[,-1] <- scale(auncert_indices[,-1])
un <- auncert_indices #un <- na.locf(uncert_indices)
un[is.na(un)] <- 0
auncert_indices$Uncertainty_Combined <- princomp(un[,c(2:6)])$scores[,1]
auncert_indices <- auncert_indices[,c(1:6,8,7)]


auncert_indices <- cbind(uncert_indices[,c(1,2,7)],auncert_indices[,c(2,7)])
colnames(auncert_indices) <- c("Date","Disp: Full sample", "Uncert: Full sample", 
                             "Disp: Stable sample","Uncert: Stable sample")


index_plot <- aconf_indices
index_plot$Date <- datums$Datum 
index_plot[,2:6] <- scale(index_plot[,2:6])
g <- ggplot(index_plot) 
g <- g + geom_line(aes(x=Date, y=`Conf (Cur): Full sample`, colour="Conf (Cur): Full sample", linetype="Conf (Cur): Full sample", size = "Conf (Cur): Full sample"))
g <- g + geom_line(aes(x=Date, y=`Conf (Exp): Full sample`, colour="Conf (Exp): Full sample", linetype="Conf (Exp): Full sample", size = "Conf (Exp): Full sample"))
g <- g + geom_line(aes(x=Date, y=`Conf (Cur): Stable sample`, colour="Conf (Cur): Stable sample", linetype="Conf (Cur): Stable sample", size = "Conf (Cur): Stable sample"))
g <- g + geom_line(aes(x=Date, y=`Conf (Exp): Stable sample`, colour="Conf (Exp): Stable sample", linetype="Conf (Exp): Stable sample", size = "Conf (Exp): Stable sample"))
g <- g + scale_linetype_manual(values=c("solid","solid","solid","solid"))
g <- g + scale_size_manual(values=c(0.8,0.9,0.8,0.9))
g <- g + scale_colour_manual(values=c("#F8766D","red","#00BFC4","blue"))
g <- g + labs(color="Legend text", linetype="Legend text", size="Legend text")
g <- g + geom_rect(data=recessions.df, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='grey', alpha=0.5)
g <- g + ylab("Standardised Indicator") + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) 
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"), expand=c(0,0),limits = as.Date(c("1990-12-31", NA)))
g <- g + theme(legend.position="bottom")
g


index_plot <- auncert_indices
index_plot$Date <- datums$Datum 
#index_plot[,2:6] <- scale(index_plot[,2:6])
g <- ggplot(index_plot) 
g <- g + geom_line(aes(x=Date, y=`Disp: Full sample`, colour="Disp: Full sample", linetype="Disp: Full sample", size = "Disp: Full sample"))
g <- g + geom_line(aes(x=Date, y=`Uncert: Full sample`, colour="Uncert: Full sample", linetype="Uncert: Full sample", size = "Uncert: Full sample"))
g <- g + geom_line(aes(x=Date, y=`Disp: Stable sample`, colour="Disp: Stable sample", linetype="Disp: Stable sample", size = "Disp: Stable sample"))
g <- g + geom_line(aes(x=Date, y=`Uncert: Stable sample`, colour="Uncert: Stable sample", linetype="Uncert: Stable sample", size = "Uncert: Stable sample"))
g <- g + scale_linetype_manual(values=c("solid","solid","solid","solid"))
g <- g + scale_size_manual(values=c(0.7,0.9,0.7,0.9))
g <- g + scale_colour_manual(values=c("#F8766D","red","#00BFC4","blue"))
g <- g + labs(color="Legend text", linetype="Legend text", size="Legend text")
g <- g + geom_rect(data=recessions.df, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='grey', alpha=0.5)
g <- g + ylab("Standardised Indicator") + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) 
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"), expand=c(0,0),limits = as.Date(c("1990-12-31", NA)))
g <- g + theme(legend.position="bottom")
g



source("corstarsl.R")
xt <- aconf_indices[,c(5,6,2,3,4)]
xt <- xtable(corstarsl(xt)[3:5,1:2], caption="Correlations between confidence indicators and real GDP growth")
print(xt, "latex",comment=FALSE, caption.placement = getOption("xtable.caption.placement", "top"), scalebox = 0.8)


source("corstarsl.R")
xt <- cbind(auncert_indices[,c(4,5,2,3)],aconf_indices[,6])
xt <- xtable(corstarsl(xt)[3:5,1:2], caption="Correlations between confidence indicators and real GDP growth")
print(xt, "latex",comment=FALSE, caption.placement = getOption("xtable.caption.placement", "top"), scalebox = 0.8)


#---------------------------------------------------
#Only with firms responded more than 50% of the time
#----------
#CONFIDENCE

c <- aggregate(BER$Q1, by=list(BER$surveyQ,BER$id,BER$Sector), FUN=length)
c$Group.1 <- as.Date(as.yearqtr(c$Group.1, format = "%YQ%q"))

n_m <- length(unique(c$Group.1[c$Group.3=="Manufacturing"]))
n_c <- length(unique(c$Group.1[c$Group.3=="Construction"]))
n_t <- length(unique(c$Group.1[c$Group.3=="Trade"]))
n_s <- length(unique(c$Group.1[c$Group.3=="Services"]))
c <- aggregate(c$x, by=list(c$Group.2,c$Group.3), FUN=sum)


ids_m <- c$Group.1[c$Group.2=="Manufacturing" & c$x>=(n_m*0.5)]
ids_c <- c$Group.1[c$Group.2=="Construction" & c$x>=(n_c*0.5)]
ids_t <- c$Group.1[c$Group.2=="Trade" & c$x>=(n_t*0.5)]
ids_s <- c$Group.1[c$Group.2=="Services" & c$x>=(n_s*0.5)]
ids <- c(ids_m,ids_c,ids_t,ids_s)

BERplot <- BER[BER$id %in% ids_m,]
BERplot <- rbind(BERplot,BER[BER$id %in% ids_c,])
BERplot <- rbind(BERplot,BER[BER$id %in% ids_t,])
BERplot <- rbind(BERplot,BER[BER$id %in% ids_s,])

plot <- aggregate(BERplot$Sector, by=list(BERplot$surveyQ,BERplot$Sector), FUN = length)
plot$Group.1 <- as.Date(as.yearqtr(plot$Group.1, format = "%YQ%q"))
g1 <- ggplot(plot, aes(x=Group.1, y=x,fill=Group.2))
g1 <- g1 + geom_bar(stat="identity")
g1 <- g1 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g1 <- g1 + scale_fill_discrete(name=" ")
g1 <- g1 + scale_y_continuous(labels=comma)
g1 <- g1 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g1 <- g1 + ylab("Number of Respondents") + xlab(" ")
g1 <- g1 + theme(legend.position="bottom")
g1 <- g1 + ggtitle("Responded > 50%") + theme(plot.title = element_text(hjust = 0.5))


ids_m1 <- c$Group.1[c$Group.2=="Manufacturing" & c$x>=(n_m*0.75)]
ids_c1 <- c$Group.1[c$Group.2=="Construction" & c$x>=(n_c*0.75)]
ids_t1 <- c$Group.1[c$Group.2=="Trade" & c$x>=(n_t*0.75)]
ids_s1 <- c$Group.1[c$Group.2=="Services" & c$x>=(n_s*0.75)]
ids1 <- c(ids_m,ids_c,ids_t,ids_s)

BERplot1 <- BER[BER$id %in% ids_m1,]
BERplot1 <- rbind(BERplot1,BER[BER$id %in% ids_c1,])
BERplot1 <- rbind(BERplot1,BER[BER$id %in% ids_t1,])
BERplot1 <- rbind(BERplot1,BER[BER$id %in% ids_s1,])

plot <- aggregate(BERplot1$Sector, by=list(BERplot1$surveyQ,BERplot1$Sector), FUN = length)
plot$Group.1 <- as.Date(as.yearqtr(plot$Group.1, format = "%YQ%q"))
g2 <- ggplot(plot, aes(x=Group.1, y=x,fill=Group.2))
g2 <- g2 + geom_bar(stat="identity")
g2 <- g2 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g2 <- g2 + scale_fill_discrete(name="")
g2 <- g2 + scale_y_continuous(labels=comma)
g2 <- g2 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g2 <- g2 + ylab("Number of Respondents") + xlab(" ")
g2 <- g2 + theme(legend.position="bottom")
g2 <- g2 + ggtitle("Responded > 75%") + theme(plot.title = element_text(hjust = 0.5))


library(gridExtra)
library(grid)

grid.arrange(g1, g2, ncol=2, nrow =1)



#Make a table to compare characteristics
karak <- aggregate(BERplot$weight,by=list(BERplot$weight),FUN=length)
karak$y <- round(karak$x/sum(karak$x)*100,2)
karak1 <- aggregate(BER$weight,by=list(BER$weight),FUN=length)[-1,]
karak1$y <- round(karak1$x/sum(karak1$x)*100,2)
karak2 <- aggregate(BERplot1$weight,by=list(BERplot1$weight),FUN=length)
karak2$y <- round(karak2$x/sum(karak2$x)*100,2)

karak <- cbind(karak1,karak[,-1],karak2[,-1])
colnames(karak) <- c("Firm Size Category","Observations","Percentage of sample",
                     "Observations","Percentage of sample")





#Include those that answered >50%
w.indicators.M <- calc_wconf(BERplot[BERplot$Sector=="Manufacturing",])
w.indicators.B <- calc_wconf(BERplot[BERplot$Sector=="Construction",])
w.indicators.T <- calc_wconf(BERplot[BERplot$Sector=="Trade",])
w.indicators.S <- calc_wconf(BERplot[BERplot$Sector=="Services",])

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

activity$Activity <- sapply(activity$Date, function(x) weighted.mean(activity[which(activity$Date==x),c(2:5)], 
                                                                     weights[weights$Date==x,-1],na.rm=TRUE))
conf$Confidence <- sapply(conf$Date, function(x) weighted.mean(conf[which(conf$Date==x),c(2:5)], 
                                                               weights[weights$Date==x,-1],na.rm=TRUE))
w.indicators <- cbind(activity[,c(1,6)],conf[,6])
colnames(w.indicators) <- c("Date","Activity","Confidence")
aconf_indices <- cbind(conf_indices[,-4:-5],w.indicators[,-1])
colnames(aconf_indices) <- c("Date","Conf (Cur): Full sample", "Conf (Exp): Full sample", 
                             "RGDP Growth","Conf (Cur): Stable sample","Conf (Exp): Stable sample")


#Include those that answered >75%
w.indicators.M <- calc_wconf(BERplot1[BERplot1$Sector=="Manufacturing",])
w.indicators.B <- calc_wconf(BERplot1[BERplot1$Sector=="Construction",])
w.indicators.T <- calc_wconf(BERplot1[BERplot1$Sector=="Trade",])
w.indicators.S <- calc_wconf(BERplot1[BERplot1$Sector=="Services",])


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

activity$Activity <- sapply(activity$Date, function(x) weighted.mean(activity[which(activity$Date==x),c(2:5)], 
                                                                     weights[weights$Date==x,-1],na.rm=TRUE))
conf$Confidence <- sapply(conf$Date, function(x) weighted.mean(conf[which(conf$Date==x),c(2:5)], 
                                                               weights[weights$Date==x,-1],na.rm=TRUE))
w.indicators <- cbind(activity[,c(1,6)],conf[,6])
colnames(w.indicators) <- c("Date","Activity","Confidence")



aconf_indices1 <- cbind(conf_indices[,-4:-5],w.indicators[,-1])
colnames(aconf_indices1) <- c("Date","Conf (Cur): Full sample", "Conf (Exp): Full sample", 
                             "RGDP Growth","Conf (Cur): Stable sample","Conf (Exp): Stable sample")



index_plot <- aconf_indices
index_plot$Date <- datums$Datum 
index_plot[,2:6] <- scale(index_plot[,2:6])
g1 <- ggplot(index_plot) 
g1 <- g1 + geom_line(aes(x=Date, y=`Conf (Cur): Full sample`, colour="Conf (Cur): Full sample", linetype="Conf (Cur): Full sample", size = "Conf (Cur): Full sample"))
g1 <- g1 + geom_line(aes(x=Date, y=`Conf (Exp): Full sample`, colour="Conf (Exp): Full sample", linetype="Conf (Exp): Full sample", size = "Conf (Exp): Full sample"))
g1 <- g1 + geom_line(aes(x=Date, y=`Conf (Cur): Stable sample`, colour="Conf (Cur): Stable sample", linetype="Conf (Cur): Stable sample", size = "Conf (Cur): Stable sample"))
g1 <- g1 + geom_line(aes(x=Date, y=`Conf (Exp): Stable sample`, colour="Conf (Exp): Stable sample", linetype="Conf (Exp): Stable sample", size = "Conf (Exp): Stable sample"))
g1 <- g1 + scale_linetype_manual(values=c("solid","solid","solid","solid"))
g1 <- g1 + scale_size_manual(values=c(0.8,0.9,0.8,0.9))
g1 <- g1 + scale_colour_manual(values=c("#F8766D","red","#00BFC4","blue"))
g1 <- g1 + labs(color="Legend text", linetype="Legend text", size="Legend text")
g1 <- g1 + geom_rect(data=recessions.df, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='grey', alpha=0.5)
g1 <- g1 + ylab("Standardised Indicator") + xlab("")
g1 <- g1 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g1 <- g1 + theme(legend.title=element_blank()) 
g1 <- g1 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"), expand=c(0,0),limits = as.Date(c("1990-12-31", NA)))
g1 <- g1 + theme(legend.position="bottom")
g1 <- g1 + ggtitle("Responded > 50%") + theme(plot.title = element_text(hjust = 0.5))


index_plot <- aconf_indices1[-1,]
index_plot$Date <- datums$Datum 
index_plot[,2:6] <- scale(index_plot[,2:6])
g2 <- ggplot(index_plot) 
g2 <- g2 + geom_line(aes(x=Date, y=`Conf (Cur): Full sample`, colour="Conf (Cur): Full sample", linetype="Conf (Cur): Full sample", size = "Conf (Cur): Full sample"))
g2 <- g2 + geom_line(aes(x=Date, y=`Conf (Exp): Full sample`, colour="Conf (Exp): Full sample", linetype="Conf (Exp): Full sample", size = "Conf (Exp): Full sample"))
g2 <- g2 + geom_line(aes(x=Date, y=`Conf (Cur): Stable sample`, colour="Conf (Cur): Stable sample", linetype="Conf (Cur): Stable sample", size = "Conf (Cur): Stable sample"))
g2 <- g2 + geom_line(aes(x=Date, y=`Conf (Exp): Stable sample`, colour="Conf (Exp): Stable sample", linetype="Conf (Exp): Stable sample", size = "Conf (Exp): Stable sample"))
g2 <- g2 + scale_linetype_manual(values=c("solid","solid","solid","solid"))
g2 <- g2 + scale_size_manual(values=c(0.8,0.9,0.8,0.9))
g2 <- g2 + scale_colour_manual(values=c("#F8766D","red","#00BFC4","blue"))
g2 <- g2 + labs(color="Legend text", linetype="Legend text", size="Legend text")
g2 <- g2 + geom_rect(data=recessions.df, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='grey', alpha=0.5)
g2 <- g2 + ylab("Standardised Indicator") + xlab("")
g2 <- g2 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g2 <- g2 + theme(legend.title=element_blank()) 
g2 <- g2 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"), expand=c(0,0),limits = as.Date(c("1990-12-31", NA)))
g2 <- g2 + theme(legend.position="bottom")
g2 <- g2 + ggtitle("Responded > 75%") + theme(plot.title = element_text(hjust = 0.5))

library(gridExtra)
library(grid)

grid_arrange_shared_legend(g1, g2, ncol=2, nrow =1)

#-----------
#UNCERTAINTY
w.uncertainty.M <- calc_wuncert(BERplot[BERplot$Sector=="Manufacturing",])
w.uncertainty.B <- calc_wuncert(BERplot[BERplot$Sector=="Construction",])
w.uncertainty.T <- calc_wuncert(BERplot[BERplot$Sector=="Trade",])
w.uncertainty.S <- calc_wuncert(BERplot[BERplot$Sector=="Services",])

weights <- GDPdata[,c(1:4,6)]
w.uncertainty <- cbind(w.uncertainty.M[,c(2,16)],w.uncertainty.B[,16],w.uncertainty.T[,16],w.uncertainty.S[,16])
colnames(w.uncertainty) <- c("Date","Manufacturing","Construction","Trade","Services")
w.uncertainty$Construction[c(28,89)] <- NA
w.uncertainty[,-1] <- scale(w.uncertainty[,-1])
w.uncertainty$Dispersion <- sapply(w.uncertainty$Date,function(x) weighted.mean(w.uncertainty[which(w.uncertainty$Date==x),c(2:5)], 
                                                                                weights[weights$Date==x,-1],na.rm=TRUE))
w.uncertainty <- seisoen(w.uncertainty)
w.uncertainty[1,3] <- NA
w.uncertainty[1:5,4] <- NA
w.uncertainty[1:53,5] <- NA
errors$id <- factor(errors$id)

w.uncert_error.M <- calc_wuncert.ee(errors[(errors$Sector=="Manufacturing") & (errors$id %in% ids_m),c(2,8,20:24)])
w.uncert_error.B <- calc_wuncert.ee(errors[(errors$Sector=="Construction") & (errors$id %in% ids_c),c(2,8,20:24)])
w.uncert_error.T <- calc_wuncert.ee(errors[(errors$Sector=="Trade") & (errors$id %in% ids_t),c(2,8,20:24)])
w.uncert_error.S <- calc_wuncert.ee(errors[(errors$Sector=="Services") & (errors$id %in% ids_s),c(2,8,20:24)])

weights <- GDPdata[,c(1:4,6)]
idio.errors <- cbind(w.uncert_error.M[,c(1,2)],w.uncert_error.B[,2],w.uncert_error.T[,2],w.uncert_error.S[,2])
colnames(idio.errors) <- c("Date","Manufacturing","Construction","Trade","Services")
aggr.errors <- cbind(w.uncert_error.M[,c(1,3)],w.uncert_error.B[,3],w.uncert_error.T[,3],w.uncert_error.S[,3])
colnames(aggr.errors) <- c("Date","Manufacturing","Construction","Trade","Services")
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

auncert_indices <- cbind(w.uncert_error,GDPdata$IMF_Uncertainty, GDPdata$SAVI,GDPgrowth4$RGDP)
colnames(auncert_indices) <-c("Date","Dispersion","Idiosyncratic_error","Aggregate_error","EPU","SAVI","RGDP_Growth")
auncert_indices[,-1] <- scale(auncert_indices[,-1])
un <- auncert_indices #un <- na.locf(uncert_indices)
un[is.na(un)] <- 0
auncert_indices$Uncertainty_Combined <- princomp(un[,c(2:6)])$scores[,1]
auncert_indices <- auncert_indices[,c(1:6,8,7)]
auncert_indices <- cbind(uncert_indices[,c(1,2:4,7)],auncert_indices[,c(2:4,7)])
colnames(auncert_indices) <- c("Date","Disp: Full sample","Aggr: Full sample","Idio: Full sample","Uncert: Full sample", 
                               "Disp: Stable sample","Aggr: Stable sample","Idio: Stable sample","Uncert: Stable sample")


w.uncertainty.M <- calc_wuncert(BERplot1[BERplot1$Sector=="Manufacturing",])
w.uncertainty.B <- calc_wuncert(BERplot1[BERplot1$Sector=="Construction",])
w.uncertainty.T <- calc_wuncert(BERplot1[BERplot1$Sector=="Trade",])
w.uncertainty.S <- calc_wuncert(BERplot1[BERplot1$Sector=="Services",])

weights <- GDPdata[,c(1:4,6)]
w.uncertainty <- cbind(w.uncertainty.M[,c(2,16)],w.uncertainty.B[,16],w.uncertainty.T[,16],w.uncertainty.S[,16])
colnames(w.uncertainty) <- c("Date","Manufacturing","Construction","Trade","Services")
w.uncertainty$Construction[c(28,89)] <- NA
w.uncertainty[,-1] <- scale(w.uncertainty[,-1])
w.uncertainty$Dispersion <- sapply(w.uncertainty$Date,function(x) weighted.mean(w.uncertainty[which(w.uncertainty$Date==x),c(2:5)], 
                                                                                weights[weights$Date==x,-1],na.rm=TRUE))
w.uncertainty <- seisoen(w.uncertainty)
w.uncertainty[1,3] <- NA
w.uncertainty[1:5,4] <- NA
w.uncertainty[1:53,5] <- NA
errors$id <- factor(errors$id)

w.uncert_error.M <- calc_wuncert.ee(errors[(errors$Sector=="Manufacturing") & (errors$id %in% ids_m1),c(2,8,20:24)])
w.uncert_error.B <- calc_wuncert.ee(errors[(errors$Sector=="Construction") & (errors$id %in% ids_c1),c(2,8,20:24)])
w.uncert_error.T <- calc_wuncert.ee(errors[(errors$Sector=="Trade") & (errors$id %in% ids_t1),c(2,8,20:24)])
w.uncert_error.S <- calc_wuncert.ee(errors[(errors$Sector=="Services") & (errors$id %in% ids_s1),c(2,8,20:24)])

weights <- GDPdata[,c(1:4,6)]
idio.errors <- cbind(w.uncert_error.M[,c(1,2)],w.uncert_error.B[,2],w.uncert_error.T[,2],w.uncert_error.S[,2])
colnames(idio.errors) <- c("Date","Manufacturing","Construction","Trade","Services")
aggr.errors <- cbind(w.uncert_error.M[,c(1,3)],w.uncert_error.B[,3],w.uncert_error.T[,3],w.uncert_error.S[,3])
colnames(aggr.errors) <- c("Date","Manufacturing","Construction","Trade","Services")
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

auncert_indices1 <- cbind(w.uncert_error,GDPdata$IMF_Uncertainty, GDPdata$SAVI,GDPgrowth4$RGDP)
colnames(auncert_indices1) <-c("Date","Dispersion","Idiosyncratic_error","Aggregate_error","EPU","SAVI","RGDP_Growth")
auncert_indices1[,-1] <- scale(auncert_indices1[,-1])
un <- auncert_indices1 #un <- na.locf(uncert_indices)
un[is.na(un)] <- 0
auncert_indices1$Uncertainty_Combined <- princomp(un[,c(2:6)])$scores[,1]
auncert_indices1 <- auncert_indices1[,c(1:6,8,7)]
auncert_indices1 <- cbind(uncert_indices[,c(1,2:4,7)],auncert_indices1[,c(2:4,7)])
colnames(auncert_indices1) <- c("Date","Disp: Full sample","Aggr: Full sample","Idio: Full sample","Uncert: Full sample", 
                               "Disp: Stable sample","Aggr: Stable sample","Idio: Stable sample","Uncert: Stable sample")



index_plot <- auncert_indices[-1,]
index_plot$Date <- datums$Datum 
#index_plot[,2:6] <- scale(index_plot[,2:6])
g1 <- ggplot(index_plot) 
g1 <- g1 + geom_line(aes(x=Date, y=`Disp: Full sample`, colour="Disp: Full sample", linetype="Disp: Full sample", size = "Disp: Full sample"))
g1 <- g1 + geom_line(aes(x=Date, y=`Uncert: Full sample`, colour="Uncert: Full sample", linetype="Uncert: Full sample", size = "Uncert: Full sample"))
g1 <- g1 + geom_line(aes(x=Date, y=`Disp: Stable sample`, colour="Disp: Stable sample", linetype="Disp: Stable sample", size = "Disp: Stable sample"))
g1 <- g1 + geom_line(aes(x=Date, y=`Uncert: Stable sample`, colour="Uncert: Stable sample", linetype="Uncert: Stable sample", size = "Uncert: Stable sample"))
g1 <- g1 + scale_linetype_manual(values=c("solid","solid","solid","solid"))
g1 <- g1 + scale_size_manual(values=c(0.7,0.9,0.7,0.9))
g1 <- g1 + scale_colour_manual(values=c("#F8766D","red","#00BFC4","blue"))
g1 <- g1 + labs(color="Legend text", linetype="Legend text", size="Legend text")
g1 <- g1 + geom_rect(data=recessions.df, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='grey', alpha=0.5)
g1 <- g1 + ylab("Standardised Indicator") + xlab("")
g1 <- g1 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g1 <- g1 + theme(legend.title=element_blank()) 
g1 <- g1 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"), expand=c(0,0),limits = as.Date(c("1990-12-31", NA)))
g1 <- g1 + theme(legend.position="bottom")
g1 <- g1 + ggtitle("Responded > 50%") + theme(plot.title = element_text(hjust = 0.5))

index_plot <- auncert_indices1[-1,]
index_plot$Date <- datums$Datum 
#index_plot[,2:6] <- scale(index_plot[,2:6])
g2 <- ggplot(index_plot) 
g2 <- g2 + geom_line(aes(x=Date, y=`Disp: Full sample`, colour="Disp: Full sample", linetype="Disp: Full sample", size = "Disp: Full sample"))
g2 <- g2 + geom_line(aes(x=Date, y=`Uncert: Full sample`, colour="Uncert: Full sample", linetype="Uncert: Full sample", size = "Uncert: Full sample"))
g2 <- g2 + geom_line(aes(x=Date, y=`Disp: Stable sample`, colour="Disp: Stable sample", linetype="Disp: Stable sample", size = "Disp: Stable sample"))
g2 <- g2 + geom_line(aes(x=Date, y=`Uncert: Stable sample`, colour="Uncert: Stable sample", linetype="Uncert: Stable sample", size = "Uncert: Stable sample"))
g2 <- g2 + scale_linetype_manual(values=c("solid","solid","solid","solid"))
g2 <- g2 + scale_size_manual(values=c(0.7,0.9,0.7,0.9))
g2 <- g2 + scale_colour_manual(values=c("#F8766D","red","#00BFC4","blue"))
g2 <- g2 + labs(color="Legend text", linetype="Legend text", size="Legend text")
g2 <- g2 + geom_rect(data=recessions.df, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='grey', alpha=0.5)
g2 <- g2 + ylab("Standardised Indicator") + xlab("")
g2 <- g2 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g2 <- g2 + theme(legend.title=element_blank()) 
g2 <- g2 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"), expand=c(0,0),limits = as.Date(c("1990-12-31", NA)))
g2 <- g2 + theme(legend.position="bottom")
g2 <- g2 + ggtitle("Responded > 75%") + theme(plot.title = element_text(hjust = 0.5))

library(gridExtra)
library(grid)

grid_arrange_shared_legend(g1, g2, ncol=2, nrow =1)


index_plot <- auncert_indices[-1,]
index_plot$Date <- datums$Datum 
#index_plot[,2:6] <- scale(index_plot[,2:6])
g <- ggplot(index_plot) 
g <- g + geom_line(aes(x=Date, y=`Aggr: Full sample`, colour="Aggr: Full sample", linetype="Aggr: Full sample", size = "Aggr: Full sample"))
g <- g + geom_line(aes(x=Date, y=`Idio: Full sample`, colour="Idio: Full sample", linetype="Idio: Full sample", size = "Idio: Full sample"))
g <- g + geom_line(aes(x=Date, y=`Aggr: Stable sample`, colour="Aggr: Stable sample", linetype="Aggr: Stable sample", size = "Aggr: Stable sample"))
g <- g + geom_line(aes(x=Date, y=`Idio: Stable sample`, colour="Idio: Stable sample", linetype="Idio: Stable sample", size = "Idio: Stable sample"))
g <- g + scale_linetype_manual(values=c("solid","solid","solid","solid"))
g <- g + scale_size_manual(values=c(0.7,0.9,0.7,0.9))
g <- g + scale_colour_manual(values=c("#F8766D","red","#00BFC4","blue"))
g <- g + labs(color="Legend text", linetype="Legend text", size="Legend text")
g <- g + geom_rect(data=recessions.df, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='grey', alpha=0.5)
g <- g + ylab("Standardised Indicator") + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) 
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"), expand=c(0,0),limits = as.Date(c("1990-12-31", NA)))
g <- g + theme(legend.position="bottom")
g



index_plot <- auncert_indices[-1,]
index_plot$Date <- datums$Datum 
#index_plot[,2:6] <- scale(index_plot[,2:6])
g1 <- ggplot(index_plot) 
g1 <- g1 + geom_line(aes(x=Date, y=`Aggr: Full sample`, colour="Aggr: Full sample", linetype="Aggr: Full sample", size = "Aggr: Full sample"))
g1 <- g1 + geom_line(aes(x=Date, y=`Idio: Full sample`, colour="Idio: Full sample", linetype="Idio: Full sample", size = "Idio: Full sample"))
g1 <- g1 + geom_line(aes(x=Date, y=`Aggr: Stable sample`, colour="Aggr: Stable sample", linetype="Aggr: Stable sample", size = "Aggr: Stable sample"))
g1 <- g1 + geom_line(aes(x=Date, y=`Idio: Stable sample`, colour="Idio: Stable sample", linetype="Idio: Stable sample", size = "Idio: Stable sample"))
g1 <- g1 + scale_linetype_manual(values=c("solid","solid","solid","solid"))
g1 <- g1 + scale_size_manual(values=c(0.7,0.9,0.7,0.9))
g1 <- g1 + scale_colour_manual(values=c("#F8766D","red","#00BFC4","blue"))
g1 <- g1 + labs(color="Legend text", linetype="Legend text", size="Legend text")
g1 <- g1 + geom_rect(data=recessions.df, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='grey', alpha=0.5)
g1 <- g1 + ylab("Standardised Indicator") + xlab("")
g1 <- g1 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g1 <- g1 + theme(legend.title=element_blank()) 
g1 <- g1 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"), expand=c(0,0),limits = as.Date(c("1990-12-31", NA)))
g1 <- g1 + theme(legend.position="bottom")
g1 <- g1 + ggtitle("Responded > 50%") + theme(plot.title = element_text(hjust = 0.5))

index_plot <- auncert_indices1[-1,]
index_plot$Date <- datums$Datum 
#index_plot[,2:6] <- scale(index_plot[,2:6])
g2 <- ggplot(index_plot) 
g2 <- g2 + geom_line(aes(x=Date, y=`Aggr: Full sample`, colour="Aggr: Full sample", linetype="Aggr: Full sample", size = "Aggr: Full sample"))
g2 <- g2 + geom_line(aes(x=Date, y=`Idio: Full sample`, colour="Idio: Full sample", linetype="Idio: Full sample", size = "Idio: Full sample"))
g2 <- g2 + geom_line(aes(x=Date, y=`Aggr: Stable sample`, colour="Aggr: Stable sample", linetype="Aggr: Stable sample", size = "Aggr: Stable sample"))
g2 <- g2 + geom_line(aes(x=Date, y=`Idio: Stable sample`, colour="Idio: Stable sample", linetype="Idio: Stable sample", size = "Idio: Stable sample"))
g2 <- g2 + scale_linetype_manual(values=c("solid","solid","solid","solid"))
g2 <- g2 + scale_size_manual(values=c(0.7,0.9,0.7,0.9))
g2 <- g2 + scale_colour_manual(values=c("#F8766D","red","#00BFC4","blue"))
g2 <- g2 + labs(color="Legend text", linetype="Legend text", size="Legend text")
g2 <- g2 + geom_rect(data=recessions.df, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='grey', alpha=0.5)
g2 <- g2 + ylab("Standardised Indicator") + xlab("")
g2 <- g2 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g2 <- g2 + theme(legend.title=element_blank()) 
g2 <- g2 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"), expand=c(0,0),limits = as.Date(c("1990-12-31", NA)))
g2 <- g2 + theme(legend.position="bottom")
g2 <- g2 + ggtitle("Responded > 75%") + theme(plot.title = element_text(hjust = 0.5))

library(gridExtra)
library(grid)

grid_arrange_shared_legend(g1, g2, ncol=2, nrow =1)



source("corstarsl.R")
xt <- aconf_indices[,c(5,6,2,3,4)]
xt <- xtable(corstarsl(xt)[3:5,1:2], caption="Correlations between confidence indicators and real GDP growth")
print(xt, "latex",comment=FALSE, caption.placement = getOption("xtable.caption.placement", "top"), scalebox = 0.8)

xt <- aconf_indices1[,c(5,6,2,3,4)]
xt <- xtable(corstarsl(xt)[3:5,1:2], caption="Correlations between confidence indicators and real GDP growth")
print(xt, "latex",comment=FALSE, caption.placement = getOption("xtable.caption.placement", "top"), scalebox = 0.8)



source("corstarsl.R")
xt <- cbind(auncert_indices[,c(6:9,2:5)],aconf_indices[,6])
xt <- xtable(corstarsl(xt)[5:9,1:4], caption="Correlations between confidence indicators and real GDP growth")
print(xt, "latex",comment=FALSE, caption.placement = getOption("xtable.caption.placement", "top"), scalebox = 0.8)


source("corstarsl.R")
xt <- cbind(auncert_indices1[-1,c(6:9,2:5)],aconf_indices[-1,6])
xt <- xtable(corstarsl(xt)[5:9,1:4], caption="Correlations between confidence indicators and real GDP growth")
print(xt, "latex",comment=FALSE, caption.placement = getOption("xtable.caption.placement", "top"), scalebox = 0.8)



#=================
#Stable sample
#Read for speed
c <- aggregate(BER$Q1, by=list(BER$surveyQ,BER$id,BER$Sector), FUN=length)
c$Group.1 <- as.Date(as.yearqtr(c$Group.1, format = "%YQ%q"))

n_m <- length(unique(c$Group.1[c$Group.3=="Manufacturing"]))
n_c <- length(unique(c$Group.1[c$Group.3=="Construction"]))
n_t <- length(unique(c$Group.1[c$Group.3=="Trade"]))
n_s <- length(unique(c$Group.1[c$Group.3=="Services"]))
c <- aggregate(c$x, by=list(c$Group.2,c$Group.3), FUN=sum)

ids_m <- c$Group.1[c$Group.2=="Manufacturing" & c$x>=(n_m*0.5)]
ids_c <- c$Group.1[c$Group.2=="Construction" & c$x>=(n_c*0.5)]
ids_t <- c$Group.1[c$Group.2=="Trade" & c$x>=(n_t*0.5)]
ids_s <- c$Group.1[c$Group.2=="Services" & c$x>=(n_s*0.5)]
ids <- c(ids_m,ids_c,ids_t,ids_s)

BERplot <- BER[BER$id %in% ids_m,]
BERplot <- rbind(BERplot,BER[BER$id %in% ids_c,])
BERplot <- rbind(BERplot,BER[BER$id %in% ids_t,])
BERplot <- rbind(BERplot,BER[BER$id %in% ids_s,])

ids_m1 <- c$Group.1[c$Group.2=="Manufacturing" & c$x>=(n_m*0.75)]
ids_c1 <- c$Group.1[c$Group.2=="Construction" & c$x>=(n_c*0.75)]
ids_t1 <- c$Group.1[c$Group.2=="Trade" & c$x>=(n_t*0.75)]
ids_s1 <- c$Group.1[c$Group.2=="Services" & c$x>=(n_s*0.75)]
ids1 <- c(ids_m1,ids_c1,ids_t1,ids_s1)

BERplot1 <- BER[BER$id %in% ids_m1,]
BERplot1 <- rbind(BERplot1,BER[BER$id %in% ids_c1,])
BERplot1 <- rbind(BERplot1,BER[BER$id %in% ids_t1,])
BERplot1 <- rbind(BERplot1,BER[BER$id %in% ids_s1,])

plot <- aggregate(BERplot$Sector, by=list(BERplot$surveyQ,BERplot$Sector), FUN = length)
plot$Group.1 <- as.Date(as.yearqtr(plot$Group.1, format = "%YQ%q"))
g1 <- ggplot(plot, aes(x=Group.1, y=x,fill=Group.2))
g1 <- g1 + geom_bar(stat="identity") #nuut
g1 <- g1 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g1 <- g1 + scale_fill_discrete(name=" ")
g1 <- g1 + scale_y_continuous(labels=comma)
g1 <- g1 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g1 <- g1 + ylab("Number of Respondents") + xlab(" ")
g1 <- g1 + theme(legend.position="bottom")
g1 <- g1 + ggtitle("Responded > 50%") + theme(plot.title = element_text(hjust = 0.5))

plot <- aggregate(BERplot1$Sector, by=list(BERplot1$surveyQ,BERplot1$Sector), FUN = length)
plot$Group.1 <- as.Date(as.yearqtr(plot$Group.1, format = "%YQ%q"))
g2 <- ggplot(plot, aes(x=Group.1, y=x,fill=Group.2))
g2 <- g2 + geom_bar(stat="identity")
g2 <- g2 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g2 <- g2 + scale_fill_discrete(name="")
g2 <- g2 + scale_y_continuous(labels=comma)
g2 <- g2 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g2 <- g2 + ylab("Number of Respondents") + xlab(" ")
g2 <- g2 + theme(legend.position="bottom")
g2 <- g2 + ggtitle("Responded > 75%") + theme(plot.title = element_text(hjust = 0.5))

library(gridExtra)
library(grid)
grid.arrange(g1, g2, ncol=2, nrow =1)




#---------------------------------
#Include those that answered >50%
w.indicators.M <- calc_wconf(BERplot[BERplot$Sector=="Manufacturing",])
w.indicators.B <- calc_wconf(BERplot[BERplot$Sector=="Construction",])
w.indicators.T <- calc_wconf(BERplot[BERplot$Sector=="Trade",])
w.indicators.S <- calc_wconf(BERplot[BERplot$Sector=="Services",])

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

activity$Activity <- sapply(activity$Date, function(x) weighted.mean(activity[which(activity$Date==x),c(2:5)], 
                                                                     weights[weights$Date==x,-1],na.rm=TRUE))
conf$Confidence <- sapply(conf$Date, function(x) weighted.mean(conf[which(conf$Date==x),c(2:5)], 
                                                               weights[weights$Date==x,-1],na.rm=TRUE))
w.indicators <- cbind(activity[,c(1,6)],conf[,6])
colnames(w.indicators) <- c("Date","Activity","Confidence")
aconf_indices <- cbind(conf_indices[,-4:-5],w.indicators[,-1])
colnames(aconf_indices) <- c("Date","Conf (Cur): Full sample", "Conf (Exp): Full sample", 
                             "RGDP Growth","Conf (Cur): Stable sample","Conf (Exp): Stable sample")


#Include those that answered >75%
w.indicators.M <- calc_wconf(BERplot1[BERplot1$Sector=="Manufacturing",])
w.indicators.B <- calc_wconf(BERplot1[BERplot1$Sector=="Construction",])
w.indicators.T <- calc_wconf(BERplot1[BERplot1$Sector=="Trade",])
w.indicators.S <- calc_wconf(BERplot1[BERplot1$Sector=="Services",])


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

activity$Activity <- sapply(activity$Date, function(x) weighted.mean(activity[which(activity$Date==x),c(2:5)], 
                                                                     weights[weights$Date==x,-1],na.rm=TRUE))
conf$Confidence <- sapply(conf$Date, function(x) weighted.mean(conf[which(conf$Date==x),c(2:5)], 
                                                               weights[weights$Date==x,-1],na.rm=TRUE))
w.indicators <- cbind(activity[,c(1,6)],conf[,6])
colnames(w.indicators) <- c("Date","Activity","Confidence")



aconf_indices1 <- cbind(conf_indices[,-4:-5],w.indicators[,-1])
colnames(aconf_indices1) <- c("Date","Conf (Cur): Full sample", "Conf (Exp): Full sample", 
                              "RGDP Growth","Conf (Cur): Stable sample","Conf (Exp): Stable sample")

index_plot <- cbind(aconf_indices,aconf_indices1[,5:6]) #nuut
index_plot$Date <- as.Date(datums$Datum, format = "%Y/%m/%d")
index_plot[,2:8] <- scale(index_plot[,2:8])
index_plot <- index_plot[,c(1,2,5,7)]
colnames(index_plot) <- c("Date","Full Sample",">50% Sample",">75% Sample")
index_plot <- melt(index_plot, id="Date")  # convert to long format
g1 <- ggplot(data=index_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g1 <- g1 + geom_line()
g1 <- g1 + ylab("Standardised Indicator") + xlab("")
g1 <- g1 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g1 <- g1 + theme(legend.title=element_blank()) 
g1 <- g1 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"), expand=c(0,0),limits = as.Date(c("1990-12-31", NA)))
g1 <- g1 + theme(legend.position="bottom")
g1 <- g1 + ggtitle("Confidence (Current)") + theme(plot.title = element_text(hjust = 0.5))

index_plot <- cbind(aconf_indices,aconf_indices1[,5:6]) #nuut
index_plot$Date <- as.Date(datums$Datum, format = "%Y/%m/%d")
index_plot[,2:8] <- scale(index_plot[,2:8])
index_plot <- index_plot[,c(1,3,6,8)]
colnames(index_plot) <- c("Date","Full Sample",">50% Sample",">75% Sample")
index_plot <- melt(index_plot, id="Date")  # convert to long format
g2 <- ggplot(data=index_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g2 <- g2 + geom_line()
g2 <- g2 + ylab("Standardised Indicator") + xlab("")
g2 <- g2 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g2 <- g2 + theme(legend.title=element_blank()) 
g2 <- g2 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"), expand=c(0,0),limits = as.Date(c("1990-12-31", NA)))
g2 <- g2 + theme(legend.position="bottom")
g2 <- g2 + ggtitle("Confidence (Expected)") + theme(plot.title = element_text(hjust = 0.5))

library(gridExtra)
library(grid)
grid_arrange_shared_legend(g1, g2, ncol=2, nrow =1)

index_plot <- aconf_indices1[,]
index_plot$Date <- as.Date(datums$Datum[-1], format = "%Y/%m/%d") 
index_plot[,2:6] <- scale(index_plot[,2:6])
g2 <- ggplot(index_plot) 
g2 <- g2 + geom_line(aes(x=Date, y=`Conf (Cur): Full sample`, colour="Conf (Cur): Full sample", linetype="Conf (Cur): Full sample", size = "Conf (Cur): Full sample"))
g2 <- g2 + geom_line(aes(x=Date, y=`Conf (Exp): Full sample`, colour="Conf (Exp): Full sample", linetype="Conf (Exp): Full sample", size = "Conf (Exp): Full sample"))
g2 <- g2 + geom_line(aes(x=Date, y=`Conf (Cur): Stable sample`, colour="Conf (Cur): Stable sample", linetype="Conf (Cur): Stable sample", size = "Conf (Cur): Stable sample"))
g2 <- g2 + geom_line(aes(x=Date, y=`Conf (Exp): Stable sample`, colour="Conf (Exp): Stable sample", linetype="Conf (Exp): Stable sample", size = "Conf (Exp): Stable sample"))
g2 <- g2 + scale_linetype_manual(values=c("solid","solid","solid","solid"))
g2 <- g2 + scale_size_manual(values=c(0.8,0.9,0.8,0.9))
g2 <- g2 + scale_colour_manual(values=c("#F8766D","red","#00BFC4","blue"))
g2 <- g2 + labs(color="Legend text", linetype="Legend text", size="Legend text")
g2 <- g2 + ylab("Standardised Indicator") + xlab("")
g2 <- g2 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g2 <- g2 + theme(legend.title=element_blank()) 
g2 <- g2 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"), expand=c(0,0),limits = as.Date(c("1990-12-31", NA)))
g2 <- g2 + theme(legend.position="bottom")
g2 <- g2 + ggtitle("Responded > 75%") + theme(plot.title = element_text(hjust = 0.5))

library(gridExtra)
library(grid)
grid_arrange_shared_legend(g1, g2, ncol=2, nrow =1)


source('corstarsl.R')
corstarsl(aconf_indices[,-1])
corstarsl(aconf_indices1[,-1])

#------------
w.uncertainty.M <- calc_wuncert(BERplot[BERplot$Sector=="Manufacturing",])
w.uncertainty.B <- calc_wuncert(BERplot[BERplot$Sector=="Construction",])
w.uncertainty.T <- calc_wuncert(BERplot[BERplot$Sector=="Trade",])
w.uncertainty.S <- calc_wuncert(BERplot[BERplot$Sector=="Services",])

weights <- GDPdata[,c(1:4,6)]
w.uncertainty <- cbind(w.uncertainty.M[,c(2,16)],w.uncertainty.B[,16],w.uncertainty.T[,16],w.uncertainty.S[,16])
colnames(w.uncertainty) <- c("Date","Manufacturing","Construction","Trade","Services")
w.uncertainty$Construction[c(28,89)] <- NA
w.uncertainty[,-1] <- scale(w.uncertainty[,-1])
w.uncertainty$Dispersion <- sapply(w.uncertainty$Date,function(x) weighted.mean(w.uncertainty[which(w.uncertainty$Date==x),c(2:5)], 
                                                                                weights[weights$Date==x,-1],na.rm=TRUE))

w.uncertainty <- seisoen(w.uncertainty)
w.uncertainty[1,3] <- NA
w.uncertainty[1:5,4] <- NA
w.uncertainty[1:53,5] <- NA


errors$id <- factor(errors$id)


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
    w.errors <- as.data.frame(t(sapply(datumso$Datum, function(kwartaal) weeg.3(data[data$Datum==kwartaal,]))))
    w.errors <- cbind(datumso$Datum,w.errors)
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

#Read for speed
m_errors <- read.csv2("Man_errors4.csv", header=TRUE)[,-1]
m_errors$Datum <- as.Date(m_errors$Datum)
b_errors <- read.csv2("Bui_errors4.csv", header=TRUE)[,-1]
b_errors$Datum <- as.Date(b_errors$Datum)
t_errors <- read.csv2("Tra_errors4.csv", header=TRUE)[,-1]
t_errors$Datum <- as.Date(t_errors$Datum)
s_errors <- read.csv2("Ser_errors4.csv", header=TRUE)[,-1]
s_errors$Datum <- as.Date(s_errors$Datum)

#Calculate indicators with smaller sample
errors <- rbind(m_errors,b_errors,t_errors,s_errors)

w.uncert_error.M <- calc_wuncert.ee(errors[(errors$Sector=="Manufacturing") & (errors$id %in% ids_m),c(2,8,20:24)])
w.uncert_error.B <- calc_wuncert.ee(errors[(errors$Sector=="Construction") & (errors$id %in% ids_c),c(2,8,20:24)])
w.uncert_error.T <- calc_wuncert.ee(errors[(errors$Sector=="Trade") & (errors$id %in% ids_t),c(2,8,20:24)])
w.uncert_error.S <- calc_wuncert.ee(errors[(errors$Sector=="Services") & (errors$id %in% ids_s),c(2,8,20:24)])

weights <- GDPdata[,c(1:4,6)]
idio.errors <- cbind(w.uncert_error.M[,c(1,2)],w.uncert_error.B[,2],w.uncert_error.T[,2],w.uncert_error.S[,2])
colnames(idio.errors) <- c("Date","Manufacturing","Construction","Trade","Services")
aggr.errors <- cbind(w.uncert_error.M[,c(1,3)],w.uncert_error.B[,3],w.uncert_error.T[,3],w.uncert_error.S[,3])
colnames(aggr.errors) <- c("Date","Manufacturing","Construction","Trade","Services")
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

auncert_indices <- cbind(w.uncert_error,GDPdata$IMF_Uncertainty, GDPdata$SAVI,GDPgrowth4$RGDP)
colnames(auncert_indices) <-c("Date","Dispersion","Idiosyncratic_error","Aggregate_error","EPU","SAVI","RGDP_Growth")
auncert_indices[,-1] <- scale(auncert_indices[,-1])
un <- auncert_indices #un <- na.locf(uncert_indices)
un[is.na(un)] <- 0
auncert_indices$Uncertainty_Combined <- princomp(un[,c(2:6)])$scores[,1]
auncert_indices <- auncert_indices[,c(1:6,8,7)]
auncert_indices <- cbind(uncert_indices[,c(1,2:4,7)],auncert_indices[,c(2:4,7)])
colnames(auncert_indices) <- c("Date","Disp: Full sample","Aggr: Full sample","Idio: Full sample","Uncert: Full sample", 
                               "Disp: Stable sample","Aggr: Stable sample","Idio: Stable sample","Uncert: Stable sample")


#--------------------
w.uncertainty.M <- calc_wuncert(BERplot1[BERplot1$Sector=="Manufacturing",])
w.uncertainty.B <- calc_wuncert(BERplot1[BERplot1$Sector=="Construction",])
w.uncertainty.T <- calc_wuncert(BERplot1[BERplot1$Sector=="Trade",])
w.uncertainty.S <- calc_wuncert(BERplot1[BERplot1$Sector=="Services",])

weights <- GDPdata[,c(1:4,6)]
w.uncertainty <- cbind(w.uncertainty.M[,c(2,16)],w.uncertainty.B[,16],w.uncertainty.T[,16],w.uncertainty.S[,16])
colnames(w.uncertainty) <- c("Date","Manufacturing","Construction","Trade","Services")
w.uncertainty$Construction[c(28,89)] <- NA
w.uncertainty[,-1] <- scale(w.uncertainty[,-1])
w.uncertainty$Dispersion <- sapply(w.uncertainty$Date,function(x) weighted.mean(w.uncertainty[which(w.uncertainty$Date==x),c(2:5)], 
                                                                                weights[weights$Date==x,-1],na.rm=TRUE))
w.uncertainty <- seisoen(w.uncertainty)
w.uncertainty[1,3] <- NA
w.uncertainty[1:5,4] <- NA
w.uncertainty[1:53,5] <- NA
errors$id <- factor(errors$id)

w.uncert_error.M <- calc_wuncert.ee(errors[(errors$Sector=="Manufacturing") & (errors$id %in% ids_m1),c(2,8,20:24)])
w.uncert_error.B <- calc_wuncert.ee(errors[(errors$Sector=="Construction") & (errors$id %in% ids_c1),c(2,8,20:24)])
w.uncert_error.T <- calc_wuncert.ee(errors[(errors$Sector=="Trade") & (errors$id %in% ids_t1),c(2,8,20:24)])
w.uncert_error.S <- calc_wuncert.ee(errors[(errors$Sector=="Services") & (errors$id %in% ids_s1),c(2,8,20:24)])

weights <- GDPdata[,c(1:4,6)]
idio.errors <- cbind(w.uncert_error.M[,c(1,2)],w.uncert_error.B[,2],w.uncert_error.T[,2],w.uncert_error.S[,2])
colnames(idio.errors) <- c("Date","Manufacturing","Construction","Trade","Services")
aggr.errors <- cbind(w.uncert_error.M[,c(1,3)],w.uncert_error.B[,3],w.uncert_error.T[,3],w.uncert_error.S[,3])
colnames(aggr.errors) <- c("Date","Manufacturing","Construction","Trade","Services")
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

auncert_indices1 <- cbind(w.uncert_error,GDPdata$IMF_Uncertainty, GDPdata$SAVI,GDPgrowth4$RGDP)
colnames(auncert_indices1) <-c("Date","Dispersion","Idiosyncratic_error","Aggregate_error","EPU","SAVI","RGDP_Growth")
auncert_indices1[,-1] <- scale(auncert_indices1[,-1])
un <- auncert_indices1 #un <- na.locf(uncert_indices)
un[is.na(un)] <- 0
auncert_indices1$Uncertainty_Combined <- princomp(un[,c(2:6)])$scores[,1]
auncert_indices1 <- auncert_indices1[,c(1:6,8,7)]
auncert_indices1 <- cbind(uncert_indices[,c(1,2:4,7)],auncert_indices1[,c(2:4,7)])
colnames(auncert_indices1) <- c("Date","Disp: Full sample","Aggr: Full sample","Idio: Full sample","Uncert: Full sample", 
                                "Disp: Stable sample","Aggr: Stable sample","Idio: Stable sample","Uncert: Stable sample")




index_plot <- auncert_indices[,]
index_plot$Date <- as.Date(datums$Datum[], format = "%Y/%m/%d") 
g1 <- ggplot(index_plot)  #nuut
g1 <- g1 + geom_line(aes(x=Date, y=`Disp: Full sample`, colour="Disp: Full sample", linetype="Disp: Full sample", size = "Disp: Full sample"))
#g1 <- g1 + geom_line(aes(x=Date, y=`Uncert: Full sample`, colour="Uncert: Full sample", linetype="Uncert: Full sample", size = "Uncert: Full sample"))
g1 <- g1 + geom_line(aes(x=Date, y=`Disp: Stable sample`, colour="Disp: Stable sample", linetype="Disp: Stable sample", size = "Disp: Stable sample"))
#g1 <- g1 + geom_line(aes(x=Date, y=`Uncert: Stable sample`, colour="Uncert: Stable sample", linetype="Uncert: Stable sample", size = "Uncert: Stable sample"))
g1 <- g1 + scale_linetype_manual(values=c("solid","solid","solid","solid"))
g1 <- g1 + scale_size_manual(values=c(0.7,0.9,0.7,0.9))
g1 <- g1 + scale_colour_manual(values=c("#F8766D","red","#00BFC4","blue"))
g1 <- g1 + labs(color="Legend text", linetype="Legend text", size="Legend text")
g1 <- g1 + ylab("Standardised Indicator") + xlab("")
g1 <- g1 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g1 <- g1 + theme(legend.title=element_blank()) 
g1 <- g1 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"), expand=c(0,0),limits = as.Date(c("1990-12-31", NA)))
g1 <- g1 + theme(legend.position="bottom")
g1 <- g1 + ggtitle("Responded > 50%") + theme(plot.title = element_text(hjust = 0.5))

index_plot <- auncert_indices1[,]
index_plot$Date <- as.Date(datums$Datum[], format = "%Y/%m/%d")
g2 <- ggplot(index_plot) 
g2 <- g2 + geom_line(aes(x=Date, y=`Disp: Full sample`, colour="Disp: Full sample", linetype="Disp: Full sample", size = "Disp: Full sample"))
#g2 <- g2 + geom_line(aes(x=Date, y=`Uncert: Full sample`, colour="Uncert: Full sample", linetype="Uncert: Full sample", size = "Uncert: Full sample"))
g2 <- g2 + geom_line(aes(x=Date, y=`Disp: Stable sample`, colour="Disp: Stable sample", linetype="Disp: Stable sample", size = "Disp: Stable sample"))
#g2 <- g2 + geom_line(aes(x=Date, y=`Uncert: Stable sample`, colour="Uncert: Stable sample", linetype="Uncert: Stable sample", size = "Uncert: Stable sample"))
g2 <- g2 + scale_linetype_manual(values=c("solid","solid","solid","solid"))
g2 <- g2 + scale_size_manual(values=c(0.7,0.9,0.7,0.9))
g2 <- g2 + scale_colour_manual(values=c("#F8766D","red","#00BFC4","blue"))
g2 <- g2 + labs(color="Legend text", linetype="Legend text", size="Legend text")
g2 <- g2 + ylab("Standardised Indicator") + xlab("")
g2 <- g2 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g2 <- g2 + theme(legend.title=element_blank()) 
g2 <- g2 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"), expand=c(0,0),limits = as.Date(c("1990-12-31", NA)))
g2 <- g2 + theme(legend.position="bottom")
g2 <- g2 + ggtitle("Responded > 75%") + theme(plot.title = element_text(hjust = 0.5))

library(gridExtra)
library(grid)
grid_arrange_shared_legend(g1, g2, ncol=2, nrow =1)


index_plot <- auncert_indices[,]
index_plot$Date <- as.Date(datums$Datum[], format = "%Y/%m/%d") 
g1 <- ggplot(index_plot)  #nuut
#g1 <- g1 + geom_line(aes(x=Date, y=`Disp: Full sample`, colour="Disp: Full sample", linetype="Disp: Full sample", size = "Disp: Full sample"))
g1 <- g1 + geom_line(aes(x=Date, y=`Uncert: Full sample`, colour="Uncert: Full sample", linetype="Uncert: Full sample", size = "Uncert: Full sample"))
#g1 <- g1 + geom_line(aes(x=Date, y=`Disp: Stable sample`, colour="Disp: Stable sample", linetype="Disp: Stable sample", size = "Disp: Stable sample"))
g1 <- g1 + geom_line(aes(x=Date, y=`Uncert: Stable sample`, colour="Uncert: Stable sample", linetype="Uncert: Stable sample", size = "Uncert: Stable sample"))
g1 <- g1 + scale_linetype_manual(values=c("solid","solid","solid","solid"))
g1 <- g1 + scale_size_manual(values=c(0.7,0.9,0.7,0.9))
g1 <- g1 + scale_colour_manual(values=c("#F8766D","red","#00BFC4","blue"))
g1 <- g1 + labs(color="Legend text", linetype="Legend text", size="Legend text")
g1 <- g1 + ylab("Standardised Indicator") + xlab("")
g1 <- g1 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g1 <- g1 + theme(legend.title=element_blank()) 
g1 <- g1 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"), expand=c(0,0),limits = as.Date(c("1990-12-31", NA)))
g1 <- g1 + theme(legend.position="bottom")
g1 <- g1 + ggtitle("Responded > 50%") + theme(plot.title = element_text(hjust = 0.5))

index_plot <- auncert_indices1[,]
index_plot$Date <- as.Date(datums$Datum[], format = "%Y/%m/%d")
g2 <- ggplot(index_plot) 
#g2 <- g2 + geom_line(aes(x=Date, y=`Disp: Full sample`, colour="Disp: Full sample", linetype="Disp: Full sample", size = "Disp: Full sample"))
g2 <- g2 + geom_line(aes(x=Date, y=`Uncert: Full sample`, colour="Uncert: Full sample", linetype="Uncert: Full sample", size = "Uncert: Full sample"))
#g2 <- g2 + geom_line(aes(x=Date, y=`Disp: Stable sample`, colour="Disp: Stable sample", linetype="Disp: Stable sample", size = "Disp: Stable sample"))
g2 <- g2 + geom_line(aes(x=Date, y=`Uncert: Stable sample`, colour="Uncert: Stable sample", linetype="Uncert: Stable sample", size = "Uncert: Stable sample"))
g2 <- g2 + scale_linetype_manual(values=c("solid","solid","solid","solid"))
g2 <- g2 + scale_size_manual(values=c(0.7,0.9,0.7,0.9))
g2 <- g2 + scale_colour_manual(values=c("#F8766D","red","#00BFC4","blue"))
g2 <- g2 + labs(color="Legend text", linetype="Legend text", size="Legend text")
g2 <- g2 + ylab("Standardised Indicator") + xlab("")
g2 <- g2 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g2 <- g2 + theme(legend.title=element_blank()) 
g2 <- g2 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"), expand=c(0,0),limits = as.Date(c("1990-12-31", NA)))
g2 <- g2 + theme(legend.position="bottom")
g2 <- g2 + ggtitle("Responded > 75%") + theme(plot.title = element_text(hjust = 0.5))

library(gridExtra)
library(grid)
grid_arrange_shared_legend(g1, g2, ncol=2, nrow =1)



index_plot <- auncert_indices[,]
index_plot$Date <- as.Date(datums$Datum[], format = "%Y/%m/%d") 
g1 <- ggplot(index_plot) 
g1 <- g1 + geom_line(aes(x=Date, y=`Aggr: Full sample`, colour="Aggr: Full sample", linetype="Aggr: Full sample", size = "Aggr: Full sample"))
#g1 <- g1 + geom_line(aes(x=Date, y=`Idio: Full sample`, colour="Idio: Full sample", linetype="Idio: Full sample", size = "Idio: Full sample"))
g1 <- g1 + geom_line(aes(x=Date, y=`Aggr: Stable sample`, colour="Aggr: Stable sample", linetype="Aggr: Stable sample", size = "Aggr: Stable sample"))
#g1 <- g1 + geom_line(aes(x=Date, y=`Idio: Stable sample`, colour="Idio: Stable sample", linetype="Idio: Stable sample", size = "Idio: Stable sample"))
g1 <- g1 + scale_linetype_manual(values=c("solid","solid","solid","solid"))
g1 <- g1 + scale_size_manual(values=c(0.7,0.9,0.7,0.9))
g1 <- g1 + scale_colour_manual(values=c("#F8766D","red","#00BFC4","blue"))
g1 <- g1 + labs(color="Legend text", linetype="Legend text", size="Legend text")
g1 <- g1 + ylab("Standardised Indicator") + xlab("")
g1 <- g1 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g1 <- g1 + theme(legend.title=element_blank()) 
g1 <- g1 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"), expand=c(0,0),limits = as.Date(c("1990-12-31", NA)))
g1 <- g1 + theme(legend.position="bottom")
g1 <- g1 + ggtitle("Responded > 50%") + theme(plot.title = element_text(hjust = 0.5))

index_plot <- auncert_indices1[,]
index_plot$Date <- as.Date(datums$Datum[], format = "%Y/%m/%d") 
g2 <- ggplot(index_plot) 
g2 <- g2 + geom_line(aes(x=Date, y=`Aggr: Full sample`, colour="Aggr: Full sample", linetype="Aggr: Full sample", size = "Aggr: Full sample"))
#g2 <- g2 + geom_line(aes(x=Date, y=`Idio: Full sample`, colour="Idio: Full sample", linetype="Idio: Full sample", size = "Idio: Full sample"))
g2 <- g2 + geom_line(aes(x=Date, y=`Aggr: Stable sample`, colour="Aggr: Stable sample", linetype="Aggr: Stable sample", size = "Aggr: Stable sample"))
#g2 <- g2 + geom_line(aes(x=Date, y=`Idio: Stable sample`, colour="Idio: Stable sample", linetype="Idio: Stable sample", size = "Idio: Stable sample"))
g2 <- g2 + scale_linetype_manual(values=c("solid","solid","solid","solid"))
g2 <- g2 + scale_size_manual(values=c(0.7,0.9,0.7,0.9))
g2 <- g2 + scale_colour_manual(values=c("#F8766D","red","#00BFC4","blue"))
g2 <- g2 + labs(color="Legend text", linetype="Legend text", size="Legend text")
g2 <- g2 + ylab("Standardised Indicator") + xlab("")
g2 <- g2 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g2 <- g2 + theme(legend.title=element_blank()) 
g2 <- g2 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"), expand=c(0,0),limits = as.Date(c("1990-12-31", NA)))
g2 <- g2 + theme(legend.position="bottom")
g2 <- g2 + ggtitle("Responded > 75%") + theme(plot.title = element_text(hjust = 0.5))

library(gridExtra)
library(grid)
grid_arrange_shared_legend(g1, g2, ncol=2, nrow =1)



index_plot <- auncert_indices[,]
index_plot$Date <- as.Date(datums$Datum[], format = "%Y/%m/%d") 
g1 <- ggplot(index_plot) 
#g1 <- g1 + geom_line(aes(x=Date, y=`Aggr: Full sample`, colour="Aggr: Full sample", linetype="Aggr: Full sample", size = "Aggr: Full sample"))
g1 <- g1 + geom_line(aes(x=Date, y=`Idio: Full sample`, colour="Idio: Full sample", linetype="Idio: Full sample", size = "Idio: Full sample"))
#g1 <- g1 + geom_line(aes(x=Date, y=`Aggr: Stable sample`, colour="Aggr: Stable sample", linetype="Aggr: Stable sample", size = "Aggr: Stable sample"))
g1 <- g1 + geom_line(aes(x=Date, y=`Idio: Stable sample`, colour="Idio: Stable sample", linetype="Idio: Stable sample", size = "Idio: Stable sample"))
g1 <- g1 + scale_linetype_manual(values=c("solid","solid","solid","solid"))
g1 <- g1 + scale_size_manual(values=c(0.7,0.9,0.7,0.9))
g1 <- g1 + scale_colour_manual(values=c("#F8766D","red","#00BFC4","blue"))
g1 <- g1 + labs(color="Legend text", linetype="Legend text", size="Legend text")
g1 <- g1 + ylab("Standardised Indicator") + xlab("")
g1 <- g1 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g1 <- g1 + theme(legend.title=element_blank()) 
g1 <- g1 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"), expand=c(0,0),limits = as.Date(c("1990-12-31", NA)))
g1 <- g1 + theme(legend.position="bottom")
g1 <- g1 + ggtitle("Responded > 50%") + theme(plot.title = element_text(hjust = 0.5))

index_plot <- auncert_indices1[,]
index_plot$Date <- as.Date(datums$Datum[], format = "%Y/%m/%d") 
g2 <- ggplot(index_plot) 
#g2 <- g2 + geom_line(aes(x=Date, y=`Aggr: Full sample`, colour="Aggr: Full sample", linetype="Aggr: Full sample", size = "Aggr: Full sample"))
g2 <- g2 + geom_line(aes(x=Date, y=`Idio: Full sample`, colour="Idio: Full sample", linetype="Idio: Full sample", size = "Idio: Full sample"))
#g2 <- g2 + geom_line(aes(x=Date, y=`Aggr: Stable sample`, colour="Aggr: Stable sample", linetype="Aggr: Stable sample", size = "Aggr: Stable sample"))
g2 <- g2 + geom_line(aes(x=Date, y=`Idio: Stable sample`, colour="Idio: Stable sample", linetype="Idio: Stable sample", size = "Idio: Stable sample"))
g2 <- g2 + scale_linetype_manual(values=c("solid","solid","solid","solid"))
g2 <- g2 + scale_size_manual(values=c(0.7,0.9,0.7,0.9))
g2 <- g2 + scale_colour_manual(values=c("#F8766D","red","#00BFC4","blue"))
g2 <- g2 + labs(color="Legend text", linetype="Legend text", size="Legend text")
g2 <- g2 + ylab("Standardised Indicator") + xlab("")
g2 <- g2 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g2 <- g2 + theme(legend.title=element_blank()) 
g2 <- g2 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"), expand=c(0,0),limits = as.Date(c("1990-12-31", NA)))
g2 <- g2 + theme(legend.position="bottom")
g2 <- g2 + ggtitle("Responded > 75%") + theme(plot.title = element_text(hjust = 0.5))

library(gridExtra)
library(grid)
grid_arrange_shared_legend(g1, g2, ncol=2, nrow =1)



index_plot <- cbind(auncert_indices,auncert_indices1[,6:9],aconf_indices1[,4]) #nuut
index_plot$Date <- as.Date(datums$Datum, format = "%Y/%m/%d")
index_plot[,2:14] <- scale(index_plot[,2:14])
index_plot <- index_plot[,c(1,2,6,10)]
colnames(index_plot) <- c("Date","Full Sample",">50% Sample",">75% Sample")
index_plot <- melt(index_plot, id="Date")  # convert to long format
g1 <- ggplot(data=index_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g1 <- g1 + geom_line()
g1 <- g1 + ylab("Standardised Indicator") + xlab("")
g1 <- g1 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g1 <- g1 + theme(legend.title=element_blank()) 
g1 <- g1 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"), expand=c(0,0),limits = as.Date(c("1990-12-31", NA)))
g1 <- g1 + theme(legend.position="bottom")
g1 <- g1 + ggtitle("Dispersion") + theme(plot.title = element_text(hjust = 0.5))

index_plot <- cbind(auncert_indices,auncert_indices1[,6:9],aconf_indices1[,4]) #nuut
index_plot$Date <- as.Date(datums$Datum, format = "%Y/%m/%d")
index_plot[,2:14] <- scale(index_plot[,2:14])
index_plot <- index_plot[,c(1,3,7,11)]
colnames(index_plot) <- c("Date","Full Sample",">50% Sample",">75% Sample")
index_plot <- melt(index_plot, id="Date")  # convert to long format
g2 <- ggplot(data=index_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g2 <- g2 + geom_line()
g2 <- g2 + ylab("Standardised Indicator") + xlab("")
g2 <- g2 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g2 <- g2 + theme(legend.title=element_blank()) 
g2 <- g2 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"), expand=c(0,0),limits = as.Date(c("1990-12-31", NA)))
g2 <- g2 + theme(legend.position="bottom")
g2 <- g2 + ggtitle("Aggregate Error") + theme(plot.title = element_text(hjust = 0.5))

index_plot <- cbind(auncert_indices,auncert_indices1[,6:9],aconf_indices1[,4]) #nuut
index_plot$Date <- as.Date(datums$Datum, format = "%Y/%m/%d")
index_plot[,2:14] <- scale(index_plot[,2:14])
index_plot <- index_plot[,c(1,4,8,12)]
colnames(index_plot) <- c("Date","Full Sample",">50% Sample",">75% Sample")
index_plot <- melt(index_plot, id="Date")  # convert to long format
g3 <- ggplot(data=index_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g3 <- g3 + geom_line()
g3 <- g3 + ylab("Standardised Indicator") + xlab("")
g3 <- g3 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g3 <- g3 + theme(legend.title=element_blank()) 
g3 <- g3 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"), expand=c(0,0),limits = as.Date(c("1990-12-31", NA)))
g3 <- g3 + theme(legend.position="bottom")
g3 <- g3 + ggtitle("Idiosyncratic Error") + theme(plot.title = element_text(hjust = 0.5))

index_plot <- cbind(auncert_indices,auncert_indices1[,6:9],aconf_indices1[,4]) #nuut
index_plot$Date <- as.Date(datums$Datum, format = "%Y/%m/%d")
index_plot[,2:14] <- scale(index_plot[,2:14])
index_plot <- index_plot[,c(1,5,9,13)]
colnames(index_plot) <- c("Date","Full Sample",">50% Sample",">75% Sample")
index_plot <- melt(index_plot, id="Date")  # convert to long format
g4 <- ggplot(data=index_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g4 <- g4 + geom_line()
g4 <- g4 + ylab("Standardised Indicator") + xlab("")
g4 <- g4 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g4 <- g4 + theme(legend.title=element_blank()) 
g4 <- g4 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"), expand=c(0,0),limits = as.Date(c("1990-12-31", NA)))
g4 <- g4 + theme(legend.position="bottom")
g4 <- g4 + ggtitle("Uncertainty") + theme(plot.title = element_text(hjust = 0.5))

library(gridExtra)
library(grid)

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



index_plot <- services
colnames(index_plot) <- c("Date","Confidence (Current)", "Confidence (Expected)","BER BCI", "RGDP Growth")
index_plot$Date <- datums$Datum
index_plot[,-1] <- scale(index_plot[,-1])
index_plot <- melt(index_plot, id="Date")  # convert to long format
g4 <- ggplot(index_plot, aes(x=Date,y=value,group=variable,colour=variable,linetype=variable)) 
g4 <- g4 + scale_linetype_manual(values=c("solid","solid","dashed","twodash"))
g4 <- g4 + geom_line()
g4 <- g4 + theme(legend.title=element_blank())
g4 <- g4 + ggtitle("Services") + theme(plot.title = element_text(hjust = 0.5))
g4 <- g4 + ylab("") + xlab("")
g4 <- g4 + theme(legend.position="bottom")
g4 <- g4 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g4 <- g4 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))







source('corstarsl.R')
corstarsl(cbind(auncert_indices[,-1],conf_indices$RGDP_Growth))
corstarsl(cbind(auncert_indices1[,-1],conf_indices$RGDP_Growth))



