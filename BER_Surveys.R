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

pub_b <- read.csv("Building_BER_Published.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)
ref_b <- read.csv("Ref Series_Build.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)

pub_a <- read.csv("Arc_Published.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)

pub_m <- read.csv("Manufacturing_BER_Published.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)
ref_m <- read.csv("Ref Series_Manufac.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)

pub_r <- read.csv("Retail_BER_Published.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)
ref_r <- read.csv("Ref Series_Retail.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)
ad_r <- read.csv("Retail_Adjustment.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)
ad_r[,-1] <- ad_r[,-1]+pub_r[,-1]

pub_w <- read.csv("Wholesale_BER_Published.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)
#ref_w <- read.csv("Ref Series_Wholesale.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)
ad_w <- read.csv("Wholesale_Adjustment.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)
ad_w[,-1] <- ad_w[,-1]+pub_w[,-1]

pub_v <- read.csv("Motor_BER_Published.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)
#ref_v <- read.csv("Ref Series_Motor.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)

pub_s <- read.csv("Services_BER_Published.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)
#ref_s <- read.csv("Ref Series_Services.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)

#-----------------------
#BUILDING
BER.B <- read.csv("Building_93Q2-17Q2.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)
#BER.B <- read.csv("Building_full.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)
colnames(BER.B)[1:6] <- c("region","id","sector","weight","factor","surveyQ")
BER.B$surveyQ <- toupper(BER.B$surveyQ)
BER.B$sector <- factor(BER.B$sector) #could include labels
BER.B$id <- factor(BER.B$id)
BER.B$region <- factor(BER.B$region)

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

#Clean regions
#Mode <- function(x) {
#    ux <- na.remove(unique(x))
#    ux[which.max(tabulate(match(x, ux)))]
#}
#fout <- c("1995Q1")
#for(i in levels(BER.B$id)) {
#    BER.B$region[(BER.B$surveyQ %in% fout) & (BER.B$id == i)] <- Mode(BER.B$region[(BER.B$id == i)]) 
#    
#}

#Exclude Latecomers and old quesions
BER.B <- BER.B[BER.B$Latecomer == FALSE | is.na(BER.B$Latecomer),]
BER.B <- BER.B[,1:20]
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

#--------------------------
#Architects, QSs and Civil Engineers
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
qs <- skoon(read.csv("QS.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE))
civil <- skoon(read.csv("Civils.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE))[,-21:-22]


##=============================
#MANUFACTURING

#Correcting the old faktor variable 
BER.M <- read.csv("Manufacturing_current faktor.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)
colnames(BER.M)[1:7] <- c("region","id","sector","weight","turnover","factor","surveyQ")
BER.M$surveyQ <- toupper(BER.M$surveyQ)
BER.M$sector <- factor(BER.M$sector) #could include labels
BER.M$id <- factor(BER.M$id)
BER.M$surveyQ <- factor(BER.M$surveyQ)

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
# replace 1,2,3 (Up, Same, Down) responses with 1,0,-1
for(i in 8:64) {
    BER.M[,i] <- replace(BER.M[,i], BER.M[,i]==2, 0)
    BER.M[,i] <- replace(BER.M[,i], BER.M[,i]==3,-1)
}

for(i in 43:49) {
    BER.M[,i] <- replace(BER.M[,i], BER.M[,i]==0, 0.5)
    BER.M[,i] <- replace(BER.M[,i], BER.M[,i]==-1, 0)
}

#-----------------------
#New faktor variable 
BER.Mn <- read.csv("Manufacturing_new faktor.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)
colnames(BER.Mn)[1:7] <- c("region","id","sector","weight","turnover","factor","surveyQ")
BER.Mn$surveyQ <- toupper(BER.Mn$surveyQ)
BER.Mn$sector <- factor(BER.Mn$sector) #could include labels
BER.Mn$id <- factor(BER.Mn$id)

#Clean SurveyQ variable
BER.Mn$temp <- NULL
for(i in 1:nrow(BER.Mn)) {
    ifelse(substr(BER.Mn$surveyQ[i], 1, 1)==9, 
           BER.Mn$temp[i] <- paste0("19",BER.Mn$surveyQ[i],sep=""),
           BER.Mn$temp[i] <- paste0("20",BER.Mn$surveyQ[i],sep=""))
}
BER.Mn$surveyQ <- BER.Mn$temp
BER.Mn <- BER.Mn[,-ncol(BER.Mn)]
BER.Mn$surveyQ <- factor(BER.Mn$surveyQ)

#Exlcude Latecomers and old questions
BER.Mn <- BER.Mn[BER.Mn$Latecomer == FALSE | is.na(BER.Mn$Latecomer),]
BER.Mn <- BER.Mn[,!grepl("X",colnames(BER.Mn))]    
BER.Mn <- BER.Mn[,1:64]

# replace 1,2,3 (Up, Same, Down) responses with 1,0,-1
for(i in 8:64) {
    BER.Mn[,i] <- replace(BER.Mn[,i], BER.Mn[,i]==2, 0)
    BER.Mn[,i] <- replace(BER.Mn[,i], BER.Mn[,i]==3,-1)
}
for(i in 43:49) {
    BER.Mn[,i] <- replace(BER.Mn[,i], BER.Mn[,i]==0, 0.5)
    BER.Mn[,i] <- replace(BER.Mn[,i], BER.Mn[,i]==-1, 0)
}

#-----------------------
#New 2-step weighting 
BER.M2 <- read.csv("Manufacturing_new faktor_sep.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)
colnames(BER.M2)[1:9] <- c("region","id","sector","weight","turnover","firmw","sectorw","factor","surveyQ")
BER.M2$surveyQ <- toupper(BER.M2$surveyQ)
BER.M2$sector <- factor(BER.M2$sector) #could include labels
BER.M2$id <- factor(BER.M2$id)
BER.M2$surveyQ <- factor(BER.M2$surveyQ)
BER.M2$factor <- as.numeric(as.character(BER.M2$factor))
#BER.M$factor <- BER.M$firmw * BER.M$sectorw

BER.M2 <- BER.M2[BER.M2$Latecomer == FALSE | is.na(BER.M2$Latecomer),]
BER.M2 <- BER.M2[,!grepl("X",colnames(BER.M2))]    
BER.M2 <- BER.M2[,1:66]

#Maak factor reg
Ej <- aggregate(BER.M2$firmw, by=list(BER.M2$surveyQ,BER.M2$sector), FUN=sum)
BER.M2 <-  merge(BER.M2, Ej, by.x=c("surveyQ","sector"), by.y=c("Group.1","Group.2"),all = TRUE)
BER.M2$factor <- BER.M2$factor/BER.M2$x
BER.M2 <- BER.M2[,c(3,4,2,5:9,1,10:66)]

for(i in 10:66) {
    BER.M2[,i] <- replace(BER.M2[,i], BER.M2[,i]==2, 0)
    BER.M2[,i] <- replace(BER.M2[,i], BER.M2[,i]==3,-1)
}
for(i in 45:51) {
    BER.M2[,i] <- replace(BER.M2[,i], BER.M2[,i]==0, 0.5)
    BER.M2[,i] <- replace(BER.M2[,i], BER.M2[,i]==-1, 0)
}



#=============================
#TRADE
#=============================
#RETAIL
BER.R <- read.csv("Retail_92Q2-17Q2.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)
colnames(BER.R)[1:6] <- c("region","id","sector","weight","factor","surveyQ")
BER.R$surveyQ <- toupper(BER.R$surveyQ)
BER.R$sector <- factor(BER.R$sector) #could include labels
BER.R$id <- factor(BER.R$id)

#Clean SurveyQ variable
BER.R$temp <- NULL
for(i in 1:nrow(BER.R)) {
    ifelse(substr(BER.R$surveyQ[i], 1, 1)==9, 
           BER.R$temp[i] <- paste0("19",BER.R$surveyQ[i],sep=""),
           BER.R$temp[i] <- paste0("20",BER.R$surveyQ[i],sep=""))
}
BER.R$surveyQ <- BER.R$temp
BER.R <- BER.R[,-ncol(BER.R)]
BER.R$surveyQ <- factor(BER.R$surveyQ)

#Clean regions
Mode <- function(x) {
    ux <- na.remove(unique(x))
    ux[which.max(tabulate(match(x, ux)))]
}
foute <- c("1997Q2")#,"2000Q3","2000Q4","2001Q1")
korrek <- c("1996Q1","1996Q2","1996Q3","1996Q4","1997Q1",
            "1997Q3","1997Q4","1998Q1","1998Q2","1998Q3",
            "1998Q4")#,"2004Q1","2004Q2","2004Q3","2004Q4",
#"2005Q1","2005Q2","2005Q3","2006Q1","2006Q2","2006Q3","2006Q4")
for(i in levels(BER.R$id)) {
    if(!is.na(Mode(BER.R$region[(BER.R$surveyQ %in% korrek) & (BER.R$id == i)]))) {
        BER.R$region[(BER.R$surveyQ %in% foute) & (BER.R$id == i)] <- Mode(BER.R$region[(BER.R$surveyQ %in% korrek) & (BER.R$id == i)]) 
    }
}
BER.R$region <- factor(BER.R$region)

BER.R <- BER.R[BER.R$Latecomer == FALSE | is.na(BER.R$Latecomer),]
BER.R <- BER.R[,1:21]
BER.R <- BER.R[,!grepl("X",colnames(BER.R))]    

for(i in 7:21) {
    BER.R[,i] <- replace(BER.R[,i], BER.R[,i]==2, 0)
    BER.R[,i] <- replace(BER.R[,i], BER.R[,i]==3,-1)
}


#-------------------
#WHOLESALE

BER.W <- read.csv("Wholesale_92Q2-17Q2.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)
colnames(BER.W)[1:6] <- c("region","id","sector","weight","factor","surveyQ")
BER.W$surveyQ <- toupper(BER.W$surveyQ)
BER.W$sector <- factor(BER.W$sector) #could include labels
BER.W$id <- factor(BER.W$id)
BER.W$region <- factor(BER.W$region)

#Clean SurveyQ variable
BER.W$temp <- NULL
for(i in 1:nrow(BER.W)) {
    ifelse(substr(BER.W$surveyQ[i], 1, 1)==9, 
           BER.W$temp[i] <- paste0("19",BER.W$surveyQ[i],sep=""),
           BER.W$temp[i] <- paste0("20",BER.W$surveyQ[i],sep=""))
}
BER.W$surveyQ <- BER.W$temp
BER.W <- BER.W[,-ncol(BER.W)]
BER.W$surveyQ <- factor(BER.W$surveyQ)

BER.W <- BER.W[BER.W$Latecomer == FALSE | is.na(BER.W$Latecomer),]
BER.W <- BER.W[,1:21]
BER.W <- BER.W[,!grepl("X",colnames(BER.W))]    

for(i in 7:21) {
    BER.W[,i] <- replace(BER.W[,i], BER.W[,i]==2, 0)
    BER.W[,i] <- replace(BER.W[,i], BER.W[,i]==3,-1)
}
#-------------------
#MOTOR VEHICLES

BER.V <- read.csv("Motor_92Q2-17Q2.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)
colnames(BER.V)[1:6] <- c("region","id","sector","weight","factor","surveyQ")
BER.V$surveyQ <- toupper(BER.V$surveyQ)
BER.V$sector <- factor(BER.V$sector) #could include labels
BER.V$id <- factor(BER.V$id)
BER.V$region <- factor(BER.V$region)

#Clean SurveyQ variable
BER.V$temp <- NULL
for(i in 1:nrow(BER.V)) {
    ifelse(substr(BER.V$surveyQ[i], 1, 1)==9, 
           BER.V$temp[i] <- paste0("19",BER.V$surveyQ[i],sep=""),
           BER.V$temp[i] <- paste0("20",BER.V$surveyQ[i],sep=""))
}
BER.V$surveyQ <- BER.V$temp
BER.V <- BER.V[,-ncol(BER.V)]
BER.V$surveyQ <- factor(BER.V$surveyQ)

#Latecomers and old questions excluded
BER.V <- BER.V[BER.V$Latecomer == FALSE | is.na(BER.V$Latecomer),]
BER.V <- BER.V[,1:28]
BER.V <- BER.V[,!grepl("X",colnames(BER.V))]    

#Replace Values
for(i in 7:28) {
    BER.V[,i] <- replace(BER.V[,i], BER.V[,i]==2, 0)
    BER.V[,i] <- replace(BER.V[,i], BER.V[,i]==3,-1)
}


#================================
#SERVICES

BER.S <- read.csv("Services_05Q2-17Q2.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)
colnames(BER.S)[1:6] <- c("region","id","sector","weight","factor","surveyQ")
BER.S$surveyQ <- toupper(BER.S$surveyQ)
BER.S$sector <- factor(BER.S$sector) #could include labels
BER.S$id <- factor(BER.S$id)
BER.S$region <- factor(BER.S$region)

#Clean SurveyQ variable
BER.S$temp <- NULL
for(i in 1:nrow(BER.S)) {
    ifelse(substr(BER.S$surveyQ[i], 1, 1)==9, 
           BER.S$temp[i] <- paste0("19",BER.S$surveyQ[i],sep=""),
           BER.S$temp[i] <- paste0("20",BER.S$surveyQ[i],sep=""))
}
BER.S$surveyQ <- BER.S$temp
BER.S <- BER.S[,-ncol(BER.S)]
BER.S$surveyQ <- factor(BER.S$surveyQ)

#Eclude Latecomers and old quesions
BER.S <- BER.S[BER.S$Latecomer == FALSE | is.na(BER.S$Latecomer),]
BER.S <- BER.S[,1:21]
BER.S <- BER.S[,!grepl("X",colnames(BER.S))]    

#Replace Values
for(i in 7:21) {
    BER.S[,i] <- replace(BER.S[,i], BER.S[,i]==2, 0)
    BER.S[,i] <- replace(BER.S[,i], BER.S[,i]==3,-1)
}

##=====================##
## DEFINE SECTOR CODES ##
##=====================##

#BUILDING
residential <- c(5000,6000)
non_residential <- c(5010,6010)
contractor_res <- 5000
contractor_nonres <- 5010
contractor <- c(5000,5010)
subcon_res <- 6000
subcon_nonres <- 6010
subcon <- c(6000,6010)
all_b <- c(5000,5010,6000,6010)
streke <- unique(BER.B$region)

arcs <- 99
civils <- c(700,701,702,703)
qss <- 88

#-----------------------
#MANUFACTURING
food <- c(1010, 1011,1013,1019,1020,1021)
textiles <- c(1120,1040,1049,1042,1060,1070)
wood <- c(1109,1110,1080,1081)
chemicals <- c(1130,1140,1149,1219)
glass <- c(1153,1159)
metals <- c(1160,1161,1170,1179,1181,1182,1189)
electrical <- c(1190,1191,1199,1192,1194)
transport.m <- c(1200,1201,1209)
furniture <- c(1090,1099)
all_m <- as.numeric(as.character(unique(BER.M$sector)))

consumer <- c(1010,1011,1019,1049,1060,1070,1090,1099,1110,1120,1149,1189,1192,1020,1021)
intermediate <- c(1013,1040,1042,1080,1081,1109,1130,1140,1153,1159,1160,1161,
                  1179,1191,1199,1219)
capital <- c(1170,1181,1182,1189,1190,1194,1200,1201,1209)
#streke <- unique(BER.M$region)


#-----------------------
#TRADE
#RETAIL
retailsd <- c(3110,3120,3160,3140,3130,3170,3150)
retailnd <- c(3230,3210,3240,3220)
retaild <- c(3330,3370,3310,3340,3350,3320,3380,3360)
#retailo <- c(3410,3420)
hardware <- 3330 
other_duarbles <- c(3370,3310,3340,3350,3320,3380,3360)
all_r <- as.numeric(as.character(unique(BER.R$sector)))
#streke <- unique(BER.R$region)

#WHOLESALE
wholenc <- c(2120,2110,2130,2140)
wholec <- c(2250,2220,2230,2240,2210,2270,2260)
all_w <- as.numeric(as.character(unique(BER.W$sector)))
#streke <- unique(BER.W$region)

#MOTOR
all_v <- as.numeric(as.character(unique(BER.V$sector)))

#----------------------------
#SERVICES
catering <- c(6000,6001,6020,6030,6011)
transport.s <- c(7020,7010,7070,7090,7080,7060,7000,7040,7100,7120,7110,7050)
realestate <- c(8000,8010,8020)
business <- c(8040,8080,8070,8090,8060,8050,8030,
              8150,8120,8210,8180,8140,8160,8190,8100,8200,8230,8130,8110,8170,8240,8220)
#community <- c(9000,9010,9030,9050,9060,9020,9040)
all_s <- as.numeric(as.character(unique(BER.S$sector)))
#streke <- unique(BER.S$region)

##=====================##
##Define Functions
##=====================##

ongeweeg <- function(data,sektor,streek,d) {
    data <- data[data$sector %in% sektor & data$region %in% streek,]
    sector <- aggregate(data[,(match("surveyQ",colnames(data))+1):ncol(data)], 
                        by=list(data$surveyQ), FUN=mean, na.rm=TRUE)
    sector$Obs <- aggregate(data$Q2A, by=list(data$surveyQ), FUN=length)[,2]
    sector <- merge(datums[d,],sector, by.x="Date", by.y="Group.1", all=TRUE)
    sector <- sector[,c(1:2,ncol(sector),3:(ncol(sector)-1))]
    sector[,-1:-3] <- na.approx(sector[,-1:-3]*100)
    return(sector)
}


geweeg <- function(data,sektor,streek,d) {
    weeg <- function(temp) {  #calculate weighted mean for each quarter for all columns
        temp <- cbind(factor=temp$factor,temp$factor*temp[(match("surveyQ",colnames(temp))+1):ncol(temp)])
        temp <- colSums(temp, na.rm=TRUE, dims = 1)/    #calculate the sum(wi*xi)/sum(wi)
            sapply(colnames(temp), function(x) sum(temp$factor[!is.na(temp[colnames(temp) == x])]))
        return(temp) #weight only by those that responded to a specific question
    }
    
    data <- data[data$sector %in% sektor & data$region %in% streek,]
    sector <- data.frame()
    for(kwartaal in levels(data$surveyQ)) {
        sector <- rbind(sector,weeg(data[data$surveyQ==kwartaal,]))
    }
    sector[,1] <- levels(data$surveyQ)
    colnames(sector) <- colnames(data)[(match("surveyQ",colnames(data))):ncol(data)]
    sector$Obs <- aggregate(data$Q2A, by=list(data$surveyQ), FUN=length)[,2]
    sector <- merge(datums[d,],sector, by.x="Date", by.y="surveyQ", all=TRUE)
    sector <- sector[,c(1:2,ncol(sector),3:(ncol(sector)-1))]
    sector[,-1:-3] <- na.approx(sector[,-1:-3]*100)
    return(sector)
}


#Explicit 2-step weighting
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
    man <- BER.M2[BER.M2$sector %in% sektor & BER.M2$region %in% streek,]
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
##======================##
## CALCULATE INDICATORS ##
##======================##

#BUILDING
Building <- ongeweeg(BER.B,all_b,streke,6:102)
BER.B$factor <- BER.B$weight
Building_w <- geweeg(BER.B,all_b,streke,6:102)

Residential <- ongeweeg(BER.B,residential,streke,6:102)
Non_residential <- ongeweeg(BER.B,non_residential,streke,6:102)
Contractor_res <- ongeweeg(BER.B,contractor_res,streke,6:102)
Contractor_nonres <- ongeweeg(BER.B,contractor_nonres,streke,6:102)
Contractor <- ongeweeg(BER.B,contractor,streke,6:102)
Subcon_res <- ongeweeg(BER.B,subcon_res,streke,6:102)
Subcon_nonres <- ongeweeg(BER.B,subcon_nonres,streke,6:102)
Subcon <- ongeweeg(BER.B,subcon,streke,6:102)

WC.B <- ongeweeg(BER.B,all_b,1,6:102)
KZN.B <- ongeweeg(BER.B,all_b,5,6:102)
GP.B <- ongeweeg(BER.B,all_b,6,6:102)

#Interpolasie
Contractor[c(3,22,29,51),c(4,5,7,9,11,13)] <- pub_b[c(3,22,29,51),2:7]
Contractor_res[c(3,22,29,51),c(4,5,7,9,11,13)] <- pub_b[c(3,22,29,51),11:16]
Contractor_nonres[c(3,22,29,51),c(4,5,7,9,11,13)] <- pub_b[c(3,22,29,51),20:25]
Subcon[c(3,22,29,51),c(4,5,7,9,11,13)] <- pub_b[c(3,22,29,51),29:34]
Subcon_res[c(3,22,29,51),c(4,5,7,9,11,13)] <- pub_b[c(3,22,29,51),38:43]
Subcon_nonres[c(3,22,29,51),c(4,5,7,9,11,13)] <- pub_b[c(3,22,29,51),47:52]

#----------------------------
Architects <- ongeweeg(arc,arcs,streke,38:102)
Civils <- ongeweeg(civil,civils,streke,38:102)
QSs <- ongeweeg(qs,qss,streke,38:102)

#Interpolasie
Architects[c(19),c(3,4,6,8,10,12)] <- pub_a[c(19),2:7]
QSs[c(11,19),c(3,4,6,8,10,12)] <- pub_a[c(11,19),8:13]
Civils[c(19),c(3,4,6,8,10,12:15)] <- pub_a[c(19),14:22]

#==========================
#MANUFACTURING
Manufacturing <- geweeg(BER.M,all_m,streke,1:100)
Manufacturing_u <- ongeweeg(BER.M,all_m,streke,1:100)

Food <- geweeg(BER.M,food,streke,1:100)
Textiles <- geweeg(BER.M,textiles,streke,1:100)
Wood <- geweeg(BER.M,wood,streke,1:100)
Chemicals <- geweeg(BER.M,chemicals,streke,1:100)
Glass <- geweeg(BER.M,glass,streke,1:100)
Metals <- geweeg(BER.M,metals,streke,1:100)
Electrical <- geweeg(BER.M,electrical,streke,1:100)
Transport <- geweeg(BER.M,transport.m,streke,1:100)
Furniture <- geweeg(BER.M,furniture,streke,1:100)

Consumer <- geweeg(BER.M,consumer,streke,1:100)
Intermediate <- geweeg(BER.M,intermediate,streke,1:100)
Capital <- geweeg(BER.M,capital,streke,1:100)

WC.M <- geweeg(BER.M,all_m,1,1:100)
KZN.M <- geweeg(BER.M,all_m,5,1:100)
GP.M <- geweeg(BER.M,all_m,6,1:100)

#Interpolasie
Manufacturing[24,c(seq(4,32,by=2),34:49)] <- pub_m[24,2:32]
Manufacturing[33,c(seq(4,32,by=2),34:49)] <- pub_m[33,2:32]
Manufacturing[56,c(seq(4,32,by=2),34:49)] <- pub_m[56,2:32]
Manufacturing_u[24,c(seq(4,32,by=2),34:49)] <- pub_m[24,2:32]
Manufacturing_u[33,c(seq(4,32,by=2),34:49)] <- pub_m[33,2:32]
Manufacturing_u[56,c(seq(4,32,by=2),34:49)] <- pub_m[56,2:32]

#------------------
#New faktor
Manufacturing_n <- geweeg(BER.Mn,all_m,streke,1:100)

Food_n <- geweeg(BER.Mn,food,streke,1:100)
Textiles_n <- geweeg(BER.Mn,textiles,streke,1:100)
Wood_n <- geweeg(BER.Mn,wood,streke,1:100)
Chemicals_n <- geweeg(BER.Mn,chemicals,streke,1:100)
Glass_n <- geweeg(BER.Mn,glass,streke,1:100)
Metals_n <- geweeg(BER.Mn,metals,streke,1:100)
Electrical_n <- geweeg(BER.Mn,electrical,streke,1:100)
Transport_n <- geweeg(BER.Mn,transport.m,streke,1:100)
Furniture_n <- geweeg(BER.Mn,furniture,streke,1:100)

Consumer_n <- geweeg(BER.Mn,consumer,streke,1:100)
Intermediate_n <- geweeg(BER.Mn,intermediate,streke,1:100)
Capital_n <- geweeg(BER.Mn,capital,streke,1:100)

WC.M_n <- geweeg(BER.Mn,all_m,1,1:100)
KZN.M_n <- geweeg(BER.Mn,all_m,5,1:100)
GP.M_n <- geweeg(BER.Mn,all_m,6,1:100)

Manufacturing_n[24,c(seq(4,32,by=2),34:49)] <- pub_m[24,2:32]
Manufacturing_n[33,c(seq(4,32,by=2),34:49)] <- pub_m[33,2:32]
Manufacturing_n[56,c(seq(4,32,by=2),34:49)] <- pub_m[56,2:32]

#-------------------
#Two-step
Manufacturing_n2 <- maak.index(all_m)[1:100,]

BER.M2 <- BER.M2[,-6:-7]
Manufacturing_2 <- geweeg(BER.M2,all_m,streke,1:100)

Food_2 <- geweeg(BER.M2,food,streke,1:100)
Textiles_2 <- geweeg(BER.M2,textiles,streke,1:100)
Wood_2 <- geweeg(BER.M2,wood,streke,1:100)
Chemicals_2 <- geweeg(BER.M2,chemicals,streke,1:100)
Glass_2 <- geweeg(BER.M2,glass,streke,1:100)
Metals_2 <- geweeg(BER.M2,metals,streke,1:100)
Electrical_2 <- geweeg(BER.M2,electrical,streke,1:100)
Transport_2 <- geweeg(BER.M2,transport.m,streke,1:100)
Furniture_2 <- geweeg(BER.M2,furniture,streke,1:100)

Consumer_2 <- geweeg(BER.M2,consumer,streke,1:100)
Intermediate_2 <- geweeg(BER.M2,intermediate,streke,1:100)
Capital_2 <- geweeg(BER.M2,capital,streke,1:100)

WC.M_2 <- geweeg(BER.M2,all_m,1,1:100)
KZN.M_2 <- geweeg(BER.M2,all_m,5,1:100)
GP.M_2 <- geweeg(BER.M2,all_m,6,1:100)


Manufacturing_2[24,c(seq(4,32,by=2),34:49)] <- pub_m[24,2:32]
Manufacturing_2[33,c(seq(4,32,by=2),34:49)] <- pub_m[33,2:32]
Manufacturing_2[56,c(seq(4,32,by=2),34:49)] <- pub_m[56,2:32]
Manufacturing_n2[24,c(seq(3,31,by=2),33:48)] <- pub_m[24,2:32]
Manufacturing_n2[33,c(seq(3,31,by=2),33:48)] <- pub_m[33,2:32]
Manufacturing_n2[56,c(seq(3,31,by=2),33:48)] <- pub_m[56,2:32]

#==========================
#TRADE
#RETAIL
Retail <- geweeg(BER.R,all_r,streke,2:102)
Retail_u <- ongeweeg(BER.R,all_r,streke,2:102)

Retailsd <- geweeg(BER.R,retailsd,streke,2:102)
Retailnd <- geweeg(BER.R,retailnd,streke,2:102)
Retaild <- geweeg(BER.R,retaild,streke,2:102)
#retailo <- geweeg(BER.R,retailo,streke,2:102)

Hardware <- geweeg(BER.R,hardware,streke,2:102)
Other_duarbles <- geweeg(BER.R,other_duarbles,streke,2:102)

WC.R <- geweeg(BER.R,all_r,1,2:102)
KZN.R <- geweeg(BER.R,all_r,5,2:102)
GP.R <- geweeg(BER.R,all_r,6,2:102)

#Interpolasie
Retail[3,c(4,seq(5,17,by=2),18)] <- ad_r[3,2:10]
Retail[6,c(4,seq(5,17,by=2),18)] <- ad_r[6,2:10]
Retail[55,c(4,seq(5,17,by=2),18)] <- ad_r[55,2:10]
Retailsd[3,c(4,seq(5,17,by=2),18)] <- ad_r[55,2:10]
Retailsd[6,c(4,seq(5,17,by=2),18)] <- ad_r[6,11:19]
Retailsd[55,c(4,seq(5,17,by=2),18)] <- ad_r[55,11:19]
Retailnd[3,c(4,seq(5,17,by=2),18)] <- ad_r[3,20:28]
Retailnd[6,c(4,seq(5,17,by=2),18)] <- ad_r[6,20:28]
Retailnd[55,c(4,seq(5,17,by=2),18)] <- ad_r[55,20:28]
Retaild[3,c(4,seq(5,17,by=2),18)] <- ad_r[3,29:37]
Retaild[6,c(4,seq(5,17,by=2),18)] <- ad_r[6,29:37]
Retaild[55,c(4,seq(5,17,by=2),18)] <- ad_r[55,29:37]

WC.R[55,c(4,seq(5,17,by=2),18)] <- ad_r[55,38:46]
GP.R[55,c(4,seq(5,17,by=2),18)] <- ad_r[55,47:55]
KZN.R[55,c(4,seq(5,17,by=2),18)] <- ad_r[55,56:64]

#-------------------------
#WHOLESALE
Wholesale <- geweeg(BER.W,all_w,streke,2:102)
Wholesale_u <- ongeweeg(BER.W,all_w,streke,2:102)

Wholec <- geweeg(BER.W,wholec,streke,2:102)
Wholenc <- geweeg(BER.W,wholenc,streke,2:102)

WC.W <- geweeg(BER.W,all_w,1,2:102)
GP.W <- geweeg(BER.W,all_w,6,2:102)
KZN.W <- geweeg(BER.W,all_w,5,2:102)


#Interpolasie
Wholesale[3,c(4,seq(5,17,by=2),18)] <- ad_w[3,2:10]
Wholesale[6,c(4,seq(5,17,by=2),18)] <- ad_w[6,2:10]
Wholesale[55,c(4,seq(5,17,by=2),18)] <- ad_w[55,2:10]
Wholec[3,c(4,seq(5,17,by=2),18)] <- ad_w[3,11:19]
Wholec[6,c(4,seq(5,17,by=2),18)] <- ad_w[6,11:19]
Wholec[55,c(4,seq(5,17,by=2),18)] <- ad_w[55,11:19]
Wholenc[3,c(4,seq(5,17,by=2),18)] <- ad_w[3,20:28]
Wholenc[6,c(4,seq(5,17,by=2),18)] <- ad_w[6,20:28]
Wholenc[55,c(4,seq(5,17,by=2),18)] <- ad_w[55,20:28]

WC.W[55,c(4,seq(5,17,by=2),18)] <- ad_w[55,29:37]
GP.W[55,c(4,seq(5,17,by=2),18)] <- ad_w[55,38:46]
KZN.W[55,c(4,seq(5,17,by=2),18)] <- ad_w[55,47:55]


#-------------------------
#MOTOR
Motor <- geweeg(BER.V,all_v,streke,2:102)
Motor_u <- ongeweeg(BER.V,all_v,streke,2:102)

WC.V <- geweeg(BER.V,all_v,1,2:102)
GP.V <- geweeg(BER.V,all_v,6,2:102)
KZN.V <- geweeg(BER.V,all_v,5,2:102)


#Interpolasie
p.inter <- c(4,5,7,9,11,12,13,15,17,18,19,21,23,25)
Motor[3,p.inter] <- pub_v[3,2:15]
Motor[6,p.inter] <- pub_v[6,2:15]
Motor[55,p.inter] <- pub_v[55,2:15]

p.inter <- c(4,5,7,9,11)
WC.V[55,p.inter] <- pub_v[55,16:20]
GP.V[55,p.inter] <- pub_v[55,21:25]
KZN.V[55,p.inter] <- pub_v[55,26:30]


#===========================================
#SERVICES
Services_w <- geweeg(BER.S,all_s,streke,54:102)
Services <- ongeweeg(BER.S,all_s,streke,54:102)

ServicesC <- ongeweeg(BER.S,catering,streke,54:102)
ServicesT <- ongeweeg(BER.S,transport.s,streke,54:102)
ServicesR <- ongeweeg(BER.S,realestate,streke,54:102)
ServicesB <- ongeweeg(BER.S,business,streke,54:102)

WC.S <- ongeweeg(BER.S,all_s,1,54:102)
GP.S <- ongeweeg(BER.S,all_s,6,54:102)
KZN.S <- ongeweeg(BER.S,all_s,5,54:102)

#Interpolasie
p.inter <- c(4,5,7,9,11,13,15:18)
Services[3,p.inter] <- pub_s[3,2:11]
ServicesC[3,p.inter] <- pub_s[3,13:22]
ServicesT[3,p.inter] <- pub_s[3,24:33]
ServicesR[3,p.inter] <- pub_s[3,35:44]
ServicesB[3,p.inter] <- pub_s[3,46:55]

p.inter <- c(4,5,7,9,11,13,15:18)
WC.S[3,p.inter] <- pub_s[3,79:88]
GP.S[3,p.inter] <- pub_s[3,68:77]
KZN.S[3,p.inter] <- pub_s[3,90:99]


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


indicator_plot <- cbind(Manufacturing_2[,c("Datum","Q20")],Manufacturing_n2[,c("Q20")])
colnames(indicator_plot) <- c("Date","Two-Step","Explicit 2-step")    
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

#-------------------------

#=============================
#TRADE
#=============================
#RETAIL

indicator_plot <- cbind(Retail[,c("Datum","Q1")],ad_r[,"Total_Q1"],pub_r[,"Total_Q1"]) 
colnames(indicator_plot) <- c("Date","Weighted","Adjusted","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_line()
g <- g + ylab("Indicator") + xlab("")
g <- g + ggtitle("Retail")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) 
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g <- g + theme(legend.position="bottom")
g

indicator_plot <- cbind(Retail[,c("Datum","Q6A")],pub_r[,"Total_Q6A"]) 
colnames(indicator_plot) <- c("Date","Microdata","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_line()
g <- g + ylab("Indicator") + xlab("")
g <- g + ggtitle("Retail Categories")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) 
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g <- g + theme(legend.position="bottom")
g


indicator_plot <- cbind(Hardware[,c("Datum","Q1")],Other_duarbles[,"Q1"]) 
colnames(indicator_plot) <- c("Date","Hardware","Other_duarbles")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_line()
g <- g + ylab("Indicator") + xlab("")
g <- g + ggtitle("Retail")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) 
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g <- g + theme(legend.position="bottom")
g



#Total Retail
indicator_plot <- cbind(Retail[,c("Datum","Q1")],ad_r[,"Total_Q1"],pub_r[,"Total_Q1"]) 
colnames(indicator_plot) <- c("Date","Weighted","Adjusted","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g1 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g1 <- g1 + geom_line()
g1 <- g1 + ylab("Indicator") + xlab("")
g1 <- g1 + theme(legend.title=element_blank())
g1 <- g1 + ggtitle("Retail: Q1") 
g1 <- g1 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g1 <- g1 + theme(legend.title=element_blank()) 
g1 <- g1 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g1 <- g1 + theme(legend.position="none")

indicator_plot <- cbind(Retail[,c("Datum","Q2A")],ad_r[,"Total_Q2A"],pub_r[,"Total_Q2A"]) 
colnames(indicator_plot) <- c("Date","Weighted","Adjusted","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g2 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g2 <- g2 + geom_line()
g2 <- g2 + ylab("") + xlab("")
g2 <- g2 + theme(legend.title=element_blank())
g2 <- g2 + ggtitle("Retail: Q2A") 
g2 <- g2 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g2 <- g2 + theme(legend.title=element_blank()) 
g2 <- g2 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g2 <- g2 + theme(legend.position="none")

indicator_plot <- cbind(Retail[,c("Datum","Q3A")],ad_r[,"Total_Q3A"],pub_r[,"Total_Q3A"]) 
colnames(indicator_plot) <- c("Date","Weighted","Adjusted","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g3 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g3 <- g3 + geom_line()
g3 <- g3 + ylab("Indicator") + xlab("")
g3 <- g3 + theme(legend.title=element_blank())
g3 <- g3 + ggtitle("Retail: Q3A") 
g3 <- g3 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g3 <- g3 + theme(legend.title=element_blank()) 
g3 <- g3 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g3 <- g3 + theme(legend.position="bottom",plot.margin=unit(c(-0.5,0.4,0,0.4), "cm"))

indicator_plot <- cbind(Retail[,c("Datum","Q8")],ad_r[,"Total_Q8"],pub_r[,"Total_Q8"]) 
colnames(indicator_plot) <- c("Date","Weighted","Adjusted","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g4 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g4 <- g4 + geom_line()
g4 <- g4 + ylab("") + xlab("")
g4 <- g4 + theme(legend.title=element_blank())
g4 <- g4 + ggtitle("Retail: Q8") 
g4 <- g4 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g4 <- g4 + theme(legend.title=element_blank()) 
g4 <- g4 + theme(legend.position="bottom",plot.margin=unit(c(-0.5,0.4,0,0.4), "cm"))
g4 <- g4 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

library(gridExtra)
grid.arrange(g1, g2, g3, g4, ncol=2, nrow =2)



#Semi-Durables
indicator_plot <- cbind(Retailsd[,c("Datum","Q1")],ad_r[,"Semi_Q1"],pub_r[,"Semi_Q1"]) 
colnames(indicator_plot) <- c("Date","Weighted","Adjusted","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g1 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g1 <- g1 + geom_line()
g1 <- g1 + ylab("Indicator") + xlab("")
g1 <- g1 + theme(legend.title=element_blank())
g1 <- g1 + ggtitle("Retail Semi-Durable: Q1") 
g1 <- g1 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g1 <- g1 + theme(legend.title=element_blank()) 
g1 <- g1 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g1 <- g1 + theme(legend.position="none")

indicator_plot <- cbind(Retailsd[,c("Datum","Q2A")],ad_r[,"Semi_Q2A"],pub_r[,"Semi_Q2A"]) 
colnames(indicator_plot) <- c("Date","Weighted","Adjusted","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g2 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g2 <- g2 + geom_line()
g2 <- g2 + ylab("") + xlab("")
g2 <- g2 + theme(legend.title=element_blank())
g2 <- g2 + ggtitle("Retail Semi-Durable: Q2A") 
g2 <- g2 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g2 <- g2 + theme(legend.title=element_blank()) 
g2 <- g2 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g2 <- g2 + theme(legend.position="none")

indicator_plot <- cbind(Retailsd[,c("Datum","Q3A")],ad_r[,"Semi_Q3A"],pub_r[,"Semi_Q3A"]) 
colnames(indicator_plot) <- c("Date","Weighted","Adjusted","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g3 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g3 <- g3 + geom_line()
g3 <- g3 + ylab("Indicator") + xlab("")
g3 <- g3 + theme(legend.title=element_blank())
g3 <- g3 + ggtitle("Retail Semi-Durable: Q3A") 
g3 <- g3 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g3 <- g3 + theme(legend.title=element_blank()) 
g3 <- g3 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g3 <- g3 + theme(legend.position="bottom",plot.margin=unit(c(-0.5,0.4,0,0.4), "cm"))

indicator_plot <- cbind(Retailsd[,c("Datum","Q8")],ad_r[,"Semi_Q8"],pub_r[,"Semi_Q8"]) 
colnames(indicator_plot) <- c("Date","Weighted","Adjusted","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g4 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g4 <- g4 + geom_line()
g4 <- g4 + ylab("") + xlab("")
g4 <- g4 + theme(legend.title=element_blank())
g4 <- g4 + ggtitle("Retail Semi-Durable: Q8") 
g4 <- g4 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g4 <- g4 + theme(legend.title=element_blank()) 
g4 <- g4 + theme(legend.position="bottom",plot.margin=unit(c(-0.5,0.4,0,0.4), "cm"))
g4 <- g4 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

library(gridExtra)
grid.arrange(g1, g2, g3, g4, ncol=2, nrow =2)



#Non-Durables
indicator_plot <- cbind(Retailnd[,c("Datum","Q1")],ad_r[,"NonD_Q1"],pub_r[,"NonD_Q1"]) 
colnames(indicator_plot) <- c("Date","Weighted","Adjusted","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g1 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g1 <- g1 + geom_line()
g1 <- g1 + ylab("Indicator") + xlab("")
g1 <- g1 + theme(legend.title=element_blank())
g1 <- g1 + ggtitle("Retail: Q1") 
g1 <- g1 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g1 <- g1 + theme(legend.title=element_blank()) 
g1 <- g1 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g1 <- g1 + theme(legend.position="none")

indicator_plot <- cbind(Retailnd[,c("Datum","Q2A")],ad_r[,"NonD_Q2A"],pub_r[,"NonD_Q2A"]) 
colnames(indicator_plot) <- c("Date","Weighted","Adjusted","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g2 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g2 <- g2 + geom_line()
g2 <- g2 + ylab("") + xlab("")
g2 <- g2 + theme(legend.title=element_blank())
g2 <- g2 + ggtitle("Retail: Q2A") 
g2 <- g2 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g2 <- g2 + theme(legend.title=element_blank()) 
g2 <- g2 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g2 <- g2 + theme(legend.position="none")

indicator_plot <- cbind(Retailnd[,c("Datum","Q3A")],ad_r[,"NonD_Q3A"],pub_r[,"NonD_Q3A"]) 
colnames(indicator_plot) <- c("Date","Weighted","Adjusted","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g3 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g3 <- g3 + geom_line()
g3 <- g3 + ylab("Indicator") + xlab("")
g3 <- g3 + theme(legend.title=element_blank())
g3 <- g3 + ggtitle("Retail: Q3A") 
g3 <- g3 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g3 <- g3 + theme(legend.title=element_blank()) 
g3 <- g3 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g3 <- g3 + theme(legend.position="bottom",plot.margin=unit(c(-0.5,0.4,0,0.4), "cm"))

indicator_plot <- cbind(Retailnd[,c("Datum","Q8")],ad_r[,"NonD_Q8"],pub_r[,"NonD_Q8"]) 
colnames(indicator_plot) <- c("Date","Weighted","Adjusted","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g4 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g4 <- g4 + geom_line()
g4 <- g4 + ylab("") + xlab("")
g4 <- g4 + theme(legend.title=element_blank())
g4 <- g4 + ggtitle("Retail: Q8") 
g4 <- g4 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g4 <- g4 + theme(legend.title=element_blank()) 
g4 <- g4 + theme(legend.position="bottom",plot.margin=unit(c(-0.5,0.4,0,0.4), "cm"))
g4 <- g4 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

library(gridExtra)
grid.arrange(g1, g2, g3, g4, ncol=2, nrow =2)



#Durables
indicator_plot <- cbind(Retaild[,c("Datum","Q1")],ad_r[,"Durables_Q1"],pub_r[,"Durables_Q1"]) 
colnames(indicator_plot) <- c("Date","Weighted","Adjusted","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g1 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g1 <- g1 + geom_line()
g1 <- g1 + ylab("Indicator") + xlab("")
g1 <- g1 + theme(legend.title=element_blank())
g1 <- g1 + ggtitle("Retail: Q1") 
g1 <- g1 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g1 <- g1 + theme(legend.title=element_blank()) 
g1 <- g1 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g1 <- g1 + theme(legend.position="none")

indicator_plot <- cbind(Retaild[,c("Datum","Q2A")],ad_r[,"Durables_Q2A"],pub_r[,"Durables_Q2A"]) 
colnames(indicator_plot) <- c("Date","Weighted","Adjusted","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g2 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g2 <- g2 + geom_line()
g2 <- g2 + ylab("") + xlab("")
g2 <- g2 + theme(legend.title=element_blank())
g2 <- g2 + ggtitle("Retail: Q2A") 
g2 <- g2 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g2 <- g2 + theme(legend.title=element_blank()) 
g2 <- g2 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g2 <- g2 + theme(legend.position="none")

indicator_plot <- cbind(Retaild[,c("Datum","Q3A")],ad_r[,"Durables_Q3A"],pub_r[,"Durables_Q3A"]) 
colnames(indicator_plot) <- c("Date","Weighted","Adjusted","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g3 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g3 <- g3 + geom_line()
g3 <- g3 + ylab("Indicator") + xlab("")
g3 <- g3 + theme(legend.title=element_blank())
g3 <- g3 + ggtitle("Retail: Q3A") 
g3 <- g3 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g3 <- g3 + theme(legend.title=element_blank()) 
g3 <- g3 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g3 <- g3 + theme(legend.position="bottom",plot.margin=unit(c(-0.5,0.4,0,0.4), "cm"))

indicator_plot <- cbind(Retaild[,c("Datum","Q8")],ad_r[,"Durables_Q8"],pub_r[,"Durables_Q8"]) 
colnames(indicator_plot) <- c("Date","Weighted","Adjusted","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g4 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g4 <- g4 + geom_line()
g4 <- g4 + ylab("") + xlab("")
g4 <- g4 + theme(legend.title=element_blank())
g4 <- g4 + ggtitle("Retail: Q8") 
g4 <- g4 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g4 <- g4 + theme(legend.title=element_blank()) 
g4 <- g4 + theme(legend.position="bottom",plot.margin=unit(c(-0.5,0.4,0,0.4), "cm"))
g4 <- g4 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

library(gridExtra)
grid.arrange(g1, g2, g3, g4, ncol=2, nrow =2)


#Regions
BERplot <- aggregate(BER.R$id, by=list(BER.R$surveyQ,BER.R$region), FUN = length)
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


indicator_plot <- cbind(WC.R[,c("Datum","Q1")],ad_r[,"WC_Q1"],pub_r[,"WC_Q1"]) 
colnames(indicator_plot) <- c("Date","Microdata","Adjusted","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g1 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g1 <- g1 + geom_line()
g1 <- g1 + ylab("Indicator") + xlab("")
g1 <- g1 + theme(legend.title=element_blank())
g1 <- g1 + ggtitle("WC: Q1") 
g1 <- g1 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g1 <- g1 + theme(legend.title=element_blank()) 
g1 <- g1 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g1 <- g1 + theme(legend.position="none")

indicator_plot <- cbind(GP.R[,c("Datum","Q1")],ad_r[,"GP_Q1"],pub_r[,"GP_Q1"]) 
colnames(indicator_plot) <- c("Date","Microdata","Adjusted","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g2 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g2 <- g2 + geom_line()
g2 <- g2 + ylab("") + xlab("")
g2 <- g2 + theme(legend.title=element_blank())
g2 <- g2 + ggtitle("GP: Q1") 
g2 <- g2 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g2 <- g2 + theme(legend.title=element_blank()) 
g2 <- g2 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g2 <- g2 + theme(legend.position="none")

indicator_plot <- cbind(KZN.R[,c("Datum","Q1")],ad_r[,"KZN_Q1"],pub_r[,"KZN_Q1"]) 
colnames(indicator_plot) <- c("Date","Microdata","Adjusted","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g3 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g3 <- g3 + geom_line()
g3 <- g3 + ylab("Indicator") + xlab("")
g3 <- g3 + theme(legend.title=element_blank())
g3 <- g3 + ggtitle("KZN: Q1") 
g3 <- g3 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g3 <- g3 + theme(legend.title=element_blank()) 
g3 <- g3 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g3 <- g3 + theme(legend.position="bottom",plot.margin=unit(c(-0.5,0.4,0,0.4), "cm"))

indicator_plot <- cbind(WC.R[,c("Datum","Q1")],GP.R[,c("Q1")],KZN.R[,c("Q1")]) 
colnames(indicator_plot) <- c("Date","WC","GP","KZN")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g4 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g4 <- g4 + geom_line()
g4 <- g4 + ylab("") + xlab("")
g4 <- g4 + theme(legend.title=element_blank())
g4 <- g4 + ggtitle("Regions: Q1") 
g4 <- g4 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g4 <- g4 + theme(legend.title=element_blank()) 
g4 <- g4 + theme(legend.position="bottom",plot.margin=unit(c(-0.5,0.4,0,0.4), "cm"))
g4 <- g4 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

library(gridExtra)
grid.arrange(g1, g2, g3, g4, ncol=2, nrow =2)



indicator_plot <- cbind(WC.R[,c("Datum","Q2A")],ad_r[,"WC_Q2A"],pub_r[,"WC_Q2A"]) 
colnames(indicator_plot) <- c("Date","Microdata","Adjusted","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g1 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g1 <- g1 + geom_line()
g1 <- g1 + ylab("Indicator") + xlab("")
g1 <- g1 + theme(legend.title=element_blank())
g1 <- g1 + ggtitle("WC: Q2A") 
g1 <- g1 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g1 <- g1 + theme(legend.title=element_blank()) 
g1 <- g1 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g1 <- g1 + theme(legend.position="none")

indicator_plot <- cbind(GP.R[,c("Datum","Q2A")],ad_r[,"GP_Q2A"],pub_r[,"GP_Q2A"]) 
colnames(indicator_plot) <- c("Date","Microdata","Adjusted","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g2 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g2 <- g2 + geom_line()
g2 <- g2 + ylab("") + xlab("")
g2 <- g2 + theme(legend.title=element_blank())
g2 <- g2 + ggtitle("GP: Q2A") 
g2 <- g2 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g2 <- g2 + theme(legend.title=element_blank()) 
g2 <- g2 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g2 <- g2 + theme(legend.position="none")

indicator_plot <- cbind(KZN.R[,c("Datum","Q2A")],ad_r[,"KZN_Q2A"],pub_r[,"KZN_Q2A"]) 
colnames(indicator_plot) <- c("Date","Microdata","Adjusted","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g3 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g3 <- g3 + geom_line()
g3 <- g3 + ylab("Indicator") + xlab("")
g3 <- g3 + theme(legend.title=element_blank())
g3 <- g3 + ggtitle("KZN: Q2A") 
g3 <- g3 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g3 <- g3 + theme(legend.title=element_blank()) 
g3 <- g3 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g3 <- g3 + theme(legend.position="bottom",plot.margin=unit(c(-0.5,0.4,0,0.4), "cm"))

indicator_plot <- cbind(WC.R[,c("Datum","Q2A")],GP.R[,c("Q2A")],KZN.R[,c("Q2A")]) 
colnames(indicator_plot) <- c("Date","WC","GP","KZN")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g4 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g4 <- g4 + geom_line()
g4 <- g4 + ylab("") + xlab("")
g4 <- g4 + theme(legend.title=element_blank())
g4 <- g4 + ggtitle("Regions: Q2A") 
g4 <- g4 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g4 <- g4 + theme(legend.title=element_blank()) 
g4 <- g4 + theme(legend.position="bottom",plot.margin=unit(c(-0.5,0.4,0,0.4), "cm"))
g4 <- g4 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

library(gridExtra)
grid.arrange(g1, g2, g3, g4, ncol=2, nrow =2)

#--------------------------------
#Reference Series

indicator_plot <- cbind(Retail[,c("Datum","Q2A","Q3A")],ref_r[,c("Total_SARB","Total_StatsSA")]) 
colnames(indicator_plot)[1] <- c("Date")#,"Microdata","Published")
indicator_plot[,-1] <- scale(indicator_plot[,-1])
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_line()
g <- g + ylab("Indicator") + xlab("")
g <- g + ggtitle("Retail Categories")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) 
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g <- g + theme(legend.position="bottom")
g


retail_trade <- cbind(Retail[,c("Datum","Q2A","Q3A")],ref_r[,c("Total_SARB","Total_StatsSA")]) 
source("corstarsl.R")
xt <- xtable(corstarsl(retail_trade[,-1])[3:4,1:2])
print(xt, "latex",comment=FALSE,scalebox = 0.8)

Q2A <- retail_trade[,2]
Q3A <- retail_trade[,3] 
Sales_SARB <- retail_trade[,4]
Sales_StatsSA <- retail_trade[,5]

par(mfrow=c(2,2))
ccf(Q2A, Sales_SARB, na.action = na.pass, ylim=c(-0.2, 0.8))
ccf(Q2A, Sales_StatsSA, na.action = na.pass, ylim=c(-0.2, 0.8))
ccf(Q3A, Sales_SARB, na.action = na.pass, ylim=c(-0.2, 0.8))
ccf(Q3A, Sales_StatsSA, na.action = na.pass, ylim=c(-0.2, 0.8))





indicator_plot <- cbind(Retaild[,c("Datum","Q2A","Q3A")],ref_r[,c("Durable")])
indicator_plot[,-1] <- scale(indicator_plot[,-1])
colnames(indicator_plot) <- c("Date","Q2A","Q3A","StatsSA")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g1 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g1 <- g1 + geom_line()
g1 <- g1 + ylab("Indicator") + xlab("")
g1 <- g1 + theme(legend.title=element_blank())
g1 <- g1 + ggtitle("Durable Goods") 
g1 <- g1 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g1 <- g1 + theme(legend.title=element_blank()) 
g1 <- g1 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g1 <- g1 + theme(legend.position="none")

indicator_plot <- cbind(Retailsd[,c("Datum","Q2A","Q3A")],ref_r[,c("Semi")])
indicator_plot[,-1] <- scale(indicator_plot[,-1])
colnames(indicator_plot) <- c("Date","Q2A","Q3A","StatsSA")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g2 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g2 <- g2 + geom_line()
g2 <- g2 + ylab("") + xlab("")
g2 <- g2 + theme(legend.title=element_blank())
g2 <- g2 + ggtitle("Semi-Durable Goods") 
g2 <- g2 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g2 <- g2 + theme(legend.title=element_blank()) 
g2 <- g2 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g2 <- g2 + theme(legend.position="none")


indicator_plot <- cbind(Retailnd[,c("Datum","Q2A","Q3A")],ref_r[,c("NonD")])
indicator_plot[,-1] <- scale(indicator_plot[,-1])
colnames(indicator_plot) <- c("Date","Q2A","Q3A","StatsSA")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g3 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g3 <- g3 + geom_line()
g3 <- g3 + ylab("Indicator") + xlab("")
g3 <- g3 + theme(legend.title=element_blank())
g3 <- g3 + ggtitle("Non-Durable Goods") 
g3 <- g3 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g3 <- g3 + theme(legend.title=element_blank()) 
g3 <- g3 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g3 <- g3 + theme(legend.position="bottom",plot.margin=unit(c(-0.5,0.4,0,0.4), "cm"))

library(gridExtra)
grid.arrange(g1, g2, g3, ncol=2, nrow =2)


source("corstarsl.R")

retail_trade <- cbind(Retaild[,c("Datum","Q2A","Q3A")],ref_r[,c("Durable")])
xt1 <- corstarsl(retail_trade[,-1])[3,1:2]
retail_trade <- cbind(Retailsd[,c("Datum","Q2A","Q3A")],ref_r[,c("Semi")])
xt2 <- corstarsl(retail_trade[,-1])[3,1:2]
retail_trade <- cbind(Retailnd[,c("Datum","Q2A","Q3A")],ref_r[,c("NonD")])
xt3 <- corstarsl(retail_trade[,-1])[3,1:2]
xt <- rbind(xt1,xt2,xt3)
row.names(xt) <- c("Durable Goods","Semi-Durable Goods","Non-Durable Goods")
xt <- xtable(xt)
print(xt, "latex",comment=FALSE,scalebox = 0.8)


colnames(retail_trade) <- c("Date","Q2A","Q3A","StatsSA")


Q2A <- retail_trade[,2]
Q3A <- retail_trade[,3] 
#Sales_SARB <- retail_trade[,4]
Sales_StatsSA <- retail_trade[,4]

par(mfrow=c(1,2))
#ccf(Q2A, Sales_SARB, na.action = na.pass, ylim=c(-0.2, 0.8))
ccf(Q2A, Sales_StatsSA, na.action = na.pass, ylim=c(-0.2, 0.8))
#ccf(Q3A, Sales_SARB, na.action = na.pass, ylim=c(-0.2, 0.8))
ccf(Q3A, Sales_StatsSA, na.action = na.pass, ylim=c(-0.2, 0.8))


#===============================================
#WHOLESALE

indicator_plot <- cbind(Wholesale[,c("Datum","Q1")],ad_w[,"Total_Q1"],pub_w[,"Total_Q1"]) 
colnames(indicator_plot) <- c("Date","Weighted","Adjusted","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_line()
g <- g + ylab("Indicator") + xlab("")
g <- g + ggtitle("Wholesale")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) 
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g <- g + theme(legend.position="bottom")
g

indicator_plot <- cbind(Wholesale[,c("Datum","Q6A")],ad_w[,"Total_Q6A"],pub_w[,"Total_Q6A"]) 
colnames(indicator_plot) <- c("Date","Weighted","Adjusted","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_line()
g <- g + ylab("Indicator") + xlab("")
g <- g + ggtitle("Wholesale")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) 
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g <- g + theme(legend.position="bottom")
g



#Total Wholesale
indicator_plot <- cbind(Wholesale[,c("Datum","Q1")],ad_w[,"Total_Q1"]),pub_w[,"Total_Q1"]) 
colnames(indicator_plot) <- c("Date","Weighted","Adjusted","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g1 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g1 <- g1 + geom_line()
g1 <- g1 + ylab("Indicator") + xlab("")
g1 <- g1 + theme(legend.title=element_blank())
g1 <- g1 + ggtitle("Wholesale: Q1") 
g1 <- g1 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g1 <- g1 + theme(legend.title=element_blank()) 
g1 <- g1 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g1 <- g1 + theme(legend.position="none")

indicator_plot <- cbind(Wholesale[,c("Datum","Q2A")],ad_w[,"Total_Q2A"],pub_w[,"Total_Q2A"]) 
colnames(indicator_plot) <- c("Date","Weighted","Adjusted","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g2 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g2 <- g2 + geom_line()
g2 <- g2 + ylab("") + xlab("")
g2 <- g2 + theme(legend.title=element_blank())
g2 <- g2 + ggtitle("Wholesale: Q2A") 
g2 <- g2 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g2 <- g2 + theme(legend.title=element_blank()) 
g2 <- g2 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g2 <- g2 + theme(legend.position="none")

indicator_plot <- cbind(Wholesale[,c("Datum","Q3A")],ad_w[,"Total_Q3A"],pub_w[,"Total_Q3A"]) 
colnames(indicator_plot) <- c("Date","Weighted","Adjusted","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g3 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g3 <- g3 + geom_line()
g3 <- g3 + ylab("Indicator") + xlab("")
g3 <- g3 + theme(legend.title=element_blank())
g3 <- g3 + ggtitle("Wholesale: Q3A") 
g3 <- g3 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g3 <- g3 + theme(legend.title=element_blank()) 
g3 <- g3 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g3 <- g3 + theme(legend.position="bottom",plot.margin=unit(c(-0.5,0.4,0,0.4), "cm"))

indicator_plot <- cbind(Wholesale[,c("Datum","Q8")],ad_w[,"Total_Q8"],pub_w[,"Total_Q8"]) 
colnames(indicator_plot) <- c("Date","Weighted","Adjusted","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g4 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g4 <- g4 + geom_line()
g4 <- g4 + ylab("") + xlab("")
g4 <- g4 + theme(legend.title=element_blank())
g4 <- g4 + ggtitle("Wholesale: Q8") 
g4 <- g4 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g4 <- g4 + theme(legend.title=element_blank()) 
g4 <- g4 + theme(legend.position="bottom",plot.margin=unit(c(-0.5,0.4,0,0.4), "cm"))
g4 <- g4 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

library(gridExtra)
grid.arrange(g1, g2, g3, g4, ncol=2, nrow =2)



#Wholesale Consumer Goods
indicator_plot <- cbind(Wholec[,c("Datum","Q1")],ad_w[,"CG_Q1"],pub_w[,"CG_Q1"]) 
colnames(indicator_plot) <- c("Date","Weighted","Adjusted","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g1 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g1 <- g1 + geom_line()
g1 <- g1 + ylab("Indicator") + xlab("")
g1 <- g1 + theme(legend.title=element_blank())
g1 <- g1 + ggtitle("Wholesale Consumer Goods: Q1") 
g1 <- g1 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g1 <- g1 + theme(legend.title=element_blank()) 
g1 <- g1 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g1 <- g1 + theme(legend.position="none")

indicator_plot <- cbind(Wholec[,c("Datum","Q2A")],ad_w[,"CG_Q2A"],pub_w[,"CG_Q2A"]) 
colnames(indicator_plot) <- c("Date","Weighted","Adjusted","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g2 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g2 <- g2 + geom_line()
g2 <- g2 + ylab("") + xlab("")
g2 <- g2 + theme(legend.title=element_blank())
g2 <- g2 + ggtitle("Wholesale Consumer Goods: Q2A") 
g2 <- g2 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g2 <- g2 + theme(legend.title=element_blank()) 
g2 <- g2 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g2 <- g2 + theme(legend.position="none")

indicator_plot <- cbind(Wholec[,c("Datum","Q3A")],ad_w[,"CG_Q3A"],pub_w[,"CG_Q3A"]) 
colnames(indicator_plot) <- c("Date","Weighted","Adjusted","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g3 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g3 <- g3 + geom_line()
g3 <- g3 + ylab("Indicator") + xlab("")
g3 <- g3 + theme(legend.title=element_blank())
g3 <- g3 + ggtitle("Wholesale Consumer Goods: Q3A") 
g3 <- g3 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g3 <- g3 + theme(legend.title=element_blank()) 
g3 <- g3 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g3 <- g3 + theme(legend.position="bottom",plot.margin=unit(c(-0.5,0.4,0,0.4), "cm"))

indicator_plot <- cbind(Wholec[,c("Datum","Q8")],ad_w[,"CG_Q8"],pub_w[,"CG_Q8"]) 
colnames(indicator_plot) <- c("Date","Weighted","Adjusted","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g4 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g4 <- g4 + geom_line()
g4 <- g4 + ylab("") + xlab("")
g4 <- g4 + theme(legend.title=element_blank())
g4 <- g4 + ggtitle("Wholesale Consumer Goods: Q8") 
g4 <- g4 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g4 <- g4 + theme(legend.title=element_blank()) 
g4 <- g4 + theme(legend.position="bottom",plot.margin=unit(c(-0.5,0.4,0,0.4), "cm"))
g4 <- g4 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

library(gridExtra)
grid.arrange(g1, g2, g3, g4, ncol=2, nrow =2)



#Wholesale Non-Consumer Goods
indicator_plot <- cbind(Wholenc[,c("Datum","Q1")],ad_w[,"NonCG_Q1"],pub_w[,"NonCG_Q1"]) 
colnames(indicator_plot) <- c("Date","Weighted","Adjusted","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g1 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g1 <- g1 + geom_line()
g1 <- g1 + ylab("Indicator") + xlab("")
g1 <- g1 + theme(legend.title=element_blank())
g1 <- g1 + ggtitle("Non-Consumer Goods: Q1") 
g1 <- g1 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g1 <- g1 + theme(legend.title=element_blank()) 
g1 <- g1 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g1 <- g1 + theme(legend.position="none")

indicator_plot <- cbind(Wholenc[,c("Datum","Q2A")],ad_w[,"NonCG_Q2A"],pub_w[,"NonCG_Q2A"]) 
colnames(indicator_plot) <- c("Date","Weighted","Adjusted","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g2 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g2 <- g2 + geom_line()
g2 <- g2 + ylab("") + xlab("")
g2 <- g2 + theme(legend.title=element_blank())
g2 <- g2 + ggtitle("Non-Consumer Goods: Q2A") 
g2 <- g2 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g2 <- g2 + theme(legend.title=element_blank()) 
g2 <- g2 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g2 <- g2 + theme(legend.position="none")

indicator_plot <- cbind(Wholenc[,c("Datum","Q3A")],ad_w[,"NonCG_Q3A"],pub_w[,"NonCG_Q3A"]) 
colnames(indicator_plot) <- c("Date","Weighted","Adjusted","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g3 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g3 <- g3 + geom_line()
g3 <- g3 + ylab("Indicator") + xlab("")
g3 <- g3 + theme(legend.title=element_blank())
g3 <- g3 + ggtitle("Non-Consumer Goods: Q3A") 
g3 <- g3 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g3 <- g3 + theme(legend.title=element_blank()) 
g3 <- g3 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g3 <- g3 + theme(legend.position="bottom",plot.margin=unit(c(-0.5,0.4,0,0.4), "cm"))

indicator_plot <- cbind(Wholenc[,c("Datum","Q8")],ad_w[,"NonCG_Q8"],pub_w[,"NonCG_Q8"]) 
colnames(indicator_plot) <- c("Date","Weighted","Adjusted","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g4 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g4 <- g4 + geom_line()
g4 <- g4 + ylab("") + xlab("")
g4 <- g4 + theme(legend.title=element_blank())
g4 <- g4 + ggtitle("Non-Consumer Goods: Q8") 
g4 <- g4 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g4 <- g4 + theme(legend.title=element_blank()) 
g4 <- g4 + theme(legend.position="bottom",plot.margin=unit(c(-0.5,0.4,0,0.4), "cm"))
g4 <- g4 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

library(gridExtra)
grid.arrange(g1, g2, g3, g4, ncol=2, nrow =2)



#Regions
BERplot <- aggregate(BER.W$id, by=list(BER.W$surveyQ,BER.W$region), FUN = length)
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


indicator_plot <- cbind(WC.W[,c("Datum","Q1")],ad_w[,"WC_Q1"],pub_w[,"WC_Q1"]) 
colnames(indicator_plot) <- c("Date","Microdata","Adjusted","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g1 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g1 <- g1 + geom_line()
g1 <- g1 + ylab("Indicator") + xlab("")
g1 <- g1 + theme(legend.title=element_blank())
g1 <- g1 + ggtitle("WC: Q1") 
g1 <- g1 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g1 <- g1 + theme(legend.title=element_blank()) 
g1 <- g1 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g1 <- g1 + theme(legend.position="none")

indicator_plot <- cbind(GP.W[,c("Datum","Q1")],ad_w[,"GP_Q1"],pub_w[,"GP_Q1"]) 
colnames(indicator_plot) <- c("Date","Microdata","Adjusted","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g2 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g2 <- g2 + geom_line()
g2 <- g2 + ylab("") + xlab("")
g2 <- g2 + theme(legend.title=element_blank())
g2 <- g2 + ggtitle("GP: Q1") 
g2 <- g2 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g2 <- g2 + theme(legend.title=element_blank()) 
g2 <- g2 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g2 <- g2 + theme(legend.position="none")

indicator_plot <- cbind(KZN.W[,c("Datum","Q1")],ad_w[,"KZN_Q1"],pub_w[,"KZN_Q1"]) 
colnames(indicator_plot) <- c("Date","Microdata","Adjusted","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g3 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g3 <- g3 + geom_line()
g3 <- g3 + ylab("Indicator") + xlab("")
g3 <- g3 + theme(legend.title=element_blank())
g3 <- g3 + ggtitle("KZN: Q1") 
g3 <- g3 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g3 <- g3 + theme(legend.title=element_blank()) 
g3 <- g3 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g3 <- g3 + theme(legend.position="bottom",plot.margin=unit(c(-0.5,0.4,0,0.4), "cm"))

indicator_plot <- cbind(WC.W[,c("Datum","Q1")],GP.W[,c("Q1")],KZN.W[,c("Q1")]) 
colnames(indicator_plot) <- c("Date","WC","GP","KZN")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g4 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g4 <- g4 + geom_line()
g4 <- g4 + ylab("") + xlab("")
g4 <- g4 + theme(legend.title=element_blank())
g4 <- g4 + ggtitle("Regions: Q1") 
g4 <- g4 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g4 <- g4 + theme(legend.title=element_blank()) 
g4 <- g4 + theme(legend.position="bottom",plot.margin=unit(c(-0.5,0.4,0,0.4), "cm"))
g4 <- g4 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

library(gridExtra)
grid.arrange(g1, g2, g3, g4, ncol=2, nrow =2)



indicator_plot <- cbind(WC.W[,c("Datum","Q2A")],ad_w[,"WC_Q2A"],pub_w[,"WC_Q2A"]) 
colnames(indicator_plot) <- c("Date","Microdata","Adjusted","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g1 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g1 <- g1 + geom_line()
g1 <- g1 + ylab("Indicator") + xlab("")
g1 <- g1 + theme(legend.title=element_blank())
g1 <- g1 + ggtitle("WC: Q2A") 
g1 <- g1 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g1 <- g1 + theme(legend.title=element_blank()) 
g1 <- g1 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g1 <- g1 + theme(legend.position="none")

indicator_plot <- cbind(GP.W[,c("Datum","Q2A")],ad_w[,"GP_Q2A"],pub_w[,"GP_Q2A"]) 
colnames(indicator_plot) <- c("Date","Microdata","Adjusted","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g2 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g2 <- g2 + geom_line()
g2 <- g2 + ylab("") + xlab("")
g2 <- g2 + theme(legend.title=element_blank())
g2 <- g2 + ggtitle("GP: Q2A") 
g2 <- g2 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g2 <- g2 + theme(legend.title=element_blank()) 
g2 <- g2 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g2 <- g2 + theme(legend.position="none")

indicator_plot <- cbind(KZN.W[,c("Datum","Q2A")],ad_w[,"KZN_Q2A"],pub_w[,"KZN_Q2A"]) 
colnames(indicator_plot) <- c("Date","Microdata","Adjusted","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g3 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g3 <- g3 + geom_line()
g3 <- g3 + ylab("Indicator") + xlab("")
g3 <- g3 + theme(legend.title=element_blank())
g3 <- g3 + ggtitle("KZN: Q2A") 
g3 <- g3 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g3 <- g3 + theme(legend.title=element_blank()) 
g3 <- g3 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g3 <- g3 + theme(legend.position="bottom",plot.margin=unit(c(-0.5,0.4,0,0.4), "cm"))

indicator_plot <- cbind(WC.W[,c("Datum","Q2A")],GP.W[,c("Q2A")],KZN.W[,c("Q2A")]) 
colnames(indicator_plot) <- c("Date","WC","GP","KZN")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g4 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g4 <- g4 + geom_line()
g4 <- g4 + ylab("") + xlab("")
g4 <- g4 + theme(legend.title=element_blank())
g4 <- g4 + ggtitle("Regions: Q2A") 
g4 <- g4 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g4 <- g4 + theme(legend.title=element_blank()) 
g4 <- g4 + theme(legend.position="bottom",plot.margin=unit(c(-0.5,0.4,0,0.4), "cm"))
g4 <- g4 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

library(gridExtra)
grid.arrange(g1, g2, g3, g4, ncol=2, nrow =2)


#===============================================
#MOTOR VEHICLES

#New Vehicles
indicator_plot <- cbind(Motor[,c("Datum","Q1")],pub_v[,"New_Q1"]) 
colnames(indicator_plot) <- c("Date","Weighted","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_line()
g <- g + ylab("Indicator") + xlab("")
g <- g + ggtitle("New Vehicles Q1")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) 
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g <- g + theme(legend.position="bottom")
g

indicator_plot <- cbind(Motor[,c("Datum","Q2A")],pub_v[,"New_Q2A"]) 
colnames(indicator_plot) <- c("Date","Weighted","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_line()
g <- g + ylab("Indicator") + xlab("")
g <- g + ggtitle("New Vehicles")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) 
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g <- g + theme(legend.position="bottom")
g



#New Vehicles
indicator_plot <- cbind(Motor[,c("Datum","Q1")],pub_v[,"New_Q1"]) 
colnames(indicator_plot) <- c("Date","Weighted","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g1 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g1 <- g1 + geom_line()
g1 <- g1 + ylab("Indicator") + xlab("")
g1 <- g1 + theme(legend.title=element_blank())
g1 <- g1 + ggtitle("New Vehicles: Q1") 
g1 <- g1 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g1 <- g1 + theme(legend.title=element_blank()) 
g1 <- g1 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g1 <- g1 + theme(legend.position="none")

indicator_plot <- cbind(Motor[,c("Datum","Q2A")],pub_v[,"New_Q2A"]) 
colnames(indicator_plot) <- c("Date","Weighted","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g2 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g2 <- g2 + geom_line()
g2 <- g2 + ylab("") + xlab("")
g2 <- g2 + theme(legend.title=element_blank())
g2 <- g2 + ggtitle("New Vehicles: Q2A") 
g2 <- g2 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g2 <- g2 + theme(legend.title=element_blank()) 
g2 <- g2 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g2 <- g2 + theme(legend.position="none")

indicator_plot <- cbind(Motor[,c("Datum","Q3A")],pub_v[,"New_Q3A"]) 
colnames(indicator_plot) <- c("Date","Weighted","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g3 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g3 <- g3 + geom_line()
g3 <- g3 + ylab("Indicator") + xlab("")
g3 <- g3 + theme(legend.title=element_blank())
g3 <- g3 + ggtitle("New Vehicles: Q3A") 
g3 <- g3 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g3 <- g3 + theme(legend.title=element_blank()) 
g3 <- g3 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g3 <- g3 + theme(legend.position="bottom",plot.margin=unit(c(-0.5,0.4,0,0.4), "cm"))

indicator_plot <- cbind(Motor[,c("Datum","Q5")],pub_v[,"New_Q5"]) 
colnames(indicator_plot) <- c("Date","Weighted","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g4 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g4 <- g4 + geom_line()
g4 <- g4 + ylab("") + xlab("")
g4 <- g4 + theme(legend.title=element_blank())
g4 <- g4 + ggtitle("New Vehicles: Q5") 
g4 <- g4 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g4 <- g4 + theme(legend.title=element_blank()) 
g4 <- g4 + theme(legend.position="bottom",plot.margin=unit(c(-0.5,0.4,0,0.4), "cm"))
g4 <- g4 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

library(gridExtra)
grid.arrange(g1, g2, g3, g4, ncol=2, nrow =2)



#Used Vehicles
indicator_plot <- cbind(Motor[,c("Datum","Q6")],pub_v[,"Used_Q1"]) 
colnames(indicator_plot) <- c("Date","Weighted","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g1 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g1 <- g1 + geom_line()
g1 <- g1 + ylab("Indicator") + xlab("")
g1 <- g1 + theme(legend.title=element_blank())
g1 <- g1 + ggtitle("Used Vehicles: Q1") 
g1 <- g1 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g1 <- g1 + theme(legend.title=element_blank()) 
g1 <- g1 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g1 <- g1 + theme(legend.position="none")

indicator_plot <- cbind(Motor[,c("Datum","Q7A")],pub_v[,"Used_Q2A"]) 
colnames(indicator_plot) <- c("Date","Weighted","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g2 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g2 <- g2 + geom_line()
g2 <- g2 + ylab("") + xlab("")
g2 <- g2 + theme(legend.title=element_blank())
g2 <- g2 + ggtitle("Used Vehicles: Q2A") 
g2 <- g2 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g2 <- g2 + theme(legend.title=element_blank()) 
g2 <- g2 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g2 <- g2 + theme(legend.position="none")

indicator_plot <- cbind(Motor[,c("Datum","Q8A")],pub_v[,"Used_Q3A"]) 
colnames(indicator_plot) <- c("Date","Weighted","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g3 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g3 <- g3 + geom_line()
g3 <- g3 + ylab("Indicator") + xlab("")
g3 <- g3 + theme(legend.title=element_blank())
g3 <- g3 + ggtitle("Used Vehicles: Q3A") 
g3 <- g3 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g3 <- g3 + theme(legend.title=element_blank()) 
g3 <- g3 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g3 <- g3 + theme(legend.position="bottom",plot.margin=unit(c(-0.5,0.4,0,0.4), "cm"))

indicator_plot <- cbind(Motor[,c("Datum","Q9")],pub_v[,"Used_Q5"]) 
colnames(indicator_plot) <- c("Date","Weighted","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g4 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g4 <- g4 + geom_line()
g4 <- g4 + ylab("") + xlab("")
g4 <- g4 + theme(legend.title=element_blank())
g4 <- g4 + ggtitle("Used Vehicles: Q5") 
g4 <- g4 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g4 <- g4 + theme(legend.title=element_blank()) 
g4 <- g4 + theme(legend.position="bottom",plot.margin=unit(c(-0.5,0.4,0,0.4), "cm"))
g4 <- g4 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

library(gridExtra)
grid.arrange(g1, g2, g3, g4, ncol=2, nrow =2)


#Spare Parts
indicator_plot <- cbind(Motor[,c("Datum","Q10")],pub_v[,"Spare_Q1"]) 
colnames(indicator_plot) <- c("Date","Weighted","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g1 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g1 <- g1 + geom_line()
g1 <- g1 + ylab("Indicator") + xlab("")
g1 <- g1 + theme(legend.title=element_blank())
g1 <- g1 + ggtitle("Spare Parts: Q1") 
g1 <- g1 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g1 <- g1 + theme(legend.title=element_blank()) 
g1 <- g1 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g1 <- g1 + theme(legend.position="none")

indicator_plot <- cbind(Motor[,c("Datum","Q11A")],pub_v[,"Spare_Q2A"]) 
colnames(indicator_plot) <- c("Date","Weighted","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g2 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g2 <- g2 + geom_line()
g2 <- g2 + ylab("") + xlab("")
g2 <- g2 + theme(legend.title=element_blank())
g2 <- g2 + ggtitle("Spare Parts: Q2A") 
g2 <- g2 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g2 <- g2 + theme(legend.title=element_blank()) 
g2 <- g2 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g2 <- g2 + theme(legend.position="none")

indicator_plot <- cbind(Motor[,c("Datum","Q12A")],pub_v[,"Spare_Q3A"]) 
colnames(indicator_plot) <- c("Date","Weighted","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g3 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g3 <- g3 + geom_line()
g3 <- g3 + ylab("Indicator") + xlab("")
g3 <- g3 + theme(legend.title=element_blank())
g3 <- g3 + ggtitle("Spare Parts: Q3A") 
g3 <- g3 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g3 <- g3 + theme(legend.title=element_blank()) 
g3 <- g3 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g3 <- g3 + theme(legend.position="bottom",plot.margin=unit(c(-0.5,0.4,0,0.4), "cm"))

indicator_plot <- cbind(Motor[,c("Datum","Q14")],pub_v[,"Spare_Q5"]) 
colnames(indicator_plot) <- c("Date","Weighted","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g4 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g4 <- g4 + geom_line()
g4 <- g4 + ylab("") + xlab("")
g4 <- g4 + theme(legend.title=element_blank())
g4 <- g4 + ggtitle("Spare Parts: Q5") 
g4 <- g4 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g4 <- g4 + theme(legend.title=element_blank()) 
g4 <- g4 + theme(legend.position="bottom",plot.margin=unit(c(-0.5,0.4,0,0.4), "cm"))
g4 <- g4 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

library(gridExtra)
grid.arrange(g1, g2, g3, g4, ncol=2, nrow =2)


#Regions
BERplot <- aggregate(BER.V$id, by=list(BER.V$surveyQ,BER.V$region), FUN = length)
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


indicator_plot <- cbind(WC.V[,c("Datum","Q1")],pub_v[,"WC_Q1"]) 
colnames(indicator_plot) <- c("Date","Microdata","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g1 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g1 <- g1 + geom_line()
g1 <- g1 + ylab("Indicator") + xlab("")
g1 <- g1 + theme(legend.title=element_blank())
g1 <- g1 + ggtitle("WC: Q1") 
g1 <- g1 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g1 <- g1 + theme(legend.title=element_blank()) 
g1 <- g1 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g1 <- g1 + theme(legend.position="none")

indicator_plot <- cbind(GP.V[,c("Datum","Q1")],pub_v[,"GP_Q1"]) 
colnames(indicator_plot) <- c("Date","Microdata","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g2 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g2 <- g2 + geom_line()
g2 <- g2 + ylab("") + xlab("")
g2 <- g2 + theme(legend.title=element_blank())
g2 <- g2 + ggtitle("GP: Q1") 
g2 <- g2 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g2 <- g2 + theme(legend.title=element_blank()) 
g2 <- g2 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g2 <- g2 + theme(legend.position="none")

indicator_plot <- cbind(KZN.V[,c("Datum","Q1")],pub_v[,"KZN_Q1"]) 
colnames(indicator_plot) <- c("Date","Microdata","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g3 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g3 <- g3 + geom_line()
g3 <- g3 + ylab("Indicator") + xlab("")
g3 <- g3 + theme(legend.title=element_blank())
g3 <- g3 + ggtitle("KZN: Q1") 
g3 <- g3 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g3 <- g3 + theme(legend.title=element_blank()) 
g3 <- g3 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g3 <- g3 + theme(legend.position="bottom",plot.margin=unit(c(-0.5,0.4,0,0.4), "cm"))

indicator_plot <- cbind(WC.V[,c("Datum","Q1")],GP.V[,c("Q1")],KZN.V[,c("Q1")]) 
colnames(indicator_plot) <- c("Date","WC","GP","KZN")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g4 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g4 <- g4 + geom_line()
g4 <- g4 + ylab("") + xlab("")
g4 <- g4 + theme(legend.title=element_blank())
g4 <- g4 + ggtitle("Regions: Q1") 
g4 <- g4 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g4 <- g4 + theme(legend.title=element_blank()) 
g4 <- g4 + theme(legend.position="bottom",plot.margin=unit(c(-0.5,0.4,0,0.4), "cm"))
g4 <- g4 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

library(gridExtra)
grid.arrange(g1, g2, g3, g4, ncol=2, nrow =2)



indicator_plot <- cbind(WC.V[,c("Datum","Q2A")],pub_v[,"WC_Q2A"]) 
colnames(indicator_plot) <- c("Date","Microdata","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g1 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g1 <- g1 + geom_line()
g1 <- g1 + ylab("Indicator") + xlab("")
g1 <- g1 + theme(legend.title=element_blank())
g1 <- g1 + ggtitle("WC: Q2A") 
g1 <- g1 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g1 <- g1 + theme(legend.title=element_blank()) 
g1 <- g1 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g1 <- g1 + theme(legend.position="none")

indicator_plot <- cbind(GP.V[,c("Datum","Q2A")],pub_v[,"GP_Q2A"]) 
colnames(indicator_plot) <- c("Date","Microdata","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g2 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g2 <- g2 + geom_line()
g2 <- g2 + ylab("") + xlab("")
g2 <- g2 + theme(legend.title=element_blank())
g2 <- g2 + ggtitle("GP: Q2A") 
g2 <- g2 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g2 <- g2 + theme(legend.title=element_blank()) 
g2 <- g2 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g2 <- g2 + theme(legend.position="none")

indicator_plot <- cbind(KZN.V[,c("Datum","Q2A")],pub_v[,"KZN_Q2A"]) 
colnames(indicator_plot) <- c("Date","Microdata","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g3 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g3 <- g3 + geom_line()
g3 <- g3 + ylab("Indicator") + xlab("")
g3 <- g3 + theme(legend.title=element_blank())
g3 <- g3 + ggtitle("KZN: Q2A") 
g3 <- g3 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g3 <- g3 + theme(legend.title=element_blank()) 
g3 <- g3 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g3 <- g3 + theme(legend.position="bottom",plot.margin=unit(c(-0.5,0.4,0,0.4), "cm"))

indicator_plot <- cbind(WC.V[,c("Datum","Q2A")],GP.V[,c("Q2A")],KZN.V[,c("Q2A")]) 
colnames(indicator_plot) <- c("Date","WC","GP","KZN")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g4 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g4 <- g4 + geom_line()
g4 <- g4 + ylab("") + xlab("")
g4 <- g4 + theme(legend.title=element_blank())
g4 <- g4 + ggtitle("Regions: Q2A") 
g4 <- g4 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g4 <- g4 + theme(legend.title=element_blank()) 
g4 <- g4 + theme(legend.position="bottom",plot.margin=unit(c(-0.5,0.4,0,0.4), "cm"))
g4 <- g4 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

library(gridExtra)
grid.arrange(g1, g2, g3, g4, ncol=2, nrow =2)




#===============================================
#SERVICES
#===============================================

#Services
indicator_plot <- cbind(Services[,c("Datum","Q1")],pub_s[,"Total_Q1"]) 
colnames(indicator_plot) <- c("Date","Microdata","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_line()
g <- g + ylab("Indicator") + xlab("")
g <- g + ggtitle("Services Q1")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) 
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g <- g + theme(legend.position="bottom")
g

indicator_plot <- cbind(Services[,c("Datum","Q2A")],pub_s[,"Total_Q2A"]) 
colnames(indicator_plot) <- c("Date","Microdata","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g <- g + geom_line()
g <- g + ylab("Indicator") + xlab("")
g <- g + ggtitle("Services Q2A")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) 
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g <- g + theme(legend.position="bottom")
g



#Total Services
indicator_plot <- cbind(Services[,c("Datum","Q1")],pub_s[,"Total_Q1"]) 
colnames(indicator_plot) <- c("Date","Microdata","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g1 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g1 <- g1 + geom_line()
g1 <- g1 + ylab("Indicator") + xlab("")
g1 <- g1 + theme(legend.title=element_blank())
g1 <- g1 + ggtitle("Services: Q1") 
g1 <- g1 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g1 <- g1 + theme(legend.title=element_blank()) 
g1 <- g1 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g1 <- g1 + theme(legend.position="none")

indicator_plot <- cbind(Services[,c("Datum","Q2A")],pub_s[,"Total_Q2A"]) 
colnames(indicator_plot) <- c("Date","Microdata","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g2 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g2 <- g2 + geom_line()
g2 <- g2 + ylab("") + xlab("")
g2 <- g2 + theme(legend.title=element_blank())
g2 <- g2 + ggtitle("Services: Q2A") 
g2 <- g2 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g2 <- g2 + theme(legend.title=element_blank()) 
g2 <- g2 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g2 <- g2 + theme(legend.position="none")

indicator_plot <- cbind(Services[,c("Datum","Q3A")],pub_s[,"Total_Q3A"]) 
colnames(indicator_plot) <- c("Date","Microdata","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g3 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g3 <- g3 + geom_line()
g3 <- g3 + ylab("Indicator") + xlab("")
g3 <- g3 + theme(legend.title=element_blank())
g3 <- g3 + ggtitle("Services: Q3A") 
g3 <- g3 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g3 <- g3 + theme(legend.title=element_blank()) 
g3 <- g3 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g3 <- g3 + theme(legend.position="bottom",plot.margin=unit(c(-0.5,0.4,0,0.4), "cm"))

indicator_plot <- cbind(Services[,c("Datum","Q8")],pub_s[,"Total_Q8"]) 
colnames(indicator_plot) <- c("Date","Microdata","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g4 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g4 <- g4 + geom_line()
g4 <- g4 + ylab("") + xlab("")
g4 <- g4 + theme(legend.title=element_blank())
g4 <- g4 + ggtitle("Services: Q8") 
g4 <- g4 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g4 <- g4 + theme(legend.title=element_blank()) 
g4 <- g4 + theme(legend.position="bottom",plot.margin=unit(c(-0.5,0.4,0,0.4), "cm"))
g4 <- g4 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

library(gridExtra)
grid.arrange(g1, g2, g3, g4, ncol=2, nrow =2)



#Confidence
indicator_plot <- cbind(ServicesC[,c("Datum","Q1")],pub_s[,"Accom_Q1"]) 
colnames(indicator_plot) <- c("Date","Microdata","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g1 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g1 <- g1 + geom_line()
g1 <- g1 + ylab("Indicator") + xlab("")
g1 <- g1 + theme(legend.title=element_blank())
g1 <- g1 + ggtitle("Catering: Q1") 
g1 <- g1 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g1 <- g1 + theme(legend.title=element_blank()) 
g1 <- g1 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g1 <- g1 + theme(legend.position="none")

indicator_plot <- cbind(ServicesR[,c("Datum","Q1")],pub_s[,"RE_Q1"]) 
colnames(indicator_plot) <- c("Date","Microdata","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g2 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g2 <- g2 + geom_line()
g2 <- g2 + ylab("") + xlab("")
g2 <- g2 + theme(legend.title=element_blank())
g2 <- g2 + ggtitle("Real Estate: Q1") 
g2 <- g2 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g2 <- g2 + theme(legend.title=element_blank()) 
g2 <- g2 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g2 <- g2 + theme(legend.position="none")

indicator_plot <- cbind(ServicesT[,c("Datum","Q1")],pub_s[,"Trans_Q1"]) 
colnames(indicator_plot) <- c("Date","Microdata","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g3 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g3 <- g3 + geom_line()
g3 <- g3 + ylab("Indicator") + xlab("")
g3 <- g3 + theme(legend.title=element_blank())
g3 <- g3 + ggtitle("Transport: Q1") 
g3 <- g3 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g3 <- g3 + theme(legend.title=element_blank()) 
g3 <- g3 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g3 <- g3 + theme(legend.position="bottom",plot.margin=unit(c(-0.5,0.4,0,0.4), "cm"))

indicator_plot <- cbind(ServicesB[,c("Datum","Q1")],pub_s[,"BS_Q1"]) 
colnames(indicator_plot) <- c("Date","Microdata","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g4 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g4 <- g4 + geom_line()
g4 <- g4 + ylab("") + xlab("")
g4 <- g4 + theme(legend.title=element_blank())
g4 <- g4 + ggtitle("Business: Q1") 
g4 <- g4 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g4 <- g4 + theme(legend.title=element_blank()) 
g4 <- g4 + theme(legend.position="bottom",plot.margin=unit(c(-0.5,0.4,0,0.4), "cm"))
g4 <- g4 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

library(gridExtra)
grid.arrange(g1, g2, g3, g4, ncol=2, nrow =2)


#Sales Volumes
indicator_plot <- cbind(ServicesC[,c("Datum","Q2A")],pub_s[,"Accom_Q2A"]) 
colnames(indicator_plot) <- c("Date","Microdata","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g1 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g1 <- g1 + geom_line()
g1 <- g1 + ylab("Indicator") + xlab("")
g1 <- g1 + theme(legend.title=element_blank())
g1 <- g1 + ggtitle("Catering: Q2A") 
g1 <- g1 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g1 <- g1 + theme(legend.title=element_blank()) 
g1 <- g1 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g1 <- g1 + theme(legend.position="none")

indicator_plot <- cbind(ServicesR[,c("Datum","Q2A")],pub_s[,"RE_Q2A"]) 
colnames(indicator_plot) <- c("Date","Microdata","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g2 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g2 <- g2 + geom_line()
g2 <- g2 + ylab("") + xlab("")
g2 <- g2 + theme(legend.title=element_blank())
g2 <- g2 + ggtitle("Real Estate: Q2A") 
g2 <- g2 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g2 <- g2 + theme(legend.title=element_blank()) 
g2 <- g2 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g2 <- g2 + theme(legend.position="none")

indicator_plot <- cbind(ServicesT[,c("Datum","Q2A")],pub_s[,"Trans_Q2A"]) 
colnames(indicator_plot) <- c("Date","Microdata","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g3 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g3 <- g3 + geom_line()
g3 <- g3 + ylab("Indicator") + xlab("")
g3 <- g3 + theme(legend.title=element_blank())
g3 <- g3 + ggtitle("Transport: Q2A") 
g3 <- g3 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g3 <- g3 + theme(legend.title=element_blank()) 
g3 <- g3 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g3 <- g3 + theme(legend.position="bottom",plot.margin=unit(c(-0.5,0.4,0,0.4), "cm"))

indicator_plot <- cbind(ServicesB[,c("Datum","Q2A")],pub_s[,"BS_Q2A"]) 
colnames(indicator_plot) <- c("Date","Microdata","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g4 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g4 <- g4 + geom_line()
g4 <- g4 + ylab("") + xlab("")
g4 <- g4 + theme(legend.title=element_blank())
g4 <- g4 + ggtitle("Business: Q2A") 
g4 <- g4 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g4 <- g4 + theme(legend.title=element_blank()) 
g4 <- g4 + theme(legend.position="bottom",plot.margin=unit(c(-0.5,0.4,0,0.4), "cm"))
g4 <- g4 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

library(gridExtra)
grid.arrange(g1, g2, g3, g4, ncol=2, nrow =2)


#Regions
BERplot <- aggregate(BER.S$id, by=list(BER.S$surveyQ,BER.S$region), FUN = length)
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


indicator_plot <- cbind(WC.S[,c("Datum","Q1")],pub_s[,"WC_Q1"]) 
colnames(indicator_plot) <- c("Date","Microdata","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g1 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g1 <- g1 + geom_line()
g1 <- g1 + ylab("Indicator") + xlab("")
g1 <- g1 + theme(legend.title=element_blank())
g1 <- g1 + ggtitle("WC: Q1") 
g1 <- g1 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g1 <- g1 + theme(legend.title=element_blank()) 
g1 <- g1 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g1 <- g1 + theme(legend.position="none")

indicator_plot <- cbind(GP.S[,c("Datum","Q1")],pub_s[,"GP_Q1"]) 
colnames(indicator_plot) <- c("Date","Microdata","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g2 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g2 <- g2 + geom_line()
g2 <- g2 + ylab("") + xlab("")
g2 <- g2 + theme(legend.title=element_blank())
g2 <- g2 + ggtitle("GP: Q1") 
g2 <- g2 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g2 <- g2 + theme(legend.title=element_blank()) 
g2 <- g2 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g2 <- g2 + theme(legend.position="none")

indicator_plot <- cbind(KZN.S[,c("Datum","Q1")],pub_s[,"KZN_Q1"]) 
colnames(indicator_plot) <- c("Date","Microdata","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g3 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g3 <- g3 + geom_line()
g3 <- g3 + ylab("Indicator") + xlab("")
g3 <- g3 + theme(legend.title=element_blank())
g3 <- g3 + ggtitle("KZN: Q1") 
g3 <- g3 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g3 <- g3 + theme(legend.title=element_blank()) 
g3 <- g3 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g3 <- g3 + theme(legend.position="bottom",plot.margin=unit(c(-0.5,0.4,0,0.4), "cm"))

indicator_plot <- cbind(WC.S[,c("Datum","Q1")],GP.S[,c("Q1")],KZN.S[,c("Q1")]) 
colnames(indicator_plot) <- c("Date","WC","GP","KZN")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g4 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g4 <- g4 + geom_line()
g4 <- g4 + ylab("") + xlab("")
g4 <- g4 + theme(legend.title=element_blank())
g4 <- g4 + ggtitle("Regions: Q1") 
g4 <- g4 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g4 <- g4 + theme(legend.title=element_blank()) 
g4 <- g4 + theme(legend.position="bottom",plot.margin=unit(c(-0.5,0.4,0,0.4), "cm"))
g4 <- g4 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

library(gridExtra)
grid.arrange(g1, g2, g3, g4, ncol=2, nrow =2)



indicator_plot <- cbind(WC.S[,c("Datum","Q2A")],pub_s[,"WC_Q2A"]) 
colnames(indicator_plot) <- c("Date","Microdata","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g1 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g1 <- g1 + geom_line()
g1 <- g1 + ylab("Indicator") + xlab("")
g1 <- g1 + theme(legend.title=element_blank())
g1 <- g1 + ggtitle("WC: Q2A") 
g1 <- g1 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g1 <- g1 + theme(legend.title=element_blank()) 
g1 <- g1 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g1 <- g1 + theme(legend.position="none")

indicator_plot <- cbind(GP.S[,c("Datum","Q2A")],pub_s[,"GP_Q2A"]) 
colnames(indicator_plot) <- c("Date","Microdata","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g2 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g2 <- g2 + geom_line()
g2 <- g2 + ylab("") + xlab("")
g2 <- g2 + theme(legend.title=element_blank())
g2 <- g2 + ggtitle("GP: Q2A") 
g2 <- g2 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g2 <- g2 + theme(legend.title=element_blank()) 
g2 <- g2 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g2 <- g2 + theme(legend.position="none")

indicator_plot <- cbind(KZN.S[,c("Datum","Q2A")],pub_s[,"KZN_Q2A"]) 
colnames(indicator_plot) <- c("Date","Microdata","Published")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g3 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g3 <- g3 + geom_line()
g3 <- g3 + ylab("Indicator") + xlab("")
g3 <- g3 + theme(legend.title=element_blank())
g3 <- g3 + ggtitle("KZN: Q2A") 
g3 <- g3 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g3 <- g3 + theme(legend.title=element_blank()) 
g3 <- g3 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))
g3 <- g3 + theme(legend.position="bottom",plot.margin=unit(c(-0.5,0.4,0,0.4), "cm"))

indicator_plot <- cbind(WC.S[,c("Datum","Q2A")],GP.S[,c("Q2A")],KZN.S[,c("Q2A")]) 
colnames(indicator_plot) <- c("Date","WC","GP","KZN")
indicator_plot <- melt(indicator_plot, id="Date")  # convert to long format
g4 <- ggplot(data=indicator_plot,aes(x=Date, y=value, group=variable, colour=variable)) 
g4 <- g4 + geom_line()
g4 <- g4 + ylab("") + xlab("")
g4 <- g4 + theme(legend.title=element_blank())
g4 <- g4 + ggtitle("Regions: Q2A") 
g4 <- g4 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g4 <- g4 + theme(legend.title=element_blank()) 
g4 <- g4 + theme(legend.position="bottom",plot.margin=unit(c(-0.5,0.4,0,0.4), "cm"))
g4 <- g4 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"))

library(gridExtra)
grid.arrange(g1, g2, g3, g4, ncol=2, nrow =2)



