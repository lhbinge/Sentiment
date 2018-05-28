##==================================================================##
## ----------------------- BER SURVEYS -----------------------------##
##==================================================================##
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
suppressMessages(library(zoo))

##==================##
## READING THE DATA ##
##==================##
datums <- read.csv("dates.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)
datums$Datum <- as.Date(datums$Datum, format = "%Y/%m/%d")

pub_b <- read.csv("Building_BER_Published.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)
ref_b <- read.csv("Ref Series_Build.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)
pub_a <- read.csv("Arc_Published.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)

pub_m <- read.csv("Manufacturing_BER_Published.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)
ref_m <- read.csv("Ref Series_Manufac.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)
ref_me <- read.csv("Ref Series_Manufac_exp.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)

pub_r <- read.csv("Retail_BER_Published.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)
ref_r <- read.csv("Ref Series_Retail.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)
ad_r <- read.csv("Retail_Adjustment.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)
ad_r[,-1] <- ad_r[,-1]+pub_r[,-1]

pub_w <- read.csv("Wholesale_BER_Published.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)
#ref_w <- read.csv("Ref Series_Wholesale.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)
ad_w <- read.csv("Wholesale_Adjustment.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)
ad_w[,-1] <- ad_w[,-1]+pub_w[,-1]

pub_v <- read.csv("Motor_BER_Published.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)
ref_v <- read.csv("Ref Series_Vehicles.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)

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

#-----------------------
#Exports: New faktor variable 
BER.Mne <- read.csv("Manufacturing_new faktor_exp.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)[,c(-6,-7)]
colnames(BER.Mne)[1:7] <- c("region","id","sector","weight","turnover","factor","surveyQ")
BER.Mne$surveyQ <- toupper(BER.Mne$surveyQ)
BER.Mne$sector <- factor(BER.Mne$sector) #could include labels
BER.Mne$id <- factor(BER.Mne$id)

#Clean SurveyQ variable
BER.Mne$temp <- NULL
for(i in 1:nrow(BER.Mne)) {
    ifelse(substr(BER.Mne$surveyQ[i], 1, 1)==9, 
           BER.Mne$temp[i] <- paste0("19",BER.Mne$surveyQ[i],sep=""),
           BER.Mne$temp[i] <- paste0("20",BER.Mne$surveyQ[i],sep=""))
}
BER.Mne$surveyQ <- BER.Mne$temp
BER.Mne <- BER.Mne[,-ncol(BER.Mne)]
BER.Mne$surveyQ <- factor(BER.Mne$surveyQ)

#Exlcude Latecomers and old questions
BER.Mne <- BER.Mne[BER.Mne$Latecomer == FALSE | is.na(BER.Mne$Latecomer),]
BER.Mne <- BER.Mne[,!grepl("X",colnames(BER.Mne))]    
BER.Mne <- BER.Mne[,1:64]

# replace 1,2,3 (Up, Same, Down) responses with 1,0,-1
for(i in 8:64) {
    BER.Mne[,i] <- replace(BER.Mne[,i], BER.Mne[,i]==2, 0)
    BER.Mne[,i] <- replace(BER.Mne[,i], BER.Mne[,i]==3,-1)
}
for(i in 43:49) {
    BER.Mne[,i] <- replace(BER.Mne[,i], BER.Mne[,i]==0, 0.5)
    BER.Mne[,i] <- replace(BER.Mne[,i], BER.Mne[,i]==-1, 0)
}

#-----------------------
#Exports: New 2-step weighting 
BER.M2e <- read.csv("Manufacturing_new faktor_exp.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)
colnames(BER.M2e)[1:9] <- c("region","id","sector","weight","turnover","firmw","sectorw","factor","surveyQ")
BER.M2e$surveyQ <- toupper(BER.M2e$surveyQ)
BER.M2e$sector <- factor(BER.M2e$sector) #could include labels
BER.M2e$id <- factor(BER.M2e$id)
BER.M2e$temp <- NULL
for(i in 1:nrow(BER.M2e)) {
    ifelse(substr(BER.M2e$surveyQ[i], 1, 1)==9, 
           BER.M2e$temp[i] <- paste0("19",BER.M2e$surveyQ[i],sep=""),
           BER.M2e$temp[i] <- paste0("20",BER.M2e$surveyQ[i],sep=""))
}
BER.M2e$surveyQ <- BER.M2e$temp
BER.M2e <- BER.M2e[,-ncol(BER.M2e)]
BER.M2e$surveyQ <- factor(BER.M2e$surveyQ)
BER.M2e$factor <- as.numeric(as.character(BER.M2e$factor))
BER.M2e <- BER.M2e[BER.M2e$Latecomer == FALSE | is.na(BER.M2e$Latecomer),]
BER.M2e <- BER.M2e[,!grepl("X",colnames(BER.M2e))]    
BER.M2e <- BER.M2e[,1:66]

#Maak factor reg
Ej <- aggregate(BER.M2e$firmw, by=list(BER.M2e$surveyQ,BER.M2e$sector), FUN=sum)
BER.M2e <-  merge(BER.M2e, Ej, by.x=c("surveyQ","sector"), by.y=c("Group.1","Group.2"),all = TRUE)
BER.M2e$factor <- BER.M2e$factor/BER.M2e$x
BER.M2e <- BER.M2e[,c(3,4,2,5:9,1,10:66)]

for(i in 10:66) {
    BER.M2e[,i] <- replace(BER.M2e[,i], BER.M2e[,i]==2, 0)
    BER.M2e[,i] <- replace(BER.M2e[,i], BER.M2e[,i]==3,-1)
}
for(i in 45:51) {
    BER.M2e[,i] <- replace(BER.M2e[,i], BER.M2e[,i]==0, 0.5)
    BER.M2e[,i] <- replace(BER.M2e[,i], BER.M2e[,i]==-1, 0)
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
#Exclude new recruits for 2010Q4
#BER.R$New2010Q4 <- FALSE
#BER.R$New2010Q4 <- replace(BER.R$New2010Q4, (BER.R$surveyQ=="2010Q4" & BER.R$NewRecruit==TRUE), TRUE)
#BER.R <- BER.R[BER.R$New2010Q4 == FALSE | is.na(BER.R$New2010Q4),]

BER.R <- BER.R[,1:21]
BER.R <- BER.R[,!grepl("X",colnames(BER.R))]    

for(i in 7:21) {
    BER.R[,i] <- replace(BER.R[,i], BER.R[,i]==2, 0)
    BER.R[,i] <- replace(BER.R[,i], BER.R[,i]==3,-1)
}

#---------------------
#New faktor variable 
BER.Rn <- read.csv("Retail_new faktor.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)
colnames(BER.Rn)[1:6] <- c("region","id","sector","weight","factor","surveyQ")
BER.Rn$surveyQ <- toupper(BER.Rn$surveyQ)
BER.Rn$sector <- factor(BER.Rn$sector) #could include labels
BER.Rn$id <- factor(BER.Rn$id)

#Clean SurveyQ variable
BER.Rn$temp <- NULL
for(i in 1:nrow(BER.Rn)) {
    ifelse(substr(BER.Rn$surveyQ[i], 1, 1)==9, 
           BER.Rn$temp[i] <- paste0("19",BER.Rn$surveyQ[i],sep=""),
           BER.Rn$temp[i] <- paste0("20",BER.Rn$surveyQ[i],sep=""))
}
BER.Rn$surveyQ <- BER.Rn$temp
BER.Rn <- BER.Rn[,-ncol(BER.Rn)]
BER.Rn$surveyQ <- factor(BER.Rn$surveyQ)
BER.Rn$factor <- as.numeric(as.character(BER.Rn$factor))    

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
for(i in levels(BER.Rn$id)) {
    if(!is.na(Mode(BER.Rn$region[(BER.Rn$surveyQ %in% korrek) & (BER.Rn$id == i)]))) {
        BER.Rn$region[(BER.Rn$surveyQ %in% foute) & (BER.Rn$id == i)] <- Mode(BER.Rn$region[(BER.Rn$surveyQ %in% korrek) & (BER.Rn$id == i)]) 
    }
}
BER.Rn$region <- factor(BER.Rn$region)

BER.Rn <- BER.Rn[BER.Rn$Latecomer == FALSE | is.na(BER.Rn$Latecomer),]
#Exclude new recruits for 2010Q4
#BER.Rn$New2010Q4 <- FALSE
#BER.Rn$New2010Q4 <- replace(BER.Rn$New2010Q4, (BER.Rn$surveyQ=="2010Q4" & BER.Rn$NewRecruit==TRUE), TRUE)
#BER.Rn <- BER.Rn[BER.Rn$New2010Q4 == FALSE | is.na(BER.Rn$New2010Q4),]

BER.Rn <- BER.Rn[,1:21]
BER.Rn <- BER.Rn[,!grepl("X",colnames(BER.Rn))]    

for(i in 7:21) {
    BER.Rn[,i] <- replace(BER.Rn[,i], BER.Rn[,i]==2, 0)
    BER.Rn[,i] <- replace(BER.Rn[,i], BER.Rn[,i]==3,-1)
}

#-----------------------
#New 2-step weighting 
BER.R2 <- read.csv("Retail_new faktor_sep.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)
colnames(BER.R2)[1:8] <- c("region","id","sector","weight","firmw","sectorw","factor","surveyQ")
BER.R2$surveyQ <- toupper(BER.R2$surveyQ)
BER.R2$sector <- factor(BER.R2$sector) #could include labels
BER.R2$id <- factor(BER.R2$id)

BER.R2$firmw <- as.numeric(as.character(BER.R2$firmw))
BER.R2$sectorw <- as.numeric(as.character(BER.R2$sectorw))
BER.R2$factor <- as.numeric(as.character(BER.R2$factor))    

#Clean SurveyQ variable
BER.R2$temp <- NULL
for(i in 1:nrow(BER.R2)) {
    ifelse(substr(BER.R2$surveyQ[i], 1, 1)==9, 
           BER.R2$temp[i] <- paste0("19",BER.R2$surveyQ[i],sep=""),
           BER.R2$temp[i] <- paste0("20",BER.R2$surveyQ[i],sep=""))
}
BER.R2$surveyQ <- BER.R2$temp
BER.R2 <- BER.R2[,-ncol(BER.R2)]
BER.R2$surveyQ <- factor(BER.R2$surveyQ)

#Clean regions
foute <- c("1997Q2")#,"2000Q3","2000Q4","2001Q1")
korrek <- c("1996Q1","1996Q2","1996Q3","1996Q4","1997Q1",
            "1997Q3","1997Q4","1998Q1","1998Q2","1998Q3",
            "1998Q4")#,"2004Q1","2004Q2","2004Q3","2004Q4",
#"2005Q1","2005Q2","2005Q3","2006Q1","2006Q2","2006Q3","2006Q4")
for(i in levels(BER.R2$id)) {
    if(!is.na(Mode(BER.R2$region[(BER.R2$surveyQ %in% korrek) & (BER.R2$id == i)]))) {
        BER.R2$region[(BER.R2$surveyQ %in% foute) & (BER.R2$id == i)] <- Mode(BER.R2$region[(BER.R2$surveyQ %in% korrek) & (BER.R2$id == i)]) 
    }
}
BER.R2$region <- factor(BER.R2$region)

BER.R2 <- BER.R2[BER.R2$Latecomer == FALSE | is.na(BER.R2$Latecomer),]
BER.R2 <- BER.R2[,1:23]
BER.R2 <- BER.R2[,!grepl("X",colnames(BER.R2))]    

for(i in 9:23) {
    BER.R2[,i] <- replace(BER.R2[,i], BER.R2[,i]==2, 0)
    BER.R2[,i] <- replace(BER.R2[,i], BER.R2[,i]==3,-1)
}

#Maak factor reg
Ej <- aggregate(BER.R2$firmw, by=list(BER.R2$surveyQ,BER.R2$sector), FUN=sum)
BER.R2 <-  merge(BER.R2, Ej, by.x=c("surveyQ","sector"), by.y=c("Group.1","Group.2"),all = TRUE)
BER.R2$factor <- BER.R2$factor/BER.R2$x*100
BER.R2 <- BER.R2[,c(3,4,2,5:8,1,9:23)]


#=================
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

#---------------------
#New faktor variable 
BER.Wn <- read.csv("Wholesale_new faktor.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)
colnames(BER.Wn)[1:6] <- c("region","id","sector","weight","factor","surveyQ")
BER.Wn$surveyQ <- toupper(BER.Wn$surveyQ)
BER.Wn$sector <- factor(BER.Wn$sector) #could include labels
BER.Wn$id <- factor(BER.Wn$id)

#Clean SurveyQ variable
BER.Wn$temp <- NULL
for(i in 1:nrow(BER.Wn)) {
    ifelse(substr(BER.Wn$surveyQ[i], 1, 1)==9, 
           BER.Wn$temp[i] <- paste0("19",BER.Wn$surveyQ[i],sep=""),
           BER.Wn$temp[i] <- paste0("20",BER.Wn$surveyQ[i],sep=""))
}
BER.Wn$surveyQ <- BER.Wn$temp
BER.Wn <- BER.Wn[,-ncol(BER.Wn)]
BER.Wn$surveyQ <- factor(BER.Wn$surveyQ)
BER.Wn$region <- factor(BER.Wn$region)

BER.Wn <- BER.Wn[BER.Wn$Latecomer == FALSE | is.na(BER.Wn$Latecomer),]
BER.Wn <- BER.Wn[,1:21]
BER.Wn <- BER.Wn[,!grepl("X",colnames(BER.Wn))]    

for(i in 7:21) {
    BER.Wn[,i] <- replace(BER.Wn[,i], BER.Wn[,i]==2, 0)
    BER.Wn[,i] <- replace(BER.Wn[,i], BER.Wn[,i]==3,-1)
}

#--------------------
#New 2-step weighting 
BER.W2 <- read.csv("Wholesale_new faktor_sep.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)
colnames(BER.W2)[1:8] <- c("region","id","sector","weight","firmw","sectorw","factor","surveyQ")
BER.W2$surveyQ <- toupper(BER.W2$surveyQ)
BER.W2$sector <- factor(BER.W2$sector) #could include labels
BER.W2$id <- factor(BER.W2$id)

BER.W2$firmw <- as.numeric(as.character(BER.W2$firmw))
BER.W2$sectorw <- as.numeric(as.character(BER.W2$sectorw))
BER.W2$factor <- as.numeric(as.character(BER.W2$factor))    

#Clean SurveyQ variable
BER.W2$temp <- NULL
for(i in 1:nrow(BER.W2)) {
    ifelse(substr(BER.W2$surveyQ[i], 1, 1)==9, 
           BER.W2$temp[i] <- paste0("19",BER.W2$surveyQ[i],sep=""),
           BER.W2$temp[i] <- paste0("20",BER.W2$surveyQ[i],sep=""))
}
BER.W2$surveyQ <- BER.W2$temp
BER.W2 <- BER.W2[,-ncol(BER.W2)]
BER.W2$surveyQ <- factor(BER.W2$surveyQ)
BER.W2$region <- factor(BER.W2$region)

BER.W2 <- BER.W2[BER.W2$Latecomer == FALSE | is.na(BER.W2$Latecomer),]
BER.W2 <- BER.W2[,1:23]
BER.W2 <- BER.W2[,!grepl("X",colnames(BER.W2))]    

for(i in 9:23) {
    BER.W2[,i] <- replace(BER.W2[,i], BER.W2[,i]==2, 0)
    BER.W2[,i] <- replace(BER.W2[,i], BER.W2[,i]==3,-1)
}

#Maak factor reg
Ej <- aggregate(BER.W2$firmw, by=list(BER.W2$surveyQ,BER.W2$sector), FUN=sum)
BER.W2 <-  merge(BER.W2, Ej, by.x=c("surveyQ","sector"), by.y=c("Group.1","Group.2"),all = TRUE)
BER.W2$factor <- BER.W2$factor/BER.W2$x*100
BER.W2 <- BER.W2[,c(3,4,2,5:8,1,9:23)]


#===========================
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

#----------
#New faktor
BER.Vn <- read.csv("Motor_new faktor.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)
colnames(BER.Vn)[1:6] <- c("region","id","sector","weight","factor","surveyQ")
BER.Vn$surveyQ <- toupper(BER.Vn$surveyQ)
BER.Vn$sector <- factor(BER.Vn$sector) #could include labels
BER.Vn$id <- factor(BER.Vn$id)
BER.Vn$region <- factor(BER.Vn$region)

BER.Vn$temp <- NULL
for(i in 1:nrow(BER.Vn)) {
    ifelse(substr(BER.Vn$surveyQ[i], 1, 1)==9, 
           BER.Vn$temp[i] <- paste0("19",BER.Vn$surveyQ[i],sep=""),
           BER.Vn$temp[i] <- paste0("20",BER.Vn$surveyQ[i],sep=""))
}
BER.Vn$surveyQ <- BER.Vn$temp
BER.Vn <- BER.Vn[,-ncol(BER.Vn)]
BER.Vn$surveyQ <- factor(BER.Vn$surveyQ)

BER.Vn <- BER.Vn[BER.Vn$Latecomer == FALSE | is.na(BER.Vn$Latecomer),]
BER.Vn <- BER.Vn[,1:28]
BER.Vn <- BER.Vn[,!grepl("X",colnames(BER.Vn))]    

for(i in 7:28) {
    BER.Vn[,i] <- replace(BER.Vn[,i], BER.Vn[,i]==2, 0)
    BER.Vn[,i] <- replace(BER.Vn[,i], BER.Vn[,i]==3,-1)
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

for(i in 18:21) {
    BER.S[,i] <- replace(BER.S[,i], BER.S[,i]==0, 0.5)
    BER.S[,i] <- replace(BER.S[,i], BER.S[,i]==-1, 0)
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
streke <- unique(BER.M$region)

#-----------------------
#TRADE
#RETAIL
retailsd <- c(3110,3120,3160,3140,3130,3170,3150)
retailnd <- c(3230,3210,3240,3220)
retaild <- c(3330,3370,3310,3340,3350,3320,3380,3360)
retailo <- c(3410,3420)
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
community <- c(9000,9010,9030,9050,9060,9020,9040)
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

#-------------------
#Unweighted
Manufacturing_u <- ongeweeg(BER.M,all_m,streke,1:100)

Food_u <- ongeweeg(BER.M,food,streke,1:100)
Textiles_u <- ongeweeg(BER.M,textiles,streke,1:100)
Wood_u <- ongeweeg(BER.M,wood,streke,1:100)
Chemicals_u <- ongeweeg(BER.M,chemicals,streke,1:100)
Glass_u <- ongeweeg(BER.M,glass,streke,1:100)
Metals_u <- ongeweeg(BER.M,metals,streke,1:100)
Electrical_u <- ongeweeg(BER.M,electrical,streke,1:100)
Transport_u <- ongeweeg(BER.M,transport.m,streke,1:100)
Furniture_u <- ongeweeg(BER.M,furniture,streke,1:100)

Consumer_u <- ongeweeg(BER.M,consumer,streke,1:100)
Intermediate_u <- ongeweeg(BER.M,intermediate,streke,1:100)
Capital_u <- ongeweeg(BER.M,capital,streke,1:100)

WC.M_u <- ongeweeg(BER.M,all_m,1,1:100)
KZN.M_u <- ongeweeg(BER.M,all_m,5,1:100)
GP.M_u <- ongeweeg(BER.M,all_m,6,1:100)

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

#-------------
#Exports
#New faktor
Manufacturing_ne <- geweeg(BER.Mne,all_m,streke,1:100)
Food_ne <- geweeg(BER.Mne,food,streke,1:100)
Textiles_ne <- geweeg(BER.Mne,textiles,streke,1:100)
Wood_ne <- geweeg(BER.Mne,wood,streke,1:100)
Chemicals_ne <- geweeg(BER.Mne,chemicals,streke,1:100)
Glass_ne <- geweeg(BER.Mne,glass,streke,1:100)
Metals_ne <- geweeg(BER.Mne,metals,streke,1:100)
Electrical_ne <- geweeg(BER.Mne,electrical,streke,1:100)
Transport_ne <- geweeg(BER.Mne,transport.m,streke,1:100)
Furniture_ne <- geweeg(BER.Mne,furniture,streke,1:100)

#Consumer_ne <- geweeg(BER.Mne,consumer,streke,1:100)
#Intermediate_ne <- geweeg(BER.Mne,intermediate,streke,1:100)
#Capital_ne <- geweeg(BER.Mne,capital,streke,1:100)

#WC.M_ne <- geweeg(BER.Mne,all_m,1,1:100)
#KZN.M_ne <- geweeg(BER.Mne,all_m,5,1:100)
#GP.M_ne <- geweeg(BER.Mne,all_m,6,1:100)

#Manufacturing_ne[24,c(seq(4,32,by=2),34:49)] <- pub_m[24,2:32]
#Manufacturing_ne[33,c(seq(4,32,by=2),34:49)] <- pub_m[33,2:32]
#Manufacturing_ne[56,c(seq(4,32,by=2),34:49)] <- pub_m[56,2:32]

#-------------------
#Two-step
#Manufacturing_n2e <- maak.index(all_m)[1:100,]
BER.M2e <- BER.M2e[,-6:-7]
Manufacturing_2e <- geweeg(BER.M2e,all_m,streke,1:100)
Food_2e <- geweeg(BER.M2e,food,streke,1:100)
Textiles_2e <- geweeg(BER.M2e,textiles,streke,1:100)
Wood_2e <- geweeg(BER.M2e,wood,streke,1:100)
Chemicals_2e <- geweeg(BER.M2e,chemicals,streke,1:100)
Glass_2e <- geweeg(BER.M2e,glass,streke,1:100)
Metals_2e <- geweeg(BER.M2e,metals,streke,1:100)
Electrical_2e <- geweeg(BER.M2e,electrical,streke,1:100)
Transport_2e <- geweeg(BER.M2e,transport.m,streke,1:100)
Furniture_2e <- geweeg(BER.M2e,furniture,streke,1:100)

#Consumer_2e <- geweeg(BER.M2e,consumer,streke,1:100)
#Intermediate_2e <- geweeg(BER.M2e,intermediate,streke,1:100)
#Capital_2e <- geweeg(BER.M2e,capital,streke,1:100)
#WC.M_2e <- geweeg(BER.M2e,all_m,1,1:100)
#KZN.M_2e <- geweeg(BER.M2e,all_m,5,1:100)
#GP.M_2e <- geweeg(BER.M2e,all_m,6,1:100)
#Manufacturing_2e[24,c(seq(4,32,by=2),34:49)] <- pub_m[24,2:32]
#Manufacturing_2e[33,c(seq(4,32,by=2),34:49)] <- pub_m[33,2:32]
#Manufacturing_2e[56,c(seq(4,32,by=2),34:49)] <- pub_m[56,2:32]
#Manufacturing_n2e[24,c(seq(3,31,by=2),33:48)] <- pub_m[24,2:32]
#Manufacturing_n2e[33,c(seq(3,31,by=2),33:48)] <- pub_m[33,2:32]
#Manufacturing_n2e[56,c(seq(3,31,by=2),33:48)] <- pub_m[56,2:32]


#==========================
#TRADE
#==========================
#RETAIL
Retail <- geweeg(BER.R,all_r,streke,2:102)
Retail_u <- ongeweeg(BER.R,all_r,streke,2:102)
Retailsd <- geweeg(BER.R,retailsd,streke,2:102)
Retailnd <- geweeg(BER.R,retailnd,streke,2:102)
Retaild <- geweeg(BER.R,retaild,streke,2:102)
#Retailo <- geweeg(BER.R,retailo,streke,2:102)
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

#------------------
#New faktor
Retail_n <- geweeg(BER.Rn,all_r,streke,2:102)
Retailsd_n <- geweeg(BER.Rn,retailsd,streke,2:102)
Retailnd_n <- geweeg(BER.Rn,retailnd,streke,2:102)
Retaild_n <- geweeg(BER.Rn,retaild,streke,2:102)
#Retailo_n <- geweeg(BER.Rn,retailo,streke,2:102)
Hardware_n <- geweeg(BER.Rn,hardware,streke,2:102)
Other_duarbles_n <- geweeg(BER.Rn,other_duarbles,streke,2:102)
WC.R_n <- geweeg(BER.Rn,all_r,1,2:102)
KZN.R_n <- geweeg(BER.Rn,all_r,5,2:102)
GP.R_n <- geweeg(BER.Rn,all_r,6,2:102)

#Interpolasie
Retail_n[3,c(4,seq(5,17,by=2),18)] <- ad_r[3,2:10]
Retail_n[6,c(4,seq(5,17,by=2),18)] <- ad_r[6,2:10]
Retail_n[55,c(4,seq(5,17,by=2),18)] <- ad_r[55,2:10]
Retailsd_n[3,c(4,seq(5,17,by=2),18)] <- ad_r[55,2:10]
Retailsd_n[6,c(4,seq(5,17,by=2),18)] <- ad_r[6,11:19]
Retailsd_n[55,c(4,seq(5,17,by=2),18)] <- ad_r[55,11:19]
Retailnd_n[3,c(4,seq(5,17,by=2),18)] <- ad_r[3,20:28]
Retailnd_n[6,c(4,seq(5,17,by=2),18)] <- ad_r[6,20:28]
Retailnd_n[55,c(4,seq(5,17,by=2),18)] <- ad_r[55,20:28]
Retaild_n[3,c(4,seq(5,17,by=2),18)] <- ad_r[3,29:37]
Retaild_n[6,c(4,seq(5,17,by=2),18)] <- ad_r[6,29:37]
Retaild_n[55,c(4,seq(5,17,by=2),18)] <- ad_r[55,29:37]
WC.R_n[55,c(4,seq(5,17,by=2),18)] <- ad_r[55,38:46]
GP.R_n[55,c(4,seq(5,17,by=2),18)] <- ad_r[55,47:55]
KZN.R_n[55,c(4,seq(5,17,by=2),18)] <- ad_r[55,56:64]

#------------------
#2-step
#Retail_n2 <- maak.index(all_r)[1:100,]
#BER.R2 <- BER.R2[,-5:-6]
Retail_2 <- geweeg(BER.R2,all_r,streke,2:102)
Retailsd_2 <- geweeg(BER.R2,retailsd,streke,2:102)
Retailnd_2 <- geweeg(BER.R2,retailnd,streke,2:102)
Retaild_2 <- geweeg(BER.R2,retaild,streke,2:102)
#Retailo_2 <- geweeg(BER.R2,retailo,streke,2:102)
Hardware_2 <- geweeg(BER.R2,hardware,streke,2:102)
Other_duarbles_2 <- geweeg(BER.R2,other_duarbles,streke,2:102)
WC.R_2 <- geweeg(BER.R2,all_r,1,2:102)
KZN.R_2 <- geweeg(BER.R2,all_r,5,2:102)
GP.R_2 <- geweeg(BER.R2,all_r,6,2:102)

#Interpolasie
Retail_2[3,c(4,seq(5,17,by=2),18)] <- ad_r[3,2:10]
Retail_2[6,c(4,seq(5,17,by=2),18)] <- ad_r[6,2:10]
Retail_2[55,c(4,seq(5,17,by=2),18)] <- ad_r[55,2:10]
Retailsd_2[3,c(4,seq(5,17,by=2),18)] <- ad_r[55,2:10]
Retailsd_2[6,c(4,seq(5,17,by=2),18)] <- ad_r[6,11:19]
Retailsd_2[55,c(4,seq(5,17,by=2),18)] <- ad_r[55,11:19]
Retailnd_2[3,c(4,seq(5,17,by=2),18)] <- ad_r[3,20:28]
Retailnd_2[6,c(4,seq(5,17,by=2),18)] <- ad_r[6,20:28]
Retailnd_2[55,c(4,seq(5,17,by=2),18)] <- ad_r[55,20:28]
Retaild_2[3,c(4,seq(5,17,by=2),18)] <- ad_r[3,29:37]
Retaild_2[6,c(4,seq(5,17,by=2),18)] <- ad_r[6,29:37]
Retaild_2[55,c(4,seq(5,17,by=2),18)] <- ad_r[55,29:37]

WC.R_2[55,c(4,seq(5,17,by=2),18)] <- ad_r[55,38:46]
GP.R_2[55,c(4,seq(5,17,by=2),18)] <- ad_r[55,47:55]
KZN.R_2[55,c(4,seq(5,17,by=2),18)] <- ad_r[55,56:64]

#==========================
#WHOLESALE
Wholesale <- geweeg(BER.W,all_w,streke,2:102)
Wholesale_u <- ongeweeg(BER.W,all_w,streke,2:102)
Wholec <- geweeg(BER.W,wholec,streke,2:102)
Wholenc <- geweeg(BER.W,wholenc,streke,2:102)
WC.W <- geweeg(BER.W,all_w,1,2:102)
GP.W <- geweeg(BER.W,all_w,6,2:102)
KZN.W <- geweeg(BER.W,all_w,5,2:102)

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

#----------
#New faktor
Wholesale_n <- geweeg(BER.Wn,all_w,streke,2:102)
Wholec_n <- geweeg(BER.Wn,wholec,streke,2:102)
Wholenc_n <- geweeg(BER.Wn,wholenc,streke,2:102)
WC.W_n <- geweeg(BER.Wn,all_w,1,2:102)
GP.W_n <- geweeg(BER.Wn,all_w,6,2:102)
KZN.W_n <- geweeg(BER.Wn,all_w,5,2:102)

Wholesale_n[3,c(4,seq(5,17,by=2),18)] <- ad_w[3,2:10]
Wholesale_n[6,c(4,seq(5,17,by=2),18)] <- ad_w[6,2:10]
Wholesale_n[55,c(4,seq(5,17,by=2),18)] <- ad_w[55,2:10]
Wholec_n[3,c(4,seq(5,17,by=2),18)] <- ad_w[3,11:19]
Wholec_n[6,c(4,seq(5,17,by=2),18)] <- ad_w[6,11:19]
Wholec_n[55,c(4,seq(5,17,by=2),18)] <- ad_w[55,11:19]
Wholenc_n[3,c(4,seq(5,17,by=2),18)] <- ad_w[3,20:28]
Wholenc_n[6,c(4,seq(5,17,by=2),18)] <- ad_w[6,20:28]
Wholenc_n[55,c(4,seq(5,17,by=2),18)] <- ad_w[55,20:28]

WC.W_n[55,c(4,seq(5,17,by=2),18)] <- ad_w[55,29:37]
GP.W_n[55,c(4,seq(5,17,by=2),18)] <- ad_w[55,38:46]
KZN.W_n[55,c(4,seq(5,17,by=2),18)] <- ad_w[55,47:55]

#----------
#2-step
Wholesale_2 <- geweeg(BER.W2,all_w,streke,2:102)
Wholec_2 <- geweeg(BER.W2,wholec,streke,2:102)
Wholenc_2 <- geweeg(BER.W2,wholenc,streke,2:102)
WC.W_2 <- geweeg(BER.W2,all_w,1,2:102)
GP.W_2 <- geweeg(BER.W2,all_w,6,2:102)
KZN.W_2 <- geweeg(BER.W2,all_w,5,2:102)

Wholesale_2[3,c(4,seq(5,17,by=2),18)] <- ad_w[3,2:10]
Wholesale_2[6,c(4,seq(5,17,by=2),18)] <- ad_w[6,2:10]
Wholesale_2[55,c(4,seq(5,17,by=2),18)] <- ad_w[55,2:10]
Wholec_2[3,c(4,seq(5,17,by=2),18)] <- ad_w[3,11:19]
Wholec_2[6,c(4,seq(5,17,by=2),18)] <- ad_w[6,11:19]
Wholec_2[55,c(4,seq(5,17,by=2),18)] <- ad_w[55,11:19]
Wholenc_2[3,c(4,seq(5,17,by=2),18)] <- ad_w[3,20:28]
Wholenc_2[6,c(4,seq(5,17,by=2),18)] <- ad_w[6,20:28]
Wholenc_2[55,c(4,seq(5,17,by=2),18)] <- ad_w[55,20:28]

WC.W_2[55,c(4,seq(5,17,by=2),18)] <- ad_w[55,29:37]
GP.W_2[55,c(4,seq(5,17,by=2),18)] <- ad_w[55,38:46]
KZN.W_2[55,c(4,seq(5,17,by=2),18)] <- ad_w[55,47:55]

#==========================
#MOTOR VEHICLES
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

#----------
#New faktor
Motor_n <- geweeg(BER.Vn,all_v,streke,2:102)
WC.V_n <- geweeg(BER.Vn,all_v,1,2:102)
GP.V_n <- geweeg(BER.Vn,all_v,6,2:102)
KZN.V_n <- geweeg(BER.Vn,all_v,5,2:102)

p.inter <- c(4,5,7,9,11,12,13,15,17,18,19,21,23,25)
Motor_n[3,p.inter] <- pub_v[3,2:15]
Motor_n[6,p.inter] <- pub_v[6,2:15]
Motor_n[55,p.inter] <- pub_v[55,2:15]
p.inter <- c(4,5,7,9,11)
WC.V_n[55,p.inter] <- pub_v[55,16:20]
GP.V_n[55,p.inter] <- pub_v[55,21:25]
KZN.V_n[55,p.inter] <- pub_v[55,26:30]

#==========================
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
p.inter <- c(4,5,7,9,11,13)
Services[3,p.inter] <- pub_s[3,2:7]
ServicesC[3,p.inter] <- pub_s[3,13:18]
ServicesT[3,p.inter] <- pub_s[3,24:29]
ServicesR[3,p.inter] <- pub_s[3,35:40]
ServicesB[3,p.inter] <- pub_s[3,46:51]

p.inter <- c(4,5,7,9,11,13)
WC.S[3,p.inter] <- pub_s[3,79:84]
GP.S[3,p.inter] <- pub_s[3,68:73]
KZN.S[3,p.inter] <- pub_s[3,90:95]


