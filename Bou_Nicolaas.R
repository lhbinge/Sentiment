##================================================================##
## -------------------- BUILDING SURVEY --------------------------##
##================================================================##
setwd("C:\\Users\\Laurie\\OneDrive\\Documents\\BING\\BER Confidence Surveys\\Sentiment")
#change the working directory

#install.packages("XLConnect")
options(java.parameters = "-Xmx4g" )
#options(java.parameters = "- Xmx1024m")
suppressMessages(library(XLConnect))
suppressMessages(library(ggplot2))
suppressMessages(library(reshape2))
suppressMessages(library(scales))
suppressMessages(library(zoo))

##=====================##
## READING IN THE DATA ##
##=====================##
datums <- read.csv("dates.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)
datums$Datum <- as.Date(datums$Datum, format = "%Y/%m/%d")

pub_b <- read.csv("Building_BER_Published.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)
ref_b <- read.csv("Ref Series_Build.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)
pub_a <- read.csv("Arc_Published.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)

#-----------------------
#Building Survey
BER.B <- read.csv("Building_93Q2-17Q2.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)
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

##=====================##
## DEFINE SECTOR CODES ##
##=====================##
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


##======================##
## CALCULATE INDICATORS ##
##======================##
#BUILDING
Building <- ongeweeg(BER.B,all_b,streke,6:102)

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

##=============================
#Plot
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

##==========================
##Write series to Excel file
##==========================
Building1 <- Building[,c(1,3:17)]
Building1 <- cbind(Building1,NA,Residential[,c(1,3:17)])
Building1 <- cbind(Building1,NA,Non_residential[,c(1,3:17)])
Building1 <- cbind(Building1,NA,WC.B[,c(1,3:17)])
Building1 <- cbind(Building1,NA,KZN.B[,c(1,3:17)])
Building1 <- cbind(Building1,NA,GP.B[,c(1,3:17)])
colnames(Building1)[17] <- " "
colnames(Building1)[34] <- " "
colnames(Building1)[51] <- " "
colnames(Building1)[68] <- " "
colnames(Building1)[85] <- " "

Building1[,c(1,18,35,52,69,86)] <- lapply(Building1[,c(1,18,35,52,69,86)], as.character)
Building1[,c(-1:-2,-17:-19,-34:-36,-51:-53,-68:-70,-85:-87)] <- round(Building1[,c(-1:-2,-17:-19,-34:-36,-51:-53,-68:-70,-85:-87)],2)

header <- character(length=101)
header[1:16] <- "Total"
header[18:33] <- "Residential"
header[35:50] <- "Non-Residential"
header[52:67] <- "WC"
header[69:84] <- "KZN"
header[86:101] <- "GP"
header <- as.data.frame(t(header))

writeWorksheetToFile(file = "Building_Indicators.xlsx", data = header, sheet = "BUILD",
                     startRow = 1, header = FALSE)
writeWorksheetToFile(file = "Building_Indicators.xlsx", data = Building1, sheet = "BUILD",
                     startRow = 2)

#---------------------------------------------------
Contractor1 <- Contractor[,c(1,3:17)]
Contractor1 <- cbind(Contractor1,NA,Contractor_res[,c(1,3:17)])
Contractor1 <- cbind(Contractor1,NA,Contractor_nonres[,c(1,3:17)])
colnames(Contractor1)[17] <- " "
colnames(Contractor1)[34] <- " "
Contractor1[,c(1,18,35)] <- lapply(Contractor1[,c(1,18,35)], as.character)
Contractor1[,c(-1:-2,-17:-19,-34:-36)] <- round(Contractor1[,c(-1:-2,-17:-19,-34:-36)],2)

header <- character(length=50)
header[1:16] <- "Total"
header[18:33] <- "Residential"
header[35:50] <- "Non-Residential"
header <- as.data.frame(t(header))

writeWorksheetToFile(file = "Building_Indicators.xlsx", data = header, sheet = "BUILD-Con",
                     startRow = 1, header = FALSE)
writeWorksheetToFile(file = "Building_Indicators.xlsx", data = Contractor1, sheet = "BUILD-Con",
                     startRow = 2)

#---------------------------------------------------
Subcon1 <- Subcon[,c(1,3:17)]
Subcon1 <- cbind(Subcon1,NA,Subcon_res[,c(1,3:17)])
Subcon1 <- cbind(Subcon1,NA,Subcon_nonres[,c(1,3:17)])
colnames(Subcon1)[17] <- " "
colnames(Subcon1)[34] <- " "
Subcon1[,c(1,18,35)] <- lapply(Subcon1[,c(1,18,35)], as.character)
Subcon1[,c(-1:-2,-17:-19,-34:-36)] <- round(Subcon1[,c(-1:-2,-17:-19,-34:-36)],2)

header <- character(length=50)
header[1:16] <- "Total"
header[18:33] <- "Residential"
header[35:50] <- "Non-Residential"
header <- as.data.frame(t(header))


writeWorksheetToFile(file = "Building_Indicators.xlsx", data = header, sheet = "BUILD-Sub",
                     startRow = 1, header = FALSE)
writeWorksheetToFile(file = "Building_Indicators.xlsx", data = Subcon1, sheet = "BUILD-Sub",
                     startRow = 2)

#---------------------------------------------------
architects1 <- Architects[,c(-2)]
architects1[,c(1)] <- as.character(architects1[,c(1)])
architects1[,c(-1:-2)] <- round(architects1[,c(-1:-2)],2)

header <- character(length=13)
header[1:13] <- "Total"
header <- as.data.frame(t(header))


writeWorksheetToFile(file = "Building_Indicators.xlsx", data = header, sheet = "ARC",
                     startRow = 1, header = FALSE)
writeWorksheetToFile(file = "Building_Indicators.xlsx", data = architects1, sheet = "ARC",
                     startRow = 2)

#---------------------------------------------------
qss1 <- QSs[,c(-2)]
qss1[,c(1)] <- as.character(qss1[,c(1)])
qss1[,c(-1:-2)] <- round(qss1[,c(-1:-2)],2)

header <- character(length=13)
header[1:13] <- "Total"
header <- as.data.frame(t(header))


writeWorksheetToFile(file = "Building_Indicators.xlsx", data = header, sheet = "QS",
                     startRow = 1, header = FALSE)
writeWorksheetToFile(file = "Building_Indicators.xlsx", data = architects1, sheet = "QS",
                     startRow = 2)

#---------------------------------------------------
civils1 <- Civils[,c(-2)]
civils1[,c(1)] <- as.character(civils1[,c(1)])
civils1[,c(-1:-2)] <- round(civils1[,c(-1:-2)],2)

header <- character(length=13)
header[1:13] <- "Total"
header <- as.data.frame(t(header))


writeWorksheetToFile(file = "Building_Indicators.xlsx", data = header, sheet = "CE",
                     startRow = 1, header = FALSE)
writeWorksheetToFile(file = "Building_Indicators.xlsx", data = architects1, sheet = "CE",
                     startRow = 2)



