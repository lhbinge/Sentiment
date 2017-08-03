##=======================##
##------ BRICS ----------##
##=======================##
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

brics <- read.csv("BRICS.csv", header=TRUE, sep=",",na.strings = "", skipNul = TRUE)
brics$Date <- as.Date(brics$Date, format = "%Y/%m/%d")


##For Grpahing Business cycles
recessions.l = read.table(textConnection(
    "Peak, Trough
    1960-04-30, 1961-08-31
    1965-04-30, 1965-12-31
    1967-05-31, 1967-12-31
    1970-12-31, 1972-08-31
    1974-08-31, 1977-12-31
    1981-08-31, 1983-03-31
    1984-06-30, 1986-03-31
    1989-02-28, 1993-05-30
    1996-11-30, 1999-08-31
    2007-11-30, 2009-08-31
    2013-11-30, 2016-12-31"), sep=',',
    colClasses=c('Date', 'Date'), header=TRUE)


##===================================
#BRICS Business Cycles
##----------------------------------
#Long term
conf_indices <- cbind(brics[,c(1,7:11)])
colnames(conf_indices) <- c("Date","RGDP_Growth","Coincident_Growth","Leading_Growth","BER_BCI","SACCI_Growth")

#Growth Rates
index_plot <- conf_indices[-1:-60,]
index_plot[,-1] <- scale(index_plot[,-1])
g <- ggplot(index_plot) 
#g <- g + theme_bw()
g <- g + geom_line(aes(x=Date, y=BER_BCI, colour="BER_BCI", linetype="BER_BCI"), size = 1.1)
g <- g + geom_line(aes(x=Date, y=Leading_Growth, colour="Leading_Growth", linetype="Leading_Growth"), size = 0.71)
g <- g + geom_line(aes(x=Date, y=Coincident_Growth, colour="Coincident_Growth", linetype="Coincident_Growth"), size = 0.71)
g <- g + geom_line(aes(x=Date, y=SACCI_Growth, colour="SACCI_Growth", linetype="SACCI_Growth"), size = 1.1)
g <- g + scale_linetype_manual(values=c("solid","dashed", "longdash","twodash"))
#g <- g + scale_colour_manual(values=c("black","gray32", "gray32","black"))
g <- g + labs(color="Legend text", linetype="Legend text")
g <- g + geom_rect(data=recessions.l, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='grey', alpha=0.5)
g <- g + ylab("Indicator") + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) 
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"), expand=c(0,0),
                      limits = as.Date(c("1973-12-31", NA)))
g <- g + theme(legend.position="bottom")
g

conf_indicesl <- brics[,c(1:6)]
#Levels
index_plot <- conf_indicesl[-1:-60,]
index_plot[,-1] <- scale(index_plot[,-1])
g <- ggplot(index_plot) 
#g <- g + theme_bw()
g <- g + geom_line(aes(x=Date, y=BER_BCI, colour="BER_BCI", linetype="BER_BCI", size = "BER_BCI"))
g <- g + geom_line(aes(x=Date, y=Leading, colour="Leading", linetype="Leading", size = "Leading"))
g <- g + geom_line(aes(x=Date, y=Coincident, colour="Coincident", linetype="Coincident", size = "Coincident"))
g <- g + geom_line(aes(x=Date, y=SACCI, colour="SACCI", linetype="SACCI", size = "SACCI"))
g <- g + scale_linetype_manual(values=c("solid","dashed", "longdash","twodash"))
g <- g + scale_size_manual(values=c(1.1,0.71, 0.71,1.1))
g <- g + labs(color="Legend text", linetype="Legend text", size="Legend text")
g <- g + geom_rect(data=recessions.l, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='grey', alpha=0.5)
g <- g + ylab("Indicator") + xlab("")
g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g <- g + theme(legend.title=element_blank()) 
g <- g + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"), expand=c(0,0),
                      limits = as.Date(c("1973-12-31", NA)))
g <- g + theme(legend.position="bottom")
g



#Check correlations
source("corstarsl.R")
xt <- xtable(corstarsl(conf_indices[,-1]), caption="Correlations in Levels")
print(xt, "latex",comment=FALSE, caption.placement = getOption("xtable.caption.placement", "top"), scalebox = 0.8)

corstarsl(conf_indices[,c(2:6)])

RGDP_Growth <- conf_indices[,2]
Coincident_Growth <- conf_indices[,3]
Leading_Growth <- conf_indices[,4]
BER_BCI <- conf_indices[,5]
SACCI_Growth <- conf_indices[,6] 

par(mfrow=c(2,2),mar=c(5,4,3,3), cex=0.7, cex.main=1.1)
ccf(Coincident_Growth, RGDP_Growth, na.action = na.pass,ylim=c(-0.4, 0.9), 
    ylab = "Correlation", xlab = "Number of Lags")
ccf(Leading_Growth, RGDP_Growth, na.action = na.pass, ylim=c(-0.4, 0.9), 
    ylab = "Correlation", xlab = "Number of Lags")
ccf(BER_BCI, RGDP_Growth, na.action = na.pass, ylim=c(-0.4, 0.9), 
    ylab = "Correlation", xlab = "Number of Lags")
ccf(SACCI_Growth, RGDP_Growth, na.action = na.pass, ylim=c(-0.4, 0.9), 
    ylab = "Correlation", xlab = "Number of Lags")

#------------------------------------------
#Long term
conf_indicesl <- brics[,c(1,7,3:6)]
#conf_indicesl <- brics[,c(1,7:11)]
#conf_indicesl[,-1] <- log(conf_indicesl[,-1])
#colnames(conf_indices) <- c("Date","RGDP_Growth","Coincident","Leading","BER_BCI","SACCI")

suppressMessages(library(BCDating))

dat <- BBQ(ts(conf_indicesl[-1:-60,2],start =c(1975,1),end=c(2016,3),frequency=4), mincycle = 5, minphase = 2, name="Activity")
tp1 <- as.data.frame(show(dat))[,-3]
dat <- BBQ(ts(conf_indicesl[-1:-60,3],start =c(1975,1),end=c(2016,3),frequency=4), mincycle = 5, minphase = 2, name="Activity")
tp2 <- as.data.frame(show(dat))[,-3]
dat <- BBQ(ts(conf_indicesl[-1:-60,4],start =c(1975,1),end=c(2016,3),frequency=4), mincycle = 5, minphase = 2, name="Activity")
tp3 <- as.data.frame(show(dat))[,-3]
dat <- BBQ(ts(conf_indicesl[-1:-60,5],start =c(1975,1),end=c(2016,3),frequency=4), mincycle = 5, minphase = 2, name="Activity")
tp4 <- as.data.frame(show(dat))[,-3]
dat <- BBQ(ts(conf_indicesl[-1:-127,6],start =c(1991,3),end=c(2016,3),frequency=4), mincycle = 5, minphase = 2, name="Activity")
tp5 <- as.data.frame(show(dat))[,-3]

maak_datums <- function(data) {
    data$Peak <- as.Date(as.yearqtr(data[,1], format = "%YQ%q"), frac = 1)
    data$Trough <- as.Date(as.yearqtr(data[,2], format = "%YQ%q"), frac = 1)
    data$Peak[1] <- "1973-12-31"
    data$Trough[nrow(data)] <- "2016-12-31"
    return(data)
}

tp5$Peak[1] <- "1990-12-31"

tp1 <- maak_datums(tp1)
tp2 <- maak_datums(tp2)
tp3 <- maak_datums(tp3)
tp4 <- maak_datums(tp4)
tp5 <- maak_datums(tp5)

detach("package:BCDating", unload=TRUE)

#----------------------------------

index_plot <- conf_indicesl[-1:-60,c(1,3)]
#index_plot[,2] <- scale(index_plot[,2])
colnames(index_plot) <- c("Date","Confidence")
g1 <- ggplot(index_plot) 
g1 <- g1 + geom_line(aes(x=Date, y=Confidence, colour="Confidence"), size = 0.5)
g1 <- g1 + labs(color="Legend text")
g1 <- g1 + geom_rect(data=recessions.l, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=0), fill='grey', alpha=0.5)
g1 <- g1 + geom_rect(data=tp2, aes(xmin=Peak, xmax=Trough, ymin=0, ymax=+Inf), fill='black', alpha=0.5)
g1 <- g1 + ylab("Indicator") + xlab("")
g1 <- g1 + ggtitle("Coincident") 
g1 <- g1 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g1 <- g1 + theme(legend.title=element_blank()) 
g1 <- g1 + scale_x_date(expand=c(0,0),limits = as.Date(c("1973-12-31", NA)))
g1 <- g1 + theme(legend.position="none")


index_plot <- conf_indicesl[-1:-60,c(1,4)]
#index_plot[,2] <- scale(index_plot[,2])
colnames(index_plot) <- c("Date","Confidence")
g2 <- ggplot(index_plot) 
g2 <- g2 + geom_line(aes(x=Date, y=Confidence, colour="Confidence"), size = 0.5)
g2 <- g2 + labs(color="Legend text")
g2 <- g2 + geom_rect(data=recessions.l, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=0), fill='grey', alpha=0.5)
g2 <- g2 + geom_rect(data=tp3, aes(xmin=Peak, xmax=Trough, ymin=0, ymax=+Inf), fill='black', alpha=0.5)
g2 <- g2 + ylab("") + xlab("")
g2 <- g2 + ggtitle("Leading") 
g2 <- g2 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g2 <- g2 + theme(legend.title=element_blank()) 
g2 <- g2 + scale_x_date(expand=c(0,0),limits = as.Date(c("1973-12-31", NA)))
g2 <- g2 + theme(legend.position="none")

index_plot <- conf_indicesl[-1:-60,c(1,5)]
#index_plot[,2] <- scale(index_plot[,2])
colnames(index_plot) <- c("Date","Confidence")
g3 <- ggplot(index_plot) 
g3 <- g3 + geom_line(aes(x=Date, y=Confidence, colour="Confidence"), size = 0.5)
g3 <- g3 + labs(color="Legend text")
g3 <- g3 + geom_rect(data=recessions.l, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=0), fill='grey', alpha=0.5)
g3 <- g3 + geom_rect(data=tp4, aes(xmin=Peak, xmax=Trough, ymin=0, ymax=+Inf), fill='black', alpha=0.5)
g3 <- g3 + ylab("Indicator") + xlab("")
g3 <- g3 + ggtitle("BER BCI") 
g3 <- g3 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g3 <- g3 + theme(legend.title=element_blank()) 
g3 <- g3 + scale_x_date(expand=c(0,0),limits = as.Date(c("1973-12-31", NA)))
g3 <- g3 + theme(legend.position="none")

index_plot <- conf_indicesl[-1:-60,c(1,6)]
#index_plot[,2] <- scale(index_plot[,2])
colnames(index_plot) <- c("Date","Confidence")
g4 <- ggplot(index_plot) 
g4 <- g4 + geom_line(aes(x=Date, y=Confidence, colour="Confidence"), size = 0.5)
g4 <- g4 + labs(color="Legend text")
g4 <- g4 + geom_rect(data=recessions.l, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=0), fill='grey', alpha=0.5)
g4 <- g4 + geom_rect(data=tp5, aes(xmin=Peak, xmax=Trough, ymin=0, ymax=+Inf), fill='black', alpha=0.5)
g4 <- g4 + ylab("") + xlab("")
g4 <- g4 + ggtitle("SACCI BCI") 
g4 <- g4 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g4 <- g4 + theme(legend.title=element_blank()) 
g4 <- g4 + scale_x_date(expand=c(0,0),limits = as.Date(c("1973-12-31", NA)))
g4 <- g4 + theme(legend.position="none")

library(gridExtra)
grid.arrange(g1, g2, g3, g4, ncol=2, nrow =2)




index_plot <- conf_indicesl[-1:-60,c(1,2)]
#index_plot[,2] <- scale(index_plot[,2])
colnames(index_plot) <- c("Date","Confidence")
g1 <- ggplot(index_plot) 
g1 <- g1 + geom_line(aes(x=Date, y=Confidence, colour="Confidence"), size = 0.5)
g1 <- g1 + labs(color="Legend text")
g1 <- g1 + geom_rect(data=recessions.l, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=0), fill='grey', alpha=0.5)
g1 <- g1 + geom_rect(data=tp1, aes(xmin=Peak, xmax=Trough, ymin=0, ymax=+Inf), fill='black', alpha=0.5)
g1 <- g1 + ylab("Indicator") + xlab("")
g1 <- g1 + ggtitle("GDP") 
g1 <- g1 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g1 <- g1 + theme(legend.title=element_blank()) 
g1 <- g1 + scale_x_date(expand=c(0,0),limits = as.Date(c("1973-12-31", NA)))
g1 <- g1 + theme(legend.position="none")
g1
#---------------------------------------
#Concordance
S <- conf_indicesl
#d <- seq(tp2[1,3], tp2[1,4], by="day")[-1]
#as.Date(tp2[1,3]:tp2[2,3], origin="1960-01-01")    

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
for(i in 1:nrow(recessions.l)) {
    d <- c(d,seq(recessions.l[i,1], recessions.l[i,2], by="day")[-1]) 
}
S$SARB <- 1
S[S$Date %in% d,7] <- 0

S[1:60,2:7] <- NA
S[1:127,6] <- NA

Concord_1<-NULL
Concord_2<-NULL
p_1<-NULL
p_2<-NULL

for(k in 0:4) {
    Concord1<-NULL
    Concord2<-NULL
    p.1<-NULL
    p.2<-NULL
    
    for(i in 3:6) {
        Concord1[i-2] <- (sum(S[1:(227-k),i]*S[(1+k):227,7], na.rm = TRUE)+sum((1-S[1:(227-k),i])*(1-S[(1+k):227,7]),na.rm = TRUE))/sum(!is.na(S[,i]))
        Concord2[i-2] <- (sum(S[1:(227-k),7]*S[(1+k):227,i], na.rm = TRUE)+sum((1-S[1:(227-k),7])*(1-S[(1+k):227,i]),na.rm = TRUE))/sum(!is.na(S[,7]))
    
        s_x1 <- S[1:(227-k),i]/sqrt(var(S[1:(227-k),i], na.rm = TRUE))
        s_x2 <- S[1:(227-k),7]/sqrt(var(S[1:(227-k),7], na.rm = TRUE))
        s_y1 <- S[(1+k):227,7]/sqrt(var(S[(1+k):227,7], na.rm = TRUE))
        s_y2 <- S[(1+k):227,i]/sqrt(var(S[(1+k):227,i], na.rm = TRUE))
    
        m <- lm(s_y1 ~ s_x1, na.action=na.exclude)
        p1 <- coeftest(m,vcov. = NeweyWest)[2,4]
        m <- lm(s_y2 ~ s_x2, na.action=na.exclude)
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
colnames(Concord_1) <- colnames(S)[3:6]

mystars <- ifelse(p_2 < .01, "***", ifelse(p_2 < .05, "** ", ifelse(p_2 < .1, "* ", " ")))
Concord_2 <- matrix(paste(Concord_2, mystars, sep=""), ncol=4)
colnames(Concord_2) <- colnames(S)[3:6]



figuur <- S[-1:-60,c(-2,-7)]
for(i in 1:4) {
    for(j in 1:227)
    ifelse(figuur[j,i+1]==0,figuur[j,i+1]<-i,figuur[j,i+1]<-NA)  
}

recessions.la <- recessions.l[-1:-4,]
recessions.la[1,1] <- "1975-03-31"

index_plot <- figuur
colnames(index_plot) <- c("Date","Coincident","Leading","BER_BCI","SACCI")
g1 <- ggplot(index_plot) 
g1 <- g1 + geom_rect(data=recessions.la, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='darkgrey', alpha=0.5)
g1 <- g1 + geom_line(aes(x=Date, y=Coincident), size = 10)
g1 <- g1 + geom_line(aes(x=Date, y=Leading), size = 10)
g1 <- g1 + geom_line(aes(x=Date, y=BER_BCI), size = 10)
g1 <- g1 + geom_line(aes(x=Date, y=SACCI), size = 10)
g1 <- g1 + labs(color="Legend text")
g1 <- g1 + ylab("") + xlab("")
g1 <- g1 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g1 <- g1 + theme(legend.title=element_blank()) 
g1 <- g1 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"), expand=c(0,0),
                      limits = as.Date(c("1973-12-31", NA)))
g1 <- g1 + theme(legend.position="none")
g1 <- g1 + scale_y_continuous(limits=c(0.5, 4.5),breaks = 0:5,
                   labels = c(" ", "Coincident", "Leading", "BER BCI", "SACCI BCI", " "))
#g1 <- g1 + theme(axis.text.y=element_text(size=12))
g1


#---------------------------------------------------
#VARs
detach("package:BCDating", unload=TRUE)

calc_var <- function(data) {
    vardat <- data
    infocrit <- VARselect(vardat, lag.max = 12, type = "const")
    k_aic <- infocrit$selection[1]
    k_hq  <- infocrit$selection[2]
    k_sic <- infocrit$selection[3]
    k <- min(k_aic,k_sic,k_hq)
    var_model <- VAR(vardat,p=8,type="const")
    return(var_model)
}

var1 <- calc_var(cbind(Coincident_Growth,RGDP_Growth)[-1:-60,])
var2 <- calc_var(cbind(Leading_Growth, RGDP_Growth)[-1:-60,])
var3 <- calc_var(cbind(BER_BCI, RGDP_Growth)[-1:-60,])
var4 <- calc_var(cbind(SACCI_Growth, RGDP_Growth)[-1:-131,])



##Granger causality tests
G <- data.frame()
G[1,1] <- causality(var1,cause = "Coincident_Growth")$Granger[4]
G[1,2] <- as.numeric(as.character(causality(var1,cause = "Coincident_Growth")$Granger[1]))
G[1,3] <- as.numeric(as.character(causality(var1,cause = "Coincident_Growth")$Granger[3]))
G[2,1] <- causality(var1,cause = "RGDP_Growth")$Granger[4]
G[2,2] <- as.numeric(as.character(causality(var1,cause = "RGDP_Growth")$Granger[1]))
G[2,3] <- as.numeric(as.character(causality(var1,cause = "RGDP_Growth")$Granger[3]))

G[3,1] <- causality(var2,cause = "Leading_Growth")$Granger[4]
G[3,2] <- as.numeric(as.character(causality(var2,cause = "Leading_Growth")$Granger[1]))
G[3,3] <- as.numeric(as.character(causality(var2,cause = "Leading_Growth")$Granger[3]))
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

xt <- xtable(G, caption="Granger causality tests")
print(xt, "latex", include.rownames=FALSE,comment=FALSE, caption.placement = getOption("xtable.caption.placement", "top"), 
      scalebox = 0.8)



irf.y1 <- irf(var1,impulse = "Coincident_Growth", response = "RGDP_Growth", 
              n.ahead = 12,runs = 1000, seed=12345) 
irf.y2 <- irf(var1,impulse = "RGDP_Growth", response = "Coincident_Growth",
              n.ahead = 12,runs = 1000, seed=12345)
irf.y1 <- irf(var2,impulse = "Leading_Growth", response = "RGDP_Growth", 
              n.ahead = 12,runs = 1000, seed=12345) 
irf.y2 <- irf(var2,impulse = "RGDP_Growth", response = "Leading_Growth", 
              n.ahead = 12,runs = 1000, seed=12345)
irf.y1 <- irf(var3,impulse = "BER_BCI", response = "RGDP_Growth", 
              n.ahead = 12,runs = 1000, seed=12345) 
irf.y2 <- irf(var3,impulse = "RGDP_Growth", response = "BER_BCI", 
              n.ahead = 12,runs = 1000, seed=12345)
irf.y1 <- irf(var4,impulse = "SACCI_Growth", response = "RGDP_Growth", 
              n.ahead = 12,runs = 1000, seed=12345) 
irf.y2 <- irf(var4,impulse = "RGDP_Growth", response = "SACCI_Growth", 
              n.ahead = 12,runs = 1000, seed=12345)

par(mfrow=c(1,1), new=FALSE)
nf <- layout(matrix(c(1,2,1,2), 1, 2, byrow = TRUE))
#layout.show(nf)
par(cex=1)
plot(irf.y1,plot.type = c("single"), main="Response from SACCI Growth", xlab="Horizon in quarters")
par(cex=1,new = TRUE)
plot(irf.y2,plot.type = c("single"), main="Response from RGDP Growth", xlab="Horizon in quarters")

source("plot_varfevd.R")
#png(file="Figure6.png",width=7,height=4.5,res=150)
par(mfrow=c(1,1), new=FALSE)
#dev.off()
par(mfrow=c(1,2),cex=0.9)
plot.varfevd(fevd(var3, n.ahead = 10 ),plot.type = "single", xlab="Horizon in quarters", ylab="Proportion of variance explained")
#dev.off()

#---------------------------------
#In growth rates:
JSE <- GDPgrowth4$RJSE
Bond <- GDPdata$Bond2
TBill <- GDPdata$T.Bill
Spread <- Bond-TBill
Employment <- GDPgrowth4$Employ
Investment <- GDPgrowth4$Rinvestment 
Industrial_Production <- GDPgrowth4$RProduction

vardat <- cbind(BER_BCI=BER_BCI[129:227],JSE,Spread,RGDP_Growth=RGDP_Growth[129:227],
                Industrial_Production,Employment,Investment)  


infocrit <- VARselect(vardat, lag.max = 16, type = "const")
k_aic <- infocrit$selection[1]
k_hq  <- infocrit$selection[2]
k_sic <- infocrit$selection[3]
k <- min(k_aic,k_sic,k_hq)
vare <- VAR(vardat,p=4,type="const")

irf.y1 <- irf(vare,impulse = c("BER_BCI"),
              response = c("RGDP_Growth"), n.ahead = 12,runs = 1000, seed=12345) 
irf.y2 <- irf(vare,impulse = "RGDP_Growth", response = "BER_BCI", 
              n.ahead = 12,runs = 1000, seed=12345)

par(mfrow=c(1,1), new=FALSE)
nf <- layout(matrix(c(1,2,1,2), 1, 2, byrow = TRUE))
layout.show(nf)
#par(cex=0.6)
plot(irf.y1,plot.type = c("single"), main="Response from BER BCI", xlab="Horizon in quarters")
par(new = TRUE)
plot(irf.y2,plot.type = c("single"), main="Response from RGDP Growth", xlab="Horizon in quarters")

irf.y1 <- irf(vare,impulse = c("BER_BCI"),
              response = c("RGDP_Growth","Industrial_Production","Investment"), n.ahead = 12,runs = 1000, seed=12345) 

par(mfrow=c(1,1), new=FALSE)
par(mfrow=c(1,3),mar=c(4.2,4,2,1), cex=0.7)
plot(irf.y1,plot.type = c("single"), main="Response from BER BCI", xlab="Horizon in quarters")


source("plot_varfevd.R")

#png("figure10",width = 598, height=382, units = "cm",res=120)
par(mfrow=c(1,1),cex=0.9)
#plot.varfevd(fevd(vare, n.ahead = 10 ),plot.type = "single", xlab="Horizon in quarters", 
#             ylab="Proportion of variance explained")
plot(fevd(vare, n.ahead = 10 ),plot.type = "single", xlab="Horizon in quarters", 
             ylab="Proportion of variance explained")
#dev.off()



#SACCI
vardat <- cbind(SACCI_Growth=SACCI_Growth[129:227],JSE,Spread,RGDP_Growth=RGDP_Growth[129:227],
                Industrial_Production,Employment,Investment)[-1:-3,]  


infocrit <- VARselect(vardat, lag.max = 16, type = "const")
k_aic <- infocrit$selection[1]
k_hq  <- infocrit$selection[2]
k_sic <- infocrit$selection[3]
k <- min(k_aic,k_sic,k_hq)
vare <- VAR(vardat,p=4,type="const")

irf.y1 <- irf(vare,impulse = c("SACCI_Growth"),
              response = c("RGDP_Growth"), n.ahead = 12,runs = 1000, seed=12345) 
irf.y2 <- irf(vare,impulse = "RGDP_Growth", response = "SACCI_Growth", 
              n.ahead = 12,runs = 1000, seed=12345)

par(mfrow=c(1,1), new=FALSE)
nf <- layout(matrix(c(1,2,1,2), 1, 2, byrow = TRUE))
layout.show(nf)
par(cex=0.6)
plot(irf.y1,plot.type = c("single"), main="Response from SACCI Growth", xlab="Horizon in quarters")
par(new = TRUE)
plot(irf.y2,plot.type = c("single"), main="Response from RGDP Growth", xlab="Horizon in quarters")

irf.y1 <- irf(vare,impulse = c("SACCI_Growth"),
              response = c("RGDP_Growth","Industrial_Production","Investment"), n.ahead = 12,runs = 1000, seed=12345) 

par(mfrow=c(1,1), new=FALSE)
par(mfrow=c(1,3),mar=c(4.2,4,2,1), cex=0.62)
plot(irf.y1,plot.type = c("single"), main="Response from SACCI Growth", xlab="Horizon in quarters")

source("plot_varfevd.R")
#dev.off()
par(mfrow=c(1,1),cex=0.9)
plot.varfevd(fevd(vare, n.ahead = 10 ),plot.type = "single", xlab="Horizon in quarters", ylab="Proportion of variance explained")

plot(fevd(vare, n.ahead = 10 ))


#Tests


adf.test(BER_BCI[-1:-60], alternative = "stationary")
adf.test(conf_indicesl$SACCI[-1:-127], alternative = "stationary")
adf.test(conf_indices$SACCI_Growth[-1:-131], alternative = "stationary")
summary(ur.df(BER_BCI[-1:-60], c("none"), selectlags = c("AIC")))
summary(ur.df(conf_indices$SACCI_Growth[-1:-131], c("none"), selectlags = c("AIC")))

      
summary(ur.df(BER_BCI[-1:-60], c("none"), selectlags = c("AIC")))
summary(ur.df(BER_BCI[-1:-60], c("drift"), selectlags = c("AIC")))
summary(ur.df(BER_BCI[-1:-60], c("trend"), selectlags = c("AIC")))

serialT1 <- serial.test(var1, lags.bg=2, type="BG")
serialT2 <- serial.test(var1, lags.pt=2, type="PT.adjusted")
serialT3 <- serial.test(var1, lags.pt=2, type="PT.asymptotic")
archT <- arch.test(var1, lags.single=1, lags.multi = 1, multivariate.only = F)
normT <- normality.test(var1, multivariate.only = F)


plot(var3,plot.type = c("single"))
plot(serialT1,plot.type = c("multiple"))
methods(plot)
getAnywhere(plot.varest)
plot(serialT1,ylim.hist=c(0,2))
plot(serialT1,ylim.hist=c(0,1))
plot(normT)




#====================
#Laubsscher

MWH = read.table(textConnection(
    "Peak, Trough
    1975-12-31,1977-08-31
    1981-08-31,1983-04-30
    1984-04-30,1986-01-31
    1988-10-31,1993-07-31
    1994-12-31,1996-08-31
    1996-12-31,1999-05-31
    1999-12-31,2001-07-31
    2002-06-30,2004-01-31
    2007-10-31,2009-09-30
    2013-12-31,2016-12-31"), sep=',',
    colClasses=c('Date', 'Date'), header=TRUE)

MPV = read.table(textConnection(
    "Peak, Trough
    1975-09-30,1977-11-30
    1981-10-31,1983-05-31
    1984-05-31,1986-02-28
    1986-10-31,1987-07-31
    1989-06-30,1992-05-31
    1997-05-31,1999-07-31
    2002-11-30,2003-11-30
    2008-06-30,2009-08-31
    2014-02-28,2015-12-31"), sep=',',
    colClasses=c('Date', 'Date'), header=TRUE)

MCU = read.table(textConnection(
    "Peak, Trough
    1976-03-31, 1977-12-31
    1982-03-31, 1983-07-31
    1984-06-30,1986-09-30
    1989-07-31,1990-09-30
    1992-03-31,1993-01-31
    1995-03-31,1996-07-31
    1998-03-31,1999-06-30
    2001-03-31,2001-09-30
    2008-08-31,2009-09-30
    2013-11-30,2016-12-31"), sep=',',
    colClasses=c('Date', 'Date'), header=TRUE)


WSV = read.table(textConnection(
    "Peak, Trough
    1976-06-30,1978-01-31
    1978-10-31,1979-09-30
    1981-10-31,1983-01-31
    1984-08-31,1986-01-31
    1988-12-31,1989-10-31
    1996-04-30,1997-02-28
    2008-05-31,2009-11-30"), sep=',',
    colClasses=c('Date', 'Date'), header=TRUE)



g1 <- ggplot() 
#g1 <- g1 + geom_line(aes(x=Date, y=Confidence, colour="Confidence"), size = 0.5)
#g1 <- g1 + labs(color="Legend text")
g1 <- g1 + geom_rect(data=recessions.l, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=0), fill='grey', alpha=0.5)
g1 <- g1 + geom_rect(data=MWH, aes(xmin=Peak, xmax=Trough, ymin=0, ymax=+Inf), fill='black', alpha=0.5)
g1 <- g1 + ylab("") + xlab("")
g1 <- g1 + ggtitle("Manufacturing Working Hours") 
g1 <- g1 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g1 <- g1 + theme(legend.title=element_blank()) 
g1 <- g1 + scale_x_date(expand=c(0,0),limits = as.Date(c("1973-12-31", NA)))
g1 <- g1 + theme(legend.position="none")
g1 <- g1 + theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())

g2 <- ggplot() 
#g2 <- g2 + geom_line(aes(x=Date, y=Confidence, colour="Confidence"), size = 0.5)
#g2 <- g2 + labs(color="Legend text")
g2 <- g2 + geom_rect(data=recessions.l, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=0), fill='grey', alpha=0.5)
g2 <- g2 + geom_rect(data=MPV, aes(xmin=Peak, xmax=Trough, ymin=0, ymax=+Inf), fill='black', alpha=0.5)
g2 <- g2 + ylab("") + xlab("")
g2 <- g2 + ggtitle("Manufacturing Production Volumes") 
g2 <- g2 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g2 <- g2 + theme(legend.title=element_blank()) 
g2 <- g2 + scale_x_date(expand=c(0,0),limits = as.Date(c("1973-12-31", NA)))
g2 <- g2 + theme(legend.position="none")
g2 <- g2 + theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())

g3 <- ggplot() 
#g3 <- g3 + geom_line(aes(x=Date, y=Confidence, colour="Confidence"), size = 0.5)
#g3 <- g3 + labs(color="Legend text")
g3 <- g3 + geom_rect(data=recessions.l, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=0), fill='grey', alpha=0.5)
g3 <- g3 + geom_rect(data=MCU, aes(xmin=Peak, xmax=Trough, ymin=0, ymax=+Inf), fill='black', alpha=0.5)
g3 <- g3 + ylab("") + xlab("")
g3 <- g3 + ggtitle("Manufacturing Capacity Utilisation") 
g3 <- g3 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g3 <- g3 + theme(legend.title=element_blank()) 
g3 <- g3 + scale_x_date(expand=c(0,0),limits = as.Date(c("1973-12-31", NA)))
g3 <- g3 + theme(legend.position="none")
g3 <- g3 + theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())

g4 <- ggplot() 
#g4 <- g4 + geom_line(aes(x=Date, y=Confidence, colour="Confidence"), size = 0.5)
g4 <- g4 + labs(color="Legend text")
g4 <- g4 + geom_rect(data=recessions.l, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=0), fill='grey', alpha=0.5)
g4 <- g4 + geom_rect(data=WSV, aes(xmin=Peak, xmax=Trough, ymin=0, ymax=+Inf), fill='black', alpha=0.5)
g4 <- g4 + ylab("") + xlab("")
g4 <- g4 + ggtitle("Wholesale Sales Volumes") 
g4 <- g4 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g4 <- g4 + theme(legend.title=element_blank()) 
g4 <- g4 + scale_x_date(expand=c(0,0),limits = as.Date(c("1973-12-31", NA)))
g4 <- g4 + theme(legend.position="none")
g4 <- g4 + theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())

library(gridExtra)
grid.arrange(g1, g2, g3, g4, ncol=2, nrow =2)



Median_all = read.table(textConnection(
    "Peak, Trough
    1975-12-31,1977-12-31
    1981-10-31,1983-04-30
    1984-04-30,1986-01-31
    1989-03-31,1992-11-30
    1997-02-28,1999-05-31
    2008-02-28,2009-09-30"), sep=',',
    colClasses=c('Date', 'Date'), header=TRUE)


Median_majority = read.table(textConnection(
    "Peak, Trough
    1975-10-31,1977-11-30
    1981-10-31,1983-04-30
    1984-04-30,1986-01-31
    1989-03-31,1992-11-30
    1997-02-28,1999-05-31
    2008-02-28,2009-09-30
    2014-02-28,2016-12-31"), sep=',',
    colClasses=c('Date', 'Date'), header=TRUE)


g1 <- ggplot() 
#g1 <- g1 + geom_line(aes(x=Date, y=Confidence, colour="Confidence"), size = 0.5)
#g1 <- g1 + labs(color="Legend text")
g1 <- g1 + geom_rect(data=recessions.l, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=0), fill='grey', alpha=0.5)
g1 <- g1 + geom_rect(data=Median_all, aes(xmin=Peak, xmax=Trough, ymin=0, ymax=+Inf), fill='black', alpha=0.5)
g1 <- g1 + ylab("") + xlab("")
g1 <- g1 + ggtitle("Median (all)") 
g1 <- g1 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g1 <- g1 + theme(legend.title=element_blank()) 
g1 <- g1 + scale_x_date(expand=c(0,0),limits = as.Date(c("1973-12-31", NA)))
g1 <- g1 + theme(legend.position="none")
g1 <- g1 + theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())

g2 <- ggplot() 
#g2 <- g2 + geom_line(aes(x=Date, y=Confidence, colour="Confidence"), size = 0.5)
#g2 <- g2 + labs(color="Legend text")
g2 <- g2 + geom_rect(data=recessions.l, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=0), fill='grey', alpha=0.5)
g2 <- g2 + geom_rect(data=Median_majority, aes(xmin=Peak, xmax=Trough, ymin=0, ymax=+Inf), fill='black', alpha=0.5)
g2 <- g2 + ylab("") + xlab("")
g2 <- g2 + ggtitle("Median turning points") 
g2 <- g2 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g2 <- g2 + theme(legend.title=element_blank()) 
#g2 <- g2 + scale_x_date(expand=c(0,0),limits = as.Date(c("1973-12-31", NA)))
g2 <- g2 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"), expand=c(0,0),
                      limits = as.Date(c("1973-12-31", NA)))
g2 <- g2 + theme(legend.position="none")
g2 <- g2 + theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())
g2
#library(gridExtra)
#grid.arrange(g1, g2, ncol=1, nrow =2)



#Concordance
colnames(S) <- c("Date","MWH","MPV","MCU","WSV","BER_BCI","Median")

d <- NULL
for(i in 1:nrow(MWH)) {
    d <- c(d,seq(MWH[i,1], MWH[i,2], by="day")[-1]) 
}
S[,2] <- 1
S[S$Date %in% d,2] <- 0

d <- NULL
for(i in 1:nrow(MPV)) {
    d <- c(d,seq(MPV[i,1], MPV[i,2], by="day")[-1]) 
}
S[,3] <- 1
S[S$Date %in% d,3] <- 0

d <- NULL
for(i in 1:nrow(MCU)) {
    d <- c(d,seq(MCU[i,1], MCU[i,2], by="day")[-1]) 
}
S[,4] <- 1
S[S$Date %in% d,4] <- 0

d <- NULL
for(i in 1:nrow(WSV)) {
    d <- c(d,seq(WSV[i,1], WSV[i,2], by="day")[-1]) 
}
S[,5] <- 1
S[S$Date %in% d,5] <- 0

d <- NULL
for(i in 1:nrow(tp4)) {
    d <- c(d,seq(tp4[i,3], tp4[i,4], by="day")[-1]) 
}
S[,6] <- 1
S[S$Date %in% d,6] <- 0


d <- NULL
for(i in 1:nrow(Median_majority)) {
    d <- c(d,seq(Median_majority[i,1], Median_majority[i,2], by="day")[-1]) 
}
S[,7] <- 1
S[S$Date %in% d,7] <- 0

#S[1:60,2:7] <- NA
#S[1:127,6] <- NA


figuur <- S[,c(1,7,2:6)]
for(i in 1:6) {
    for(j in 1:227)
        ifelse(figuur[j,i+1]==0,figuur[j,i+1]<-i,figuur[j,i+1]<-NA)  
}

recessions.la <- recessions.l[-1:-4,]
recessions.la[1,1] <- "1975-03-31"


index_plot <- figuur
g1 <- ggplot(index_plot) 
g1 <- g1 + geom_rect(data=recessions.la, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='darkgrey', alpha=0.5)
g1 <- g1 + geom_line(aes(x=Date, y=MWH), size = 10)
g1 <- g1 + geom_line(aes(x=Date, y=MPV), size = 10)
g1 <- g1 + geom_line(aes(x=Date, y=MCU), size = 10)
g1 <- g1 + geom_line(aes(x=Date, y=WSV), size = 10)
#g1 <- g1 + geom_line(aes(x=Date, y=BER_BCI), size = 10)
g1 <- g1 + geom_line(aes(x=Date, y=Median), size = 10)
g1 <- g1 + labs(color="Legend text")
g1 <- g1 + ylab("") + xlab("")
g1 <- g1 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
g1 <- g1 + theme(legend.title=element_blank()) 
g1 <- g1 + scale_x_date(labels = date_format("%Y"),breaks = date_breaks("year"), expand=c(0,0),
                        limits = as.Date(c("1973-12-31", NA)))
g1 <- g1 + theme(legend.position="none")
g1 <- g1 + scale_y_continuous(limits=c(0.5, 5.5),breaks = 0:6,
                              labels = c(" ", "Median","Manufacturing \nWorking Hours", "Manufacturing \nProduction Volumes", "Manufacturing \nCapacity Utilisation", "Wholesale \nSales Volumes", " "))
#g1 <- g1 + theme(axis.text.y=element_text(size=12))
g1


