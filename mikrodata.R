
setwd("C:\\Users\\Laurie\\OneDrive\\Documents\\BING\\BER Confidence Surveys\\Before 2001\\Klaar")

extract <- function(filename) {
    bou <- scan(filename,sep="\n",what =character())
    bou <- as.data.frame(bou)
    bou$Streek <- substr(bou[,1],1,2)
    bou$Idnr <- substr(bou[,1],3,7)
    bou$Sektor <- substr(bou[,1],8,11)	
    bou$Gewig <- substr(bou[,1],12,12)
    bou$OpnameQ <- substr(bou[,1],13,15)
    bou$Dag <- substr(bou[,1],16,17)
    #hoeveel <- nchar(as.character(bou$bou))[1]
    for(i in 19:81) {
        bou[,(i-11)] <- substr(bou[,1],i,i)
    }
    return(bou)
}

files <- list.files(pattern = "^boudata")


bou.files <- list()
tel <- 0
for(f in files) { 
    tel <- tel + 1
    bou.files[[tel]] <- extract(f)
    #assign(paste0("bou.",tel),extract(f)) 
    #bou <- rbind(bou, extract(f))
}

bou.tot <- bou.files[[1]] 
for(b in 2:tel) {
    bou.tot <- rbind(bou.tot,bou.files[[b]]) 
}

#Q1AX	Q1PX	Q3A	Q3P	Q3AX	Q3PX	Q4AX	Q4PX	Q6	Q7	Q6bX	Q8	Q10	Q6eX	Q9	Q7aX	Q7bX	Q7cX	Q7dX	Q1	Q2A	Q2P


