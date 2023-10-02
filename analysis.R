# remove everything from environment
rm(list=ls())

# where is the data
# dataLocation <- "C:/Users/owner/Desktop/bayesianGhost/data/"
dataLocation <- "data/"

# get csv files names
dataFiles <- list.files(dataLocation)

# 
db_raw <- lapply(paste0(dataLocation,dataFiles), read.csv)
for (i in 1:length(dataFiles)) {
  temp <- db_raw[[i]]
  temp <- temp[!is.na(temp$index),c("stim","index","workerId","noise","action",
                                    "scramble","communicative","response")]
  temp <- data.frame(temp[seq(1,nrow(temp),by=2),],
                     confidence=temp$response[seq(2,nrow(temp),by=2)])
  colnames(temp)[c(-3:-7)] <- c("video","trial","detection","confidence")
  temp$detection <- ifelse(temp$detection=="yes",1,0)
  temp$confidence <- as.integer(temp$confidence)
  temp$correct <- ifelse((1-temp$scramble)==temp$detection,1,0)
  if (i == 1) {
    db <- temp
  } else {
    db <- rbind(db,temp)
  }
}



if (!require(ggplot2)) {install.packages("ggplot2")}; library(ggplot2)
table(db$scramble)
table(db$communicative)
table(db$detection)
table(db$confidence)
ggplot(db, aes(x=scramble,y=detection)) + stat_summary()
ggplot(db, aes(x=communicative,y=detection,col=as.factor(scr))) + stat_summary()
ggplot(db, aes(x=scramble,y=confidence)) + stat_summary()
ggplot(db, aes(x=detection,y=confidence)) + stat_summary()
ggplot(db, aes(x=confidence,y=detection)) + stat_summary()
ggplot(db, aes(x=communicative,y=detection)) + stat_summary()

