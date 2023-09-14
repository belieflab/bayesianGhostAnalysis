rm(list=ls())

dataLocation <- "C:/Users/owner/Desktop/bayesianGhost/data/"

dataFiles <- list.files(dataLocation)

db_raw <- read.csv(paste0(dataLocation,dataFiles[1]))
db <- db_raw[!is.na(db_raw$index),c("stim","index","workerId","response")]
db <- data.frame(db[seq(1,nrow(db),by=2),],
                 confidence=db$response[seq(2,nrow(db),by=2)])
colnames(db) <- c("video","trial","workerId","detection","confidence")
db$noise <- as.integer(substr(db$video,14,16))
db$scr <- ifelse(grepl("scr1",db$video),1,0)
db$con <- ifelse(grepl("con1",db$video),1,0)
db$detection <- ifelse(db$detection=="yes",1,0)
db$confidence <- as.integer(db$confidence)
db$correct <- ifelse((1-db$scr)==db$detection,1,0)


library("ggplot2")
table(db$scr)
table(db$con)
table(db$detection)
table(db$confidence)
ggplot(db, aes(x=scr,y=detection)) + stat_summary()
ggplot(db, aes(x=con,y=detection,col=as.factor(scr))) + stat_summary()
ggplot(db, aes(x=scr,y=confidence)) + stat_summary()
ggplot(db, aes(x=detection,y=confidence)) + stat_summary()
ggplot(db, aes(x=confidence,y=detection)) + stat_summary()
ggplot(db, aes(x=con,y=detection)) + stat_summary()

