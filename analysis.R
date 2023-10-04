# remove everything from environment
rm(list=ls())

# where is the data
# dataLocation <- "C:/Users/owner/Desktop/bayesianGhost/data/"
dataLocation <- "data/behaviour/"

# get csv files names
dataFiles <- list.files(dataLocation)
qualtric <- read.csv("data/qualtrics/bayesianGhost_October 3, 2023_14.23.csv")

# read all subjects 
db_raw <- lapply(paste0(dataLocation,dataFiles), read.csv)
for (i in 1:length(dataFiles)) {
  temp <- db_raw[[i]]
  # duration in minutes
  duration <- (max(temp$time_elapsed)/1000)/60
  # filter relevant columns
  temp <- temp[!is.na(temp$index),c("stim","index","workerId","interview_date",
                                    "noise","action","scramble","communicative",
                                    "response","confidence")]
  # temp <- data.frame(temp[seq(1,nrow(temp),by=2),],
  #                    confidence=temp$response[seq(2,nrow(temp),by=2)])
  temp$response <- c("",temp$response[1:(nrow(temp)-1)])
  temp <- temp[!is.na(temp$confidence),]
  colnames(temp)[c(-3:-8)] <- c("video","trial","detection","confidence")
  temp$detection <- ifelse(temp$detection=="yes",1,0)
  temp$confidence <- as.integer(temp$confidence)
  temp$correct <- ifelse((1-temp$scramble)==temp$detection,1,0)
  if (i == 1) {
    db <- temp
  } else {
    db <- rbind(db,temp)
  }
}



# # # # Basic raw Viualization # # # #
if (!require(ggplot2)) {install.packages("ggplot2")}; library(ggplot2)
table(db$scramble)
table(db$communicative)
table(db$detection)
hist(db$confidence)
ggplot(db, aes(x=scramble,y=detection)) + stat_summary()
ggplot(db, aes(x=communicative,y=detection,col=as.factor(scramble))) + stat_summary()
ggplot(db, aes(x=scramble,y=confidence)) + stat_summary()
ggplot(db, aes(x=detection,y=confidence)) + stat_summary()
ggplot(db, aes(x=confidence,y=detection)) + stat_summary()
ggplot(db, aes(x=communicative,y=detection)) + stat_summary()

