source("api/getSurvey.R")
source("api/getTask.R")

# get qualtrics
qualtrics <- getSurvey("ghost") # name is found in surveyIds.R
qualtrics$survey_duration <- qualtrics$EndDate - qualtrics$StartDate

# get merged behaviour 
behaviour <- getTask("ghost", "workerId") # name is found in mongo collections

# create subjects vector
subject <- unique(behaviour$workerId)

# loop subjects to get task duration 
for (i in 1:length(subject)) {
  temp <- behaviour[behaviour$workerId == subject[i],]
  # task duration in minutes
  task_duration <- (max(temp$time_elapsed)/1000)/60
  # survey duration in minutes
  survey_duration <- qualtrics$survey_duration[qualtrics$workerId == subject[i]]
  # filter relevant columns
  temp <- temp[,c("stim","index","workerId","interview_date","noise","action",
                  "scramble","communicative","response","confidence")]
  # temp <- data.frame(temp[seq(1,nrow(temp),by=2),],
  #                    confidence=temp$response[seq(2,nrow(temp),by=2)])
  temp$response <- c(NA,temp$response[1:(nrow(temp)-1)])
  temp <- temp[!is.na(temp$confidence),]
  colnames(temp) <- c("video","trial","workerId","interview_date","noise","action",
                      "scramble","communicative","response","confidence")
  temp$detection <- ifelse(temp$response=="yes",1,0)
  temp$correct <- ifelse((1-temp$scramble)==temp$detection,1,0)
  if (i == 1) {
    db <- temp
    genChar <- data.frame(workerId=subject[i],interview_date=temp$interview_date[1],
                          task_duration=task_duration,
                          survey_duration=survey_duration)
  } else {
    db <- rbind(db,temp)
    genChar <- rbind(genChar,data.frame(workerId=subject[i],interview_date=temp$interview_date[1],
                                        task_duration=task_duration,
                                        survey_duration=survey_duration))
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
