source("api/getSurvey.R")
source("api/getTask.R")

# get qualtrics
qualtrics <- getSurvey("ghost") # name is found in surveyIds.R
qualtrics$survey_duration <- qualtrics$EndDate - qualtrics$StartDate

source("function.R")
# score questionnaires
qualtrics <- scoreQuestionnaire(qualtrics)

# get merged behaviour 
behaviour <- getTask("ghost", "workerId") # name is found in mongo collections

# create subjects vector
subject <- unique(behaviour$workerId)

# loop subjects to get task duration 
for (i in 1:length(subject)) {
  temp <- behaviour[behaviour$workerId == subject[i],]
  # task duration in minutes
  # task_duration <- (max(temp$time_elapsed)/1000)/60
  task_duration <- NA
  questTemp <- qualtrics[qualtrics$workerId == subject[i],]
  # survey duration in minutes
  survey_duration <- questTemp$survey_duration
  # filter relevant columns
  temp <- temp[,c("video","trial","workerId","interview_date","noise","action",
                  "scramble","communicative","response","confidence")]
  # temp <- temp[,c("stim","index","workerId","interview_date","noise","action",
  #                 "scramble","communicative","response","confidence")]
  # temp <- data.frame(temp[seq(1,nrow(temp),by=2),],
  #                    confidence=temp$response[seq(2,nrow(temp),by=2)])
  # temp$response <- c(NA,temp$response[1:(nrow(temp)-1)])
  # temp <- temp[!is.na(temp$confidence),]
  # colnames(temp) <- c("video","trial","workerId","interview_date","noise","action",
  #                     "scramble","communicative","response","confidence")
  temp$detection <- ifelse(temp$response=="yes",1,0)
  # temp$correct <- ifelse((1-temp$scramble)==temp$detection,1,0)
  if (i == 1) {
    db <- data.frame(temp,bpe=questTemp$bpe,
                     referential_dich=questTemp$referential_dich,
                     persecution_dich=questTemp$persecution_dich,
                     paranoia_dich=questTemp$paranoia_dich)
    genChar <- data.frame(workerId=subject[i],interview_date=temp$interview_date[1],
                          task_duration=task_duration,
                          survey_duration=survey_duration)
  } else {
    db <- rbind(db,data.frame(temp,bpe=questTemp$bpe,
                              referential_dich=questTemp$referential_dich,
                              persecution_dich=questTemp$persecution_dich,
                              paranoia_dich=questTemp$paranoia_dich))
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

db$correct <- ifelse((1-db$scramble) == db$detection,1,0)

db$trial_type <- as.factor(paste0(db$scramble,db$communicative))
levels(db$trial_type) = c("Signal_IND","Signal_COM","Noise_IND","Noise_COM")

p1 <- ggplot(db, aes(x=trial_type,y=correct,col=paranoia_dich)) +
  labs(title="Detection Agent B",x="Trial Type",y="p(correct)",
       col="Paranoia:") +
  stat_summary(aes(group=workerId), alpha=0.1, size=0.5,
               position = position_dodge(0.2), geom="point") +
  stat_summary(position = position_dodge(0.2)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 30,hjust=1)) 

p2 <- ggplot(db, aes(x=trial_type,y=detection,col=paranoia_dich)) +
  labs(title="Detection Agent B",x="Trial Type",y="p(detection)",
       col="Paranoia:") +
  stat_summary(aes(group=workerId), alpha=0.1, size=0.5,
               position = position_dodge(0.2), geom="point") +
  stat_summary(position = position_dodge(0.2)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 30,hjust=1)) 

p3 <- ggplot(db, aes(x=trial_type,y=confidence,col=paranoia_dich)) +
  labs(title="Confidence (1 to 5)",x="Trial Type",y="Confidence",
       col="Paranoia:") +
  stat_summary(aes(group=workerId), alpha=0.1, size=0.5,
               position = position_dodge(0.2), geom="point") +
  stat_summary(position = position_dodge(0.2)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 30,hjust=1)) 

library(ggpubr)
ggpubr::ggarrange(p2,p3, common.legend = T)



ggplot(db, aes(x=scramble,y=detection,col=paranoia_dich)) + stat_summary()
ggplot(db, aes(x=communicative,y=detection,col=as.factor(scramble))) + stat_summary()
ggplot(db, aes(x=scramble,y=confidence)) + stat_summary()
ggplot(db, aes(x=detection,y=confidence)) + stat_summary()
ggplot(db, aes(x=confidence,y=detection)) + stat_summary()
ggplot(db, aes(x=communicative,y=detection)) + stat_summary()
