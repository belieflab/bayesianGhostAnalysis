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


db <- prepareDataForAnalysis(behaviour, qualtrics)
lf <- db$lf
wf <- db$wf



# # # # Basic raw Viualization # # # #
if (!require(ggplot2)) {install.packages("ggplot2")}; library(ggplot2)
table(lf$scramble)
table(lf$communicative)
table(lf$detection)
hist(lf$confidence)

p1 <- ggplot(lf, aes(x=trial_type,y=correct,col=paranoia_dich)) +
  labs(title="Detection Agent B",x="Trial Type",y="p(correct)",
       col="Paranoia:") +
  stat_summary(aes(group=workerId), alpha=0.1, size=0.5,
               position = position_dodge(0.2), geom="point") +
  stat_summary(position = position_dodge(0.2)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 30,hjust=1)) 

p2 <- ggplot(lf, aes(x=trial_type,y=detection,col=paranoia_dich)) +
  labs(title="Detection Agent B",x="Trial Type",y="p(detection)",
       col="Paranoia:") +
  stat_summary(aes(group=workerId), alpha=0.1, size=0.5,
               position = position_dodge(0.2), geom="point") +
  stat_summary(position = position_dodge(0.2)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 30,hjust=1)) 

p3 <- ggplot(lf, aes(x=trial_type,y=confidence,col=paranoia_dich)) +
  labs(title="Confidence (1 to 5)",x="Trial Type",y="Confidence",
       col="Paranoia:") +
  stat_summary(aes(group=workerId), alpha=0.1, size=0.5,
               position = position_dodge(0.2), geom="point") +
  stat_summary(position = position_dodge(0.2)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 30,hjust=1)) 

library(ggpubr)
ggpubr::ggarrange(p2,p3, common.legend = T)


wf


ggplot(lf, aes(x=scramble,y=detection,col=paranoia_dich)) + stat_summary()
ggplot(lf, aes(x=communicative,y=detection,col=as.factor(scramble))) + stat_summary()
ggplot(lf, aes(x=scramble,y=confidence)) + stat_summary()
ggplot(lf, aes(x=detection,y=confidence)) + stat_summary()
ggplot(lf, aes(x=confidence,y=detection)) + stat_summary()
ggplot(lf, aes(x=communicative,y=detection)) + stat_summary()
