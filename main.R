rm(list=ls())
# source api
source("api/getSurvey.R")
source("api/getTask.R")

# source R functions
source("functions.R")

# check participant status
completion_code <- "42536966"
status <- cloudResearchControl(completion_code)

# get qualtrics
qualtrics <- getSurvey("ghost", label=T) # name is found in surveyIds.R
qualtrics$survey_duration <- qualtrics$EndDate - qualtrics$StartDate

# score questionnaires
qualtrics <- scoreQuestionnaire(qualtrics)

# get merged behaviour 
behaviour <- getTask("ghost", "workerId") # name is found in mongo collections

# clean behaviour
behaviour <- cleanBehaviour(behaviour)

# combine behaviour and questionnaire
db <- prepareDataForAnalysis(behaviour, qualtrics)
lf <- db$lf
wf <- db$wf



# # # # Basic raw Visualization # # # #
if (!require(ggplot2)) {install.packages("ggplot2")}; library(ggplot2)
table(lf$scramble)
table(lf$communicative)
table(lf$detection)
hist(lf$confidence)

p1 <- ggplot(lf, aes(x=trial_type,y=correct,col=paranoia_dich)) +
  labs(title="Detection Agent B",x="Trial Type",y="p(correct)",
       col="Paranoia:") +
  # stat_summary(aes(group=workerId), alpha=0.1, size=0.5,
  #              position = position_dodge(0.2), geom="point") +
  stat_summary(position = position_dodge(0.2)) +
  theme_classic() + facet_grid(noise ~ .) +
  theme(axis.text.x = element_text(angle = 30,hjust=1)) 

p2 <- ggplot(lf, aes(x=trial_type,y=detection,col=paranoia_dich)) +
  labs(title="Detection Agent B",x="Trial Type",y="p(detection)",
       col="Paranoia:") +
  # stat_summary(aes(group=workerId), alpha=0.1, size=0.5,
  #              position = position_dodge(0.2), geom="point") +
  stat_summary(position = position_dodge(0.2)) +
  theme_classic() + facet_grid(noise ~ .) +
  theme(axis.text.x = element_text(angle = 30,hjust=1)) 

p3 <- ggplot(lf, aes(x=trial_type,y=confidence,col=paranoia_dich)) +
  labs(title="Confidence (1 to 5)",x="Trial Type",y="Confidence",
       col="Paranoia:") +
  # stat_summary(aes(group=workerId), alpha=0.1, size=0.5,
  #              position = position_dodge(0.2), geom="point") +
  stat_summary(position = position_dodge(0.2)) +
  theme_classic() + facet_grid(noise ~ .) +
  theme(axis.text.x = element_text(angle = 30,hjust=1)) 

library(ggpubr)
ggpubr::ggarrange(p2,p3, common.legend = T)


library(reshape2)
library(lmerTest)



# # # # Sensitivity # # # #
wf2 <- melt(wf,measure.vars = c("com_dprime","ind_dprime"))
wf2$action <- substr(wf2$variable,1,3)

mod1 <- lmer(value~action+(1|workerId),REML=F,wf2); summary(mod1)

wft3 <- wf2 %>% group_by(action) %>% 
  summarise(mean=mean(value,na.rm=T),std=std(value,na.rm=T))
wft3$action <- ifelse(wft3$action=="com","Communicative","Individual") 
p1 <- ggplot(wft3, aes(x=action,y=mean,fill=action)) + 
  labs(y="Sensitivity (d')",x="Interaction", fill="Rate:") +
  geom_bar(stat="identity", color="black", position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-std, ymax=mean+std), width=.2,
                position=position_dodge(.9)) +
  theme_bw() + theme(axis.text.x = element_text(angle = 15, hjust = 1),
                     legend.position = "none")
p1



# # # # Response Criterion # # # #
wf2 <- melt(wf,measure.vars = c("com_c","ind_c"))
wf2$action <- substr(wf2$variable,1,3)

mod2 <- lmer(value~action+(1|workerId),REML=F,wf2); summary(mod2)

wft3 <- wf2 %>% group_by(action) %>% 
  summarise(mean=mean(value,na.rm=T),std=std(value,na.rm=T))
wft3$action <- ifelse(wft3$action=="com","Communicative","Individual") 
p2 <- ggplot(wft3, aes(x=action,y=mean,fill=action)) + 
  labs(y="Response Criterion (C)",x="Interaction",fill="Rate:") +
  geom_bar(stat="identity", color="black", position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-std, ymax=mean+std), width=.2,
                position=position_dodge(.9)) +
  theme_bw() + theme(axis.text.x = element_text(angle = 15, hjust = 1),
                     legend.position = "none")
p2



# # # # Hit and False Alarm rates # # # #
wf2 <- melt(wf,measure.vars = c("com_hit","com_fa","ind_hit","ind_fa"))
wf2$action <- substr(wf2$variable,1,3)
wf2$rate <- substr(wf2$variable,5,nchar(as.character(wf2$variable)))

mod3 <- lmer(value~action*rate+(1|workerId),REML=F,wf2); summary(mod3)

wf3 <- wf2 %>% group_by(action,rate) %>% 
  summarise(mean=mean(value,na.rm=T),std=std(value,na.rm=T))
wf3$rate <- ifelse(wf3$rate=="fa","False Alarm","Hit") 
wf3$action <- ifelse(wf3$action=="com","Communicative","Individual") 
p3 <- ggplot(wf3, aes(x=rate,y=mean,fill=action)) + 
  labs(y="Probability",x="Rate",fill="Interaction:") +
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-std, ymax=mean+std), width=.2,
                position=position_dodge(.9)) +
  # geom_signif(stat="identity",
  #             data=data.frame(x=c(0.875, 1.875), xend=c(1.125, 2.125),
  #                             y=c(5.8, 8.5), annotation=c("**", "NS")),
              # aes(x=x,xend=xend, y=y, yend=y, annotation=annotation)) +
  # geom_signif(comparisons=list(c("S1", "S2")), annotations="***",
  #             y_position = 9.3, tip_length = 0, vjust=0.4) +
  theme_bw() + theme(axis.text.x = element_text(angle = 15, hjust = 1))
p3


ggarrange(ggarrange(p1,p2,ncol=2,labels = c("A","B")),
          p3,nrow=2,labels = c("","C"))

summary(mod1)
summary(mod2)
summary(mod3)


mod <- lmer(value~action+(1|workerId),REML=F,wf2[wf2$rate == "fa",]); summary(mod)
mod <- lmer(value~action+(1|workerId),REML=F,wf2[wf2$rate == "hit",]); summary(mod)

p3 <- ggplot(wf3, aes(x=action,y=mean,fill=rate)) + 
  labs(y="Probability",x="Rate",fill="Rate:") +
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-std, ymax=mean+std), width=.2,
                position=position_dodge(.9)) +
  scale_fill_manual(values = c("blue","green")) +
  # geom_signif(stat="identity",
  #             data=data.frame(x=c(0.875, 1.875), xend=c(1.125, 2.125),
  #                             y=c(5.8, 8.5), annotation=c("**", "NS")),
  # aes(x=x,xend=xend, y=y, yend=y, annotation=annotation)) +
  # geom_signif(comparisons=list(c("S1", "S2")), annotations="***",
  #             y_position = 9.3, tip_length = 0, vjust=0.4) +
  theme_bw() + theme(axis.text.x = element_text(angle = 15, hjust = 1))
p3

