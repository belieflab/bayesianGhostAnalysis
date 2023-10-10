rm(list=ls())
# source api
source("api/getSurvey.R")
source("api/getTask.R")

# source R functions
source("functions.R")

# libraries needed (mostly for visualization)
if (!require(ggplot2)) {install.packages("ggplot2")}; library(ggpubr)
if (!require(ggplot2)) {install.packages("ggplot2")}; library(ggplot2)
if (!require(reshape2)) {install.packages("reshape2")}; library(reshape2)
if (!require(lmerTest)) {install.packages("lmerTest")}; library(lmerTest)
if (!require(ggsignif)) {install.packages("ggsignif")}; library(ggsignif)

# check participant status
completion_code <- "42536966"
status <- cloudResearchControl(completion_code)

# get qualtrics
qualtrics <- getSurvey("ghost", label=T) # name is found in surveyIds.R
qualtrics$survey_duration <- qualtrics$EndDate - qualtrics$StartDate

# score questionnaires
qualtrics <- scoreQuestionnaire(qualtrics)
# write.csv(qualtrics,"../data/qualtrics.csv")
qualtrics <- read.csv("../data/qualtrics.csv")

# get merged behaviour 
behaviour <- getTask("ghost", "workerId") # name is found in mongo collections

# clean behaviour
behaviour <- cleanBehaviour(behaviour)
# write.csv(behaviour,"../data/behaviour.csv")
behaviour <- read.csv("../data/behaviour.csv")

# combine behaviour and questionnaire
db <- prepareDataForAnalysis(behaviour, qualtrics)
lf <- db$lf
lwf <- db$lwf
wf <- db$wf

# get frequencies by noise type
noise5 <- prepareDataForAnalysis(behaviour[behaviour$noise==5,], qualtrics)$lwf
noise5$noise<-5
noise10 <- prepareDataForAnalysis(behaviour[behaviour$noise==10,], qualtrics)$lwf
noise10$noise<-10
noise20 <- prepareDataForAnalysis(behaviour[behaviour$noise==20,], qualtrics)$lwf
noise20$noise<-20
lwf <- rbind(noise5,noise10,noise20)

# write.csv(lf,"../data/lf.csv")
# write.csv(wf,"../data/wf.csv")







# # # # Sensitivity # # # #
# transform data
wf2 <- melt(wf,measure.vars = c("com_dprime","ind_dprime"))
wf2$action <- substr(wf2$variable,1,3)
# status
mod1 <- lmer(value~action+(1|workerId),REML=F,wf2); summary(mod1)
summary(lmer(value~action+(1|workerId),REML=F,wf2[wf2$paranoia_dich=="average",]))
summary(lmer(value~action+(1|workerId),REML=F,wf2[wf2$paranoia_dich=="elevated",]))
# get averages
wft3 <- wf2 %>% group_by(action) %>% 
  summarise(mean=mean(value,na.rm=T),std=std(value,na.rm=T))
wft3$action <- ifelse(wft3$action=="com","Communicative","Individual") 
p1 <- ggplot(wft3, aes(x=action,y=mean,fill=action)) + 
  labs(y="Sensitivity (d')",x="Interaction", fill="Rate:") +
  geom_bar(stat="identity", color="black", position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-std, ymax=mean+std), width=.2,
                position=position_dodge(.9)) +
  coord_cartesian(ylim = c(0,0.8)) +
  geom_signif(comparisons=list(c("Communicative","Individual")), annotations = "***",
              y_position = 0.75, tip_length = 0.1, vjust=0.5) +
  # facet_grid(. ~ paranoia_dich) +
  theme_bw() + theme(axis.text.x = element_text(angle = 15, hjust = 1),
                     legend.position = "none")
p1



# # # # Response Criterion # # # #
# transform data
wf2 <- melt(wf,measure.vars = c("com_c","ind_c"))
wf2$action <- substr(wf2$variable,1,3)
# stats
mod2 <- lmer(value~action+(1|workerId),REML=F,wf2); summary(mod2)
# get averages
wft3 <- wf2 %>% group_by(action) %>% 
  summarise(mean=mean(value,na.rm=T),std=std(value,na.rm=T))
wft3$action <- ifelse(wft3$action=="com","Communicative","Individual") 
p2 <- ggplot(wft3, aes(x=action,y=mean,fill=action)) + 
  labs(y="Response Criterion (C)",x="Interaction",fill="Rate:") +
  geom_bar(stat="identity", color="black", position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-std, ymax=mean+std), width=.2,
                position=position_dodge(.9)) +
  coord_cartesian(ylim = c(0,0.8)) +
  geom_signif(comparisons=list(c("Communicative","Individual")), annotations = "ns",
              y_position = 0.45, tip_length = 0.1, vjust=0) +
  # facet_grid(. ~ paranoia_dich) +
  theme_bw() + theme(axis.text.x = element_text(angle = 15, hjust = 1),
                     legend.position = "none")
p2



# # # # Hit and False Alarm rates # # # #
# transform data
wf2 <- melt(wf,measure.vars = c("com_hit","com_fa","ind_hit","ind_fa"))
wf2$action <- substr(wf2$variable,1,3)
wf2$rate <- substr(wf2$variable,5,nchar(as.character(wf2$variable)))
# stats
mod3 <- lmer(value~action*rate+(1|workerId),REML=F,wf2); summary(mod3)
summary(lmer(value~action+(1|workerId),REML=F,wf2[wf2$rate=="fa",]))
summary(lmer(value~action+(1|workerId),REML=F,wf2[wf2$rate=="hit",]))
# get averages
wf3 <- wf2 %>% group_by(action,rate) %>% 
  summarise(mean=mean(value,na.rm=T),std=std(value,na.rm=T))
wf3$rate <- ifelse(wf3$rate=="fa","False Alarm","Hit") 
wf3$action <- ifelse(wf3$action=="com","Communicative","Individual") 
lineSize <- 0.25
p3 <- ggplot(wf3, aes(x=rate,y=mean,fill=action)) + 
  labs(y="Probability",x="Rate",fill="Interaction:") +
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-std, ymax=mean+std), width=.2,
                position=position_dodge(.9)) +
  coord_cartesian(ylim = c(0,0.8)) +
  # false alarm rate
  geom_segment(x=1-lineSize, xend=1+lineSize,y=0.4, yend=0.4) +
  annotate("text",x=1, y=0.45,label="ns") +
  # hit rate
  geom_segment(x=2-lineSize, xend=2+lineSize,y=0.6, yend=0.6) +
  annotate("text",x=2, y=0.62,label="*") +
  # rate
  geom_signif(comparisons=list(c("False Alarm","Hit")), annotations="***",
              y_position = 0.75, tip_length = 0.1, vjust=0.5) +
  # facet_grid(. ~ paranoia_dich) +
  theme_bw() + theme(axis.text.x = element_text(angle = 15, hjust = 1))
p3

# combine plots 1 to 3
fig1 <- ggarrange(ggarrange(p1,p2,ncol=2,labels = c("A","B")),
                  p3,nrow=2,labels = c("","C"))
summary(mod1)
summary(mod2)
summary(mod3)

print_fig <- 0
if (print_fig == 1) {
  ggsave("figures/figure1.png",
         plot = fig1, width = 12, height = 12, units = "cm", dpi = 1800, 
         limitsize = T)
}




# get averages
summary(lmer(freq ~ action + (1|workerId),REML=F,lwf[lwf$cells=="Hit",]))
summary(lmer(freq ~ action + (1|workerId),REML=F,lwf[lwf$cells=="FA",]))
summary(lmer(freq ~ action + (1|workerId),REML=F,lwf[lwf$cells=="Ms",]))
summary(lmer(freq ~ action + (1|workerId),REML=F,lwf[lwf$cells=="CR",]))

lwf2 <- lwf#[lwf$cells=="CR"|lwf$cells=="FA",]
mod <- lmer(freq ~ cells*action+(1|workerId),REML=F,lwf2); summary(mod)

lwf3 <- lwf2 %>% group_by(paranoia_dich,action,cells,noise) %>% 
  summarise(mean=mean(freq,na.rm=T),std=std(freq,na.rm=T))
# lwf3$cells <- factor(lwf3$cells,levels = c("FA","CR"))
lwf3$cells <- factor(lwf3$cells,levels = c("Hit","FA","Ms","CR"))#c("FA","CR","Hit","Ms")

fig2 <- ggplot(lwf3, aes(x=action,y=mean,fill=cells)) + 
  labs(y="Mean Frequency",x="Interaction",fill="Trial Type") +
  geom_bar(stat="identity", color="black", position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-std, ymax=mean+std), width=.2,
                position=position_dodge(.9)) +
  # scale_fill_manual(values = c("blue","green")) +
  # scale_fill_manual(values = c("blue","green","red","grey")) +
  scale_fill_manual(values = c("red","blue","grey","green")) +
  facet_grid(noise ~ paranoia_dich) +
  theme_bw() + theme(axis.text.x = element_text(angle = 15, hjust = 1))
fig2

print_fig <- 0
if (print_fig == 1) {
  ggsave("figures/figure2.png",
         plot = fig1, width = 12, height = 12, units = "cm", dpi = 1800, 
         limitsize = T)
}


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





# # # # Basic raw Visualization # # # #
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


ggarrange(p2,p3, common.legend = T)