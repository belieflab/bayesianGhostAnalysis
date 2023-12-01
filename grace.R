rm(list=ls())
# source api
source("api/getSurvey.R")
source("api/getTask.R")

# source R functions
source("functions.R")

# libraries needed (mostly for visualization)
loadPackages(libraries = c("ggplot2","ggpubr","reshape2","lmerTest","ggsignif"))

# check participant status
completion_code <- "42536966"
status <- cloudResearchControl(completion_code)

# get qualtrics
qualtrics <- getSurvey("ghost", label=F) # name is found in surveyIds.R
# get survey_duration in minutes
qualtrics$duration_minutes <- qualtrics$`Duration (in seconds)`/60
# remove non valid workerIds and NAs
qualtrics <- qualtrics[qualtrics$Progress==100,]

# as data frame
qualtrics <- as.data.frame(qualtrics)

# combine dataframes
qualtrics <- scoreQuestionnaire(qualtrics)

behaviour <- read.csv("/Users/gracehart/behaviour.csv")

db <- prepareDataForAnalysis(behaviour, qualtrics)
lf <- db$lf
lwf <- db$lwf
wf <- db$wf

# combine behaviour and qualtrics data.frames
prepareDataForAnalysis <- function (behaviour, qualtrics) {
  # create subjects vector
  subject <- unique(behaviour$workerId)
  
  # loop subjects to get task duration 
  for (i in 1:length(subject)) {
    temp <- behaviour[behaviour$workerId == subject[i],]
    # task duration in minutes
    task_duration <- (max(temp$time_elapsed)/1000)/60
    # get subject i qualtrics scores
    questTemp <- qualtrics[qualtrics$workerId == subject[i],]
    # survey duration in minutes
    survey_duration <- questTemp$duration_minutes
    # filter relevant columns
    temp <- temp[,c("video","trial","workerId","interview_date","noise","action",
                    "scramble","communicative","response","confidence")]
    # create Signal Detection Theory (SDT) cells
    temp$cells <- factor(paste0(temp$scramble,temp$response),
                         levels=c("0yes","1yes","0no","1no"))
    # correct label them: hit, false alarm, miss, correct rejection
    levels(temp$cells) <- c("Hit","FA","Ms","CR")
    
    # get SDT parameters for all, COM, and IND trials
    all <- sdtModel(temp, events = c("Hit","FA","Ms","CR"))
    COM <- temp[temp$communicative == 1,]
    COM <- sdtModel(COM, events = c("Hit","FA","Ms","CR"))
    IND <- temp[temp$communicative == 0,]
    IND <- sdtModel(IND, events = c("Hit","FA","Ms","CR"))
    
    # detection binary variable
    temp$detection <- ifelse(temp$response=="yes",1,0)
    # correct binary variable
    temp$correct <- ifelse((1-temp$scramble) == temp$detection,1,0)
    # trial type based on "Seeing Bayesian Ghost" from Lisa.
    temp$trial_type <- factor(paste0(temp$scramble,temp$communicative),
                              levels = c("00","01","10","11"))
    levels(temp$trial_type) = c("Signal_IND","Signal_COM","Noise_IND","Noise_COM")
    # combine multiple subjects
    if (i == 1) {
      lf <- data.frame(temp,bpe=questTemp$bpe,
                       paranoia_dich=questTemp$persecution_dich,
                       dabbs_fear=questTemp$dabbs_fear) # Grace & Santiago (22/11/2023)
      lwf <- data.frame(workerId=subject[i],bpe=questTemp$bpe,
                        paranoia_dich=questTemp$persecution_dich,
                        action=rep(c("COM","IND"),each=4),
                        cells=names(c(COM$sdtTable,IND$sdtTable)),
                        freq=c(COM$sdtTable,IND$sdtTable))
      wf <- data.frame(workerId=subject[i],interview_date=temp$interview_date[1],
                       task_duration=task_duration,
                       survey_duration=survey_duration,
                       pCorrect=mean(temp$correct,na.rm = T),
                       # communicative
                       com_dprime=COM$sensitivity,
                       com_c=COM$response_criterion,
                       com_hit=COM$hit_rate,
                       com_fa=COM$fa_rate,
                       # individual
                       ind_dprime=IND$sensitivity,
                       ind_c=IND$response_criterion,
                       ind_hit=IND$hit_rate,
                       ind_fa=IND$fa_rate,
                       # questionnaires
                       bpe=questTemp$bpe,
                       paranoia_dich=questTemp$persecution_dich)
    } else {
      lf <- rbind(lf,data.frame(temp,bpe=questTemp$bpe,
                                paranoia_dich=questTemp$persecution_dich,
                                dabbs_fear=questTemp$dabbs_fear)) # Grace & Santiago (22/11/2023)
      lwf <- rbind(lwf,data.frame(workerId=subject[i],bpe=questTemp$bpe,
                                  paranoia_dich=questTemp$persecution_dich,
                                  action=rep(c("COM","IND"),each=4),
                                  cells=names(c(COM$sdtTable,IND$sdtTable)),
                                  freq=c(COM$sdtTable,IND$sdtTable)))
      wf <- rbind(wf,data.frame(workerId=subject[i],interview_date=temp$interview_date[1],
                                task_duration=task_duration,
                                survey_duration=survey_duration,
                                pCorrect=mean(temp$correct,na.rm = T),
                                # communicative
                                com_dprime=COM$sensitivity,
                                com_c=COM$response_criterion,
                                com_hit=COM$hit_rate,
                                com_fa=COM$fa_rate,
                                # individual
                                ind_dprime=IND$sensitivity,
                                ind_c=IND$response_criterion,
                                ind_hit=IND$hit_rate,
                                ind_fa=IND$fa_rate,
                                # questionnaires
                                bpe=questTemp$bpe,
                                paranoia_dich=questTemp$persecution_dich))
    }
  }
  # function output 
  return(list(lf=lf,lwf=lwf,wf=wf))
}

# combine multiple files in the same location (file must have the same columns) 
rawPooler <- function (data_location = NULL) {
  # get all files names from location
  data_files <- list.files(data_location)
  # filter only by csvs
  data_files <- data_files[grepl(".csv",data_files)]
  # read all subjects in a list
  raw_data_list <- lapply(paste0(data_location,data_files), read.csv)
  # combine rbind elements within the list
  raw_data_dataframe <- dplyr::bind_rows(raw_data_list)
  # get output
  return(raw_data_dataframe)
}

# # # # Death Anxiety Beliefs and Behaviours Scale # # # #
## Identify all columns from the questionnaire
dabbs <- data.frame(workerId=qualtrics$workerId, qualtrics[,grepl("dabbs",names(qualtrics))])

dabbs$dabbs_fear_total <- rowSums(dabbs[, grepl("fear", names(dabbs))])
hist(dabbs$dabbs_fear_total)

## Sum of Fear Columns

# Convert specific columns to numeric
qualtrics[, c("dabbs_fear_1", "dabbs_fear_2", "dabbs_fear_3", "dabbs_fear_4")] <- 
  lapply(qualtrics[, c("dabbs_fear_1", "dabbs_fear_2", "dabbs_fear_3", "dabbs_fear_4")], as.numeric)

# Now you can calculate the row sums
cols_fear <- c("dabbs_fear_1", "dabbs_fear_2", "dabbs_fear_3", "dabbs_fear_4")
qualtrics$dabbs_fear_sum <- rowSums(qualtrics[, cols_fear], na.rm = TRUE)
print(qualtrics$dabbs_fear_sum)

## Sum of TBA Columns

# Convert specific columns to numeric
qualtrics[, c("dabbs_tba_1", "dabbs_tba_2", "dabbs_tba_3", "dabbs_tba_4", "dabbs_tba_5", "dabbs_tba_6", "dabbs_tba_7")] <- 
  lapply(qualtrics[, c("dabbs_tba_1", "dabbs_tba_2", "dabbs_tba_3", "dabbs_tba_4", "dabbs_tba_5", "dabbs_tba_6", "dabbs_tba_7")], as.numeric)

# Now you can calculate the row sums
cols_tba <- c("dabbs_tba_1", "dabbs_tba_2", "dabbs_tba_3", "dabbs_tba_4", "dabbs_tba_5", "dabbs_tba_6", "dabbs_tba_7")
qualtrics$dabbs_tba_sum <- rowSums(qualtrics[, cols_tba], na.rm = TRUE)
print(qualtrics$dabbs_tba_sum)

## Sum of Avoid Columns
# Convert specific columns to numeric
qualtrics[, c("dabbs_avoid_1", "dabbs_avoid_2", "dabbs_avoid_3", "dabbs_avoid_4", "dabbs_avoid_5", "dabbs_avoid_6", "dabbs_avoid_7")] <- 
  lapply(qualtrics[, c("dabbs_avoid_1", "dabbs_avoid_2", "dabbs_avoid_3", "dabbs_avoid_4", "dabbs_avoid_5", "dabbs_avoid_6", "dabbs_avoid_7")], as.numeric)

# Now you can calculate the row sums
cols_avoid <- c("dabbs_avoid_1", "dabbs_avoid_2", "dabbs_avoid_3", "dabbs_avoid_4", "dabbs_avoid_5", "dabbs_avoid_6", "dabbs_avoid_7")
qualtrics$dabbs_avoid_sum <- rowSums(qualtrics[, cols_avoid], na.rm = TRUE)
print(qualtrics$dabbs_avoid_sum)

## LRM Models
  
library(lmerTest)
m1 <- glmer(detection ~ paranoia_dich * communicative + (1|workerId), family=binomial, lf[lf$scramble==1,])
summary(m1)

# lf$bpe_high <- ifelse(lf$bpe > median(lf$bpe), "high", "low")
m2 <- glmer(detection ~ bpe * communicative + (1|workerId), family=binomial, lf[lf$scramble==1,])
summary(m2)

# dabbs_fear LRM : lf$dabbs_fear_high <- ifelse(lf$dabbs_fear > median(lf$dabbs_fear), "high", "low")
m3 <- glmer(detection ~ dabbs_fear * communicative+(1|workerId), family=binomial, lf[lf$scramble==1,])
summary(m3)

# dabbs_tba LRM :
m4 <- glmer(detection ~ dabbs_tba * communicative + (1|workerId), family=binomial, lf[lf$scramble==1,])
summary(m4)

# dabbs_behaviors LRM :
m5 <- glmer(detection ~ dabbs_behaviors * communicative + (1|workerId), family=binomial, lf[lf$scramble==1,])
summary(m5)


