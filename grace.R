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


# # # # Death Anxiety Beliefs and Behaviours Scale # # # #
# identify all columns from the questionnaire
dabbs <- data.frame(workerId=qualtrics$workerId, qualtrics[,grepl("dabbs",names(qualtrics))])

dabbs$dabbs_fear_total <- rowSums(dabbs_fear[, grepl("fear", names(dabbs))])

# get the sum of fear columns
qualtrics$dabbs_fear <- rowSum(qualtrics[,cols_fear])
# your task will be to get the subscales for tba and avoid
