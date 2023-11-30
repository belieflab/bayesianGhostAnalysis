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
