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

# check cloudResearch completion code csv
cloudResearchControl <- function(completion_code) {
  files <- list.files("cloudResearch/")
  control <- lapply(paste0("cloudResearch/",files), read.csv)
  # combine list if multiple controler files (multiple cloudResearch studies)
  for (i in 1:length(files)) {
    if (i == 1) {
      db <- control[[i]]
    } else {
      db <- rbind(db,control[[i]])
    }
  }
  # how many have good completion_code
  good <- db[db$Actual.Completion.Code == completion_code &
               !is.na(db$Actual.Completion.Code),]
  bad <- db[db$Actual.Completion.Code != completion_code |
              is.na(db$Actual.Completion.Code),]
  message(paste0(nrow(good), " are good participants"))
  message(paste0(sum(bad$ApprovalStatus=="Not Submitted"), 
                 " participants are Not Submitted"))
  message(paste0(sum(bad$ApprovalStatus=="Pending"), 
                 " participants are bad and Pending"))
  goodIds <- good$AmazonIdentifier
  badIds <- bad$AmazonIdentifier[bad$ApprovalStatus=="Pending"]
  return(list(goodIds=goodIds,badIds=badIds))
}

scoreQuestionnaire <- function(qualtrics) {
  # as data frame
  qualtrics <- as.data.frame(qualtrics)
  
  # # # # Revised Green Paranoid Thought Scale # # # #
  cols_rgpts <- colnames(qualtrics)[grepl("rgpts_",colnames(qualtrics))]
  # reference (A)
  referential <- qualtrics[,grepl("rgpts_referential",colnames(qualtrics)),]
  # referential <- referential-1 # if qualtrics is imported as values and not strings
  qualtrics$referential <- rowSums(referential)
  # qualtrics$referential - qualtrics$referential - ncol(referential) # if qualtrics is imported as values and not strings
  qualtrics$referential_dich <- ifelse(qualtrics$referential <= 9,"average","elevated")
  # persecution (B)
  persecution <- qualtrics[,grepl("rgpts_persecution",colnames(qualtrics)),]
  # persecution <- persecution-1 # if qualtrics is imported as values and not strings
  qualtrics$persecution <- rowSums(persecution)
  # qualtrics$persecution - qualtrics$persecution - ncol(persecution) # if qualtrics is imported as values and not strings
  qualtrics$persecution_dich <- ifelse(qualtrics$persecution <= 5,"average","elevated")
  # paranoia (A+B)
  qualtrics$paranoia <- qualtrics$referential + qualtrics$persecution
  # at least one subscale elevated
  qualtrics$paranoia_dich <- ifelse(qualtrics$referential_dich == "elevated" |
                                          qualtrics$persecution_dich == "elevated",
                                        "elevated","average")
  
  # # # # Beliefs in Purpose of Events # # # #
  # Lindeman, M., & Aarnio, K. (2007). Superstitious, magical, and paranormal beliefs: An integrative model. Journal of research in Personality, 41(4), 731-744.
  cols_bpe <- colnames(qualtrics)[grepl("bpe_",colnames(qualtrics))]
  cols_bpe <- cols_bpe[grepl("_1",cols_bpe)]
  # sum all items  
  qualtrics$bpe <- rowSums(qualtrics[,cols_bpe])/length(cols_bpe)
  
  # # # # Peters Delusion Inventory # # # #
  cols_pdi <- colnames(qualtrics)[grepl("pdi_Q2#1",colnames(qualtrics))]
  # get pdi 
  pdi <- qualtrics[,cols_pdi]
  pdi <- ifelse(pdi == "YES",1,0)
  # pdi <- as.data.frame(ifelse(pdi == 1,1,0))
  # sum all items  
  qualtrics$pdi <- rowSums(pdi,na.rm=T)/length(cols_pdi)
  
  # return
  return(qualtrics)
}

cleanBehaviour <- function(behaviour) {
  # create subjects vector
  subject <- unique(behaviour$workerId)
  for (i in 1:length(subject)) {
    temp <- behaviour[behaviour$workerId==subject[i],]
    # filter relevant columns
    temp <- temp[!is.na(temp$index),c("measure","visit","interview_date","workerId",
                                      "time_elapsed","stim","index","noise","action",
                                      "scramble","communicative","response","confidence")]
    # temp <- data.frame(temp[seq(1,nrow(temp),by=2),],
    #                    confidence=temp$response[seq(2,nrow(temp),by=2)])
    temp$response <- c("",temp$response[1:(nrow(temp)-1)])
    temp <- temp[!is.na(temp$confidence),]
    colnames(temp) <- c("measure","visit","interview_date","workerId",
                        "time_elapsed","video","trial","noise","action","scramble",
                        "communicative","response","confidence")
    temp$detection <- ifelse(temp$response=="yes",1,0)
    #temp$confidence <- as.integer(temp$confidence)
    temp$correct <- ifelse((1-temp$scramble)==temp$detection,1,0)
    # combine ith subject
    if (i == 1) {
      db <- temp
    } else {
      db <- rbind(db,temp)
    }
  }
  db$video <- basename(db$video)
  # function output 
  return(db)
}

prepareDataForAnalysis <- function (behaviour, qualtrics) {
  # create subjects vector
  subject <- unique(behaviour$workerId)
  
  # loop subjects to get task duration 
  for (i in 1:length(subject)) {
    temp <- behaviour[behaviour$workerId == subject[i],]
    # task duration in minutes
    task_duration <- (max(temp$time_elapsed)/1000)/60
    # task_duration <- NA
    # get subject i qualtrics scores
    questTemp <- qualtrics[qualtrics$workerId == subject[i],]
    # survey duration in minutes
    survey_duration <- questTemp$survey_duration
    # filter relevant columns
    temp <- temp[,c("video","trial","workerId","interview_date","noise","action",
                    "scramble","communicative","response","confidence")]
    # create SDT cells
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
    
    # temp <- temp[,c("stim","index","workerId","interview_date","noise","action",
    #                 "scramble","communicative","response","confidence")]
    # temp <- data.frame(temp[seq(1,nrow(temp),by=2),],
    #                    confidence=temp$response[seq(2,nrow(temp),by=2)])
    # temp$response <- c(NA,temp$response[1:(nrow(temp)-1)])
    # temp <- temp[!is.na(temp$confidence),]
    # colnames(temp) <- c("video","trial","workerId","interview_date","noise","action",
    #                     "scramble","communicative","response","confidence")
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
                       referential_dich=questTemp$referential_dich,
                       persecution_dich=questTemp$persecution_dich,
                       paranoia_dich=questTemp$paranoia_dich)
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
                       referential_dich=questTemp$referential_dich,
                       persecution_dich=questTemp$persecution_dich,
                       paranoia_dich=questTemp$paranoia_dich)
    } else {
      lf <- rbind(lf,data.frame(temp,bpe=questTemp$bpe,
                                referential_dich=questTemp$referential_dich,
                                persecution_dich=questTemp$persecution_dich,
                                paranoia_dich=questTemp$paranoia_dich))
      wf <- rbind(wf, wf <- data.frame(workerId=subject[i],interview_date=temp$interview_date[1],
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
                                       referential_dich=questTemp$referential_dich,
                                       persecution_dich=questTemp$persecution_dich,
                                       paranoia_dich=questTemp$paranoia_dich))
    }
  }
  # function output 
  return(list(lf=lf,wf=wf))
}

sdtModel <- function (data, events) {
  # NOTE: events must be ordered as follows: hit, FA, Ms, CR
  # SDT cell frequencies 
  sdtTable <- colSums(data$cells==t(matrix(rep(events,nrow(data)),ncol=nrow(data))))
  hit_rate <- sdtTable[1]/(sdtTable[1]+sdtTable[3])
  fa_rate <- sdtTable[2]/(sdtTable[2]+sdtTable[4])
  # http://wise.cgu.edu/wise-tutorials/tutorial-signal-detection-theory/signal-detection-d-defined-2/
  if (hit_rate == 0 | is.nan(hit_rate)) {
    hit_rate <- 1/nrow(data)
  } else if (hit_rate == 1) {
    hit_rate <- (nrow(data)-1)/nrow(data)
  }
  if (fa_rate == 0 | is.nan(fa_rate)) {
    fa_rate <- 1/nrow(data)
  } else if (fa_rate == 1) {
    fa_rate <- (nrow(data)-1)/nrow(data)
  }
  # sensitivity (d')
  sensitivity <- qnorm(hit_rate) - qnorm(fa_rate)
  # response criterion
  response_criterion  <- -1*(qnorm(hit_rate) + qnorm(fa_rate)) / 2
  # prepare output
  names(sdtTable) <- events
  # return list
  return(list(sensitivity=sensitivity,response_criterion=response_criterion,
              sdtTable=sdtTable,hit_rate=hit_rate,fa_rate=fa_rate))
}

# function for standard error
std <- function(x,na.rm) sd(x,na.rm=na.rm)/sqrt(length(x))
