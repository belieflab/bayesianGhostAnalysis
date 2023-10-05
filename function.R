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

scoreQuestionnaire <- function(qualtrics) {
  
  qualtrics <- as.data.frame(qualtrics)
  
  # # # # Revised Green Paranoid Thought Scale # # # #
  cols_rgpts <- colnames(qualtrics)[grepl("rgpts_",colnames(qualtrics))]
  # reference (A)
  referential <- qualtrics[,grepl("rgpts_referential",colnames(qualtrics)),]
  referential <- referential-1
  qualtrics$referential <- rowSums(referential)
  qualtrics$referential - qualtrics$referential - ncol(referential)
  qualtrics$referential_dich <- ifelse(qualtrics$referential <= 9,"average","elevated")
  # persecution (B)
  persecution <- qualtrics[,grepl("rgpts_persecution",colnames(qualtrics)),]
  persecution <- persecution-1
  qualtrics$persecution <- rowSums(persecution)
  qualtrics$persecution - qualtrics$persecution - ncol(persecution)
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
  pdi <- as.data.frame(ifelse(pdi == 1,1,0))
  # sum all items  
  qualtrics$pdi <- rowSums(pdi,na.rm=T)/length(cols_pdi)
  
  # return
  return(qualtrics)
}
