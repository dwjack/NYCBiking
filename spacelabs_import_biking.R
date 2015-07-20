# Function to import data files from 90207 ABP monitor
# created Dec 18 2014

require(lubridate)
require(plyr)
require(dplyr)

# IMPORT DATA ------

file <- "~/Dropbox/APB_Pilot_Data_encrypted/ABP_encrypted/BM1255M,V2\ ABP015\ 2014-08-12\ 10.44.00.abp"

spacelabs.import <- function(file) {
  ID <- read.csv(file, fileEncoding="UTF-16LE", header=F, nrows=50, sep=" ", stringsAsFactors=F)
  #visit <- ID[1,1]      # assumes that visit number is entered for first name
  studyid <- ID[1,2]    # assumes that study id is entered for last name
  #session <- ID[2,1]    # assumes that session id is entered for patient id 
  header.end <- max(which(ID$V1 == "Unknown")) + 1
  nobs <- as.numeric(ID[header.end,1]) # this is not always in the same place, sometimes it's row 8 and sometimes row 9
  BP <- read.csv(file, fileEncoding="UTF-16LE", header=F, skip=51, nrows=nobs) # load BP data 
  BP <- BP[,1:7]
  names(BP) <- c("hour", "minute", "SBP", "MAP", "DBP", "HR", "event_code")
  timestart <-51+nobs
  DS <- read.csv(file, fileEncoding="UTF-16LE", header=F, skip=timestart, nrows=nobs) # load date stamps
  names(DS) <- c( "month", "day","year", "code")
  BP$studyid <- studyid
  #BP$visit <- visit
  #BP$session <- session
  BP$nobs <- nobs
  BP <- cbind(BP, DS)
  BP$date_time <- paste(BP$year, BP$month, BP$day, BP$hour, BP$minute, sep="-")
  BP$date_time <- ymd_hm(BP$date_time)
  BP$file <- basename(file)
  drop <- names(BP) %in% c("day", "month", "year", "hour", "minute", "NA")
  BP <- BP[!drop] 
  BP
}



BP <- spacelabs.import(file)
variable.names(BP)

