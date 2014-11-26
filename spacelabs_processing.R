# Function to import data files from 90207 ABP monitor
# created June 20, 2014
# to do:
#   1.  create count of errors
#   2.  summary statistics (mean daytime, mean nighttime, drop, what else?)

require(lubridate)
require(plyr)
require(dplyr)

# IMPORT DATA ------

files <- list.files("~/Dropbox/APB_Pilot_Data_encrypted/ABP_encrypted",pattern = ".abp", recursive=F, full.names=T) 
length(files) #29
  
spacelabs.import <- function(file) {
  ID <- read.csv(file, fileEncoding="UTF-16LE", header=F, nrows=50, sep=" ", stringsAsFactors=F)
  visit <- ID[1,1]      # assumes that visit number is entered for first name
  studyid <- ID[1,2]    # assumes that study id is entered for last name
  session <- ID[2,1]    # assumes that session id is entered for patient id 
  header.end <- max(which(ID$V1 == "Unknown")) + 1
  nobs <- as.numeric(ID[header.end,1]) # this is not always in the same place, sometimes it's row 8 and sometimes row 9
  BP <- read.csv(file, fileEncoding="UTF-16LE", header=F, skip=51, nrows=nobs) # load BP data 
  BP <- BP[,1:7]
    names(BP) <- c("hour", "minute", "SBP", "MAP", "DBP", "HR", "event_code")
  timestart <-51+nobs
  DS <- read.csv(file, fileEncoding="UTF-16LE", header=F, skip=timestart, nrows=nobs) # load date stamps
    names(DS) <- c( "month", "day","year", "code")
    BP$mstudyid <- studyid
    BP$visit <- visit
    BP$session <- session
    BP$nobs <- nobs
  BP <- cbind(BP, DS)
  BP$date_time <- paste(BP$year, BP$month, BP$day, BP$hour, BP$minute, sep="-")
    BP$date_time <- ymd_hm(BP$date_time)
    BP$is_night <- 0 
    BP$is_night[BP$hour >= 20] <- 1 # after 8 pm
    BP$is_night[BP$hour < 6] <- 1  # before 6 am
    BP$night_ten_four <- 0
  BP$night_ten_four[BP$hour >=22] <- 1
  BP$night_ten_four[BP$hour < 4] <- 1
  BP$day_eight_six <- 0
  BP$day_eight_six[BP$hour >=8 & BP$hour <18] <- 1
  BP$file <- basename(file)
  drop <- names(BP) %in% c("day", "month", "year", "hour", "minute", "NA")
  BP <- BP[!drop] 
  BP
}



ABP_stacked <- ldply(files, spacelabs.import, .progress = "text")  

unique(ABP_stacked$event_code) # check nothing other than "", NA, EE

ABP_stacked$event_code <- ifelse((ABP_stacked$event_code %in% c("", NA)), 0, 1) # change error coding to 0 for no error, 1 for error. (Most files will have "" if no error and "EE" if error, but if there was no error in the entire session all event codes will be NA)

# GENERATE SUMMARY STATISTICS -----

# Error percentages
ABP_errors <- ABP_stacked %.% group_by(file) %.% dplyr:: summarise(sum(event_code), nobs[1], length(SBP)) # compare last 2 columns to see if there are any discrepancies
names(ABP_errors) <- c("file", "errors", "nobs", "nrows")
ABP_errors$percent_errors <- ABP_errors$errors/ABP_errors$nobs * 100


# remove Error rows from the data
ABP_stacked_noerr <- ABP_stacked[ABP_stacked$event_code ==0,]
row.names(ABP_stacked_noerr) <- NULL

# Summary stats for the good data
ABP_summary <- ABP_stacked_noerr %.% group_by(file) %.% dplyr::summarise(mstudyid[1], visit[1], date_time[1], session[1], nobs[1], length(SBP), min(SBP), mean(SBP), max(SBP), sd(SBP), min(DBP), mean(DBP), max(DBP), sd(DBP), mean(MAP), mean(HR), sum(is_night), sum(night_ten_four), sum(day_eight_six))

names(ABP_summary) <- c("file", "mstudyid", "visit", "date_time", "session", "total_obs", "good_obs", "min_SBP", "mean_SBP", "max_SBP", "sd_SBP", "min_DBP", "mean_DBP", "max_DBP", "sd_DBP", "mean_MAP", "mean_HR", "night_obs", "night_ten_four", "day_eight_six")

# add day_obs 
ABP_summary$day_obs <- ABP_summary$total_obs - ABP_summary$night_obs

# add validity (according to IDACO criteria for Asia)
ABP_summary$is_valid <- ifelse(ABP_summary$day_obs >=10 & ABP_summary$night_obs >=5, 1, 0) # 3 out of 29 invalid

# validity using truncated ranges (day = 8a-6p; night = 10p-4a)
ABP_summary$is_valid2 <- ifelse(ABP_summary$day_eight_six >=10 & ABP_summary$night_ten_four >=5, 1, 0) # 5 out of 29 invalid

# change the variables for the file with info entered backward
ABP_summary$mstudyid[ABP_summary$file == "V1,BM1329M ABP020 2014-08-20 07.25.00.abp"] <- "BM1329M"
ABP_summary$visit[ABP_summary$file == "V1,BM1329M ABP020 2014-08-20 07.25.00.abp"] <- "V1"

# SUMMARY STATS----
ABP_stats_overall <- ABP_summary %.% dplyr::summarise(length(file), mean(total_obs), mean(good_obs))
ABP_stats_byvisit <- ABP_summary %.% group_by(visit) %.% dplyr::summarise(length(file), mean(total_obs), mean(good_obs))

# SUMMARY HISTOGRAMS ----
# observations and errors
par(mfrow = c(2,2))
hist(ABP_summary$total_obs, main = paste(nrow(ABP_summary), "ABP Sessions"), xlab = "Number of Total Observations", col = "grey")
hist(ABP_summary$good_obs, main = paste(nrow(ABP_summary), "ABP Sessions"), xlab = "Number of Good Observations", col = "grey")
hist(ABP_errors$errors, main = paste(nrow(ABP_errors), "ABP Sessions"), xlab = "Total Errors per Session", col = "grey")
hist(ABP_errors$percent_errors, main = paste(nrow(ABP_errors), "ABP Sessions"), xlab = "Error Percentage per Session", col = "grey")

# summary BP measures
par(mfrow = c(3,2))
for (i in c(8,9,10, 12,13,14)) {
  hist(ABP_summary[,i], main = paste(nrow(ABP_errors), "ABP Sessions"),xlab = names(ABP_summary[i]), col = "grey")
}


## Time series plots -----

pdf(file= "ABP_plots.pdf", width = 10, height = 10, title = paste("ABP sessions as of", format(Sys.Date(), format = "%b%d")))
par(mfrow = c(3,3))
for (i in 1:length(unique(ABP_stacked_noerr$file))) {
  data <- ABP_stacked_noerr[ABP_stacked_noerr$file == unique(ABP_stacked_noerr$file)[i],]
  plot(data$date_time, data$SBP, pch = 16, ylim = c(20, 160), main = paste(data$mstudyid[1], data$visit[1]), xlab = paste("Time (", round(difftime(data$date_time[nrow(data)], data$date_time[1], units = "hours"), digits = 1), "hours,", nrow(data), "readings)"), ylab = "mmHg", xaxt = "n")
  lines(data$date_time, data$SBP)
  points(data$date_time, data$DBP, pch = 16, col = "blue")
  lines(data$date_time, data$DBP, col = "blue")
  
  # x axis
  hours <- seq(from = ceiling_date(data$date_time[1], unit = "hour"), to = floor_date(data$date_time[nrow(data)], unit = "hour"), by = "hour")
  axis(1, at = hours, labels = format(hours, format = "%H"))
  
  # hypertension lines
  abline(h = 140, lty = "dotted") 
  abline(h = 90, col = "blue", lty = "dotted") 
  
  # add points for errors
  data2 <- ABP_stacked_errors[ABP_stacked_errors$file == unique(ABP_stacked_noerr$file)[i],]
  points(x = data2$date_time, y = rep(25,times = length(data2$date_time)), col = "red")
  
  # add shading for night (21h- 5h)
  for (i in c(5:0, 24:21)) {
  night_start <-  
    ifelse(i %in% hour(hours), i, night_start)
    }
  for (i in c(21:23, 0:5)) {
    night_end <-  
      ifelse(i %in% hour(hours), i, night_end)
  }
  
 rect(xleft = hours[hour(hours) == night_start], ybottom = 15, xright = hours[hour(hours) == night_end], ytop = 165, density = 20, angle = 45,col = "grey") 
legend("topright", legend = c("SBP", "DBP", "err"), pch = c(16, 16, 1), col = c("black", "blue", "red"), cex = 0.68)
}
dev.off()



