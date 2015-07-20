# script for reading Pulmonary Diagnostics data into R

# replace with correct file path
file <- "~/Desktop/Desktop/desktop/temp/DJ_bike_study_dry_run_2.txt"

# read the data in to a dataframe called pdl
  
pdl <- read.table(file, skip = 12, fill = TRUE, allowEscapes = FALSE,   row.names = NULL )

# note that this reads in the header information from each page
# the following line drops that information

pdl <- pdl[complete.cases(pdl),]

names(pdl) <- c("time", "work", "VO2/kg", "VE", "Vt", "RR", "RQ", "HR", "level")

# add a column for rider ID


head(pdl)

pdl$VE <- as.numeric(levels(pdl$VE))[pdl$VE]
pdl$HR <- as.numeric(levels(pdl$HR))[pdl$HR]
pdl$RR <- as.numeric(levels(pdl$RR))[pdl$RR]
pdl$Vt <- as.numeric(levels(pdl$Vt))[pdl$Vt]


plot(pdl$RR)
plot(pdl$Vt)
