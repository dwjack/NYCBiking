file <- "~/Desktop/Desktop/desktop/temp/DJ_bike_study_dry_run_2.txt"

  
pdl <- read.table(file, skip = 12, fill = TRUE, allowEscapes = FALSE,   row.names = NULL )

pdl2 <- pdl[complete.cases(pdl),]

names(pdl) <- c("time", "work", "VO2/kg", "VE", "Vt", "RR", "RQ", "HR", "level")

head(pdl)

pdl$VE <- as.numeric(levels(pdl$VE))[pdl$VE]
pdl$HR <- as.numeric(levels(pdl$HR))[pdl$HR]
pdl$RR <- as.numeric(levels(pdl$RR))[pdl$RR]
pdl$Vt <- as.numeric(levels(pdl$Vt))[pdl$Vt]


plot(pdl$RR)
plot(pdl$Vt)
