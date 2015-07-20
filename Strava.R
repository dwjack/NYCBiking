# plots GPX data from Strava
# follows
# https://gist.github.com/mollietaylor/4210660


library(maptools)
library(plotKML) 
library(ggmap)
library(raster)
library(lubridate)
library(sp)
library(zoo)

file <- "~/Downloads/activities/20121022-215814-Ride.gpx"  #  file to plot, as downloaded from Strava
route<-readGPX(file)
route <- as.data.frame(route$tracks)  #  extract the relevant info from the list
names(route)<-c("lon", "lat", "ele", "time")


mapImageData <- get_googlemap(center = c(lon = median(route[,1]), lat = median(route[,2])),
                              zoom = 12,
                              #  size = c(500,500),
                              maptype = c("terrain"))



ggmap(mapImageData,
      extent = "device") + # takes out axes, etc.
  geom_point(aes(x = lon,
                 y = lat),
             data = route,
             colour = "blue",
             size = 1,
             pch = 20)


