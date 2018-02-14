###########################################################
# FUNCTION TO DOWNLOAD CDEC DATA FROM GOVERNMENT COMPUTER #
###########################################################
# Author: Vanessa Tobias, CDFW
# Contact: vanessa.tobias@wildlife.ca.gov or vanessadtobias@gmail.com
# This code was modified from sharpshootR::CDECquery
# Date last updated: 2016-07-26
# Modifications:
#  1. download from cdec4gov.water.ca.gov instead of the public cdec site
#  2. return only 'datetime' and the sensor reading ('value')
#  3. rename 'value' with the sensor number (with the prefix "s") to facilitate merging

library(sharpshootR)
CDEC4govquery <- function (id, sensor, interval = "D", start, end) 
{
  opt.original <- options(stringsAsFactors = FALSE)
  if (missing(id) | missing(sensor) | missing(start) | missing(end)) 
    stop("missing arguments", call. = FALSE)
  u <- paste0("http://cdec4gov.water.ca.gov/cgi-progs/queryCSV?station_id=", 
              id, "&sensor_num=", sensor, "&dur_code=", interval, "&start_date=", 
              start, "&end_date=", end, "&data_wish=Download CSV Data Now")
  u <- URLencode(u)
  tf <- tempfile()
  suppressWarnings(download.file(url = u, destfile = tf, quiet = TRUE))
  d <- try(read.csv(file = tf, header = TRUE, skip = 1, quote = "'", 
                    na.strings = "m", stringsAsFactors = FALSE, colClasses = c("character", 
                                                                               "character", "numeric")), silent = TRUE)
  if (class(d) == "try-error") {
    d <- data.frame(d1= gsub("-", "", start), d2="0000", d3=NA)  #d has 3 columns
  }
  #if (nrow(d) == 0) 
  #  stop("query returned no data", call. = FALSE)
  d$datetime <- as.POSIXct(paste(d[[1]], d[[2]]), format = "%Y%m%d %H%M")
  d[[1]] <- NULL
  d[[1]] <- NULL
  names(d)[1] <- paste("s", sensor, sep="")
  return(d[, c("datetime", paste("s", sensor, sep=""))])
}

#DOWNLOAD ALL AVAILABLE DATA FOR SPECIFIED X-YEARS * 1-STATION * 1-SENSOR COMBOS
# AND COMPILE THEM INTO ONE DATA.FRAME:
library(data.table)
CDECcompyrs <- function(first.year, last.year, station, sensor)
{
  yr.list <- seq(first.year, last.year, by=1)
  for(k in yr.list)
  {
    assign(paste(station, sensor, k, sep="."),
           CDEC4govquery(id=station, sensor=sensor, interval='E', 
                         start=paste(k, "-01-01", sep=""), 
                         end=paste(k, "-12-31", sep="")))
  }
  return(rbindlist(lapply(paste(station, sensor, yr.list, sep="."), function(x) get(x))))
}

#NOTE THAT THESE FUNCTIONS DON'T GIVE INFORMATIVE ERRORS.
# THEY USE CODE THAT WAS MODIFIED FROM CDECquery WHICH TENDS 
# TO JUST GIVE YOU A GENERIC WEB ERROR,
# EVEN IF THE PROBLEM IS THAT YOUR DATA REQUEST WAS TOO BIG.

#####################################################################################
#UNFINISHED CODE THAT WILL EVENTUALLY MAKE A TABLE OUR THE DATA DOWNLOADED 
#FOR A LIST OF SENSORS FOR EACH STATION * YEARS COMBINATION
# 
# st.list <- ndelta$STA[ndelta$s25==1 & ndelta$s27==1 & ndelta$s61==1]  #a dataset with a column for station name and one column for each of the sensors.  a one in the sensor column indicates that the station has that sensor.
# s.list <- c(25, 27, 61) #temp, turbidity, DO
# first.year <- 2015
# last.year <- 2016
# for (j in st.list[3]) #stations
# {
#   print(paste("start", j, sep=" "))
#   for (i in s.list) #sensors
#   {
#     print(paste("start download", j, i, sep=" "))
#     temp1 <- CDECcompyrs(first.year, last.year, j, i)
#     print(paste("finish download", j, i, sep=" "))
#     #temp1$month <- temp1$year <- NULL
#     if(which(s.list==i)==1) {
#       assign(as.character(j), temp1)
#     } else {
#       names(temp1)[1] <- "datetime2"
#       if(length(temp1[[1]]==1)){
#         rm(temp1)
#       } else{
#         assign(as.character(j), merge(eval(parse(text=as.character(j))), temp1, by.x="datetime", by.y="datetime2", all=TRUE))
#       }
#       
#     }
#     
#     #rm(temp1)
#     gc()
#   }
# }