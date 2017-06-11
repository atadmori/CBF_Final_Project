library(RCurl)
library(rjson)
library(RJSONIO)
library(reshape)
library(devtools)
library(cognizer)
library(jpeg)
library(tools)
library(utils)

options(scipen=999, stringsAsFactors = F)
# latitude <- 29.93#39.75764085 
# longitude <- -99.07##-105.0069346
units <- "e"

x_function <- function(filename) {
  
  working_dir <- "C:\\Users\\atadmori\\Dropbox\\amer\\cbf\\"
  setwd(working_dir)
  
  test <- paste0("exiftool.exe -csv ",filename,"")
  
  photo.data <- read.csv(textConnection(shell(test, intern = TRUE)), stringsAsFactors = FALSE)
  # date.photo.taken <- photo.data$GPSDateStamp
  photo.lat <- photo.data$GPSLatitude
  photo.long <- photo.data$GPSLongitude
  
  dms2dec <- function(dms, separators = c("deg", "\'", "\"")) {
    # version 1.0 (25 Sep 3013)
    # dms: a vector (or column) of latitude or longitude in degrees-minutes-seconds-hemisfere, e.g. 41° 34' 10.956" N (with or without spaces)
    # separators: the characters that are separating degrees, minutes and seconds in dms
    # source: https://modtools.wordpress.com/2013/09/25/dms2dec/
    
    dms <- as.character(dms)
    dms <- gsub(pattern = " ", replacement = "", x = dms)
    for (s in separators) dms <- gsub(pattern = s, replacement = "_splitHere_", x = dms)
    
    splits <- strsplit(dms, split = "_splitHere_")
    n <- length(dms)
    deg <- min <- sec <- hem <- vector("character", n)
    
    for (i in 1:n) {
      deg[i] <- splits[[i]][1]
      min[i] <- splits[[i]][2]
      sec[i] <- splits[[i]][3]
      hem[i] <- splits[[i]][4]
    }
    dec <- as.numeric(deg) + (as.numeric(min) / 60) + (as.numeric(sec) / 3600)
    sign <- ifelse (hem %in% c("N", "E"), 1, -1)
    dec <- sign * dec
    return(dec)
  }  
  
  latitude <- dms2dec(photo.lat)
  longitude <- dms2dec(photo.long)
  
  #############################################
  ### Using Visual Recognition from Watson ####
  #############################################
  
  # IMAGE_API_KEY <- "a778bb82989d69a1a76ae9e26e042744dc9b7fcb"
  IMAGE_API_KEY <- read.table("image_api.txt")
  original.photo <- readJPEG(filename)
  
  for (i in 3:6) {
    jpeg(paste0("Pixels",i,".jpeg"), width=640*i, height=426*i)
    plot(as.raster(original.photo))
    dev.off()
  }
  
  analysis.photo <- "Pixels6.jpeg"
  
  image_classify <- image_classify(analysis.photo, IMAGE_API_KEY)
  
  json_file <- lapply(image_classify, function(x) {
    x[sapply(x, is.null)] <- NA
    unlist(x)
  })
  
  class.table <- do.call("rbind", json_file)
  class.table <- as.matrix(class.table[,-1])
  class.table <- as.matrix(class.table[1:20,])
  class.table <- cbind(class.table[1:10], class.table[11:20])
  
  ##################################################
  ### Match Classes against Activity Dictionary ####
  ##################################################
  
  dict_ski <- c("ski", "skiing", "cross-country skiing", "slope")
  dict_bike <- c("bicycle", "bike", "cycling", "cyclist", "biking")
  
  if(sum(na.omit(pmatch(dict_ski, class.table))) > 0){
    activity_id = 1
  } else if(sum(na.omit(pmatch(dict_bike, class.table))) > 0){
    activity_id = 2
  } else {
    activity_id = 0
  }
  class(latitude)
  # RICH \|/
  APIkey <- read.table("weather_api.txt")
  WCurl <- paste0("http://api.weather.com/v1/geocode/", as.character(as.numeric(latitude)), 
                  "/", as.character(as.numeric(longitude)), "/", 
                  "forecast/daily/5day.json?apiKey=", APIkey,
                  "&units=", units)
  json <- getURL(WCurl)
  result <- fromJSON(json)
  forecasts <- lapply(result$forecasts, function(x) {
    x[sapply(x, is.null)] <- ""
    unlist(x)
  })
  forecasts <- do.call("rbind", forecasts)
  forecasts <- data.frame(forecasts)
  forecasts <- forecasts[,1:27]
  #forecasts <- melt(forecasts, id = c(1:4))
  # final <- data.frame(fore_time=forecasts$fcst_valid_local,
  #                     lat=result$metadata[4], 
  #                     long=result$metadata[5],
  #                     measurement=forecasts$variable,
  #                     value=forecasts$value)
  final <- data.frame(day=as.Date(forecasts$fcst_valid_local), dow=forecasts$dow, max_F=forecasts$max_temp, min_F=forecasts$min_temp, narrative=forecasts$narrative)
  final$is_good_weather <- ifelse(regexpr("(S|s)un", final$narrative) > 0, 1, 
                                  ifelse(regexpr("(P|p)artly (C|c)loudy", final$narrative) > 0, 1, 0))
  ### DUMMMY VAR UNTIL JOINED WITH CODE ###
  # activity_id <- 2 #skiing = 1, biking = 2
  # if fully implemented dictionary, then this would be joined to a table determining 
  # which activity is good vs. bad, i.e. skiing isn't good above 60F but biking is...
  ### DUMMMY VAR UNTIL JOINED WITH CODE ###
  final$is_good_temp <- ifelse(activity_id == 1 & (final$max_F >= 0 & final$max_F <= 35), 1, 
                               ifelse(activity_id == 2 & final$max_F >= 60, 1, 0))
  final$is_good_day <- as.integer(final$is_good_temp & final$is_good_weather)
  
  days_a <- paste(as.character(final[final$is_good_day == 1,]$dow), sep="' '", collapse=", ")
  activity_a <- ifelse(activity_id == 1, "skiing", "biking")
  
  ski_stuff <- c("ski boots", "ski goggles", "skiis", "ski jackets", "ski socks", "ski pants", "ski poles")
  bike_stuff <- c("bike jerseys", "bike shorts", "cycling shoes", "bike tune-ups", "bike helmets")
  
  if (activity_id == 1) {
    stuff_a <- sample(ski_stuff, 1)
  } else if (activity_id == 2) {
    stuff_a <- sample(bike_stuff, 1)
  } else 
    stuff_a <- ""
  
  outputText <- ifelse(sum(final$is_good_day) == 0, paste("Looks like weather is not looking good for next week! :("),
                       paste0("Weather is looking good for ",activity_a,
                              " for the coming ",days_a,
                              "! Save 25% off of your next purchase of ",stuff_a,
                              " at Mom n Pop ", toTitleCase(activity_a), " Shop")
  )
  
  # output$text <- renderText({outputText})
  # print("done")
  
  return(outputText)
}

winDialog(type = c("ok"), x_function("ski.jpg"))
winDialog(type = c("ok"), x_function("bike.jpg"))
