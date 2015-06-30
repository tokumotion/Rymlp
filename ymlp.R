library(XML)
library(RCurl)
library(stringr)
library(tidyr)
library(lubridate)
library(dplyr)

fileURL <- "https://www.ymlp.com/api/Archive.GetList?Key=1110HY413CGR76P283UQ&Username=jdreload&StartDate=2015-01-01&NumberPerPage=1000&ShowTestMessages=0"
xdata <- getURL(fileURL)
doc <- xmlParse(xdata)
letter <- xmlToList(doc)
x <- do.call("rbind", lapply(letter, as.data.frame, stringsAsFactors = FALSE))
row.names(x) <- NULL
rm(fileURL)
rm(xdata)
rm(doc)
rm(letter)

original_locale<-Sys.getlocale(category = "LC_TIME")
Sys.setlocale(category = "LC_TIME", locale = "es_ES.UTF-8")
x$Date <- parse_date_time(x$Date, "dby HM")
Sys.setlocale(category = "LC_TIME", locale = original_locale)
rm(original_locale)

x$OpenRate <- as.numeric(x$OpenRate)/100
x$ClickThroughRate <- as.numeric(x$ClickThroughRate, na.rm = FALSE)/100
x$Recipients <- as.numeric(x$Recipients)
x$Delivered <- as.numeric(x$Delivered)
x$Bounced <- as.numeric(x$Bounced)
x$TotalOpens <- as.numeric(x$TotalOpens)
x$UniqueOpens <- as.numeric(x$UniqueOpens)
x$TotalClicks <- as.numeric(x$TotalClicks)
x$UniqueClicks <- as.numeric(x$UniqueClicks)
x$TestMessage <- NULL
x$Forwards <- NULL
x$FromEmail <- NULL
x$Filters <- NULL

NissanNewsletter <- x %>%
  filter(FromName == "Nissan")

bouncesFun <- function (x) {
  list = paste("https://www.ymlp.com/api/Archive.GetBounces?Key=1110HY413CGR76P283UQ&Username=jdreload&NumberPerPage=99999&NewsletterID=",
        x, sep = "")
  FinalData <- data.frame()
  for (i in list) {
    data <- xmlToList(xmlParse(getURL(i)))
    betterData <- do.call("rbind", lapply(data, as.data.frame))
    betterData$Newsletter <- str_sub(i, start = length(i) - 6)
    row.names(betterData) <- NULL
    FinalData <- rbind(FinalData, betterData)
  }
  return(FinalData)
}

clicksFun <- function (x) {
  list = paste("https://www.ymlp.com/api/Archive.GetClicks?Key=1110HY413CGR76P283UQ&Username=jdreload&NumberPerPage=99999&NewsletterID=",
               x, sep = "")
  FinalData <- data.frame()
  for (i in list) {
    data <- xmlToList(xmlParse(getURL(i)))
    betterData <- do.call("rbind", lapply(data, as.data.frame, stringAsFactors = FALSE))
    betterData$Newsletter <- str_sub(i, start = length(i) - 6)
    row.names(betterData) <- NULL
    FinalData <- rbind(FinalData, betterData)
  }
  original_locale<-Sys.getlocale(category = "LC_TIME")
  Sys.setlocale(category = "LC_TIME", locale = "es_ES.UTF-8")
  FinalData$Timestamp <- parse_date_time(FinalData$Timestamp, "dby HM")
  Sys.setlocale(category = "LC_TIME", locale = original_locale)
  return(FinalData)
}

opensFun <- function (x) {
  list = paste("https://www.ymlp.com/api/Archive.GetOpens?Key=1110HY413CGR76P283UQ&Username=jdreload&NumberPerPage=99999&NewsletterID=",
               x, sep = "")
  FinalData <- data.frame()
  for (i in list) {
    data <- xmlToList(xmlParse(getURL(i)))
    betterData <- do.call("rbind", lapply(data, as.data.frame, stringAsFactors = FALSE))
    betterData$Newsletter <- str_sub(i, start = length(i) - 6)
    row.names(betterData) <- NULL
    FinalData <- rbind(FinalData, betterData)
  }
  original_locale<-Sys.getlocale(category = "LC_TIME")
  Sys.setlocale(category = "LC_TIME", locale = "es_ES.UTF-8")
  FinalData$Timestamp <- parse_date_time(FinalData$Timestamp, "dby HM")
  Sys.setlocale(category = "LC_TIME", locale = original_locale)
  return(FinalData)
}

deliveredFun <- function (x) {
  list = paste("https://www.ymlp.com/api/Archive.GetDelivered?Key=1110HY413CGR76P283UQ&Username=jdreload&NumberPerPage=99999&NewsletterID=",
               x, sep = "")
  FinalData <- data.frame()
  for (i in list) {
    data <- xmlToList(xmlParse(getURL(i)))
    betterData <- do.call("rbind", lapply(data, as.data.frame, stringAsFactors = FALSE))
    betterData$Newsletter <- str_sub(i, start = length(i) - 6)
    row.names(betterData) <- NULL
    FinalData <- rbind(FinalData, betterData)
  }
  return(FinalData)
}

# El API tiene un límite de 100 contactos como default

Bounces <- bouncesFun(NissanNewsletter$NewsletterID)
Clicks <- clicksFun(NissanNewsletter$NewsletterID)
Opens <- opensFun(NissanNewsletter$NewsletterID)
Delivered <- deliveredFun(NissanNewsletter$NewsletterID)
