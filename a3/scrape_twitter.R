library(rtweet)
library(tidyverse)
library(sf)
library(ggplot2)

#set API auth values 
api_key <- "9vsMsCSKhdX8033x5ZqY1ABPB"
api_secret_key <- "SWPWbeUxsZun1D2lLolWGIKmN7KHLPsPG0kP1SYP8zXHNg7UcB"
access_token <- "839241365720559616-tbha4voeQPTy7a93PNTOw2Bs5aVAuM8"
access_token_secret <- "KBRhQB8LD58HLIowvONQp0TIFFoQDt105qwThEuIBQryp"

## authenticate via web browser
token <- create_token(
  app = "rstatsjournalismresearch",
  consumer_key = api_key,
  consumer_secret = api_secret_key,
  access_token = access_token,
  access_secret = access_token_secret)

sf <- st_read('A3/cities_geo_layoffs.shp', geometry_column = 'geometry')

long <- sf$geometry[[1]][1]
lat <- sf$geometry[[1]][2]

search_fullarchive(n=500, )