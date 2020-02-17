library(readxl)
library(tidyverse)
library(stringr)
library(naniar)

# read in data
lyofs_2015 <- read_csv('a3/data/warn_ohio_2015.csv')
lyofs_2016 <- read_csv('a3/data/warn_ohio_2016.csv')
lyofs_2017 <- read_csv('a3/data/warn_ohio_2017.csv')
lyofs_2018 <- read_csv('a3/data/warn_ohio_2018.csv')
lyofs_2019 <- read_csv('a3/data/warn_ohio_2019.csv')

#add year column
lyofs_2015$year <- 2015
lyofs_2016$year <- 2016
lyofs_2017$year <- 2017
lyofs_2018$year <- 2018
lyofs_2019$year <- 2019

#combine years
df <- rbind(lyofs_2015,
                  lyofs_2016,
                  lyofs_2017,
                  lyofs_2018,
                  lyofs_2019)

#replace various null indicators
df <- replace_with_na_all(df,
                    condition = ~.x %in% c('Unknown', 'unknown', 'none', 'N/A', 'None'))

#clean date columns
df$date_recd <- str_remove(df$date_recd, '\n')
df$date_recd <- str_replace_all(df$date_recd, '/', '-')
df$date_recd <- parse_date(df$date_recd, format = '%m-%d-%Y')

df$date_layoff <- str_remove(df$date_layoff, '\n')
df$date_layoff <- gsub("(\\d{1,2})[/-](\\d{1,2})[/-](\\d{4}).*","\\1-\\2-\\3",df$date_layoff)
df$date_layoff <- parse_date(df$date_layoff, format = '%m-%d-%Y')

#clean and split city/county column
df$`city/county` <- str_replace_all(df$`city/county`, '\n', ' ') 
df <- df %>% separate(`city/county`, into = c('city', 'county'), sep = '/')
df$city <- tolower(df$city)
df$county <- tolower(df$county)

#set layoff counts to integer
df$num_affected <- as.integer(df$num_affected)

#get dummy for union representation 
df$union_rep <- case_when(
  is.na(df$union) ~ 0,
  TRUE ~ 1
)

#save data
write_csv(df, 'a3/ohio_clean.csv')
