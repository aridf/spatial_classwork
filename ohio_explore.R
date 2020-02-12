library(tidyverse)
library(ggplot2)
theme_set(theme_bw())
library(sf)

df <- read_csv('a3/ohio_clean.csv')

city_lookup <- read_csv('a3/city_populations.csv')
city_lookup$asciiname <- tolower(city_lookup$asciiname)
colnames(city_lookup) <- c('city', 'pop2019', 'location') 

df <- df %>% arrange(desc(num_affected))

df_city <- df %>% 
  group_by(city) %>%
  summarise(sum(num_affected, na.rm = T)) %>%
  set_names('city', 'num_affected') %>%
  arrange(desc(num_affected))

df_county <- df %>% 
  group_by(county) %>%
  summarise(sum(num_affected, na.rm = T)) %>%
  set_names('county', 'num_affected')

df <- merge(city_lookup, df_city, all.x = T)

df$prop_affected <- df$num_affected / df$pop2019
df <- df %>% arrange(desc(df$prop_affected))
head(df %>% select(-location))

df <- df %>% 
  separate_rows(location, sep = '=|,') %>%
  group_by(city) %>%
  slice(2:3) %>% 
  mutate('dim' = c('lat', 'long')) %>%
  pivot_wider(names_from = 'dim', values_from = location) %>%
  mutate(long = as.double(long), lat = as.double(lat)) %>%
  mutate(layoff = ifelse(is.na(prop_affected), 0, 1))

#f <- tempfile()
#download.file("http://www2.census.gov/geo/tiger/GENZ2010/gz_2010_us_050_00_20m.zip", destfile = f)
#unzip(f, exdir = ".")

US <- st_read("gz_2010_us_050_00_20m.shp")
ohio <- US[US$STATE == '39',]  

#split data into laid off and non-laid off cities
ohio_yes <- df[which(df$layoff == 1),]
ohio_no <- df[which(df$layoff == 0),]

ohio_yes_sf <- st_as_sf(ohio_yes, coords = c('long', 'lat'),
                           crs = 4326, agr = "constant")
ohio_no_sf <- st_as_sf(ohio_no, coords = c('long', 'lat'),
                       crs = 4326, agr = "constant")

tiff('a3/plot1.tiff', units = 'in', width = 5, height = 5, res = 1080)
ggplot(data = ohio) +
  geom_sf(aes(fill)) + 
  geom_sf(data = ohio_no_sf, shape = 20, color='black', alpha = 0.25) +
  geom_sf(data = ohio_yes_sf, shape = 20, color='maroon', aes(size = num_affected),
          alpha = 0.75, show.legend = 'point') 
dev.off()
