library(tidyverse)
library(ggplot2)
theme_set(theme_bw())
library(sf)

#read in cleaned data
df <- read_csv('a3/ohio_clean.csv')

#read in info on cities
city_lookup <- read_csv('a3/city_populations.csv')
city_lookup$asciiname <- tolower(city_lookup$asciiname)
colnames(city_lookup) <- c('city', 'pop2019', 'location') 

df <- df %>% arrange(desc(num_affected))

#get number of affected workers by city
df_city <- df %>% 
  group_by(city) %>%
  summarise(sum(num_affected, na.rm = T)) %>%
  set_names('city', 'num_affected') %>%
  arrange(desc(num_affected))

#NOT RUN
# get number of affected workers by county
#df_county <- df %>% 
#  group_by(county) %>%
#  summarise(sum(num_affected, na.rm = T)) %>%
#  set_names('county', 'num_affected')

# merge city layoffs with city geo lookup
df <- merge(city_lookup, df_city, all.x = T)

#get proportion affected measure by city
df$prop_affected <- df$num_affected / df$pop2019
df <- df %>% arrange(desc(df$prop_affected))
head(df %>% select(-location))

#extract geographic information from the provided google url
df <- df %>% 
  separate_rows(location, sep = '=|,') %>%
  group_by(city) %>%
  slice(2:3) %>% 
  mutate('dim' = c('lat', 'long')) %>%
  pivot_wider(names_from = 'dim', values_from = location) %>%
  mutate(long = as.double(long), lat = as.double(lat)) %>%
  mutate(layoff = ifelse(is.na(prop_affected), 0, 1))

#RUN ONCE
#Get US county shapefile and save locally
#f <- tempfile()
#download.file("http://www2.census.gov/geo/tiger/GENZ2010/gz_2010_us_050_00_20m.zip", destfile = f)
#unzip(f, exdir = ".")

#get shapefile of ohio
US <- st_read("gz_2010_us_050_00_20m.shp")
ohio <- US[US$STATE == '39',]  

#split data into laid off and non-laid off cities
ohio_yes <- df[which(df$layoff == 1),]
ohio_no <- df[which(df$layoff == 0),]

#convert points to sf format
ohio_yes_sf <- st_as_sf(ohio_yes, coords = c('long', 'lat'),
                           crs = 4326, agr = "constant")
ohio_no_sf <- st_as_sf(ohio_no, coords = c('long', 'lat'),
                       crs = 4326, agr = "constant")

#save first plot
tiff('a3/plot1.tiff', units = 'in', width = 5, height = 5, res = 1080)
ggplot(data = ohio) +
  geom_sf() + 
  geom_sf(data = ohio_no_sf, shape = 20, color='black', alpha = 0.25) +
  geom_sf(data = ohio_yes_sf, shape = 20, color='maroon', aes(size = num_affected),
          alpha = 0.75, show.legend = 'point') 
dev.off()

####
## Combine w/ population data
####

#AJWBE001 = Total population
#AJWBE002 = Male
#AJWBE003-025 = Male age breakdown
#AJWBE026 = Female
#AJWBE027-049 = Male age breakdown

#read data
df_pop <- read_csv("a3/nhgis_county.csv")

#filter to ohio
df_pop <- df_pop %>% filter(STATE == 'Ohio')

#get proportion sub-21
df_pop$sub20 <- df_pop %>% 
  select(AJWBE003, AJWBE004, AJWBE005,
         AJWBE006, AJWBE007, AJWBE008,
         AJWBE027, AJWBE028, AJWBE029,
         AJWBE030, AJWBE031, AJWBE032) %>%
  rowSums()

df_pop$sub20_prop <- df_pop$sub20 / df_pop$AJWBE001

#merge into sf object
sub20_prop <- df_pop %>% select(COUNTYA, sub20_prop) %>%
  mutate(COUNTYA = as.factor(COUNTYA)) %>%
  set_names('COUNTY', 'sub20prop')
  
ohio <- merge(ohio, sub20_prop, all = T)

#convert sub20 to low-med-high
ohio$age_split <- cut(ohio$sub20prop, 3) 
ohio <- ohio %>% mutate(age_split = factor(age_split, labels = c('< 26%', '26-30%', '> 31%')))

#create second plot
tiff('a3/plot2.tiff', units = 'in', width = 5, height = 5, res = 1080)
ggplot(data = ohio) +
  geom_sf(aes(fill = age_split)) +
  scale_fill_brewer() + 
  geom_sf(data = ohio_no_sf, shape = 20, color='black', alpha = 0.25) +
  geom_sf(data = ohio_yes_sf, shape = 20, color='maroon', aes(size = prop_affected),
          alpha = 0.75, show.legend = 'point') +
  labs(title = 'Mass Layoffs in Ohio, 2015 - 2019',
       size = '% directly affected',
       fill = '% under 20')
dev.off()
