library(tidyverse)
library(readxl)
library(ggmap)
library(lubridate)
library(sf)

# read US school districts shapefile
sf_dist <- st_read('reference_shapefiles/US_schdists/schooldistrict_sy1112_tl13.shp')
sf_dist_mi <- sf_dist %>% filter(STATEFP == 26)
sf_dist_mi$GEOID <- as.character(sf_dist_mi$GEOID)

# read in michigan layoffs
sf_lyfs <- read_sf('data/state_warn_shapes/michigan/michigan.shp')

#read in states, provinces
sf_states <- read_sf('reference_shapefiles/US_states/tl_2013_us_state.shp')
sf_provinces <- st_read('lpr_000b16a_e.shp')

p1 <- ggplot() +
  geom_sf(data = sf_states, color = 'grey') +
  geom_sf(data = sf_provinces, color = 'grey') +
  geom_sf(data = sf_dist_mi, size = 0.1, color = 'darkgrey', 
          fill = '#99d8c9', alpha = 0.5) +
  geom_sf(data = sf_lyfs, shape = 20, aes(size = num),
          alpha = 0.25, show.legend = 'point', color = 'blue') +
  theme_bw()

tiff('a4/plot2.tiff', units = 'in', type='cairo', width = 5, height = 5, res = 1080)
 p1 + 
   coord_sf(xlim = c(-90, -82), ylim = c(41.75, 48))
dev.off()

tiff('a4/plot3.tiff', units = 'in', type='cairo', width = 5, height = 5, res = 1080)
p1 + 
  coord_sf(xlim = c(-83.6, -82.75), ylim = c(42.25, 42.8))
dev.off()

df_scores <- read.csv('A4/raw/seda_geodist_long_cs_v30.csv')
df_scores <- df_scores %>% filter(fips == 26)
df_scores <- as_tibble(df_scores)
df_scores$leaidC <- as.character(df_scores$leaidC)

#df_scores <- df_scores %>% mutate('yeargrade' = paste0(year,grade)) %>%
#  filter(yeargrade == '200903' | yeargrade == '200904' | 
#         yeargrade == '201004' | yeargrade == '201005' |
#         yeargrade == '201105' | yeargrade == '201106' |
#         yeargrade == '201206' | yeargrade == '201207' |
#         yeargrade == '201307' | yeargrade == '201308')

df_scores_2010 <- df_scores %>%
  filter(grade == 3) %>%
  select(leaidC, grade, year, subject, mn_all) %>%
  pivot_wider(names_from = c(subject, grade),
              values_from = mn_all) %>%
  mutate(math_d = ifelse(year == 2009, NA, (math_3 - lag(math_3)))) %>%
  mutate(ela_d = ifelse(year == 2009, NA, (ela_3 - lag(ela_3)))) %>%
  filter(year == 2010)

df_scores_total <- df_scores %>%
  filter(grade == 8) %>%
  select(leaidC, grade, year, subject, mn_all) %>%
  pivot_wider(names_from = c(subject, grade),
              values_from = mn_all) %>%
  filter(year %in% c(2009, 2013)) %>%
  mutate(math_d = ifelse(year == 2009, NA, (math_8 - lag(math_8)))) %>%
  mutate(ela_d = ifelse(year == 2009, NA, (ela_8 - lag(ela_8)))) %>%
  filter(year == 2013)
  
df_scores_total$math_d[is.na(df_scores_total$math_d)] <- mean(df_scores_total$math_d,
                                                              na.rm = T)
df_scores_total$ela_d[is.na(df_scores_total$ela_d)] <- mean(df_scores_total$ela_d,
                                                              na.rm = T)

mdq <- quantile(df_scores_total$math_d, seq(0,1, by = 0.2))
edq <- quantile(df_scores_total$ela_d, seq(0,1, by = 0.2))

df_scores_total <- df_scores_total %>%
  mutate(math_d_cat = case_when(
    math_d > 0.2 ~ 2,
    math_d > 0.05 ~ 1,
    math_d < -0.2 ~ -2,
    math_d < -0.05 ~ -1,
    TRUE ~ 0
  )) %>%
  mutate(ela_d_cat = case_when(
    ela_d > 0.2 ~ 2,
    ela_d > 0.05 ~ 1,
    ela_d < -0.2 ~ -2,
    ela_d < -0.05 ~ -1,
    TRUE ~ 0
  ))
df_scores_total$math_d_cat <- as.factor(df_scores_total$math_d_cat)
df_scores_total$ela_d_cat <- as.factor(df_scores_total$ela_d_cat)
levels(df_scores_total$math_d_cat) <- c('Decrease more than -0.20', 
                                        'Decrease between -0.20 and -0.05', 
                                        'No change (-0.05 to +0.05)',
                                        'Increase between 0.05 - 0.20',
                                        'Increase more than +0.20')

#df_scores_gradeavg <- df_scores_gradeavg %>% 
#  mutate('math_d' = math_mean_2013 - math_mean_2009) %>%
#  mutate('ela_d' = ela_mean_2013 - ela_mean_2009) %>%
#  mutate('math_d_cat' = case_when(
#    math_d > 4 ~ 1,
#    math_d < -4 ~ -1,
#    TRUE ~ 0
#  )) %>%
#  mutate('ela_d_cat' = case_when(
#    ela_d > 4 ~ 1,
#    ela_d < -4 ~ -1,
#   TRUE ~ 0
#  ))
  
sf_dist_final <- sf_dist_mi %>% merge(df_scores_total, 
                                   by.x = 'GEOID',
                                   by.y = 'leaidC')

p4 <- ggplot() +
  geom_sf(data = sf_states, color = 'grey', fill = 'darkgrey') +
  geom_sf(data = sf_provinces, color = 'grey', fill = 'darkgrey') +
  geom_sf(data = sf_dist_final, size = 0.1, color = 'grey', 
          aes(fill = math_d_cat), alpha = 1) +
  scale_fill_brewer(type = 'div') +
  geom_sf(data = sf_lyfs, shape = 16, aes(size = num),
          alpha = 0.5, show.legend = 'point', color = 'black') +
  theme_bw() 
  #coord_sf(xlim = c(-87.25, -82), ylim = c(41.75, 45))

tiff('a4/plot4.tiff', units = 'in', type='cairo', width = 9, height = 6, res = 1500)
p4 +
  guides(fill=guide_legend(title="Math Z-Score Change"),
         size=guide_legend(title="Workers in WARN Notices")) +
  labs(title = 'Geographic Correlation Between WARN Notices and Changes in Children\'s Math Achievement',
       subtitle = 'Southern Michigan Grade 8 Students, Change from 2009 to 2013',
       caption = 'WARN Notices: Michigan DTMB (https://milmi.org/warn)
                  School district achievement data: SEDA (https://exhibits.stanford.edu/data/catalog/db586ns4974)
                  School district boundary data: NCES (https://nces.ed.gov/programs/edge/Geographic/DistrictBoundaries)') +
  coord_sf(xlim = c(-86.5, -82.5), ylim = c(41.75, 44))
dev.off()

sf_cities <- st_read('A4/raw/Cities_v17a/Cities_v17a.shp')
sf_munic <- sf_cities %>% filter(NAME %in% c('Detroit', 'Pontiac', 'Troy'))

tiff('a4/plot5.tiff', units = 'in', type='cairo', width = 8, height = 6, res = 1500)
p4 +
  geom_sf(data = sf_munic, color = 'red', fill = NA) +
  coord_sf(xlim = c(-83.9, -82.8), ylim = c(42.1, 43)) +
  labs(title = 'Detroit and Neighbouring Regions') +
  guides(fill=guide_legend(title="Math Z-Score Change"),
         size=guide_legend(title="Workers in WARN Notices"))
dev.off()
