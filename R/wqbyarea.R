library(tidyverse)
library(sf)
library(lubridate)
library(units)
library(NADA)

data(rswqdat)
data(bswqdat)
data(ppseg)
data(parms)
data(rsstatloc)
data(bsstatloc)

nonbay <- c('BH01', 'P Port 2', 'P Port 3', 'PM Out', '20120409-01', 'PPC41', 'P Port 4', 'PMB01', 'NGS-S Pond')

# segments
ppsegbf <- ppseg %>% 
  rename(area = Name) %>% 
  group_by(area) %>% 
  summarise() %>% 
  st_buffer(dist = set_units(0.0001, degree)) %>% 
  st_buffer(dist = set_units(-0.0001, degree)) %>% 
  mutate(
    area = factor(area)
  )

vr <- c('chla', 'tn', 'secchi')

cols <- c("#E16A86", "#50A315", "#009ADE")
names(cols) <- levels(ppseg$area)

datin <- rswqdat %>% 
  filter(date < as.Date('2021-08-01')) %>% 
  filter(var %in% c('tn', 'chla', 'secchi')) %>% 
  filter(!qual %in% c('S', 'U')) 
# unique weeks
wklv <- rswqdat %>%
  pull(date) %>% 
  range

wklv <- seq.Date(wklv[1], wklv[2],by = 'days') %>% 
  floor_date(unit = 'week') %>% 
  unique %>% 
  crossing(date = ., area = c('Area 1', 'Area 2', 'Area 3')) %>% 
  mutate(
    fillcl = factor(area, levels = unique(area), labels = cols), 
    fillcl = as.character(fillcl)
  )

# monitoring data
rswqtmp <- rswqdat %>% 
  filter(var %in% vr) %>% 
  filter(!station %in% nonbay) %>% 
  inner_join(rsstatloc, ., by = c('station', 'source')) %>% 
  st_intersection(ppsegbf) %>% 
  st_set_geometry(NULL) %>% 
  select(-bswqstation, -nrmrng, -source, -source_lng, -uni, -lbunis) %>% 
  mutate(
    date = floor_date(date, unit = 'week'),
    cens = grepl('U', qual)
  ) %>% 
  group_by(date, var, area) %>% 
  summarise(   
    avev = ifelse(
      any(cens), mean(cenfit(val, cens), na.rm = T),
      mean(val, na.rm = T)
    ),
    .groups = 'drop'
  ) %>% 
  left_join(parms, by = 'var') %>% 
  select(-sigdig)

# baseline data
bswqtmp <- bswqdat %>% 
  select(-source, -uni) %>% 
  filter(var %in% vr) %>%
  filter(!(var == 'secchi' & grepl('S', qual))) %>% # remove secchi on bottom
  filter(yr > 2005) %>% 
  filter(!is.na(val)) %>% 
  inner_join(bsstatloc, ., by = 'station') %>% 
  st_intersection(ppsegbf) %>% 
  st_set_geometry(NULL) %>% 
  mutate(
    date = floor_date(date, unit = 'month'),
    cens = grepl('U', qual)) %>% 
  group_by(date, var, area) %>% 
  summarise(   
    avev = ifelse(
      any(cens), mean(cenfit(val, cens), na.rm = T),
      mean(val, na.rm = T)
    ),
    .groups = 'drop'
  ) %>%
  left_join(parms, by = 'var') %>% 
  select(-sigdig)
 
wqbyarea <- bind_rows(bswqtmp, rswqtmp) %>% 
  arrange(date, var, area)

write.csv(wqbyarea, file = '~/Desktop/wqbyarea.csv', row.names = F)

ggplot(wqbyarea, aes(x = date, y = avev)) +
  geom_line() + 
  facet_wrap(var ~ area, scales = 'free_y')
