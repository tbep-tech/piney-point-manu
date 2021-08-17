
# setup -------------------------------------------------------------------

library(here)
library(tbeptools)
library(tidyverse)
library(sf)
box::use(
  units = units[set_units], 
)

data(rswqdat)
data(ppseg)
data(rsstatloc)
data(parms)
data(bsstatloc)
data(bswqdat)

# stack characteristics ---------------------------------------------------

# ltb seg
ltb <- tbseg %>% 
  filter(bay_segment %in% 'LTB')

# relevant stack measurements form ngs-s, FDEP measurements
# original data from https://docs.google.com/spreadsheets/d/1AgRPWUv8TLVcLf6KmDCSIcTOVZmQL7ij/edit#gid=1685851796
stkraw <- read.csv(here('data-raw/stack_samples.csv')) %>% 
  select(var = Parameter, val = NGS.S, Units) %>% 
  filter(var %in% c('NO2NO3-N', 'Ammonia N', 'Total N', 'Total-P', 'Ortho Phosphate as P', 'Dissolved Oxygen', 'pH*')) %>% 
  filter(!(var %in% 'Dissolved Oxygen' & Units %in% 'mg/L')) %>% 
  mutate(
    var = case_when(
      var == 'NO2NO3-N' ~ 'no23', 
      var == 'Ammonia N' ~ 'nh34', 
      var == 'Total N' ~ 'tn', 
      var == 'Total-P' ~ 'tp', 
      var == 'Ortho Phosphate as P' ~ 'orthop', 
      var == 'Dissolved Oxygen' ~ 'dosat', 
      var == 'pH*' ~ 'ph'
    )
  ) %>% 
  left_join(parms, by = 'var') %>% 
  select(var, lbs, val)

bssum <- bswqdat %>% 
  filter(yr > 2005 & yr < 2021) %>% 
  filter(source == 'epchc') %>% 
  filter(var %in% unique(stkraw$var)) %>% 
  inner_join(bsstatloc, ., by = c('station')) %>% 
  .[ltb, ] %>% 
  st_set_geometry(NULL) %>% 
  group_by(yr, var) %>% 
  summarise(
    val = median(val, na.rm = T),
    .groups = 'drop'
  ) %>%
  left_join(parms, by = 'var') %>% 
  group_by(var) %>% 
  summarise(
    minv = round(min(val), unique(sigdig)),
    maxv = round(max(val), unique(sigdig)),
    avev = round(mean(val), unique(sigdig)), 
    .groups = 'drop'
  ) %>% 
  mutate(
    minv = paste0(' (', minv, ', '), 
    maxv = paste0(maxv, ')')
  ) %>% 
  unite(sumv, avev, minv, maxv, sep = '')

tab <- full_join(stkraw, bssum, by = 'var') %>% 
  select(
    `Water quality variable` = lbs, 
    `Stack value` = val, 
    `Normal mean (min, max)` = sumv
  )

stktab <- tab

save(stktab, file = here('tables/stktab.RData'))

# water quality summary table ---------------------------------------------

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

vrs <- c('chla', 'dosat', 'nh34', 'ph', 'secchi', 'temp', 'tn', 'no23', 'tp', 'sal')
nonbay <- c('BH01', 'P Port 2', 'P Port 3', 'PM Out', '20120409-01', 'PPC41', 'P Port 4', 'PMB01', 'NGS-S Pond')

sigs <- parms %>% 
  select(lbs, sigdig)

totab <- rswqdat %>% 
  left_join(rsstatloc, ., by = c('station', 'source')) %>% 
  filter(var %in% vrs) %>% 
  filter(!station %in% nonbay) %>% 
  select(station, date, lbs, val, inrng) %>% 
  st_intersection(ppsegbf) %>% 
  st_set_geometry(NULL)

totab1 <- totab %>% 
  group_by(area, lbs, inrng) %>% 
  summarise(
    cnt = n(), 
    .groups = 'drop'
  ) %>% 
  complete(area, lbs, inrng, fill = list(cnt = 0)) %>% 
  spread(inrng, cnt)

totab2 <- totab %>% 
  group_by(area, lbs) %>% 
  left_join(sigs, by = 'lbs') %>% 
  summarise(
    medv = round(median(val, na.rm = T), unique(sigdig)), 
    minv = round(min(val, na.rm = T), unique(sigdig)), 
    maxv = round(max(val, na.rm = T), unique(sigdig)), 
    cnt = n(), 
    .groups = 'drop'
  ) %>% 
  mutate(
    minv = paste0(' (', minv, ', '), 
    maxv = paste0(maxv, ')')
  ) %>% 
  unite('rng', minv, maxv, sep = '') %>% 
  unite('sumv', medv, rng, sep = '') %>% 
  left_join(totab1, by = c('area', 'lbs')) %>% 
  mutate(area = ifelse(duplicated(area), '', area)) %>% 
  select(
    Area = area, 
    `Water quality variable` = lbs, 
    `Med. (Min., Max.)` = sumv, 
    `N obs.` = cnt, 
    `in range`, 
    above, 
    below
  )

wqsumtab <- totab2
save(wqsumtab, file = here('tables/wqsumtab.RData'))
