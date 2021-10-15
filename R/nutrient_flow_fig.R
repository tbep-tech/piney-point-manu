
# setup -------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(sf)
library(here)
library(ggmap)
library(ggspatial)
library(patchwork)
library(units)
library(grid)
library(scales)
library(USAboundaries)
library(ggord)
library(lubridate)
library(patchwork)
library(tbeptools)
library(NADA)
library(RColorBrewer)
box::use(
  scales = scales[muted], 
  units = units[set_units], 
  vegan = vegan[decostand], 
  FactoMineR = FactoMineR[PCA], 
  ggnewscale[new_scale_fill], 
  grid[unit]
)

data(rsallpts)
data(bswqdat)
data(bsstatloc)
data(ppseg)
data(segmask)
data(rswqdat)
data(rsstatloc)
data(rstrndat)
data(parms)
data(rstrnpts)
data(kbrdat)
data(winddat)
data(raindat)
data(hydrodat)
data(rsphydat)
data(rsphypts)

source(here('R/funcs.R'))


vrs <- c('chla', 'dosat', 'nh34', 'ph', 'sal', 'secchi', 'temp', 'tn', 'tp')
mcrsel <- c("Red", "Green", "Brown", "Cyanobacteria")
mcrlab <- c("Red", "Green", "Brown", "Cyano")
savsel <- c('Thalassia testudinum', 'Halodule wrightii', 'Syringodium filiforme')
savlab <- c('T. testudinum', 'H. wrightii', 'S. filiforme')
wqsel <- c('chla', 'dosat', 'nh34', 'ph', 'sal', 'secchi', 'temp', 'tn', 'tp')
wqlab <- c('Chl-a', 'DOsat', 'NH3, NH4+', 'pH', 'Sal', 'Secchi', 'Temp', 'TN', 'TP')

nonbay <- c('BH01', 'P Port 2', 'P Port 3', 'PM Out', '20120409-01', 'PPC41', 'P Port 4', 'PMB01', 'NGS-S Pond')

# segments
areas <- ppseg %>% 
  rename(area = Name) %>% 
  group_by(area) %>% 
  summarise() %>% 
  st_buffer(dist = set_units(0.0001, degree)) %>% 
  st_buffer(dist = set_units(-0.0001, degree)) %>% 
  mutate(
    area = factor(area)
  )

# water quality summarized
rswqsum <- rswqdat %>% 
  filter(var %in% vrs) %>% 
  filter(date < as.Date('2021-10-01')) %>% 
  filter(source == 'fldep') %>%
  filter(!station %in% nonbay) %>% 
  inner_join(rsstatloc, ., by = c('station', 'source')) %>% 
  st_intersection(areas) %>% 
  st_set_geometry(NULL) %>% 
  filter(!grepl('S', qual)) %>% # remove secchi on bottom
  mutate(cens = grepl('U', qual)) %>% 
  select(date, var, val, cens, station, area) %>% 
  mutate(
    var = case_when(
      var == 'chla' ~ 'Chl-a', 
      var == 'dosat' ~ 'DOsat', 
      var == 'nh34' ~ 'NH3, NH4+', 
      var == 'ph' ~ 'pH', 
      var == 'secchi' ~ 'Secchi', 
      var == 'temp' ~ 'Temp', 
      var == 'tn' ~ 'TN', 
      var == 'no23' ~ 'NOx',
      var == 'tp' ~ 'TP', 
      var == 'sal' ~ 'Sal'
    ),
    date = floor_date(date, unit = 'week')
  ) %>% 
  group_by(date, var, area) %>% 
  summarise(
    val = ifelse(
      any(cens), median(cenfit(val, cens), na.rm = T), 
      median(val, na.rm = T)
    ), 
    .groups = 'drop'
  ) %>% 
  complete(date, area, var) %>% 
  group_by(area, var) %>% 
  ungroup() %>% 
  mutate(
    val = case_when(
      var %in% c('Chl-a', 'NH3, NH4+', 'NOx', 'TN', 'TP') ~ log10(1 + val),
      T ~ val
    )
  ) %>% 
  spread(var, val)

# phytoplankton summarized
physum <- rsphydat %>%
  filter(date < as.Date('2021-10-01')) %>% 
  filter(source == c('fldep')) %>%
  filter(!station %in% nonbay) %>% 
  # inner_join(rsphypts, ., by = c('station', 'source', 'typ')) %>%
  # st_intersection(areas) %>%
  # st_set_geometry(NULL) %>%
  # filter(area == 'Area 1') %>%
  mutate(
    species = case_when(
      species %in% c('Centric Diatoms', 'Skeletonema sp.', 'Asterionellopsis glacialis') ~ 'Diatoms', 
      T ~ species
      ), 
    species = factor(species, levels = unique(species))
  ) %>% 
  select(date, station, species) %>% 
  mutate(
    pa = 1
  ) %>% 
  group_by(date, station) %>% 
  complete(
    species,
    fill = list(pa = 0)
  ) %>% 
  ungroup() %>% 
  mutate(
    date = floor_date(date, unit = 'week')
  ) %>%
  group_by(date, species) %>% 
  summarize(
    foest = sum(pa) / length(pa), 
    .groups = 'drop'
  ) %>%  
  group_by(date) %>% 
  mutate(
    relfoest = foest / sum(foest)
  ) %>% 
  filter(!species %in% c('mixed algae', 'Pseudo-nitzschia sp.'))

lns <- physum %>% 
  group_by(date) %>% 
  summarize(
    ymax = sum(foest)
  )

brks <- seq.Date(min(physum$date), max(physum$date), by = '1 week')  
p3 <- ggplot() + 
  geom_area(data = physum, aes(x = date, y = foest, fill = species), stat = 'identity', color = 'lightgrey', alpha = 0.7) +
  geom_segment(data = lns, aes(x = date, xend = date, y = 0, yend = ymax), color = 'grey') +
  scale_fill_manual(values = c('#66c2a5', '#fc8d62', '#8da0cb')) +
  scale_x_date(breaks = brks, date_labels = '%b %d', expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, size = 8, hjust = 1), 
    # axis.title.x = element_blank(), 
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank(), 
    legend.title = element_blank()
  ) + 
  labs(
    y = 'Freq. Occ.', 
    x = 'Week of', 
    subtitle = '(c) Phytoplankton frequency occurrence all locations'
  )


# transect summarized
trnsum <- rstrndat %>% 
  filter(date < as.Date('2021-10-01')) %>% 
  mutate(
    date = floor_date(date, unit = 'week')
  ) %>% 
  # inner_join(rstrnpts, ., by = 'station') %>%
  # st_intersection(areas) %>%
  # st_set_geometry(NULL) %>%
  # filter(area == 'Area 1') %>%
  dplyr::group_by(date, station, location, taxa) %>%
  dplyr::summarise(
    pa = as.numeric(any(bb > 0)), 
    .groups = 'drop'
  ) %>%
  group_by(date, taxa) %>% 
  summarize(
    foest = sum(pa) / length(pa), # freq occ.
    .groups = 'drop'
  ) %>% 
  ungroup %>% 
  filter(taxa %in%  c('Red', 'Green', 'Cyanobacteria'))

lns <- trnsum %>% 
  group_by(date) %>% 
  summarize(
    ymax = sum(foest)
  )

brks <- seq.Date(min(physum$date), max(physum$date), by = '1 week')  
p4 <- ggplot() + 
  geom_area(data = trnsum, aes(x = date, y = foest, fill = taxa), stat = 'identity', color = 'lightgrey', alpha = 0.7) +
  geom_segment(data = lns, aes(x = date, xend = date, y = 0, yend = ymax), color = 'grey') +
  scale_fill_manual(values = c('tomato1', 'lightgreen', 'lightblue')) +
  scale_x_date(breaks = brks, date_labels = '%b %d', expand = c(0, 0), limits = range(brks)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, size = 8, hjust = 1), 
    # axis.title.x = element_blank(), 
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank(), 
    legend.title = element_blank()
  ) + 
  labs(
    y = 'Freq. Occ.', 
    x = 'Week of', 
    subtitle = '(d) Macroalgae frequency occurrence all locations'
  )

p3 + p4 + plot_layout(ncol = 1)
