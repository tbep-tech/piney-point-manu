
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
library(mgcv)
box::use(
  scales = scales[muted], 
  units = units[set_units], 
  vegan = vegan[decostand, metaMDS], 
  FactoMineR = FactoMineR[PCA], 
  ggnewscale[new_scale_fill, new_scale_color], 
  grid[unit], 
  readxl[read_excel]
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
data(winddat)
data(raindat)
data(hydrodat)
data(rstrnwts)
data(rsphydat)
data(bstransect)

source(here('R/funcs.R'))

# k brevis data from CL (not created in piney-point repo)
habdat <- read_excel(
    here('data-raw/TB_Karenia_brevis_to_2021-10-15_coord_box_mod_TBsegments_cl.xlsx'),
    sheet = 'subsetData_originalSegments', 
    na = 'NA'
  ) %>% 
  filter(BaySegment %in% c('MTB','LTB')) %>%
  select(date = Sample_Dat, val = Karenia_br, lat = Latitude, lng = Longitude) %>% 
  mutate(
    date = ymd(date)
  )

# map ---------------------------------------------------------------------

# piney point loc
pineypoint <- tibble(
  lon = -82.52469352586753, 
  lat = 27.629819505234703
  ) %>% 
  st_as_sf(coords = c('lon', 'lat'), crs = 4326)

# segments
areas <- ppseg %>% 
  rename(area = Name) %>% 
  group_by(area) %>% 
  summarise() %>% 
  st_buffer(dist = set_units(0.0001, degree)) %>% 
  st_buffer(dist = set_units(-0.0001, degree)) %>% 
  mutate(
    area = factor(area)
  ) %>% 
  st_intersection(segmask) %>% 
  st_cast('POLYGON') %>% 
  mutate(
    acres = st_area(.), 
    acres = set_units(acres, 'acres'), 
    acres = as.numeric(acres)
  ) %>% 
  dplyr::filter(acres > 1e4) %>% 
  st_buffer(dist = set_units(0.0005, degree)) %>% 
  st_buffer(dist = set_units(-0.0005, degree)) 

cols <- c("#E16A86", "#50A315", "#009ADE")
names(cols) <- levels(areas$area)

##
# map

buffdist <- 0.01
northloc <- 'tr' 
scaleloc <- 'bl'

# layer extent as bbox plus buffer
dat_ext <- areas %>% 
  st_bbox %>% 
  st_as_sfc %>% 
  st_buffer(dist = set_units(buffdist, degree)) %>%
  st_bbox %>% 
  unname

# reference data for ggsn, MUST have geometry named column
ggsnref <- areas %>% 
  st_bbox %>% 
  st_as_sfc %>%
  st_buffer(dist = set_units(buffdist / 2, degree)) %>% 
  st_as_sf %>%
  st_cast('POINT') %>% 
  rename(geometry = x)

# stamen base map
bsmap1 <- get_stamenmap(bbox = dat_ext, maptype = 'toner-background', zoom = 10)

# change opacity of basemap
mapatt <- attributes(bsmap1)
bsmap1_transparent <- matrix(adjustcolor(bsmap1, 
                                         alpha.f = 0.2), 
                             nrow = nrow(bsmap1))
attributes(bsmap1_transparent) <- mapatt

# plot
p1 <- ggmap(bsmap1_transparent) +
  geom_sf(data = areas, aes(fill = area), color = NA, inherit.aes = F, alpha = 0.8) +
  scale_fill_manual(values = cols, drop = F, guide = 'none') +
  new_scale_fill() + 
  geom_sf(data = bsstatloc, aes(fill = 'Long-term monitoring', shape = 'Long-term monitoring'), color = 'black', size = 2, inherit.aes= F) +
  geom_sf(data = pineypoint, aes(fill = 'Piney Point', shape = 'Piney Point'), color = 'black', size = 3, inherit.aes= F) + 
  geom_sf_text(data = areas, aes(label = area), color = 'black', inherit.aes = F, alpha = 0.8, size = 6) +
  scale_fill_manual('test', values = c('white', 'black')) +
  scale_shape_manual('test', values= c(21, 24)) +
  theme_bw() + 
  theme(
    legend.title = element_blank(), 
    panel.grid = element_blank(), 
    axis.title = element_blank(), 
    legend.justification = 'top',
    axis.text.y = element_text(size = 8), 
    axis.text.x = element_text(size = 8, angle = 30, hjust = 1),
    panel.background = element_rect(fill = 'white'),
    axis.ticks = element_line(colour = 'grey'),
    panel.border = element_rect(colour = 'grey', fill = NA), 
    legend.position = 'right'
  ) + 
  annotation_scale(location = scaleloc) +
  labs(
    title = '(a) Areas of interest'
  )

tomap <- rsallpts %>% 
  mutate(
    type = case_when(
      grepl('\\,', type) ~ 'mixed monitoring', 
      T ~ type
    )
  ) %>% 
  .[areas, ]

p2a <- ggmap(bsmap1_transparent) +
  geom_sf(data = tomap, aes(color = type), inherit.aes = F) +
  geom_sf(data = pineypoint, fill = 'black', pch = 24, color = 'black', size = 3, inherit.aes= F) + 
  theme_bw() + 
  theme(
    panel.grid = element_blank(), 
    axis.title = element_blank(), 
    legend.justification = 'top',
    axis.text.y = element_text(size = 8), 
    axis.text.x = element_text(size = 8, angle = 30, hjust = 1),
    panel.background = element_rect(fill = 'white'),
    axis.ticks = element_line(colour = 'grey'),
    panel.border = element_rect(colour = 'grey', fill = NA), 
    legend.position = 'right'
  ) +
  labs(
    color = 'Response monitoring',
    title = '(b) Sample locations'
  ) +
  guides(color = guide_legend(override.aes = list(size = 3))) + 
  annotation_north_arrow(location = northloc, which_north = "true", height = grid::unit(0.75, "cm"), 
                         width = grid::unit(0.75, "cm"))

# for inset
states <- us_states() %>% 
  filter(name %in% c('Florida', 'Georgia', 'Alabama'))
ylimrng <- states %>% 
  filter(name %in% 'Florida')
insetbb <- st_buffer(areas, dist = units::set_units(0.5, degree)) %>% 
  st_bbox() %>% 
  st_as_sfc(crs = 4326)
statebuff <- st_buffer(ylimrng, dist = 0.25)
insetylim <- st_bbox(statebuff)[c('ymin', 'ymax')]
insetxlim <- st_bbox(statebuff)[c('xmin', 'xmax')]

lbs1 <- tibble(
  lon = -86, lat = 25.6, label = 'Gulf of\nMexico'
) %>% 
  st_as_sf(coords = c('lon', 'lat'), crs = 4326)
lbs2 <- tibble(
  lon = -81.2, lat = 29, label = 'Florida'
) %>% 
  st_as_sf(coords = c('lon', 'lat'), crs = 4326)

p2b <- ggplot() + 
  geom_sf(data = states, fill = 'grey', colour = 'grey') +
  geom_sf(data = insetbb, fill = NA, color = 'blue', size = 1.5) +
  geom_sf_text(data = lbs1, aes(label = label), size = 5) + 
  geom_sf_text(data = lbs2, aes(label = label), size = 5, angle = -65) + 
  coord_sf(ylim = insetylim, xlim = insetxlim) +
  theme_void() +
  theme( 
    panel.background = element_rect(fill = '#FFFFFF99', colour = 'white'), 
    panel.border = element_rect(colour = 'black', fill = 'transparent')
  ) 

p2 <- p2a + 
  inset(ggplotGrob(p2b), xmin = -82.33, xmax = -82.02, ymin = 27.345, ymax = 27.57)

pout <- p1 + p2 + plot_layout(ncol = 2, guides = 'collect') & 
  theme(
    legend.justification = 'top', 
    legend.box.spacing = unit(0, 'cm'),
    legend.spacing.y = unit(0, 'cm')
    )

jpeg(here('figs/map.jpeg'), height = 4.2, width = 9, family = 'serif', units = 'in', res = 500)
print(pout)
dev.off()

# nutrients, chloropyll, secchi map ---------------------------------------

# nonbay stations
nonbay <- c('BH01', 'P Port 2', 'P Port 3', 'PM Out', '20120409-01', 'PPC41', 'P Port 4', 'PMB01', 'NGS-S Pond')

# colors 
vrscols <- rev(RColorBrewer::brewer.pal(n = 9, name = 'RdYlBu'))
vrscols[7:9] <- 'red'

# ggplot base size
bssz <- 13

thm <-  theme(
    axis.title = element_blank(),
    legend.position = 'bottom', 
    legend.box= 'vertical'
  ) 

# combine water quality data with locations
# removed secchi on bottom and non-detect for tn, chla
wqdat <- rswqdat %>% 
  filter(date < as.Date('2021-10-01')) %>% 
  filter(var %in% c('tn', 'chla', 'secchi')) %>% 
  filter(!station %in% nonbay) %>% 
  filter(!qual %in% c('S', 'U')) %>% 
  inner_join(rsstatloc, ., by = 'station') %>% 
  mutate(
    lng = st_coordinates(.)[, 1], 
    lat = st_coordinates(.)[, 2],
    dategrp = floor_date(date, unit = 'week'), 
    inrng = case_when(
      inrng %in% c('below', 'in range') & var %in% c('chla', 'tn') ~ 'in range', 
      inrng %in% c('above') & var %in% c('chla', 'tn') ~ 'above', 
      inrng %in% c('above', 'in range') & var %in% c('secchi') ~ 'in range',
      inrng %in% c('below') & var %in% c('secchi') ~ 'below'
    )
  ) %>% 
  select(station, date, dategrp, var, val, lat, lng, inrng)

# reference data for ggsn, MUST have geometry named column
dat_ext <- wqdat %>% 
  st_bbox %>% 
  st_as_sfc %>% 
  st_buffer(dist = set_units(0.01, degree)) %>%
  st_bbox %>% 
  unname

# stamen base map
bsmap1 <- get_stamenmap(bbox = dat_ext, maptype = 'toner-background', zoom = 11)

# change opacity of basemap
mapatt <- attributes(bsmap1)
bsmap1_transparent <- matrix(adjustcolor(bsmap1, 
                                         alpha.f = 0.1), 
                             nrow = nrow(bsmap1))
attributes(bsmap1_transparent) <- mapatt

# base map
bsmap <- ggmap(bsmap1_transparent)

toplo1 <- wqdat %>% 
  filter(var == 'tn') %>% 
  mutate(val = pmin(2, val))

# static plot
brks <- c(0.25, 0.5, 1, 2)
lbs <- c('0.25', '0.5', '1', '>2')
p1 <- bsmap +
  geom_point(data = toplo1, aes(x = lng, y = lat, size = val, fill = val, group = dategrp, color = inrng), pch = 21, alpha = 0.8) +
  scale_fill_gradientn('mg/L', trans = 'log10', colours = vrscols, breaks = brks, labels = lbs) +
  scale_color_manual('In normal range?', values = c('black', 'lightgrey')) +
  scale_size('mg/L', range = c(0.5, 5), trans = 'log10', breaks = brks, labels = lbs) + 
  coord_map() + 
  guides(
    fill = guide_legend(order = 1), 
    size = guide_legend(order = 1), 
    color = guide_legend(override.aes = list(size = 6), order = 2)
  ) + 
  theme_bw(base_size = bssz) +
  thm + 
  labs(
    title = '(a) Total Nitrogen'
  )

toplo2 <- wqdat %>% 
  filter(var %in% 'chla') %>% 
  mutate(val = pmin(30, val))

brks <- c(1, 3, 10 , 30)
lbs <- c('1', '3', '10', '>30')
p2 <- bsmap +
  geom_point(data = toplo2, aes(x = lng, y = lat, size = val, fill = val, group = dategrp, color = inrng), pch = 21, alpha = 0.8) +
  scale_fill_gradientn('ug/L', trans = 'log10', breaks = brks, labels = lbs, colours = vrscols) +
  scale_color_manual('In normal range?', values = c('black', 'lightgrey'), guide = T) +
  scale_size('ug/L', range = c(0.5, 5), breaks = brks, labels = lbs, trans = 'log10') + 
  coord_map() + 
  guides(
    fill = guide_legend(order = 1), 
    size = guide_legend(order = 1), 
    color = guide_legend(override.aes = list(size = 6), order = 2)
  ) + 
  theme_bw(base_size = bssz) +
  thm + 
  labs(
    title = '(b) Chlorophyll-a'
  )

toplo3 <- wqdat %>% 
  filter(var %in% 'secchi') %>% 
  mutate(val = pmin(5, val))

brks <- c(0.5, 2, 3.5, 5)
lbs <- c('0.5', '2', '3.5', '>5')
p3 <- bsmap +
  geom_point(data = toplo3, aes(x = lng, y = lat, size = val, fill = val, group = dategrp, color = inrng), pch = 21, alpha = 0.8) +
  scale_fill_gradientn('meters',colours = rev(vrscols), breaks = brks, labels = lbs) +
  scale_color_manual('In normal range?', values = c('black', 'grey'), guide = T) +
  scale_size('meters', range = c(6, 0.5), breaks = brks, labels = lbs) + 
  coord_map() + 
  guides(
    fill = guide_legend(order = 1), 
    size = guide_legend(order = 1), 
    color = guide_legend(override.aes = list(size = 6), order = 2)
  ) + 
  theme_bw(base_size = bssz) +
  thm + 
  labs(
    title = '(c) Secchi'
  )

p <- p1 + p2 + p3

jpeg(here('figs/wqmap.jpeg'), height = 7, width = 12, units = 'in', res = 500, family = 'serif')
print(p)
dev.off()

# seasonal trend plots ----------------------------------------------------

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

cols <- c("#E16A86", "#50A315", "#009ADE")
names(cols) <- c('Area 1', 'Area 2', 'Area 3')

datin <- rswqdat %>% 
  filter(!(var == 'secchi' & val >= 9.5)) # outlier secchi

p1 <- gamplo_fun(datin, bswqdat, ppsegbf, vr = 'tn', cols, logtr = T, rmfacet = T, ttl = '(a) Total Nitrogen', ylb = 'mg/L (log-scale)')
p2 <- gamplo_fun(datin, bswqdat, ppsegbf, vr = 'chla', cols, logtr = T, rmfacet = T, ttl = '(b) Chlorophyll-a', ylb = 'ug/L (log-scale)')
p3 <- gamplo_fun(datin, bswqdat, ppsegbf, vr = 'secchi', cols, logtr = F, rmfacet = F, ttl = '(c) Secchi', ylb = 'meters')

p <- (p1 + p2 + p3 + plot_layout(ncol = 3)) / wrap_elements(grid::textGrob('Day of year', gp = gpar(fontsize=14))) + 
  plot_layout(ncol = 1, guides = 'collect', height = c(1, 0.05)) & 
  theme(legend.position = 'top')

jpeg(here('figs/wqgam.jpeg'), height = 6, width = 8.5, units = 'in', res = 500, family = 'serif')
print(p)
dev.off()

# transect example --------------------------------------------------------

trn <- 'S3T6b'
rmdt <- as.Date('2021-04-07')

mcrdat <- rstrndat %>% 
  filter(date < as.Date('2021-10-01')) %>% 
  filter(station %in% trn) %>% 
  filter(typ == 'mcr') %>% 
  filter(date != rmdt) %>% 
  mutate(taxa = fct_drop(taxa))
savdat <- rstrndat %>% 
  filter(date < as.Date('2021-10-01')) %>% 
  filter(station %in% trn) %>% 
  filter(typ == 'sav') %>% 
  filter(date != rmdt) %>% 
  mutate(taxa = fct_drop(taxa))

mcrsel <- mcrdat %>% 
  filter(bb != 0) %>% 
  pull(taxa) %>% 
  unique %>% 
  as.character()
savsel <- savdat %>% 
  filter(bb != 0) %>% 
  pull(taxa) %>% 
  unique %>% 
  as.character()

p <- show_rstransect(savdat, mcrdat, savsel, mcrsel, rev = T, sclloc = T)

jpeg(here('figs/trnex.jpeg'), height = 7, width = 8, units = 'in', res = 500, family = 'serif')
print(p)
dev.off()

# transect frequency occurrence -------------------------------------------

mcrsel <- c("Red", "Green", "Brown", "Cyanobacteria")
savsel <- c('Thalassia testudinum', 'Halodule wrightii', 'Syringodium filiforme')

colpal <- colorRampPalette(RColorBrewer::brewer.pal(n = 8, name = 'Dark2'))
savlevs <- c('Thalassia testudinum', 'Halodule wrightii', 'Syringodium filiforme', 'Ruppia maritima', 'Halophila engelmannii', 'Halophila decipiens')
savcol <- colpal(length(savlevs))
names(savcol) <- savlevs
savcol <- savcol[savsel]
mcrcol <- c('tomato1', 'lightgreen', 'burlywood3', 'lightblue')
names(mcrcol) <- mcrsel
mcrcol <- mcrcol[mcrsel]
cols <- c(mcrcol, savcol)
cols <- c(cols, Total = 'white')

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

# # view sample effort by transect, area, month
# smpeff <- rstrndat %>%
#   inner_join(rstrnpts, ., by = 'station') %>%
#   select(-source, -type, -lng, -lat) %>%
#   st_intersection(areas) %>%
#   st_set_geometry(NULL) %>%
#   mutate(
#     mo = month(date)
#   ) %>%
#   group_by(station, area, mo) %>%
#   summarise(
#     obs = (any(bb > 0)),
#     .groups = 'drop'
#   ) %>%
#   spread(mo, obs) %>% 
#   arrange(area, station)
# View(smpeff)

# tokp <- smpeff %>%
#   gather('var', 'val', -station, -area) %>%
#   na.omit() %>%
#   group_by(station, area) %>%
#   summarise(cnt = sum(val), .groups= 'drop') %>%
#   filter(cnt >= 4) %>%
#   pull(station)

# add area
trnsum <- rstrndat %>%
  # filter(station %in% tokp) %>%
  inner_join(rstrnpts, ., by = 'station') %>% 
  st_intersection(areas) %>% 
  st_set_geometry(NULL) %>%
  dplyr::group_by(area, typ, date, station, taxa, location) %>%
  dplyr::summarise(
    pa = as.numeric(any(bb > 0))
  ) %>% 
  mutate(
    date = floor_date(date, unit = 'month'), 
    typ = factor(typ, levels = c('mcr', 'sav'), labels = c('Macroalgae', 'Seagrass'))
  ) %>% 
  group_by(area, typ, date, taxa) %>% 
  summarize(
    foest = sum(pa) / length(pa)
  ) %>%
  filter(taxa %in% c(mcrsel, savsel))

trnsumtots <- rstrndat %>%
  # filter(station %in% tokp) %>% 
  inner_join(rstrnpts, ., by = 'station') %>% 
  st_intersection(areas) %>% 
  st_set_geometry(NULL) %>%
  dplyr::group_by(area, typ, date, station, location) %>%
  dplyr::summarise(
    pa = as.numeric(any(bb > 0))
  ) %>%  
  mutate(
    date = floor_date(date, unit = 'month'), 
    typ = factor(typ, levels = c('mcr', 'sav'), labels = c('Macroalgae', 'Seagrass'))
  ) %>% 
  group_by(area, typ, date) %>% 
  summarize(
    foest = sum(pa) / length(pa)
  ) %>% 
  mutate(taxa = 'Total')

toplo <- bind_rows(trnsum, trnsumtots)

dodge <- position_dodge(width=7) 

p <- ggplot(toplo, aes(x = date, y = foest)) + 
  geom_line(aes(group = taxa), position = dodge) +
  geom_point(aes(fill = taxa, group = taxa), pch = 21, stat = 'identity', color = 'black', size = 3, position = dodge, stroke = 1) +
  facet_grid(typ ~ area) +
  theme_minimal(base_size = 14) + 
  scale_y_continuous(limits = c(0, 1))+
  scale_fill_manual(values = cols) +
  labs(
    y = 'Freq. occurrence'
  ) +
  theme(
    legend.position = 'top', 
    legend.title = element_blank(),
    strip.background = element_blank(), 
    strip.text = element_text(size = 14), 
    axis.title.x = element_blank(), 
    axis.ticks.x = element_line(), 
    panel.grid.minor = element_blank(),
    # panel.spacing=unit(2, "lines"), 
    panel.background = element_rect(fill = 'grey95', color = 'white'), 
    panel.grid.major = element_line(color = 'grey90')
  )

jpeg(here('figs/trnfrq.jpeg'), height = 6, width = 9, units = 'in', res = 500, family = 'serif')
print(p)
dev.off()

save(trnsum, file = here('data/trnsum.RData'))

# red tide, fish kills, wind, flow, precip --------------------------------

# data from https://public.myfwc.com/fwri/FishKillReport/searchresults.aspx
# requested hillsborough, pinellas, manatee 1/1/95 to early Oct
fishdat <- read.csv(here('data-raw/FishKillResultReport.csv')) %>% 
  select(
    date = textBox6, 
    county = tEMPDataTextBox,
    city = cOUNTYDataTextBox, 
    waterbody = lOCATIONDataTextBox,
    species = textBox18
  ) %>% 
  mutate(
    date = mdy(date),
    yr = year(date),
    week = floor_date(date, unit = 'week'), 
    week = factor(format(week, '%b %d')), 
    week = factor(week, levels = as.character(unique(week))), 
    county = case_when(
      county %in% c('Pinellas ', 'Pinellas', 'pinellas') ~ 'Pinellas', 
      county %in% c('Hillsborough', 'Hillsborough ') ~ 'Hillsborough', 
      T ~ county
    ),
    city = gsub('\\s+$', '', city),
    city = gsub('^St\\s', 'St. ', city),
    city = case_when(
      city %in% c('St. pete Beach', 'St. Pete Beach', 'St. Petersburg Beach') ~ 'St. Petersburg Beach', 
      city %in% 'Tierra Ceia' ~ 'Terra Ceia', 
      city %in% 'dunedin' ~ 'Dunedin',
      T ~ city
    )
  ) %>% 
  filter(date < as.Date('2021-10-01'))

# levels for week, starts on first of week from jan through july
weeklv <- seq.Date(from = as.Date('2021-01-01'), to = Sys.Date(), by = 'days') %>% 
  lubridate::floor_date(unit = 'week') %>% 
  unique %>% 
  tibble(
    dt = ., 
    yr = year(dt), 
    mo = month(dt), 
    lb = format(dt, '%b %d')
  ) %>%
  filter(yr > 2020) %>% 
  filter(mo < 10) %>% 
  pull(lb)

# habdat p1
toplo <- habdat %>%
  filter(date < as.Date('2021-10-01')) %>% 
  filter(month(date) > 3 & month(date) < 10) %>% 
  mutate(
    yr = year(date),
    yr = factor(yr, levels = seq(min(yr), max(yr)))
  ) %>% 
  complete(yr)

# plot
p1 <- ggplot(toplo, aes(x = yr, y = 1 + val)) +
  geom_point(position = position_jitter(width = 0.1), alpha = 0.6) + 
  scale_y_continuous(labels = function(x) as.numeric(format(x, scientific = FALSE))) +
  scale_x_discrete(breaks = seq(1950, 2025, by = 5)) + 
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, size = 8, hjust = 1),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  labs(
    x = 'Year',
    y = 'Cells / L',
    title = expression(paste('(a) ', italic('K. brevis'), ' Apr - Sep concentrations by year, middle/lower Tampa Bay'))
  )

# habdat p2
toplo <- habdat %>%
  filter(year(date) >= 2021) %>%
  filter(month(date) < 10) %>% 
  mutate(
    week = floor_date(date, unit = 'week'),
    week = factor(format(week, '%b %d')), 
    week = factor(week, levels = weeklv)
  ) %>%
  # group_by(week)# %>%
  # summarise(
  #   cnt = n(),
  #   y0 = min(val, na.rm = T), 
  #   y25 = quantile(val, prob = 0.25, na.rm = T),
  #   y50 = quantile(val, prob = 0.5, na.rm = T),
  #   y75 = quantile(val, prob = 0.75, na.rm = T),
  #   y100 = max(val, na.rm = T),
  #   .groups = 'drop'
  # ) %>%
  complete(week)

# plot
p2 <- ggplot(toplo, aes(x = week, y =  1 + val)) +
  geom_point(position = position_jitter(width = 0.1), alpha = 0.6) + 
  # geom_boxplot(
  #   aes(ymin = y0, lower = y25, middle = y50, upper = y75, ymax = y100),
  #   stat = "identity", width = 0.75, fill = '#00806E'
  # ) +
  # scale_y_log10(labels = function(x) as.numeric(format(x, scientific = FALSE))) +
  scale_y_continuous(labels = function(x) as.numeric(format(x, scientific = FALSE))) +
  # geom_hline(aes(yintercept = 1e5, color = 'Bloom\nconcentration')) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, size = 8, hjust = 1),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  labs(
    x= 'Week of',
    y = 'Cells / L',
    title = expression(paste('(b) ', italic('K. brevis'), ' concentrations in 2021 by week, middle/lower Tampa Bay'))
  )

# precip 
toplo <- raindat %>% 
  filter(grepl('^Tampa', station)) %>% 
  mutate(
    doy = yday(date), 
    yr = year(date), 
    mo = month(date)
  ) %>% 
  group_by(yr) %>% 
  arrange(yr, doy) %>% 
  mutate(precip_cm = cumsum(precip_cm)) %>% 
  ungroup() %>% 
  filter(mo < 10) %>% 
  mutate(
    flvl = case_when(
      yr == 2021 ~ '2021', 
      T ~ '1995 - 2020'
    ), 
    xvals = ymd(paste('2021', month(date), day(date), sep = '-'))
  )

toplo1 <- toplo %>% 
  filter(yr == 2021)
toplo2 <- toplo %>% 
  filter(yr < 2021) %>% 
  group_by(xvals) %>% 
  summarise(
    lov = quantile(precip_cm, 0.25, na.rm = T),
    hiv = quantile(precip_cm, 0.75, na.rm = T),
  ) %>% 
  ungroup()
p3 <- ggplot() + 
  geom_ribbon(data = toplo2, aes(x = xvals, ymin = lov, ymax = hiv, fill = '1995 - 2020\n25th - 75th %tile'), alpha = 0.7) +
  geom_line(data = toplo1, aes(x = xvals, y = precip_cm, color = '2021'), size = 1.2) +
  scale_color_manual(values = '#00806E') +
  scale_fill_manual(values = 'grey') + 
  scale_x_date(date_breaks = 'month', date_labels = '%b %d', expand = c(0, 0)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, size = 8, hjust = 1),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(), 
    legend.title = element_blank()
  ) +
  labs(
    x = 'Day of year',
    y = 'Precip. (cm)',
    title = '(c) Cumulative precipitation in 2021',
    color = 'Year group', 
    size = 'Year group'
  )

# flow data
toplo <- hydrodat %>% 
  mutate(
    doy = yday(date), 
    yr = year(date), 
    mo = month(date)
  ) %>% 
  group_by(yr) %>% 
  arrange(yr, doy) %>% 
  mutate(
    flow_m3 = cumsum(flow_m3),
    flow_km3 = flow_m3 / 1e9
  ) %>% 
  ungroup() %>% 
  filter(mo < 10) %>% 
  mutate(
    flvl = case_when(
      yr == 2021 ~ '2021', 
      T ~ '1995 - 2020'
    ), 
    xvals = ymd(paste('2021', month(date), day(date), sep = '-'))
  )

toplo1 <- toplo %>% 
  filter(yr == 2021)
toplo2 <- toplo %>% 
  filter(yr < 2021) %>% 
  group_by(xvals) %>% 
  summarise(
    lov = quantile(flow_km3, 0.25, na.rm = T),
    hiv = quantile(flow_km3, 0.75, na.rm = T),
  ) %>% 
  ungroup()
p4 <- ggplot() + 
  geom_ribbon(data = toplo2, aes(x = xvals, ymin = lov, ymax = hiv, fill = '1995 - 2020\n25th - 75th %tile'), alpha = 0.7) +
  geom_line(data = toplo1, aes(x = xvals, y = flow_km3, color = '2021'), size = 1.2) +
  scale_color_manual(values = '#00806E') +
  scale_fill_manual(values = 'grey') + 
  scale_x_date(date_breaks = 'month', date_labels = '%b %d', expand = c(0, 0)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, size = 8, hjust = 1),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(), 
    legend.title = element_blank()
  ) +
  labs(
    x = 'Day of year',
    y = expression(paste('Inflow (', km^3, ')')),
    title = '(d) Cumulative inflow in 2021',
    color = 'Year group', 
    size = 'Year group'
  )

# fish kills
toplo1 <- fishdat %>% 
  filter(city %in% c('Tampa', 'St. Petersburg')) %>% 
  filter(yr >= 2021) %>%  
  group_by(week, city) %>% 
  summarise(
    cnt = n(), 
    .groups = 'drop'
  ) %>% 
  mutate(
    week = factor(week, levels = weeklv)
  ) %>% 
  complete(week)

p5 <- ggplot(toplo1, aes(x = week, fill = city, y = cnt)) + 
  geom_bar(stat = 'identity', colour = 'darkgrey') + 
  labs(
    x = 'Week of',
    y = 'No. of reports', 
    title = '(e) Fish kill reports for red tide in 2021 by week'
  ) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_brewer('City', palette = 'Pastel1') + 
  theme_minimal() + 
  theme(
    # axis.ticks.x = element_line(),
    # axis.title.x = element_blank(), 
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
    legend.position = 'right', 
    panel.grid.minor.y = element_blank(), 
    panel.grid.minor.x = element_blank(), 
    panel.grid.major.x = element_blank()
  )

# wind roses
toplo <- na.omit(winddat)

dts <- as.Date(c('2021-01-01', '2021-04-01', '2021-07-03', '2021-07-05', '2021-07-07', '2021-10-01'))

toplo1 <- toplo %>% 
  filter(as.Date(datetime) >= dts[1] & as.Date(datetime) < dts[2])
toplo2 <- toplo %>% 
  filter(as.Date(datetime) >= dts[2] & as.Date(datetime) < dts[3])
toplo3 <- toplo %>% 
  filter(as.Date(datetime) >= dts[3] & as.Date(datetime) < dts[4])
toplo4 <- toplo %>% 
  filter(as.Date(datetime) >= dts[4] & as.Date(datetime) < dts[5])
toplo5 <- toplo %>% 
  filter(as.Date(datetime) >= dts[5] & as.Date(datetime) < dts[6])

spdmin <- 0
spdmax <- 14
spdres = 3
thm <- theme_minimal()

pa <- plot.windrose(spd = toplo1$wind_ms, dir = toplo1$wind_dir, spdres = spdres, spdmin = spdmin, spdmax = spdmax) + 
  labs(
    title = '(f) Wind rose plots for 2021',
    subtitle ='Jan 1st - Mar 31st'
  )
pb <- plot.windrose(spd = toplo2$wind_ms, dir = toplo2$wind_dir, spdres = spdres, spdmin = spdmin, spdmax = spdmax) + 
  labs(
    subtitle ='Apr 1st - Jul 2nd'
  )
pc <- plot.windrose(spd = toplo3$wind_ms, dir = toplo3$wind_dir, spdres = spdres, spdmin = spdmin, spdmax = spdmax) + 
  labs(
    subtitle ='Pre-storm, Jul 3rd - 4th'
  )
pd <- plot.windrose(spd = toplo4$wind_ms, dir = toplo4$wind_dir, spdres = spdres, spdmin = spdmin, spdmax = spdmax) + 
  labs(
    subtitle ='Post-storm, Jul 5th - 6th'
  )
pe <- plot.windrose(spd = toplo5$wind_ms, dir = toplo5$wind_dir, spdres = spdres, spdmin = spdmin, spdmax = spdmax) + 
  labs(
    subtitle ='Jul 7th - Sep 30th'
  )

p6 <- pa + pb + pc + pd + pe + plot_layout(ncol = 5, guides = 'collect') & 
  theme_minimal() + 
  theme(
    axis.text = element_blank(), 
    axis.title = element_blank(), 
    axis.ticks.y = element_blank(), 
    legend.position = 'right',
    plot.subtitle = element_text(size = 9)
  )

# all plots together
p34 <- (p3 + p4 + plot_layout(ncol = 2, guides = 'collect')) 
# & theme(axis.title.x = element_blank())) / wrap_elements(grid::textGrob('Day of year', gp = gpar(fontsize=11))) + 
# plot_layout(ncol = 1, heights = c(1, 0.05))
p <- p1 + p2 + p34 + p5 + p6 +
  plot_layout(ncol = 1, heights = c(1, 1, 1, 1, 1))

jpeg(here('figs/redtide.jpeg'), height = 11, width = 9, units = 'in', res = 500, family = 'serif')
print(p)
dev.off()

# nutrient flow -----------------------------------------------------------

brks <- seq.Date(as.Date('2021-03-28'), as.Date('2021-09-26'), by = '1 week')
vrs <- c('chla', 'tn')
nonbay <- c('BH01', 'P Port 2', 'P Port 3', 'PM Out', '20120409-01', 'PPC41', 'P Port 4', 'PMB01', 'NGS-S Pond')

# water quality summarized
wqsum <- rswqdat %>% 
  filter(var %in% vrs) %>% 
  filter(date < as.Date('2021-10-01')) %>% 
  filter(source == 'fldep') %>%
  filter(!station %in% nonbay) %>% 
  # inner_join(rsstatloc, ., by = c('station', 'source')) %>% 
  # st_intersection(areas) %>% 
  # st_set_geometry(NULL) %>% 
  mutate(cens = grepl('U', qual)) %>% 
  select(date, var, val, cens, station) %>% 
  mutate(
    date = floor_date(date, unit = 'week')
  ) %>% 
  group_by(date, var) %>% 
  summarise(
    medv = ifelse(
      any(cens), median(cenfit(val, cens), na.rm = T),
      median(val, na.rm = T)
    ),
    hiv = ifelse(
      any(cens), quantile(cenfit(val, cens), prob = 0.975, na.rm = T),
      quantile(val, prob = 0.975, na.rm = T)
    ),
    lov = ifelse(
      any(cens), quantile(cenfit(val, cens), prob = 0.025, na.rm = T),
      quantile(val, prob = 0.025, na.rm = T)
    ),
    .groups = 'drop'
  )

toplo1 <- wqsum %>% filter(
  var == 'tn'
)
p1 <- ggplot(toplo1, aes(x = date, y = medv)) + 
  geom_point() + 
  # geom_line() + 
  geom_errorbar(aes(ymin = lov, ymax = hiv), width = 0) +
  scale_x_date(breaks = brks, date_labels = '%b %d', limits = range(brks), expand = c(0.01, 0.01)) +
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
    y = 'mg / L', 
    x = NULL, 
    title = '(a) Total nitrogen concentration'
  )

toplo2 <- wqsum %>% filter(
  var == 'chla'
)
p2 <- ggplot(toplo2, aes(x = date, y = medv)) + 
  geom_point() + 
  # geom_line() + 
  geom_errorbar(aes(ymin = lov, ymax = hiv), width = 0) +
  scale_x_date(breaks = brks, date_labels = '%b %d', limits = range(brks), expand = c(0.01, 0.01)) +
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
    y = expression(paste(mu, 'g / L')), 
    x = NULL, 
    title = '(b) Chlorophyll-a concentration'
  )

# phytoplankton summarized
physum <- rsphydat %>%
  filter(date < as.Date('2021-10-01')) %>% 
  filter(source %in% c('epchc', 'pinco')) %>%
  filter(!station %in% nonbay) %>% 
  mutate(
    species = case_when(
      species %in% c('Skeletonema sp.', 'Skeletonema Costatum', 'Asterionellopsis glacialis', 'Asterionellopsis') ~ 'Diatoms', 
      T ~ 'other'
    ),
    species = factor(species, levels = unique(species))
  ) %>% 
  select(date, station, species, val) %>% 
  group_by(date, station) %>% 
  complete(
    species,
    fill = list(val = 0)
  ) %>% 
  ungroup() %>% 
  mutate(
    week = floor_date(date, unit = 'week')
  ) %>% 
  filter(species == 'Diatoms') %>% 
  select(week, species, val)

habsum <- habdat %>% 
  filter(year(date) >= 2021) %>%
  filter(month(date) < 10) %>% 
  mutate(
    week = floor_date(date, unit = 'week'), 
    species = 'K. brevis'
  ) %>%
  complete(week) %>% 
  select(week, species, val)

# levels for week, starts on first of week
phyplo <- bind_rows(habsum, physum) %>% 
  filter(week >= as.Date('2021-03-28')) %>% 
  group_by(week, species) %>% 
  summarise(
    medv = quantile(val, 0.5, na.rm = T), 
    lov = quantile(val, 0.025, na.rm = T), 
    hiv = quantile(val, 0.975, na.rm = T), 
    .groups = 'drop'
  )

toplo1 <- phyplo %>% 
  filter(species == 'Diatoms')

# plot
p3 <- ggplot(toplo1, aes(x = week, y = medv)) +
  geom_point() + 
  # geom_line() + 
  geom_errorbar(aes(ymin = lov, ymax = hiv), width = 0) +
  scale_y_continuous(labels = function(x) as.numeric(format(x, scientific = F))) +
  scale_x_date(breaks = brks, date_labels = '%b %d', limits = range(brks), expand = c(0.01, 0.01)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, size = 8, hjust = 1),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  labs(
    x = NULL,
    y = 'Cells / L',
    title = '(c) Diatom cell concentrations'
  )

# macroalgal weight summarized
rssub <- rstrndat %>% 
  filter(typ == 'mcr') %>% 
  select(station, location, date, taxa, bb)

bssub <- bstransect %>% 
  filter(var == 'Abundance') %>% 
  select(
    station = Transect, 
    location = Site, 
    date = Date,
    taxa = Savspecies,
    bb = aveval
  ) %>% 
  filter(date %in% rstrnwts$date & station %in% rstrnwts$station & location %in% rstrnwts$location) %>% 
  filter(grepl('^DA', taxa)) %>% 
  mutate(
    location = as.numeric(location), 
    taxa = case_when(
      grepl('Red', taxa) ~ 'Red', 
      grepl('Green', taxa) ~ 'Green', 
      grepl('Macroalgae', taxa) ~ 'Red',
      T ~ taxa
    )
  ) 

tojn <- bind_rows(rssub, bssub)
wtssub <- rstrnwts %>% 
  select(-genus) %>% 
  rename(taxa = group) %>% 
  filter(!grepl('and', taxa)) %>% 
  left_join(tojn, by = c('station', 'date', 'location', 'taxa')) %>% 
  mutate(
    weight = weight_g * 0.004 # g / 0.25m2 to kg / m2
  )

wtsmod <- wtssub %>% 
  group_by(taxa) %>% 
  nest() %>% 
  mutate(
    mod = purrr::map(data, lm, formula = weight ~ 0 + bb)
  ) %>% 
  select(taxa, mod)

wtsest <- rstrndat %>% 
  filter(taxa %in% c('Red', 'Green', 'Cyanobacteria')) %>% 
  group_by(taxa) %>% 
  nest %>% 
  left_join(wtsmod, by = 'taxa') %>% 
  mutate(
    weight_kgm2 = purrr::pmap(list(object = mod, newdata = data), predict)
  ) %>% 
  select(-mod) %>% 
  unnest(c('data', 'weight_kgm2')) %>% 
  mutate(
    week = floor_date(date, unit = 'week')
  ) %>% 
  group_by(taxa, week, station) %>%
  summarise(
    weight_kgm2 = mean(weight_kgm2, na.rm = T), 
    .groups = 'drop'
  ) %>%
  group_by(taxa, week) %>% 
  summarise(
    medv = quantile(weight_kgm2, 0.5, na.rm = T), 
    lov = quantile(weight_kgm2, 0.025, na.rm = T), 
    hiv = quantile(weight_kgm2, 0.975, na.rm = T), 
    .groups = 'drop'
  ) %>% 
  filter(taxa == 'Cyanobacteria')

p4 <- ggplot(wtsest, aes(x = week, y = medv)) + 
  geom_point() + 
  # geom_line() + 
  geom_errorbar(aes(ymin = lov, ymax = hiv), width = 0) +
  scale_x_date(breaks = brks, date_labels = '%b %d', limits = range(brks), expand = c(0.01, 0.01)) +
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
    y = expression(paste('kg / ', m^2)), 
    x = 'Week of', 
    title = '(d) Cyanobacteria macroalgae abundances'
  )

# k brevis plot
toplo2 <- phyplo %>% 
  filter(species == 'K. brevis')

p5 <- ggplot(toplo2, aes(x = week, y = medv)) +
  geom_point() + 
  # geom_line() + 
  geom_errorbar(aes(ymin = lov, ymax = hiv), width = 0) +
  scale_y_continuous(labels = function(x) as.numeric(format(x, scientific = FALSE))) +
  scale_x_date(breaks = brks, date_labels = '%b %d', limits = range(brks), expand = c(0.01, 0.01)) +
  # geom_hline(aes(yintercept = 1e5, color = 'Bloom\nconcentration')) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, size = 8, hjust = 1),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  labs(
    x = NULL,
    y = 'Cells / L',
    title = expression(paste('(e) ', italic('K. brevis'), ' cell concentrations'))
  )

# combine all
p <- p1 + p2 + p3 + p4 + p5 + plot_layout(ncol = 1)

jpeg(here('figs/nutrientflow.jpeg'), height = 8, width = 6, units = 'in', res = 500, family = 'serif')
print(p)
dev.off()

# Supplement figures ------------------------------------------------------

## weekly plots -----------------------------------------------------------

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

cols <- c("#E16A86", "#50A315", "#009ADE")
names(cols) <- levels(ppseg$area)

datin <- rswqdat %>% 
  filter(date < as.Date('2021-10-01')) %>% 
  filter(var %in% c('tn', 'chla', 'secchi')) %>% 
  filter(!qual %in% c('S', 'U')) %>% # remove secchi on bottom, nondetect for chla, tn
  filter(!(var == 'secchi' & val >= 9.5)) # outlier secchi

p1 <- wqplo_fun(datin, bswqdat, ppsegbf, vr = 'tn', cols, logtr = TRUE, rmfacet = TRUE, ttl = '(a) Total Nitrogen', ylb = 'mg/L (log-scale)')
p2 <- wqplo_fun(datin, bswqdat, ppsegbf, vr = 'chla', cols, logtr = TRUE, rmfacet = TRUE, ttl = '(b) Chlorophyll-a', ylb = 'ug/L (log-scale)')
p3 <- wqplo_fun(datin, bswqdat, ppsegbf, vr = 'secchi', cols, logtr = FALSE, ttl = '(c) Secchi', ylb = 'meters')

p <- (p1 + p2 + p3 + plot_layout(ncol = 3)) / wrap_elements(grid::textGrob('Week of', gp = gpar(fontsize=14))) + 
  plot_layout(ncol = 1, guides = 'collect', height = c(1, 0.05)) & 
  theme(legend.position = 'top')

jpeg(here('figs/wqtrnds-supp.jpeg'), height = 6, width = 8.5, units = 'in', res = 500, family = 'serif')
print(p)
dev.off()

## weekly plots - additional variables ------------------------------------

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

cols <- c("#E16A86", "#50A315", "#009ADE")
names(cols) <- levels(ppseg$area)

datin <- rswqdat %>% 
  filter(date < as.Date('2021-10-01')) %>% 
  filter(var %in% c('nh34', 'orthop', 'tp', 'dosat', 'turb', 'sal')) %>% 
  filter(!qual %in% c('S', 'U')) %>% # remove secchi on bottom, nondetect for chla, tn
  filter(!(var == 'sal' & source == 'ncf'))# there are some low salinity values in July in MR that skew the plots

p1 <- wqplo_fun(datin, bswqdat, ppsegbf, vr = 'nh34', cols, logtr = TRUE, rmfacet = TRUE, ttl = '(a) NH3, NH4+', ylb = 'mg/L (log-scale)')
p2 <- wqplo_fun(datin, bswqdat, ppsegbf, vr = 'tp', cols, logtr = TRUE, rmfacet = TRUE, ttl = '(b) Total phosphorus', ylb = 'mg/L (log-scale)')
p3 <- wqplo_fun(datin, bswqdat, ppsegbf, vr = 'orthop', cols, logtr = TRUE, ttl = '(c) Ortho-phosphate', ylb = 'mg/L (log-scale)')
p4 <- wqplo_fun(datin, bswqdat, ppsegbf, vr = 'dosat', cols, logtr = FALSE, rmfacet = TRUE, ttl = '(d) Dissolved oxygen sat.', ylb = '%')
p5 <- wqplo_fun(datin, bswqdat, ppsegbf, vr = 'turb', cols, logtr = TRUE, rmfacet = TRUE, ttl = '(e) Turbidity', ylb = 'NTU (log-scale)')
p6 <- wqplo_fun(datin, bswqdat, ppsegbf, vr = 'sal', cols, logtr = FALSE, ttl = '(f) Salinity', ylb = 'ppt')


p <- (p1 + p2 + p3 + p4 + p5 + p6 + plot_layout(ncol = 3)) / wrap_elements(grid::textGrob('Week of', gp = gpar(fontsize=14))) + 
  plot_layout(ncol = 1, guides = 'collect', height = c(1, 0.025)) & 
  theme(legend.position = 'top')

jpeg(here('figs/wqtrndsadd-supp.jpeg'), height = 9, width = 10.5, units = 'in', res = 500, family = 'serif')
print(p)
dev.off()

## seasonal trend plots - additional variables ----------------------------

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

cols <- c("#E16A86", "#50A315", "#009ADE")
names(cols) <- c('Area 1', 'Area 2', 'Area 3')

datin <- rswqdat %>% 
  filter(date < as.Date('2021-10-01')) %>% 
  filter(var %in% c('nh34', 'orthop', 'tp', 'dosat', 'turb', 'sal')) %>% 
  filter(!qual %in% c('S', 'U')) %>% # remove secchi on bottom, nondetect for chla, tn
  filter(!(var == 'sal' & source == 'ncf'))# there are some low salinity values in July in MR that skew the plots

p1 <- gamplo_fun(datin, bswqdat, ppsegbf, vr = 'nh34', cols, logtr = T, rmfacet = T, ttl = '(a) NH3, NH4+', ylb = 'mg/L (log-scale)')
p2 <- gamplo_fun(datin, bswqdat, ppsegbf, vr = 'tp', cols, logtr = T, rmfacet = T, ttl = '(b) Total phosphorus', ylb = 'mg/L (log-scale)')
p3 <- gamplo_fun(datin, bswqdat, ppsegbf, vr = 'orthop', cols, logtr = T, ttl = '(c) Ortho-phosphate', ylb = 'mg/L (log-scale)')
p4 <- gamplo_fun(datin, bswqdat, ppsegbf, vr = 'dosat', cols, logtr = F, rmfacet = T, ttl = '(d) Dissolved oxygen sat.', ylb = '%')
p5 <- gamplo_fun(datin, bswqdat, ppsegbf, vr = 'turb', cols, logtr = T, rmfacet = T, ttl = '(e) Turbidity', ylb = 'NTU (log-scale)')
p6 <- gamplo_fun(datin, bswqdat, ppsegbf, vr = 'sal', cols, logtr = F, ttl = '(f) Salinity', ylb = 'ppt')

p <- (p1 + p2 + p3 + p4 + p5 + p6 + plot_layout(ncol = 3)) / wrap_elements(grid::textGrob('Day of year', gp = gpar(fontsize=14))) + 
  plot_layout(ncol = 1, guides = 'collect', height = c(1, 0.025)) & 
  theme(legend.position = 'top')

jpeg(here('figs/wqgamadd-supp.jpeg'), height = 9, width = 10.5, units = 'in', res = 500, family = 'serif')
print(p)
dev.off()

## transect abundance -----------------------------------------------------

mcrsel <- c("Red", "Green", "Brown", "Cyanobacteria")
savsel <- c('Thalassia testudinum', 'Halodule wrightii', 'Syringodium filiforme')

colpal <- colorRampPalette(RColorBrewer::brewer.pal(n = 8, name = 'Dark2'))
savlevs <- c('Thalassia testudinum', 'Halodule wrightii', 'Syringodium filiforme', 'Ruppia maritima', 'Halophila engelmannii', 'Halophila decipiens')
savcol <- colpal(length(savlevs))
names(savcol) <- savlevs
savcol <- savcol[savsel]
mcrcol <- c('tomato1', 'lightgreen', 'burlywood3', 'lightblue')
names(mcrcol) <- mcrsel
mcrcol <- mcrcol[mcrsel]
cols <- c(mcrcol, savcol)
cols <- c(cols, Total = 'white')

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

# # view sample effort by transect, area, month
# smpeff <- rstrndat %>%
#   inner_join(rstrnpts, ., by = 'station') %>%
#   select(-source, -type, -lng, -lat) %>%
#   st_intersection(areas) %>%
#   st_set_geometry(NULL) %>%
#   mutate(
#     mo = month(date)
#   ) %>%
#   group_by(station, area, mo) %>%
#   summarise(
#     obs = (any(bb > 0)),
#     .groups = 'drop'
#   ) %>%
#   spread(mo, obs) %>% 
#   arrange(area, station)
# View(smpeff)

# tokp <- smpeff %>%
#   gather('var', 'val', -station, -area) %>%
#   na.omit() %>%
#   group_by(station, area) %>%
#   summarise(cnt = sum(val), .groups= 'drop') %>%
#   filter(cnt >= 4) %>%
#   pull(station)

# add area
trnsum <- rstrndat %>%
  # filter(station %in% tokp) %>%
  inner_join(rstrnpts, ., by = 'station') %>% 
  st_intersection(areas) %>% 
  st_set_geometry(NULL) %>%
  dplyr::group_by(area, typ, date, station, taxa, location) %>%
  dplyr::summarise(
    bb = mean(bb, na.rm = T)
  ) %>% 
  dplyr::group_by(area, typ, date, station, taxa) %>%
  dplyr::summarise(
    bb = mean(bb, na.rm = T)
  ) %>% 
  mutate(
    date = floor_date(date, unit = 'month'), 
    typ = factor(typ, levels = c('mcr', 'sav'), labels = c('Macroalgae', 'Seagrass'))
  ) %>% 
  group_by(area, typ, date, taxa) %>% 
  dplyr::summarise(
    bbave = mean(bb, na.rm = T),
    bbhiv = t.test(bb)$conf.int[2], 
    bblov = t.test(bb)$conf.int[1]
  ) %>% 
  filter(taxa %in% c(mcrsel, savsel))

trnsumtots <- rstrndat %>%
  # filter(station %in% tokp) %>% 
  inner_join(rstrnpts, ., by = 'station') %>% 
  st_intersection(areas) %>% 
  st_set_geometry(NULL) %>%
  dplyr::group_by(area, typ, date, station, taxa, location) %>%
  dplyr::summarise(
    bb = mean(bb, na.rm = T)
  ) %>% 
  dplyr::group_by(area, typ, date, station, taxa) %>%
  dplyr::summarise(
    bb = mean(bb, na.rm = T)
  ) %>%  
  mutate(
    date = floor_date(date, unit = 'month'), 
    typ = factor(typ, levels = c('mcr', 'sav'), labels = c('Macroalgae', 'Seagrass'))
  ) %>% 
  group_by(area, typ, date) %>% 
  dplyr::summarise(
    bbave = mean(bb, na.rm = T),
    bbhiv = t.test(bb)$conf.int[2], 
    bblov = t.test(bb)$conf.int[1]
  ) %>% 
  mutate(taxa = 'Total')

toplo <- bind_rows(trnsum)#, trnsumtots)

dodge <- position_dodge(width=7) 

p <- ggplot(toplo, aes(x = date, y = bbave)) + 
  geom_line(aes(group = taxa), position = dodge) +
  geom_errorbar(aes(ymin = bblov, ymax = bbhiv, group = taxa), width = 0, position = dodge) +
  geom_point(aes(fill = taxa, group = taxa), pch = 21, stat = 'identity', color = 'black', size = 3, position = dodge, stroke = 1) +
  facet_grid(typ ~ area, scales = 'free_y') +
  theme_minimal(base_size = 14) + 
  coord_cartesian(ylim = c(0, NA)) +
  scale_fill_manual(values = cols) +
  labs(
    y = 'Freq. occurrence'
  ) +
  theme(
    legend.position = 'top', 
    legend.title = element_blank(),
    strip.background = element_blank(), 
    strip.text = element_text(size = 14), 
    axis.title.x = element_blank(), 
    axis.ticks.x = element_line(), 
    panel.grid.minor = element_blank(),
    # panel.spacing=unit(2, "lines"), 
    panel.background = element_rect(fill = 'grey95', color = 'white'), 
    panel.grid.major = element_line(color = 'grey90')
  )

jpeg(here('figs/trnabu-supp.jpeg'), height = 6, width = 9, units = 'in', res = 500, family = 'serif')
print(p)
dev.off()

## wind roses -------------------------------------------------------------

toplo <- na.omit(winddat) %>% 
  mutate(
    mo = month(datetime, label = T)
  ) %>% 
  filter(mo != 'Oct')

spdmin <- 0
spdmax <- 14
spdres = 3
thm <- theme_minimal()

p <- plot.windrose(data = toplo, spd = 'wind_ms', dir = 'wind_dir', spdres = spdres, spdmin = spdmin, spdmax = spdmax) + 
  facet_wrap(~mo, ncol = 3) + 
  theme_minimal() + 
  theme(
    axis.text = element_blank(), 
    axis.title = element_blank(), 
    axis.ticks.y = element_blank(), 
    legend.position = 'right'
  )

jpeg(here('figs/windroses-supp.jpeg'), height = 6, width = 7, units = 'in', res = 500, family = 'serif')
print(p)
dev.off()

## phyto and macro frequency occurrence -----------------------------------

brks <- seq.Date(as.Date('2021-03-28'), as.Date('2021-09-26'), by = '1 week')
nonbay <- c('BH01', 'P Port 2', 'P Port 3', 'PM Out', '20120409-01', 'PPC41', 'P Port 4', 'PMB01', 'NGS-S Pond')

# phytoplankton summarized
physum <- rsphydat %>%
  filter(date < as.Date('2021-10-01')) %>%
  filter(source == c('fldep')) %>%
  filter(!station %in% nonbay) %>%
  # inner_join(rsphypts, ., by = c('station', 'source', 'typ')) %>%
  # st_intersection(areas) %>%
  # st_intersection(tbseg) %>%
  # st_set_geometry(NULL) %>%
  # filter(area %in% c('Area 1')) %>%
  # filter(bay_segment %in% c('MTB', 'LTB')) %>%
  mutate(
    species = case_when(
      species %in% c('Centric Diatoms', 'Skeletonema sp.', 'Skeletonema Costatum', 'Asterionellopsis glacialis', 'Asterionellopsis') ~ 'Diatoms',
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
  filter(!species %in% c('mixed algae', 'Pyrodinium bahamense', 'Pseudo-nitzschia sp.'))

lns <- physum %>%
  group_by(date) %>%
  summarize(
    ymax = sum(foest)
  )

p1 <- ggplot() +
  geom_area(data = physum, aes(x = date, y = foest, fill = species), stat = 'identity', color = 'lightgrey', alpha = 0.7) +
  geom_segment(data = lns, aes(x = date, xend = date, y = 0, yend = ymax), color = 'grey') +
  scale_fill_manual(values = c('#66c2a5', '#fc8d62')) +
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
    x = NULL,
    title = '(a) Phytoplankton frequency occurrence all locations'
  )

# transect summarized
trnsum <- rstrndat %>%
  filter(date < as.Date('2021-10-01')) %>%
  # filter(station %in% c('S4T1c', 'S3T6b', 'S3T5b', 'S4T2b')) %>%
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
p2 <- ggplot() +
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
    title = '(b) Macroalgae frequency occurrence all locations'
  )

p <- p1 + p2 + plot_layout(ncol = 1)

jpeg(here('figs/phymcrfoest-supp.jpeg'), height = 5, width = 7, units = 'in', res = 500, family = 'serif')
print(p)
dev.off()

## nutrient flow ordination -----------------------------------------------

vrs <- c('chla', 'tn')
nonbay <- c('BH01', 'P Port 2', 'P Port 3', 'PM Out', '20120409-01', 'PPC41', 'P Port 4', 'PMB01', 'NGS-S Pond')

# water quality summarized
wqsum <- rswqdat %>% 
  filter(var %in% vrs) %>% 
  filter(date < as.Date('2021-10-01')) %>% 
  filter(source == 'fldep') %>%
  filter(!station %in% nonbay) %>% 
  # inner_join(rsstatloc, ., by = c('station', 'source')) %>% 
  # st_intersection(areas) %>% 
  # st_set_geometry(NULL) %>% 
  mutate(cens = grepl('U', qual)) %>% 
  select(date, var, val, cens, station) %>% 
  mutate(
    date = floor_date(date, unit = 'week')
  ) %>% 
  group_by(date, var) %>% 
  summarise(
    medv = ifelse(
      any(cens), median(cenfit(val, cens), na.rm = T),
      median(val, na.rm = T)
    ),
    hiv = ifelse(
      any(cens), quantile(cenfit(val, cens), prob = 0.975, na.rm = T),
      quantile(val, prob = 0.975, na.rm = T)
    ),
    lov = ifelse(
      any(cens), quantile(cenfit(val, cens), prob = 0.025, na.rm = T),
      quantile(val, prob = 0.025, na.rm = T)
    ),
    .groups = 'drop'
  )

# phytoplankton summarized
physum <- rsphydat %>%
  filter(date < as.Date('2021-10-01')) %>% 
  filter(source %in% c('epchc', 'pinco')) %>%
  filter(!station %in% nonbay) %>% 
  mutate(
    species = case_when(
      species %in% c('Skeletonema sp.', 'Skeletonema Costatum', 'Asterionellopsis glacialis', 'Asterionellopsis') ~ 'Diatoms', 
      T ~ 'other'
    ),
    species = factor(species, levels = unique(species))
  ) %>% 
  select(date, station, species, val) %>% 
  group_by(date, station) %>% 
  complete(
    species,
    fill = list(val = 0)
  ) %>% 
  ungroup() %>% 
  mutate(
    week = floor_date(date, unit = 'week'), 
    val = val / 1e5
  ) %>% 
  filter(species == 'Diatoms') %>% 
  select(week, species, val)

# kbrevis
habsum <- habdat %>% 
  filter(year(date) >= 2021) %>%
  filter(month(date) < 10) %>% 
  mutate(
    week = floor_date(date, unit = 'week'), 
    species = 'K. brevis'
  ) %>%
  complete(week) %>% 
  select(week, species, val)

# levels for week, starts on first of week
physum <- bind_rows(habsum, physum) %>% 
  filter(week >= as.Date('2021-03-28')) %>% 
  group_by(week, species) %>% 
  summarise(
    medv = quantile(val, 0.5, na.rm = T), 
    lov = quantile(val, 0.025, na.rm = T), 
    hiv = quantile(val, 0.975, na.rm = T), 
    .groups = 'drop'
  )

rssub <- rstrndat %>% 
  filter(typ == 'mcr') %>% 
  select(station, location, date, taxa, bb)

bssub <- bstransect %>% 
  filter(var == 'Abundance') %>% 
  select(
    station = Transect, 
    location = Site, 
    date = Date,
    taxa = Savspecies,
    bb = aveval
  ) %>% 
  filter(date %in% rstrnwts$date & station %in% rstrnwts$station & location %in% rstrnwts$location) %>% 
  filter(grepl('^DA', taxa)) %>% 
  mutate(
    location = as.numeric(location), 
    taxa = case_when(
      grepl('Red', taxa) ~ 'Red', 
      grepl('Green', taxa) ~ 'Green', 
      grepl('Macroalgae', taxa) ~ 'Red',
      T ~ taxa
    )
  ) 

tojn <- bind_rows(rssub, bssub)
wtssub <- rstrnwts %>% 
  select(-genus) %>% 
  rename(taxa = group) %>% 
  filter(!grepl('and', taxa)) %>% 
  left_join(tojn, by = c('station', 'date', 'location', 'taxa')) %>% 
  mutate(
    weight = weight_g * 0.004 # g / 0.25m2 to kg / m2
  )

wtsmod <- wtssub %>% 
  group_by(taxa) %>% 
  nest() %>% 
  mutate(
    mod = purrr::map(data, lm, formula = weight ~ 0 + bb)
  ) %>% 
  select(taxa, mod)

wtsest <- rstrndat %>% 
  filter(taxa %in% c('Red', 'Green', 'Cyanobacteria')) %>% 
  group_by(taxa) %>% 
  nest %>% 
  left_join(wtsmod, by = 'taxa') %>% 
  mutate(
    weight_kgm2 = purrr::pmap(list(object = mod, newdata = data), predict)
  ) %>% 
  select(-mod) %>% 
  unnest(c('data', 'weight_kgm2')) %>% 
  mutate(
    week = floor_date(date, unit = 'week')
  ) %>% 
  group_by(taxa, week, station) %>%
  summarise(
    weight_kgm2 = mean(weight_kgm2, na.rm = T), 
    .groups = 'drop'
  ) %>%
  group_by(taxa, week) %>% 
  summarise(
    medv = quantile(weight_kgm2, 0.5, na.rm = T), 
    lov = quantile(weight_kgm2, 0.025, na.rm = T), 
    hiv = quantile(weight_kgm2, 0.975, na.rm = T), 
    .groups = 'drop'
  ) %>% 
  filter(taxa == 'Cyanobacteria') 

wqfrm <- wqsum %>% 
  select(-medv, -lov) %>% 
  group_by(var) %>%
  mutate(
    hiv = scales::rescale(hiv, to = c(0, 1)) 
  ) %>%
  spread(var, hiv) %>% 
  rename(week = date)

phyfrm <- physum %>% 
  select(-medv, -lov) %>% 
  group_by(species) %>% 
  spread(species, hiv, fill = 0) %>% 
  mutate(
    Diatoms = scales::rescale(Diatoms, to = c(0, 1)), 
    `K. brevis` = scales::rescale(`K. brevis`, to = c(0, 1))
  ) 

mcrfrm <- wtsest %>% 
  select(week, Cyanobacteria = hiv) %>% 
  mutate(
    Cyanobacteria = scales::rescale(Cyanobacteria, to = c(0, 1)), 
  )

toord <- full_join(wqfrm, phyfrm, by = 'week') %>% 
  full_join(mcrfrm, by = 'week') %>% 
  gather('var', 'val', -week) %>% 
  group_by(var) %>% 
  mutate(
    val = ifelse(is.na(val), median(val, na.rm = T), val)
  ) %>% 
  ungroup %>% 
  spread(var, val) %>% 
  mutate(
    mogrp = month(week, label = T), 
    mogrp = as.character(mogrp), 
    mogrp = case_when(
      mogrp %in% c('Mar', 'Apr') ~ 'Apr', 
      mogrp %in% c('Aug', 'Sep')  ~ 'Aug, Sep', 
      mogrp %in% c('May', 'Jun') ~ 'May, Jun',
      T  ~ mogrp
    ), 
    mogrp = factor(mogrp, levels = c('Apr' ,'May, Jun', 'Jul', 'Aug, Sep'))
  ) %>% 
  rename(
    TN = tn, 
    `Chl-a` = chla
  )

grps <- toord$mogrp
toord<- toord %>% 
  select(-week, -mogrp)

cols <- rev(RColorBrewer::brewer.pal(4, 'Set2'))
names(cols) <- levels(grps)

vec_ext <- 1
coord_fix <- F
size <- 3
repel <- F
arrow <- 0.2
txt <- 3
alpha <- 0.8
ext <- 1.1
exp <- 0.1
parse <- F
ellipse <- F

allpcadat <- metaMDS(toord)
p <- ggord(allpcadat, axes = c('1', '2'), grp_in = grps, ellipse = ellipse, cols = cols,
           parse = parse, vec_ext = vec_ext, coord_fix = coord_fix, size = size, 
           repel = repel, arrow = arrow, txt = txt, alpha = alpha, ext = ext, 
           exp = exp) +
  theme(
    legend.title = element_blank(), 
    legend.position = 'top'
  )

jpeg(here('figs/nutrientfloword-supp.jpeg'), height = 4, width = 3.5, units = 'in', res = 500, family = 'serif')
print(p)
dev.off()