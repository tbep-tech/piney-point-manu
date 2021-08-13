
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

data(rsallpts)
data(ppseg)
data(segmask)
data(rswqdat)
data(rsstatloc)

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
  geom_sf_text(data = areas, aes(label = area), color = 'black', inherit.aes = F, alpha = 0.8, size = 6) +
  scale_fill_manual(values = cols, drop = F) +
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
    legend.position = 'none'
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
  geom_sf(data = pineypoint, aes(fill = 'Piney Point'), pch = 24, color = 'black', size = 3, inherit.aes= F) + 
  scale_fill_manual(NULL, values = 'black') +
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
    color = 'Data type',
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
  geom_sf(data = continent, fill = 'grey', colour = 'grey') +
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

pout <- p1 + p2

jpeg(here('figs/map.jpeg'), height = 4.2, width = 9, family = 'serif', units = 'in', res = 500)
print(pout)
dev.off()

# nutrients, chloropyll map -----------------------------------------------

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
wqdat <- rswqdat %>% 
  filter(var %in% c('tn', 'chla', 'secchi')) %>% 
  filter(!station %in% nonbay) %>% 
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
  filter(var == 'tn')

# static plot
p1 <- bsmap +
  geom_point(data = toplo1, aes(x = lng, y = lat, size = val, fill = val, group = dategrp, color = inrng), pch = 21, alpha = 0.8) +
  scale_fill_gradientn('mg/L', trans = 'log10', colours = vrscols) +
  scale_color_manual('In normal range?', values = c('black', 'lightgrey')) +
  scale_size('mg/L', range = c(0.5, 5), trans = 'log10') + 
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
  filter(var %in% 'secchi')

brks <- c(1, 2, 3, 4)
p3 <- bsmap +
  geom_point(data = toplo3, aes(x = lng, y = lat, size = val, fill = val, group = dategrp, color = inrng), pch = 21, alpha = 0.8) +
  scale_fill_gradientn('meters',colours = rev(vrscols), breaks = brks) +
  scale_color_manual('In normal range?', values = c('black', 'grey'), guide = T) +
  scale_size('meters', range = c(6, 0.5), breaks = brks) + 
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
