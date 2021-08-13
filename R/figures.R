
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
