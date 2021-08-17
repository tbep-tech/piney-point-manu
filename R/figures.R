
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
box::use(
  scales = scales[muted], 
  units = units[set_units], 
  vegan = vegan[decostand], 
  FactoMineR = FactoMineR[PCA]
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
data(kbrdat)
data(rstrnpts)

source(here('R/funcs.R'))

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

# weekly plots ------------------------------------------------------------

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

p1 <- wqplo_fun(rswqdat, bswqdat, ppsegbf, vr = 'tn', cols, logtr = TRUE, rmfacet = TRUE, ttl = '(a) Total Nitrogen', ylb = 'mg/L (log-scale)')
p2 <- wqplo_fun(rswqdat, bswqdat, ppsegbf, vr = 'chla', cols, logtr = TRUE, rmfacet = TRUE, ttl = '(b) Chlorophyll-a', ylb = 'ug/L (log-scale)')
p3 <- wqplo_fun(rswqdat, bswqdat, ppsegbf, vr = 'secchi', cols, logtr = FALSE, ttl = '(c) Secchi', ylb = 'meters')

p <- (p1 + p2 + p3 + plot_layout(ncol = 3)) / wrap_elements(grid::textGrob('Week of', gp = gpar(fontsize=14))) + 
  plot_layout(ncol = 1, guides = 'collect', height = c(1, 0.05)) & 
  theme(legend.position = 'top')

jpeg(here('figs/wqtrnds.jpeg'), height = 6, width = 8, units = 'in', res = 500, family = 'serif')
print(p)
dev.off()

# PCA and correlations ----------------------------------------------------

vrs <- c('chla', 'dosat', 'nh34', 'ph', 'secchi', 'temp', 'tn', 'no23', 'tp', 'sal')
nonbay <- c('BH01', 'P Port 2', 'P Port 3', 'PM Out', '20120409-01', 'PPC41', 'P Port 4', 'PMB01', 'NGS-S Pond')

cols <- c("#E16A86", "#50A315", "#009ADE")
names(cols) <- c('Area 1', 'Area 2', 'Area 3')

ppsegbf <- ppseg %>% 
  rename(area = Name) %>% 
  group_by(area) %>% 
  summarise() %>% 
  st_buffer(dist = set_units(0.0001, degree)) %>% 
  st_buffer(dist = set_units(-0.0001, degree)) %>% 
  mutate(
    area = factor(area)
  )

rswqsub <- rswqdat %>% 
  filter(var %in% vrs) %>% 
  filter(source == 'fldep') %>%
  filter(!station %in% nonbay) %>% 
  inner_join(rsstatloc, ., by = c('station', 'source')) %>% 
  st_intersection(ppsegbf) %>% 
  st_set_geometry(NULL) %>% 
  select(date, var, val, station, area) %>% 
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
    val = median(val, na.rm = T), 
    .groups = 'drop'
  ) 

rswqtmp <- rswqsub %>% 
  complete(date, area, var) %>% 
  group_by(area, var) %>% 
  # mutate(
  #   val = ifelse(is.na(val), median(val, na.rm = T), val)
  # ) %>% 
  ungroup() %>% 
  mutate(
    val = case_when(
      var %in% c('Chl-a', 'NH3, NH4+', 'NOx', 'TN', 'TP') ~ log10(1 + val),
      T ~ val
    )
  ) %>%
  group_by(area) %>% 
  nest %>% 
  mutate(
    data = purrr::map(data, function(x){
      
      out <- spread(x, var, val) %>% 
        na.omit()
      
      return(out)
      
    }
    ),
    ord = purrr::pmap(list(area, data), function(area, data){
      
      toord <- data %>% 
        select(-date) %>% 
        decostand(method = 'standardize')
      
      ppp <- PCA(toord, scale.unit = F, graph = F) 
      
      if(area == 'Area 1')
        ttl <- paste('(a)', area)
      if(area == 'Area 2')
        ttl <- paste('(b)', area)
      if(area == 'Area 3')
        ttl <- paste('(c)', area)
      
      vec_ext <- 3
      coord_fix <- F
      size <- 2
      repel <- F
      arrow <- 0.2
      txt <- 3
      alpha <- 0.5
      lbcl <- cols[area]
      max.overlaps <- 10
      force <- 1
      ext <- 1.2
      exp <- 0.1
      parse <- F
      
      p1 <- ggord(ppp, axes = c('1', '2'), parse = parse, exp = exp, force = force, ext = ext, max.overlaps = max.overlaps, alpha = alpha, veccol = lbcl, labcol = lbcl, vec_ext = vec_ext, coord_fix = coord_fix, size = size, repel = repel, arrow = arrow, txt = txt) + 
        labs(title = ttl)
      p2 <- ggord(ppp, axes = c('2', '3'), parse = parse, exp = exp, force = force, ext = ext, max.overlaps = max.overlaps, alpha = alpha, veccol = lbcl, labcol = lbcl, vec_ext = vec_ext, coord_fix = coord_fix, size = size, repel = repel, arrow = arrow, txt = txt)
      
      out <- p1 + p2 + plot_layout(ncol = 2)
      
      return(out)
      
    }
    ), 
    cormat = purrr::map(data, function(x){
      
      out <- x %>% 
        select(-date) %>% 
        names %>% 
        crossing(var1 = ., var2 = .) %>% 
        filter(!var1 == var2) %>% 
        mutate(
          corv = NA_real_, 
          pval = NA_real_, 
          pstr = NA_character_
        )
      
      for(i in 1:nrow(out)){
        
        var1 <- out[[i, 'var1']]
        var2 <- out[[i, 'var2']]
        
        tst <- cor.test(x[[var1]], x[[var2]], method = 'spearman')
        
        corv <- tst$estimate
        pval <- tst$p.value
        pstr <- p_ast(pval)
        
        cri <- data.frame(corv = corv, pval = pval, pstr = pstr)
        
        out[i, c('corv', 'pval', 'pstr')] <- cri
        
      }
      
      return(out)
      
    }
    ), 
    corplo = purrr::map(cormat, function(x){
      
      pbase <- theme(
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1, size = 7), 
        axis.text.y = element_text(size = 7),
        legend.position = c(0.5, 1.12),
        legend.direction = 'horizontal', 
        panel.background = element_rect(fill = 'black')
      ) 
      
      p <- ggplot(x) + 
        geom_tile(aes(y = var1, x = var2, fill = corv), colour = 'black') + 
        geom_text(aes(y = var1, x = var2, label = pstr), size = 3) +
        pbase +
        scale_y_discrete('', expand = c(0, 0)) + 
        scale_x_discrete('', expand = c(0, 0)) + 
        scale_fill_gradientn('Corr. ', colours = c(muted("blue"), "white", muted("red")), limits = c(-1, 1)) +
        guides(fill = guide_colourbar(barheight = 0.25, barwidth = 4, label.theme = element_text(size = 6, angle = 0)))
      
      return(p)
      
    }
    )
  )

p <- (rswqtmp$ord[[1]] + rswqtmp$corplo[[1]] + plot_layout(ncol = 3)) / 
  (rswqtmp$ord[[2]]  + rswqtmp$corplo[[2]] + plot_layout(ncol = 3)) / 
  (rswqtmp$ord[[3]] + rswqtmp$corplo[[3]] + plot_layout(ncol = 3)) 

jpeg(here('figs/pcacors.jpeg'), height = 8, width = 7, units = 'in', res = 500, family = 'serif')
print(p)
dev.off()

# transect example --------------------------------------------------------

trn <- 'S3T6b'
rmdt <- as.Date('2021-04-07')

mcrdat <- rstrndat %>% 
  filter(station %in% trn) %>% 
  filter(typ == 'mcr') %>% 
  filter(date != rmdt) %>% 
  mutate(taxa = fct_drop(taxa))
savdat <- rstrndat %>% 
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

p <- show_rstransect(savdat, mcrdat, savsel, mcrsel)

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
cols <- c(savcol, mcrcol)

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

# add area
trnsum <- rstrndat %>%
  inner_join(rstrnpts, ., by = 'station') %>% 
  st_intersection(areas) %>% 
  st_set_geometry(NULL) %>%
  dplyr::group_by(area, typ, date, station, taxa, location) %>%
  dplyr::summarise(
    pa = as.numeric(any(bb > 0))
  ) %>%  
  mutate(
    date = floor_date(date, unit = 'month'), 
    typ = factor(typ, levels = c('sav', 'mcr'), labels = c('Seagrasses', 'Macroalgae')), 
    taxa = factor(taxa, levels = c(savsel, mcrsel)), 
    area = forcats::fct_drop(area), 
    area = factor(area, levels = c('Area 1', 'Area 3'), labels = c('(a) Area 1', '(b) Area 3'))
  ) %>% 
  group_by(area, typ, date, taxa) %>% 
  summarize(
    foest = sum(pa) / length(pa)
  ) %>% 
  filter(taxa %in% c(mcrsel, savsel))

p <- ggplot(trnsum, aes(x =date, y = foest, fill = taxa)) + 
  geom_bar(pch = 16, stat = 'identity', color = 'grey', alpha = 0.8) +
  facet_grid(typ ~ area, scales = 'free') +
  theme_minimal(base_size = 14) + 
  scale_fill_manual(values = cols) +
  labs(
    y = 'Freq. occurrence'
  ) +
  scale_y_continuous(expand = c(0, 0)) + 
  coord_cartesian(ylim = c(0, NA)) +
  theme(
    legend.position = 'top', 
    legend.title = element_blank(),
    strip.background = element_blank(), 
    strip.text = element_text(size = 14), 
    axis.title.x = element_blank(), 
    axis.ticks.x = element_line(), 
    panel.grid.minor = element_blank(), 
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank(), 
    strip.text.x = element_text(hjust = 0)
  )

jpeg(here('figs/trnfrq.jpeg'), height = 6, width = 8, units = 'in', res = 500, family = 'serif')
print(p)
dev.off()

# all PCA and correlations ------------------------------------------------

mcrsel <- c("Red", "Green", "Brown", "Cyanobacteria")
savsel <- c('Thalassia testudinum', 'Halodule wrightii', 'Syringodium filiforme')
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

# correlate transect data at transect scale

# add area
trnsum <- rstrndat %>%
  mutate(
    date = floor_date(date, unit = 'week')
  ) %>%
  dplyr::group_by(date, station, location, taxa) %>%
  dplyr::summarise(
    pa = as.numeric(any(bb > 0))
  ) %>%
  group_by(date, station, taxa) %>% 
  summarize(
    foest = sum(pa) / length(pa)
  )

trncors <- trnsum %>% 
  ungroup %>% 
  filter(taxa %in% c(savsel, mcrsel)) %>% 
  spread(taxa, foest) %>% 
  # filter(area %in% 'Area 3') %>% 
  select(-station, -date)


trncrs <- crossing(var1 = names(trncors), var2 = names(trncors)) %>% 
  filter(var1 != var2) %>% 
  rownames_to_column() %>% 
  group_by(rowname) %>% 
  nest %>% 
  mutate(
    crs = map(data, function(x){
      
      # variables
      vr1 <- trncors[[x$var1]]
      vr2 <- trncors[[x$var2]]
      
      # pearson
      pr_ts <- cor.test(vr1, vr2, method = 'spearman')
      pr_cr <- round(pr_ts$estimate, 2)
      pr_pv <- p_ast(pr_ts$p.value)
      pr <- paste(pr_cr, pr_pv)
      
      out <- data.frame(pr = pr, stringsAsFactors = F)
      return(out)
      
    })
  ) %>% 
  unnest(c('data', 'crs')) %>% 
  ungroup %>% 
  select(-rowname)

# correlate water quality with transect data at aggregated scale

# add area
trnsum <- rstrndat %>% 
  mutate(
    date = floor_date(date, unit = 'week')
  ) %>% 
  inner_join(rstrnpts, ., by = 'station') %>% 
  st_intersection(areas) %>% 
  st_set_geometry(NULL) %>% 
  dplyr::group_by(area, date, station, location, taxa) %>%
  dplyr::summarise(
    pa = as.numeric(any(bb > 0))
  ) %>%
  group_by(area, date, taxa) %>% 
  summarize(
    val = sum(pa) / length(pa) # freq occ.
  ) %>% 
  ungroup %>% 
  filter(taxa %in% c(savsel, mcrsel)) %>% 
  rename(var = taxa) %>%
  spread(var, val)

# water quality summary
rswqsum <- rswqdat %>% 
  filter(var %in% vrs) %>% 
  filter(source == 'fldep') %>%
  filter(!station %in% nonbay) %>% 
  inner_join(rsstatloc, ., by = c('station', 'source')) %>% 
  st_intersection(areas) %>% 
  st_set_geometry(NULL) %>% 
  select(date, var, val, station, area) %>% 
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
    val = median(val, na.rm = T), 
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

tocor <- full_join(trnsum, rswqsum, by = c('area', 'date'))

crs <- crossing(var1 = c(wqlab, mcrsel, savsel), var2 = c(wqlab, mcrsel, savsel)) %>% 
  filter(var1 != var2) %>% 
  rownames_to_column() %>% 
  group_by(rowname) %>% 
  nest %>% 
  mutate(
    crs = map(data, function(x){
      
      # variables
      vr1 <- tocor[[x$var1]]
      vr2 <- tocor[[x$var2]]
      
      # pearson
      pr_ts <- try(cor.test(vr1, vr2, method = 'spearman'))
      pr_cr <- round(pr_ts$estimate, 2)
      pr_pv <- p_ast(pr_ts$p.value)
      pr <- paste(pr_cr, pr_pv)
      
      out <- data.frame(pr = pr, stringsAsFactors = F)
      return(out)
      
    })
  ) %>% 
  unnest(c('data', 'crs')) %>% 
  ungroup %>% 
  select(-rowname) %>% 
  filter(!(var1 %in% c(mcrsel, savsel) & var2 %in% c(mcrsel, savsel)))

# combine correlations and make plot

prplo <- bind_rows(crs, trncrs) %>% 
  separate(pr, c('cor', 'sig'), sep = ' ') %>%  
  mutate(
    cor = as.numeric(cor), 
    var1 = factor(var1, levels = c(wqlab, mcrsel, savsel), labels =  c(wqlab, mcrsel, savsel)), 
    var2 = factor(var2, levels = c(wqlab, mcrsel, savsel), labels =  c(wqlab, mcrsel, savsel)), 
    sig = gsub('ns', '', sig)
  )

pbase <- theme(
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(), 
  axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1, size = 6), 
  axis.text.y = element_text(size = 6),
  legend.position = c(0.5, 1.1),
  legend.direction = 'horizontal',
  plot.margin = unit(c(4,4,0,0), "lines"),
  strip.background = element_blank(), 
  strip.text.y = element_text(angle = 0, hjust = 0, vjust = 0.5), 
  panel.background = element_rect(fill = 'black')
) 

p3 <- ggplot(prplo) + 
  geom_tile(aes(y = var1, x = var2, fill = cor), colour = 'black') + 
  geom_text(aes(y = var1, x = var2, label = sig), size = 3) +
  pbase +
  scale_y_discrete('', expand = c(0, 0)) + #, labels = parse(text = rev(labs))) + 
  scale_x_discrete('', expand = c(0, 0)) + #, labels = parse(text = rev(labs))) +
  scale_fill_gradientn('Corr. ', colours = c(muted("blue"), "white", muted("red")), limits = c(-1, 1)) +
  guides(fill = guide_colourbar(barheight = 0.25, barwidth = 5, label.theme = element_text(size = 6, angle = 0))) +
  geom_hline(yintercept = 9.5, size = 1) +
  geom_hline(yintercept = 13.5, size = 1) +
  geom_vline(xintercept = 9.5, size = 1) +
  geom_vline(xintercept = 13.5, size = 1)

toord <- tocor %>% 
  select(-date, -area) %>% 
  na.omit() %>% 
  decostand(method = 'standardize')

vec_ext <- 5
coord_fix <- F
size <- 2
repel <- F
arrow <- 0.2
txt <- 2.5
alpha <- 0.5
ext <- 1.2
exp <- 0.1
parse <- F

ppp <- PCA(toord, scale.unit = F, graph = F) 
p1 <- ggord(ppp, axes = c('1', '2'), parse = parse, vec_ext = vec_ext, coord_fix = coord_fix, size = size, repel = repel, arrow = arrow, txt = txt, alpha = alpha, ext = ext, exp = exp)
p2 <- ggord(ppp, axes = c('2', '3'), parse = parse, vec_ext = vec_ext, coord_fix = coord_fix, size = size, repel = repel, arrow = arrow, txt = txt, alpha = alpha, ext = ext, exp = exp)

p <- p1 + p2 + p3

jpeg(here('figs/allpcacors.jpeg'), height = 4, width = 10, units = 'in', res = 500, family = 'serif')
print(p)
dev.off()

# red tide and fish kills -------------------------------------------------

# data from https://public.myfwc.com/fwri/FishKillReport/searchresults.aspx
# requested hillsborough, pinellas, manatee 1/1/95 to 8/13/21
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
  )


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
  filter(mo <= 8) %>% 
  pull(lb)

# MTB subset
toplo <- kbrdat %>%
  .[tbseg[tbseg$bay_segment %in% c('MTB', 'LTB'), ], ] %>%
  filter(var == 'kb') %>% 
  mutate(
    dtgrp = quarter(date),
    yr = year(date)
  ) %>%
  st_set_geometry(NULL) %>%
  # filter(year(date) >= 1990) %>%
  mutate(
    yr = factor(yr, levels = seq(min(yr), max(yr)))
  ) %>%
  group_by(yr) %>%
  summarise(
    cnt = n(),
    y0 = min(val, na.rm = T), 
    y10 = quantile(val, prob = 0.25, na.rm = T),
    y50 = quantile(val, prob = 0.5, na.rm = T),
    y90 = quantile(val, prob = 0.75, na.rm = T),
    y100 = max(val, na.rm = T),
    .groups = 'drop'
  ) %>%
  filter(cnt > quantile(cnt, 0.25, na.rm = T)) %>%
  complete(yr) %>% 
  filter(as.numeric(as.character(yr)) >= 1995)

# plot
p1 <- ggplot(toplo, aes(x = yr)) +
  geom_boxplot(
    aes(ymin = y0, lower = y10, middle = y50, upper = y90, ymax = y100),
    stat = "identity", width = 0.75, fill = '#00806E'
  ) +
  scale_y_log10(labels = function(x) as.numeric(format(x, scientific = FALSE))) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, size = 8, hjust = 1),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  labs(
    x = 'Year',
    y = 'Cells (100k / L)',
    subtitle = expression(paste('(a) ', italic('K. brevis'), ' concentrations by year, middle/lower Tampa Bay'))
  )

# MTB subset
toplo <- kbrdat %>%
  .[tbseg[tbseg$bay_segment %in% c('LTB', 'MTB'), ], ] %>%
  filter(var == 'kb') %>% 
  filter(year(date) >= 2021) %>%
  mutate(
    week = floor_date(date, unit = 'week'),
    week = factor(format(week, '%b %d')), 
    week = factor(week, levels = weeklv)
  ) %>%
  st_set_geometry(NULL) %>%
  group_by(week) %>%
  summarise(
    cnt = n(),
    y0 = min(val, na.rm = T), 
    y10 = quantile(val, prob = 0.1, na.rm = T),
    y50 = quantile(val, prob = 0.5, na.rm = T),
    y90 = quantile(val, prob = 0.9, na.rm = T),
    y100 = max(val, na.rm = T),
    .groups = 'drop'
  ) %>%
  complete(week)

# plot
p2 <- ggplot(toplo, aes(x = week)) +
  geom_boxplot(
    aes(ymin = y0, lower = y10, middle = y50, upper = y90, ymax = y100),
    stat = "identity", width = 0.75, fill = '#00806E'
  ) +
  scale_y_log10(labels = function(x) as.numeric(format(x, scientific = FALSE))) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, size = 8, hjust = 1),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  labs(
    x= 'Week of',
    y = 'Cells (100k / L)',
    subtitle = expression(paste('(b) ', italic('K. brevis'), ' concentrations in 2021 by week, middle/lower Tampa Bay'))
  )

toplo1 <- fishdat %>% 
  filter(city %in% c('Tampa', 'St. Petersburg')) %>% 
  mutate(
    yr = factor(yr, levels = seq(min(yr), max(yr)))
  ) %>%
  group_by(yr, city) %>% 
  summarise(
    cnt = n(), 
    .groups = 'drop'
  ) %>% 
  complete(yr)

p3 <- ggplot(toplo1, aes(x = yr, fill = city, y = cnt)) + 
  geom_bar(stat = 'identity', colour = 'darkgrey') + 
  labs(
    x = 'Year',
    y = 'No. of reports',
    subtitle = '(c) Fish kill reports for red tide across years'
  ) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_brewer('City', palette = 'Pastel1') + 
  theme_minimal() + 
  theme(
    axis.ticks.x = element_line(),
    # axis.title.x = element_blank(), 
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
    legend.position = 'top', 
    panel.grid.minor.y = element_blank(), 
    panel.grid.minor.x = element_blank(), 
    panel.grid.major.x = element_blank(), 
    plot.caption = element_text(size = 10)
  )


toplo2 <- fishdat %>% 
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

p4 <- ggplot(toplo2, aes(x = week, fill = city, y = cnt)) + 
  geom_bar(stat = 'identity', colour = 'darkgrey') + 
  labs(
    x = 'Week of',
    y = 'No. of reports', 
    subtitle = '(d) Fish kill reports for red tide in 2021 by week'
  ) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_brewer('City', palette = 'Pastel1') + 
  theme_minimal() + 
  theme(
    axis.ticks.x = element_line(),
    # axis.title.x = element_blank(), 
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
    legend.position = 'top', 
    panel.grid.minor.y = element_blank(), 
    panel.grid.minor.x = element_blank(), 
    panel.grid.major.x = element_blank()
  )

p <- p1 + p2 + (p3 + p4 + plot_layout(ncol = 1, guides = 'collect') & theme(legend.position = 'top')) + plot_layout(ncol = 1, heights = c(0.2, 0.2, 0.6))

jpeg(here('figs/redtide.jpeg'), height = 8, width = 6, units = 'in', res = 500, family = 'serif')
print(p)
dev.off()
