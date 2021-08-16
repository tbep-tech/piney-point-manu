
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

# weekly plots ------------------------------------------------------------

# water quality plot fun
wqplo_fun <- function(rswqdat, bswqdat, ppseg, vr, cols, logtr = TRUE, rmfacet = FALSE, ttl, ylb){
  
  nonbay <- c('BH01', 'P Port 2', 'P Port 3', 'PM Out', '20120409-01', 'PPC41', 'P Port 4', 'PMB01', 'NGS-S Pond')
  
  ##
  # wq data
  
  # monitoring data
  rswqtmp <- rswqdat %>% 
    filter(var == vr) %>% 
    filter(!station %in% nonbay) %>% 
    inner_join(rsstatloc, ., by = c('station', 'source')) %>% 
    st_intersection(ppseg) %>% 
    st_set_geometry(NULL) %>% 
    select(-qual, -bswqstation, -nrmrng, -source, -source_lng, -uni, -lbunis) %>% 
    mutate(
      date = floor_date(date, unit = 'week'), 
      mo = month(date), 
      fillcl = factor(area, levels = levels(area), labels = cols), 
      fillcl = as.character(fillcl)
    ) 
  
  # baseline data
  bswqtmp <- bswqdat %>% 
    select(-source, -uni) %>% 
    filter(var == vr) %>% 
    filter(yr > 2005) %>% 
    inner_join(bsstatloc, ., by = 'station') %>% 
    st_intersection(ppseg) %>% 
    st_set_geometry(NULL) %>% 
    group_by(mo, var, area) %>% 
    summarise(   
      avev = mean(val, na.rm = T), 
      stdv = sd(val, na.rm = T), 
      .groups = 'drop'
    ) %>%
    left_join(parms, by = 'var') %>% 
    mutate(
      avev = round(avev, sigdig), 
      stdv = round(stdv, sigdig), 
      minv = avev - stdv, 
      minv = pmax(0, minv),
      maxv = avev + stdv,
      lbunis = gsub('^.*\\s(\\(.*\\))$', '\\1', lbs), 
      lbunis = gsub('pH', '', lbunis), 
      datestr= paste0('2021-', mo, '-01'), 
      datestr = ymd(datestr), 
      dateend = ceiling_date(datestr, unit = 'month')
    )
  
  # boxplot colors
  bxcls <- rswqtmp %>% 
    select(area, date, fillcl) %>% 
    unique
  
  p1 <- ggplot() + 
    geom_rect(data = bswqtmp, aes(xmin = datestr, xmax = dateend, ymin = minv, ymax = maxv, group = mo, fill = 'Monthly baseline (mean +/- 1 sd)'), alpha = 0.2) +
    geom_boxplot(data = rswqtmp, aes(x = date, y = val, group = date), fill= bxcls$fillcl, outlier.colour = NA, lwd = 0.5, alpha = 0.8, show.legend = F) + 
    geom_jitter(data = rswqtmp, aes(x = date, y = val, group = date), alpha = 0.4, size = 0.5) + 
    scale_fill_manual(NULL, values = 'blue') +
    scale_linetype_manual(values = 'dashed') + 
    facet_grid(area ~ ., scales = 'free_y') + 
    scale_x_date(breaks = unique(rswqtmp$date), date_labels = '%b %d', expand = c(0.05, 0.05)) +
    labs(
      y = ylb, 
      title = ttl
    ) + 
    coord_cartesian(xlim = range(rswqtmp$date)) +
    theme_minimal(base_size = 12) + 
    theme(
      legend.position = 'top', 
      strip.background = element_blank(), 
      axis.title.x = element_blank(),
      panel.grid.minor = element_blank(),
      strip.text = element_text(size = 14), 
      axis.text.x = element_text(size = 7, angle = 45, hjust = 1)
    )
  
  if(logtr)
    p1 <- p1 + 
    scale_y_log10()
  
  if(rmfacet)
    p1 <- p1 + 
    theme(
      strip.text = element_blank()
    )
  
  out <- p1
  
  return(out)
  
}

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
        
        tst <- cor.test(x[[var1]], x[[var2]])
        
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

