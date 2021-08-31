# function for formatting p-values in tables
p_ast <- function(x, usens = F){
  
  sig_cats <- c('**', '*', '')
  sig_vals <- c(-Inf, 0.005, 0.05, Inf)
  
  if(usens)
    sig_cats[3] <- 'ns'
  
  out <- cut(x, breaks = sig_vals, labels = sig_cats, right = FALSE)
  out <- as.character(out)
  
  return(out)
  
}

# water quality plot fun
wqplo_fun <- function(rswqdat, bswqdat, ppseg, vr, cols, logtr = TRUE, rmfacet = FALSE, ttl, ylb){
  
  nonbay <- c('BH01', 'P Port 2', 'P Port 3', 'PM Out', '20120409-01', 'PPC41', 'P Port 4', 'PMB01', 'NGS-S Pond')
  
  ##
  # wq data
  
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
    ) %>% 
    left_join(wklv, ., by = c('date', 'area', 'fillcl'))

  # baseline data
  bswqtmp <- bswqdat %>% 
    select(-source, -uni) %>% 
    filter(var == vr) %>%
    filter(!(var == 'secchi' & grepl('S', qual))) %>% # remove secchi on bottom
    filter(yr > 2005) %>% 
    filter(!is.na(val)) %>% 
    inner_join(bsstatloc, ., by = 'station') %>% 
    st_intersection(ppseg) %>% 
    st_set_geometry(NULL) %>% 
    mutate(cens = grepl('U', qual)) %>% 
    group_by(mo, var, area) %>% 
    summarise(   
      avev = ifelse(
        any(cens), mean(cenfit(val, cens), na.rm = T),
        mean(val, na.rm = T)
      ),
      stdv = ifelse(
        any(cens), sd(cenfit(val, cens), na.rm = T),
        sd(val, na.rm = T)
      ),
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
    na.omit() %>% 
    select(area, date, fillcl) %>% 
    unique %>% 
    arrange(area)
  
  p1 <- ggplot() + 
    geom_rect(data = bswqtmp, aes(xmin = datestr, xmax = dateend, ymin = minv, ymax = maxv, group = mo, fill = 'Monthly baseline (mean +/- 1 sd)'), alpha = 0.2) +
    geom_boxplot(data = rswqtmp, aes(x = date, y = val, group = date), fill = bxcls$fillcl, outlier.colour = NA, lwd = 0.5, alpha = 0.8, show.legend = F) + 
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
      axis.text.x = element_text(size = 6.25, angle = 45, hjust = 1)
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

# function for plotting rapid response transect data
# modified from show_transect in tbpetools
show_rstransect <- function(savdat, mcrdat, savsel, mcrsel, base_size = 12){
  
  savlevs <- c('Thalassia testudinum', 'Halodule wrightii', 'Syringodium filiforme', 'Ruppia maritima', 'Halophila engelmannii', 'Halophila decipiens')
  grplevs <- c('Red', 'Green', 'Brown', 'Cyanobacteria')
  abulabs <- c('<1%', '1-5%', '6-25%', '26-50%', '51-75%', '76-100%')
  abubrks <- c(0, 1, 2, 3, 4, 5)
  
  colpal <- colorRampPalette(RColorBrewer::brewer.pal(n = 8, name = 'Dark2'))
  
  szrng <- c(2, 10)
  
  # xlims
  savxlms <- savdat %>%
    pull(location) %>% 
    unique %>% 
    sort
  
  mcrxlms <- mcrdat %>%
    pull(location) %>% 
    unique %>% 
    sort
  
  xlms <- range(savxlms, mcrxlms)
  
  # get dates for factor levels
  # this makes sure that y values on plots are shared
  dts1 <- savdat %>% 
    pull(date)
  dts2 <- mcrdat %>% 
    pull(date)
  dts <- c(dts1, dts1) %>% 
    unique %>%
    sort %>% 
    format('%b %d')
  
  # prep sav plot data
  savdatfrm <- savdat %>%
    dplyr::mutate(
      Year = lubridate::year(date),
      location = as.numeric(as.character(location)),
      pa = ifelse(bb == 0, 0, 1), 
      date = format(date, '%b %d'), 
      date = factor(date, levels = dts)
    ) %>% 
    dplyr::select(Year, date, location, taxa, abundance, pa, bb)
  
  # sort color palette so its the same regardless of species selected
  savcol <- colpal(length(savlevs))
  names(savcol) <- savlevs
  savcol <- savcol[savsel]
  
  # legend labels
  leglab <- 'Abundance (bb)'
  
  # data with species
  toplo1a <- savdatfrm %>%
    dplyr::filter(taxa %in% !!savsel) %>% 
    dplyr::filter(pa == 1) %>%
    dplyr::mutate(
      bb = round(bb, 1),
      tltp = paste0(taxa, ', ', abundance)
    ) %>% 
    dplyr::arrange(date, location)
  
  # find overplots
  dups1 <- duplicated(toplo1a[, c('date', 'location')])
  dups2 <- duplicated(toplo1a[, c('date', 'location')], fromLast = T)
  dups <- apply(cbind(dups1, dups2), 1, any)
  toplo1a <- toplo1a %>% 
    mutate(
      dups = dups
    ) %>% 
    group_by(date, location) %>% 
    mutate(
      location = case_when(
        dups ~ location + seq(-1 * length(dups) / 3, length(dups) / 3, length.out = length(dups)), 
        T ~ location
      )
    ) %>% 
    ungroup()
  
  # data w/o species, no facet
  toplo2a <- savdatfrm %>%
    group_by(date, location) %>%
    filter(sum(pa) == 0) %>%
    ungroup() %>%
    select(date, location) %>%
    unique()
  
  pa <- ggplot2::ggplot(toplo1a, ggplot2::aes(y = date, x = location)) +
    ggplot2::geom_point(data = toplo2a, alpha = 1, colour = 'black', size = 2) +
    ggplot2::geom_point(aes(size = bb, fill = taxa), alpha = 0.8, pch = 21) +
    ggplot2::scale_fill_manual(values = savcol) +
    ggplot2::scale_radius(limits = range(abubrks), labels = abulabs, breaks = abubrks, range = szrng) +
    ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::scale_y_discrete(limits = dts, breaks = dts) + 
    ggplot2::scale_x_continuous(breaks = savxlms) +
    ggplot2::coord_cartesian(xlim = xlms) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      legend.title = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(hjust = 0), 
      axis.title.y = element_blank(), 
      axis.title.x = element_blank()
    ) +
    ggplot2::labs(
      
      x = 'Transect distance (m)',
      title = '(a) Seagrasses'
    ) + 
    guides(fill = guide_legend(override.aes = list(size = 7), order = 1))
  
  # prep mcr plot data
  mcrdatfrm <- mcrdat %>%
    dplyr::mutate(
      Year = lubridate::year(date),
      location = as.numeric(as.character(location)),
      pa = ifelse(bb == 0, 0, 1), 
      date = format(date, '%b %d'), 
      date = factor(date, levels = dts)
    ) %>% 
    dplyr::select(Year, date, location, taxa, abundance, pa, bb)
  
  # sort color palette so its the same regardless of species selected
  mcrcol <- c('tomato1', 'lightgreen', 'burlywood3', 'lightblue')
  names(mcrcol) <- grplevs
  mcrcol <- mcrcol[mcrsel]
  
  # legend labels
  leglab <- 'Abundance (bb)'
  
  # data with species
  toplo1b <- mcrdatfrm %>%
    dplyr::filter(taxa %in% mcrsel) %>% 
    dplyr::filter(pa == 1) %>%
    dplyr::mutate(
      bb = round(bb, 1),
      tltp = paste0(taxa, ', ',  abundance)
    )
  
  # jitter duplicates
  dups1 <- duplicated(toplo1b[, c('date', 'location')])
  dups2 <- duplicated(toplo1b[, c('date', 'location')], fromLast = T)
  dups <- apply(cbind(dups1, dups2), 1, any)
  toplo1b <- toplo1b %>% 
    mutate(
      dups = dups
    ) %>% 
    group_by(date, location) %>% 
    mutate(
      location = case_when(
        dups ~ location + seq(-1 * length(dups) / 3, length(dups) / 3, length.out = length(dups)), 
        T ~ location
      )
    ) %>% 
    ungroup()
  
  # data w/o species, no facet
  toplo2b <- mcrdatfrm %>%
    group_by(date, location) %>%
    filter(sum(pa) == 0) %>%
    ungroup() %>%
    select(date, location) %>%
    unique()
  
  pb <- ggplot2::ggplot(toplo1b, ggplot2::aes(y = date, x = location)) +
    ggplot2::geom_point(data = toplo2b, colour = 'black', alpha = 1, size = 2) +
    ggplot2::geom_point(inherit.aes = F, aes(colour = 'Empty sample'), x = NA, y = NA) +
    ggplot2::geom_point(aes(size = bb, fill = taxa), alpha = 0.8, pch = 21) +
    ggplot2::scale_fill_manual(values = mcrcol) +
    ggplot2::scale_colour_manual(values = 'black') +
    ggplot2::scale_radius(limits = range(abubrks), labels = abulabs, breaks = abubrks, range = szrng, guide = F) +
    ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::scale_y_discrete(limits = dts, breaks = dts) + 
    ggplot2::scale_x_continuous(breaks = mcrxlms) +
    ggplot2::coord_cartesian(xlim = xlms) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      legend.title = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(hjust = 0), 
      axis.title.y = element_blank()
    ) +
    ggplot2::labs(
      x = 'Transect distance (m)', 
      title = '(b) Macroalgae groups'
    ) +
    guides(
      fill = guide_legend(override.aes = list(size = 7), order = 1), 
      colour = guide_legend(override.aes = list(size = 2))
    )
  
  # out
  p <- pa + pb + plot_layout(ncol = 1, heights = c(0.9, 1), guides = 'collect')
  
  return(p)
  
}
