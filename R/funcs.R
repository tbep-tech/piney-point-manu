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
      axis.text.x = element_text(size = 4.5, angle = 45, hjust = 1)
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

# water quality plot fun using GAM predictions
gamplo_fun <- function(rswqdat, bswqdat, ppseg, vr, cols, logtr = TRUE, rmfacet = FALSE, ttl, ylb){
  
  nonbay <- c('BH01', 'P Port 2', 'P Port 3', 'PM Out', '20120409-01', 'PPC41', 'P Port 4', 'PMB01', 'NGS-S Pond')
  mindt <- ymd('2021-03-28')
  dtrng <- ymd(c('2006-01-01', '2021-10-01'))
  
  # monitoring data
  rswqtmp <- rswqdat %>% 
    filter(var %in% vr) %>% 
    filter(!station %in% nonbay) %>% 
    inner_join(rsstatloc, ., by = c('station', 'source')) %>% 
    st_intersection(ppseg) %>% 
    st_set_geometry(NULL) %>% 
    select(-bswqstation, -nrmrng, -source, -source_lng, -uni, -lbunis) %>% 
    mutate(
      date = floor_date(date, unit = 'week'),
      cens = grepl('U', qual)
    ) %>% 
    select(station, date, var, val, area)
  
  # baseline data
  bswqtmp <- bswqdat %>% 
    select(-source, -uni) %>% 
    filter(var %in% vr) %>%
    filter(!(var == 'secchi' & grepl('S', qual))) %>% # remove secchi on bottom
    filter(date >= dtrng[1]) %>% 
    filter(!is.na(val)) %>% 
    inner_join(bsstatloc, ., by = 'station') %>% 
    st_intersection(ppseg) %>% 
    st_set_geometry(NULL) %>% 
    mutate(
      date = floor_date(date, unit = 'month'),
      cens = grepl('U', qual)
      ) %>% 
    select(station, date, var, val, area)
  
  tomod <- bind_rows(bswqtmp, rswqtmp) %>% 
    mutate(
      yr = lubridate::year(date), 
      doy = lubridate::yday(date),
      cont_year = lubridate::decimal_date(date)
    ) %>% 
    arrange(date, var, area) %>% 
    group_by(area) %>% 
    nest()
  
  if(logtr)
    mods <- tomod %>% 
      mutate(
        mod = purrr::map(data, function(x){
          gam(log10(val) ~ s(cont_year, k = 20) + s(doy, bs = 'cc'), # + ti(cont_year, doy, bs = c('tp', 'cc')), 
              data = x[x$date < mindt, ])
        })
      )
  
  if(!logtr)
    mods <- tomod %>% 
      mutate(
        mod = purrr::map(data, function(x){
          gam(val ~ s(cont_year, k = 20) + s(doy, bs = 'cc'), # + ti(cont_year, doy, bs = c('tp', 'cc')), 
              data = x[x$date < mindt, ])
        })
      )
  
  # prediction data, daily time step, subset by doystr, doyend
  fillData <- data.frame(date = seq.Date(dtrng[1], dtrng[2], by = 'day')) %>% 
    dplyr::mutate(
      yr = lubridate::year(date), 
      doy = lubridate::yday(date),
      cont_year = lubridate::decimal_date(date)
    ) 
  
  mods <- mods %>% 
    mutate(
      prds = purrr::map(mod, function(x){
        
        out <- fillData %>% 
          mutate(
            val = predict(x, newdata = .),
            sev = predict(x, newdata = ., se = T)$se.fit,
            hiv = val + 1.96 * sev, 
            lov = val - 1.96 * sev,
            post = case_when(
              date >=mindt ~ '2021', 
              T ~ '2006 - 2020'
            ), 
            hiv = case_when(
              post == '2021' ~ hiv,
              T ~ NA_real_
            ),
            lov = case_when(
              post == '2021' ~ lov,
              T ~ NA_real_
            ),
            xvals = ymd(paste('2021', month(date), day(date), sep = '-'))
          )
        
        return(out)
        
      })
    )
  
  prds <- mods %>% 
    select(area, prds) %>% 
    unnest(prds)
  
  if(logtr)
    prds <- prds %>% 
      mutate(
        val = 10^val,
        hiv = 10^hiv,
        lov = 10^lov
      )
  
  prds1 <- prds %>% 
    filter(date < mindt)
  prds2 <- prds %>% 
    filter(date >= mindt)
  obs <- mods %>% 
    select(area, data) %>% 
    unnest('data') %>% 
    filter(date >= mindt)

  colyrs <- c(rep('lightgrey', 3), brewer.pal(9, 'Greys')[1:7])
  
  p1 <- ggplot(prds1, aes(x = xvals, group = yr)) + 
    geom_line(aes(y = val, linetype = '2006 - 2020 Baseline modelled (darker more recent)', color = yr), size = 0.5) +
    scale_color_gradientn(colors = colyrs, guide = F) + 
    new_scale_color() + 
    geom_line(data = prds2, aes(y = val, color = area, linetype = '2021 forecasted +/- 95% CI (colored by area)'), size = 2) + 
    geom_ribbon(data = prds2, aes(ymin = lov, ymax = hiv, fill = area), alpha = 0.3, color = NA) +
    geom_point(data = obs, aes(x = date, y = val, shape = '2021 samples'), color = 'black', alpha = 0.8, size = 0.5, position = position_jitter()) +
    scale_x_date(date_breaks = 'month', date_labels = '%b %d', expand = c(0, 0)) +
    facet_grid(area ~ ., scales = 'free_y') + 
    scale_color_manual(values = cols, guide = 'none') +
    scale_fill_manual(values = cols, guide = 'none') +
    scale_linetype_manual(values = c(1, 1)) + 
    guides(
      linetype = guide_legend(override.aes = list(size = c(0.5, 2), color = c('grey', cols[1])))
    ) +
    labs(
      color = 'Year', 
      y = ylb,
      x = NULL, 
      linetype = NULL, 
      title = ttl, 
      shape = NULL
    ) +
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
  
  return(p1)
  
}


# function for plotting rapid response transect data
# modified from show_transect in tbpetools
show_rstransect <- function(savdat, mcrdat, savsel, mcrsel, base_size = 12, rev = F, sclloc = T){
  
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
  
  # x axis text labels, scaling location to zero if sclloc
  savxlbs <- savxlms
  mcrxlbs <- mcrxlms
  if(sclloc){
    savxlbs <- savxlbs - min(savxlbs)
    mcrxlbs <- mcrxlbs - min(mcrxlbs)
  }
  
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
  
  if(rev)
    dts <- rev(dts)
  
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
    ggplot2::scale_radius(limits = range(abubrks), labels = abulabs, breaks = abubrks, range = szrng, guide = 'none') +
    ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::scale_y_discrete(limits = dts, breaks = dts) + 
    ggplot2::scale_x_continuous(breaks = savxlms, labels = savxlbs) +
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
      title = '(b) Seagrasses'
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
    ggplot2::scale_radius(limits = range(abubrks), labels = abulabs, breaks = abubrks, range = szrng) +
    ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::scale_y_discrete(limits = dts, breaks = dts) + 
    ggplot2::scale_x_continuous(breaks = mcrxlms, labels = mcrxlbs) +
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
      title = '(a) Macroalgae groups'
    ) +
    guides(
      fill = guide_legend(override.aes = list(size = 7), order = 1), 
      colour = guide_legend(override.aes = list(size = 2))
    )
  
  # out
  p <- pb + pa + plot_layout(ncol = 1, heights = c(0.9, 1), guides = 'collect')
  
  return(p)
  
}

# wind rose in ggplot
# from https://stackoverflow.com/questions/17266780/wind-rose-with-ggplot-r
plot.windrose <- function(data,
                          spd,
                          dir,
                          spdres = 2,
                          dirres = 30,
                          spdmin = 2,
                          spdmax = 20,
                          spdseq = NULL,
                          palette = "YlGnBu",
                          countmax = NA,
                          debug = 0){
  
  # Look to see what data was passed in to the function
  if (is.numeric(spd) & is.numeric(dir)){
    # assume that we've been given vectors of the speed and direction vectors
    data <- data.frame(spd = spd,
                       dir = dir)
    spd = "spd"
    dir = "dir"
  } else if (exists("data")){
    # Assume that we've been given a data frame, and the name of the speed 
    # and direction columns. This is the format we want for later use.    
  }  
  
  # Tidy up input data ----
  n.in <- NROW(data)
  dnu <- (is.na(data[[spd]]) | is.na(data[[dir]]))
  data[[spd]][dnu] <- NA
  data[[dir]][dnu] <- NA
  
  # figure out the wind speed bins ----
  if (missing(spdseq)){
    spdseq <- seq(spdmin,spdmax,spdres)
  } else {
    if (debug >0){
      cat("Using custom speed bins \n")
    }
  }
  # get some information about the number of bins, etc.
  n.spd.seq <- length(spdseq)
  n.colors.in.range <- n.spd.seq - 1
  
  # create the color map
  spd.colors <- colorRampPalette(brewer.pal(min(max(3,
                                                    n.colors.in.range),
                                                min(9,
                                                    n.colors.in.range)),                                               
                                            palette))(n.colors.in.range)
  
  if (max(data[[spd]],na.rm = TRUE) > spdmax){    
    spd.breaks <- c(spdseq,
                    max(data[[spd]],na.rm = TRUE))
    spd.labels <- c(paste(c(spdseq[1:n.spd.seq-1]),
                          '-',
                          c(spdseq[2:n.spd.seq])),
                    paste(spdmax,
                          "-",
                          max(data[[spd]],na.rm = TRUE)))
    spd.colors <- c(spd.colors, "grey50")
  } else{
    spd.breaks <- spdseq
    spd.labels <- paste(c(spdseq[1:n.spd.seq-1]),
                        '-',
                        c(spdseq[2:n.spd.seq]))    
  }
  data$spd.binned <- cut(x = data[[spd]],
                         breaks = spd.breaks,
                         labels = spd.labels,
                         ordered_result = TRUE)
  # clean up the data
  data <- na.omit(data)
  
  # figure out the wind direction bins
  dir.breaks <- c(-dirres/2,
                  seq(dirres/2, 360-dirres/2, by = dirres),
                  360+dirres/2)  
  dir.labels <- c(paste(360-dirres/2,"-",dirres/2),
                  paste(seq(dirres/2, 360-3*dirres/2, by = dirres),
                        "-",
                        seq(3*dirres/2, 360-dirres/2, by = dirres)),
                  paste(360-dirres/2,"-",dirres/2))
  # assign each wind direction to a bin
  dir.binned <- cut(data[[dir]],
                    breaks = dir.breaks,
                    ordered_result = TRUE)
  levels(dir.binned) <- dir.labels
  data$dir.binned <- dir.binned
  
  # Run debug if required ----
  if (debug>0){    
    cat(dir.breaks,"\n")
    cat(dir.labels,"\n")
    cat(levels(dir.binned),"\n")       
  }  
  
  # deal with change in ordering introduced somewhere around version 2.2
  if(packageVersion("ggplot2") > "2.2"){    
    # cat("Hadley broke my code\n")
    data$spd.binned = with(data, factor(spd.binned, levels = rev(levels(spd.binned))))
    spd.colors = rev(spd.colors)
  }
  
  # create the plot ----
  p.windrose <- ggplot(data = data,
                       aes(x = dir.binned,
                           fill = spd.binned)) +
    geom_bar(color = 'grey') + 
    scale_x_discrete(drop = FALSE,
                     labels = waiver()) +
    coord_polar(start = -((dirres/2)/360) * 2*pi) +
    scale_fill_manual(name = "Wind Speed (m/s)", 
                      values = spd.colors,
                      drop = FALSE) +
    theme(axis.title.x = element_blank())
  
  # adjust axes if required
  if (!is.na(countmax)){
    p.windrose <- p.windrose +
      ylim(c(0,countmax))
  }
  
  # print the plot
  print(p.windrose)  
  
  # return the handle to the wind rose
  return(p.windrose)
}
