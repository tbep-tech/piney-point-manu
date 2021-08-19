
# setup -------------------------------------------------------------------

library(here)
library(tbeptools)
library(tidyverse)
library(sf)
library(lubridate)
box::use(
  units = units[set_units], 
  forcats = forcats[fct_drop], 
  multcompView = multcompView[multcompLetters]
)

source(here('R/funcs.R'))

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
  select(var, lbs, stkval = val) %>% 
  bind_rows(
    tibble(
      var = 'chla', 
      lbs = c('Chl-a (ug/L)'), 
      stkval = NA
    )
  )

# effluent characteristics (end of pipe)
effraw <- tibble(
  var = c('no23', 'nh34', 'tn', 'tp', 'orthop', 'dosat', 'ph', 'chla'),
  lbs = c("Nitrate/Nitrite (mg/L)", "NH3, NH4+ (mg/L)", "TN (mg/L)", 
          "TP (mg/L)", "Ortho-P (mg/L)", "DO (% sat.)", "pH", "Chl-a (ug/L)"),
  effval = c(0.292, 210, 220, mean(c(140, 161)), mean(c(140, 155)), NA, NA, 105)
)

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
    avev = round(median(val), unique(sigdig)), 
    .groups = 'drop'
  ) %>% 
  mutate(
    minv = paste0(' (', minv, ', '), 
    maxv = paste0(maxv, ')')
  ) %>% 
  unite(sumv, avev, minv, maxv, sep = '')

tab <- full_join(stkraw, bssum, by = 'var') %>% 
  full_join(effraw, by = c('var', 'lbs')) %>% 
  select(
    `Water quality variable` = lbs, 
    `2019 stack value` = stkval, 
    `2021 end-of-pipe value` = effval,
    `Bay median (min, max)` = sumv
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

# water quality comparison trnds ------------------------------------------

vrs <- c('chla', 'secchi', 'tn')
nonbay <- c('BH01', 'P Port 2', 'P Port 3', 'PM Out', '20120409-01', 'PPC41', 'P Port 4', 'PMB01', 'NGS-S Pond')

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
  left_join(parms, by = c('var', 'lbs')) %>% 
  select(date, var, lbs, val, sigdig, station, area) %>% 
  mutate(
    mo = month(date, label = T)
  ) %>% 
  filter(mo %in% c('Apr', 'May', 'Jun', 'Jul')) %>% 
  mutate(mo = fct_drop(mo))

# multiple comparisons of time frames within sites, all nutrients
cmps <- rswqsub %>% 
  group_by(area, lbs) %>% 
  nest %>% 
  mutate(
    ests = map(data, function(x){
      
      # pairwise comparisons with mann-whitney (wilcox)
      grps <- unique(x$mo)
      grps <- combn(grps, 2)
      pval <- rep(NA, ncol(grps))
      for(col in 1:ncol(grps)){
        grp <- x$mo %in% grps[, col, drop = TRUE]
        res <- wilcox.test(val ~ mo, data = x[grp, ], exact = FALSE, 
                           alternative = 'two.sided')
        pval[col] <- res$p.value
      }
      
      # adjust p-values using holm sequential bonferroni
      pval <- p.adjust(pval, method = 'holm')
      
      # pval as t/f using bonferroni correction
      vecs <- rep(FALSE, ncol(grps))
      vecs[pval < 0.05] <- TRUE
      names(vecs) <- paste(grps[1, ], grps[2, ], sep = '-')
      
      # group membership based on multiple comparisons
      lets <- multcompLetters(vecs)$Letters
      
      # kruskal test
      tst <- kruskal.test(val ~ mo, x)
      
      # standard summary stats
      sums <- group_by(x, mo) %>%
        summarise(
          length = length(na.omit(val)),
          medval = round(median(val, na.rm = TRUE), unique(sigdig)),
          minval = round(min(val, na.rm = TRUE), unique(sigdig)),
          maxval = round(max(val, na.rm = TRUE), unique(sigdig)), 
          .groups = 'drop'
        ) %>% 
        mutate(
          minval = paste0(' (', minval, ', '),
          maxval = paste0(maxval, ')'), 
          krusk_pval = p_ast(tst$p.value),
          krusk_chis = round(tst$statistic, 2)
        ) %>% 
        unite('sumv', medval, minval, maxval, sep = '') %>% 
        unite('krusk', krusk_chis, krusk_pval, sep = '')
      
      data.frame(lets, sums, stringsAsFactors = FALSE)
      
    })
  ) %>% 
  dplyr::select(-data) %>% 
  unnest('ests') %>% 
  ungroup() %>% 
  mutate(
    lbs = factor(lbs, levels = c('TN (mg/L)', 'Chl-a (ug/L)', 'Secchi (m)'))
  ) %>% 
  arrange(area, lbs, mo) %>% 
  group_by(area) %>%  
  mutate(
    area = ifelse(duplicated(area), '', area), 
    lbs = ifelse(duplicated(lbs), '', as.character(lbs)), 
    krusk = ifelse(duplicated(krusk), '', krusk)
  ) %>% 
  ungroup() %>% 
  select(
    Area = area, 
    `Water quality variable` = lbs, 
    `Chi-Sq.` = krusk,
    `Comp.` = lets, 
    Month = mo, 
    `N obs.` = length, 
    `Med. (Min., Max.)` = sumv
  )

wqcmptab <- cmps
save(wqcmptab, file = here('tables/wqcmptab.RData'))

# sav and mcr trends by month ---------------------------------------------

mcrsel <- c("Red", "Green", "Cyanobacteria")
savsel <- c('Thalassia testudinum', 'Halodule wrightii', 'Syringodium filiforme')

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
#   mutate(
#     mo = month(date)
#   ) %>%
#   group_by(station, mo) %>%
#   summarise(
#     obs = (any(bb > 0)),
#     .groups = 'drop'
#   ) %>% 
#   group_by(station) %>% 
#   summarise(
#     cnt = n(), 
#     .groups = 'drop'
#   ) %>% 
#   filter(cnt >= 4) %>% 
#   pull(station)

# add area
trnsf <- rstrndat %>%
  inner_join(rstrnpts, ., by = 'station') %>% 
  filter(taxa %in% c(mcrsel, savsel)) %>% 
  # filter(station %in% smpeff) %>% 
  st_intersection(areas) %>% 
  st_set_geometry(NULL) %>% 
  select(station, date, location, typ, taxa, bb, area)

trnsum <- trnsf %>% 
  mutate(
    mo = month(date, label = T),
    pa = bb > 0
  ) %>%
  dplyr::group_by(area, typ, mo, station, taxa) %>%
  dplyr::summarise(
    fo = sum(pa) / length(pa), 
    .groups = 'drop'
  ) %>% 
  group_by(area, taxa) %>% 
  nest %>% 
  mutate(
    ests = map(data, function(x){
      
      # pairwise comparisons with mann-whitney (wilcox)
      grps <- unique(x$mo)
      grps <- combn(grps, 2)
      pval <- rep(NA, ncol(grps))
      for(col in 1:ncol(grps)){
        grp <- x$mo %in% grps[, col, drop = TRUE]
        res <- wilcox.test(fo ~ mo, data = x[grp, ], exact = FALSE, 
                           alternative = 'two.sided')
        pval[col] <- res$p.value
      }
      
      # adjust p-values using holm sequential bonferroni
      pval <- p.adjust(pval, method = 'holm')
      
      # pval as t/f using bonferroni correction
      vecs <- rep(FALSE, ncol(grps))
      vecs[pval < 0.05] <- TRUE
      names(vecs) <- paste(grps[1, ], grps[2, ], sep = '-')
      
      # group membership based on multiple comparisons
      lets <- multcompLetters(vecs)$Letters
      
      # kruskal test
      tst <- kruskal.test(fo ~ mo, x)
      
      # standard summary stats
      sums <- group_by(x, mo) %>%
        summarise(
          length = length(na.omit(fo)),
          medval = round(median(fo, na.rm = TRUE), 3),
          minval = round(min(fo, na.rm = TRUE), 3),
          maxval = round(max(fo, na.rm = TRUE), 3), 
          .groups = 'drop'
        ) %>% 
        mutate(
          minval = paste0(' (', minval, ', '),
          maxval = paste0(maxval, ')'), 
          krusk_pval = p_ast(tst$p.value),
          krusk_chis = round(tst$statistic, 2)
        ) %>% 
        unite('sumv', medval, minval, maxval, sep = '') %>% 
        unite('krusk', krusk_chis, krusk_pval, sep = '')
      
      data.frame(lets, sums, stringsAsFactors = FALSE)
      
    })
  )

ests <- trnsum %>%
  select(-data) %>% 
  unnest('ests') %>% 
  ungroup()  

savests <- ests %>% 
  filter(taxa %in% savsel) %>% 
  mutate(taxa = factor(taxa, levels = savsel)) %>% 
  arrange(area, taxa, mo) %>% 
  group_by(area) %>%  
  mutate(
    area = ifelse(duplicated(area), '', area), 
    taxa = ifelse(duplicated(taxa), '', as.character(taxa)), 
    krusk = ifelse(duplicated(krusk), '', krusk)
  ) %>% 
  ungroup() %>% 
  select(
    Area = area, 
    `Seagrass species` = taxa, 
    `Chi-Sq.` = krusk,
    `Comp.` = lets, 
    Month = mo, 
    `N obs.` = length, 
    `Med. (Min., Max.)` = sumv
  )

mcrests <- ests %>% 
  filter(taxa %in% mcrsel) %>% 
  mutate(taxa = factor(taxa, levels = mcrsel)) %>% 
  arrange(area, taxa, mo) %>% 
  group_by(area) %>%  
  mutate(
    area = ifelse(duplicated(area), '', area), 
    taxa = ifelse(duplicated(taxa), '', as.character(taxa)), 
    krusk = ifelse(duplicated(krusk), '', krusk)
  ) %>% 
  ungroup() %>% 
  select(
    Area = area, 
    `Macroalgae group` = taxa, 
    `Chi-Sq.` = krusk,
    `Comp.` = lets, 
    Month = mo, 
    `N obs.` = length, 
    `Med. (Min., Max.)` = sumv
  )

mcrtab <- mcrests
savtab <- savests

save(mcrtab, file = here('tables/mcrtab.RData'))
save(savtab, file = here('tables/savtab.RData'))
