library(tbeptools)
library(sf)
library(mapview)

tbstacks <- st_read('https://opendata.arcgis.com/datasets/6277c3b1eeae4a818f8683fc29e6b35b_0.geojson') %>% 
  .[tbshed, ]

mapview(tbstacks, zcol = 'STATUS')

table(tbstacks$STATUS)
