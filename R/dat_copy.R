# sync data files from piney-point

library(httr)
library(httpuv)

# get list of remote files in data folder
req <- GET("https://api.github.com/repos/tbep-tech/piney-point/contents/data")
stop_for_status(req)
fls <- unlist(lapply(content(req), "[", "download_url"), use.names = F)

# download and save
for(fl in fls){

  load(url(fl))
  obj <- gsub('\\.RData$', '', basename(fl))
  out <- paste('data', basename(fl), sep = '/')
  save(list = obj, file = out, version = 2)

}
