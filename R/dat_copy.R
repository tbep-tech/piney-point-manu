# # sync data files from piney-point
# 
# library(httr)
# library(httpuv)
# 
# gh_key <- Sys.getenv('GH_KEY')
# gh_secret <- Sys.getenv('GH_SECRET')
# 
# # setup app credentials
# myapp <- oauth_app(appname = "data-in-r",
#                    key = gh_key,
#                    secret = gh_secret)
# 
# # get oauth credentials
# github_token <- oauth2.0_token(oauth_endpoints('github'), app = myapp, cache = F)
# 
# # use api
# gtoken <- config(token = github_token)
# 
# # get list of remote files in data folder
# req <- GET("https://api.github.com/repos/tbep-tech/piney-point/contents/data", gtoken)
# stop_for_status(req)
# fls <- unlist(lapply(content(req), "[", "download_url"), use.names = F)
# 
# # download and save
# for(fl in fls){
#   
#   load(url(fl))
#   obj <- gsub('\\.RData$', '', basename(fl))
#   out <- paste('data', basename(fl), sep = '/')
#   save(list = obj, file = out, version = 2)
#     
# }


# sync data files from piney-point

fls <- c('bsstatloc', 'bstransect', 'bstransectocc', 'bswqdat', 'bswqdatsub', 'bswqrngs', 'kbrdat', 'lobodat', 'loboeco', 
         'mcrtmp', 'parms', 'ppseg', 'raindat', 'rsallpts', 'rsbntdat', 'rsbntpts', 'rscntdat', 'rscntthr', 'rsphydat', 'rsphypts', 
         'rsstatloc', 'rstrndat', 'rstrnlns', 'rstrnpts', 'rstrnwts', 'rswqdat', 'sbshed', 'segmask', 'tbhished', 'trnlns', 
         'trnpts', 'wqrefmap')

# download and save
for(fl in fls){
  
  pth <- paste0('https://tbep-tech.github.io/piney-point/data/', fl, '.RData')
  load(url(pth))
  out <- paste0('data/', fl, '.RData')
  save(list = fl, file = out, version = 2)
  
}