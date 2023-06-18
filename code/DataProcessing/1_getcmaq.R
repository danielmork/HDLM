# This file downloads Fused CMAQ data files
# options(echo=TRUE)
library(R.utils)

options(timeout=NA)

for(year in 2003:2003){
  print(year)

  #PM2.5 data
  download.file(paste0("https://ofmpub.epa.gov/rsig/rsigserver?data/FAQSD/outputs/",year,"_pm25_daily_average.txt.gz"),
                paste0("code/DataProcessing/CMAQ/",year,"_pm25_daily_average.txt.gz"))
  gunzip(paste0("code/DataProcessing/CMAQ/",year,"_pm25_daily_average.txt.gz"), remove=FALSE)
  file.remove(paste0("code/DataProcessing/CMAQ/",year,"_pm25_daily_average.txt.gz"))
}
