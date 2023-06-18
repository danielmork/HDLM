# options(echo=TRUE)

library(data.table)
library(lubridate)
library(tidyverse)


# https://aqs.epa.gov/aqsweb/airdata/daily_TEMP_2021.zip
#------------------------------------------------------------------------
# functions to download epa data
#------------------------------------------------------------------------

download1year <- function(year,poll){
  filename <- paste0("daily_",poll,"_",year)
  download.file(paste0("https://aqs.epa.gov/aqsweb/airdata/",filename,".zip"), paste0(filename,".zip") )
  unzip(paste0(filename,".zip"))
  datain <- fread(paste0(filename,".csv"), check.names = TRUE)
  datain <- datain[State.Name%in%c("Colorado","Wyoming") & Longitude>-105.5 & Latitude<42, list(State.Name,Site.Num,Date.Local,Arithmetic.Mean,Longitude,Latitude, County.Code,City.Name )]
  file.remove(paste0(filename,".zip"))
  file.remove(paste0(filename,".csv"))
  return(datain)
}


download_all_data <- function(years=2002:2016,poll=42602, name){
  alldata <- NULL
  for(year in years){
    alldata <- bind_rows(alldata,download1year(year,poll=poll))
  }
  alldata <- alldata %>% mutate(date=as_date(Date.Local)) %>%
    rename(LONG=Longitude, LAT=Latitude, county=County.Code, site=Site.Num, state=State.Name, city=City.Name) %>%
    select(-Date.Local) %>%
    drop_na()
  colnames(alldata)[3] <- name
  return(alldata)
}


#------------------------------------------------------------------------
# Download data ane merge it and make weekly data
#------------------------------------------------------------------------


a <- download_all_data(years=2006:2016,poll="TEMP", name="Temp")
write_csv(a, paste0("code/DataProcessing/CO_EPAairdata/allyears_Temp_epa.txt"))
rm("a")
gc()

