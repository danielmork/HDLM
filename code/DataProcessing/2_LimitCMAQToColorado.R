# options(echo=TRUE)
# this file limits the CMAQ data to Colorado
# then creates a single file for CO for all years

library(tidyverse)
library(lubridate)



# front range metro counties
fr <- 
  c("069",# larimer
    "123",# weld
    "013",# Boulder
    "031",# city of denver
    "005",# Arapahoe 
    "059",# Jefferson County
    "001",# Adams
    "035",# Douglas 
    "014",# City and County of Broomfield
    "039",# Elbert County
    "093",# Park County
    "019",# Clear Creek County
    "047",# Gilpin County
    "041",# El Paso County
    "119",# Teller County
    "043",# Fremont County
    "101" # Pueblo County
  )


#--------------------------------------------------------------------------------
# PM2.5
#--------------------------------------------------------------------------------

# limit to Colorado by year
for(year in 2002:2016){
  print(year)
  dta <- read_csv(paste0("code/DataProcessing/CMAQ/",year,"_pm25_daily_average.txt"),n_max = 1)

  if(class(attr(dta,"spec")$cols[[1]])[1]=="collector_date"){
    dta <- read_csv(paste0("code/DataProcessing/CMAQ/",year,"_pm25_daily_average.txt"),
                    col_types=list(col_date(),
                                   col_character(),
                                   col_double(),
                                   col_double(),
                                   col_double(),
                                   col_double()))
  }else{
      dta <- read_csv(paste0("code/DataProcessing/CMAQ/",year,"_pm25_daily_average.txt"),
                      col_types=list(col_date("%b-%d-%Y"),
                                     col_character(),
                                     col_double(),
                                     col_double(),
                                     col_double(),
                                     col_double()))
  }


  # rename fips
  if(any(colnames(dta)=="FIPS")){
    dta <- dta %>% rename(fips=FIPS)
  }else if(any(colnames(dta)=="Loc_Label1")){
    dta <- dta %>% rename(fips=Loc_Label1)
  }
  # rename other columns and limit to Colorado
  dta <- dta %>% rename(lat=Latitude, lon=Longitude) %>%
      filter(substring(fips,1,2)=="08")
  colnames(dta)[1] <- "date"
  colnames(dta)[5] <- "PM25_pred"
  colnames(dta)[6] <- "PM25_SEpred"
  write_csv(dta, paste0("code/DataProcessing/CO_CMAQ/",year,"_pm25_daily_average.txt"))
  rm("dta")
  gc()
}


# bind files
COdta <- NULL
for(year in 2002:2016){
  print(year)
  COdta <- bind_rows(COdta,read_csv(paste0("code/DataProcessing/CO_CMAQ/",year,"_pm25_daily_average.txt"),
                                    col_types=list(col_date(),
                                                   col_character(),
                                                   col_double(),
                                                   col_double(),
                                                   col_double(),
                                                   col_double())))
}

COdta <- COdta %>% mutate(county=substring(fips,3,5))
write_csv(COdta, paste0("code/DataProcessing/CO_CMAQ/allyears_pm25_daily_average.txt"))


# limit to front range metro
FRdta_PM25 <- COdta %>% filter(county%in%c(fr))
write_csv(FRdta_PM25, "code/DataProcessing/CO_CMAQ/allyears_pm25_daily_average_frontrange.txt")

rm(list=c("COdta"))
gc()
ls()

#--------------------------------------------------------------------------------
# save file
#--------------------------------------------------------------------------------


write_csv(FRdta_PM25, "code/DataProcessing/CO_CMAQ/combined_exposure_frontrange.txt")
