# options(echo=TRUE)
rm(list=ls())
library(tidyverse)


# read data
dta <- read_csv("code/DataProcessing/CO_CMAQ/combined_exposure_frontrange.txt")
dta <- dta %>% filter(date >= lubridate::as_date("2006-01-01"))
# dta <- dta[date %in% c("2007-01-13","2007-01-14")]
# dta
# radius <- 20; poll <- "PM25"


for(poll in c("Temp")){
    print(paste(poll))
    
    # read air pollution data to be added to the file
    load(paste0("code/DataProcessing/CO_EPAairdata/county_idw_",poll,".rda"))
    colnames(dta_idw)[c(2:3)] <- c("fips",paste0(poll,"_idw"))
    dta_idw$fips <- paste0("0",as.character(dta_idw$fips))
    dta_idw$date <- lubridate::as_date(dta_idw$date)
    dta <- left_join(dta,dta_idw, by = c("date", "fips"))
    print(dim(dta))
    
}  



# now lag exposure


dta <- dta %>% arrange(fips,date)
# dta2 <- dta %>% filter(!is.na(PM25_idw20))
# dim(dta2)

head(dta)



#--------------------------------------------------------------
# make lagged exposures

# function to make week lags
wklead <- function(x,wk){
  a <- (wk-1)*7
  b <- rowMeans(cbind(lead(x, n=0+a),
                      lead(x, n=1+a),
                      lead(x, n=2+a),
                      lead(x, n=3+a),
                      lead(x, n=4+a),
                      lead(x, n=5+a),
                      lead(x, n=6+a)), na.rm=TRUE)
  return(b)
}


# make week lags
for(l in 1:42){
  dta <- dta %>% group_by(fips) %>% mutate(!!paste0("PM25_pred_wk", l) := wklead(PM25_pred, l))
}
for(l in 1:42){
  dta <- dta %>% group_by(fips) %>% mutate(!!paste0("Temp_idw_wk", l) := wklead(Temp_idw, l))
}

dta <- dta %>% dplyr::select(-c("lon", "lat", "PM25_pred", "PM25_SEpred", "Temp_idw"))

# limit to used counties and dates of conception
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


# co_birth <- read_csv("data/co_birth_0718.csv")
co_birth <- read_csv("data/co_birth_simulated.csv")
total <- nrow(co_birth)
co_birth <- co_birth %>% 
  filter(fipscoor %in% fr, Sex %in% c("M","F")) %>% # limit to front range counties and defined sex
  mutate(DOB=lubridate::as_date(DOB, format="%m/%d/%Y"), # convert date to date format
         DOC=DOB-7*EstGest, YOC=lubridate::year(DOC)) %>%      #calculate date of conception
  filter(YOC %in% 2007:2015) # limit cohort based on conception year

co_county_dates <- co_birth %>% mutate(county=paste0("08",fipscoor, tract)) %>% dplyr::select(DOC,county) %>% unique()

colnames(co_county_dates) <- c("date","fips")
# co_county_dates
# dta
# dim(co_county_dates)
# dim(dta)
# 
dta <- left_join(co_county_dates,dta,by=c("date","fips"))
# head(dta2)
dim(dta)


write_csv(dta, file="code/DataProcessing/AnalysisData/FrontRangeExposure.csv")


