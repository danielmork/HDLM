rm(list=ls())
library(tidyverse)

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


# load data exposure data
# use this only to limit to front range counties
dta <- read_csv("code/DataProcessing/CO_CMAQ/combined_exposure_frontrange.txt")
fr_counties <- as.numeric(unique(dta$county))
rm("dta")
gc()



#--------------------------------------------------------------
# read in CO birth data

# co_birth <- read_csv("data/co_birth_0718.csv")
co_birth <- read_csv("data/co_birth_simulated.csv")
total <- nrow(co_birth)
co_birth <- co_birth %>% 
  filter(fipscoor %in% fr, Sex %in% c("M","F")) %>% # limit to front range counties and defined sex
  mutate(DOB=lubridate::as_date(DOB, format="%m/%d/%Y"), # convert date to date format
         DOC=DOB-7*EstGest, YOC=lubridate::year(DOC)) %>%      #calculate date of conception
  filter(YOC %in% 2007:2015) # limit cohort based on conception year

# co_county_dates <- co_birth %>% mutate(county=paste0("08",fipscoor, tract)) %>% select(DOC,county) %>% unique()
# readr::write_csv(co_county_dates,"CO_EPAairdata/co_county_dates.csv")

#--------------------------------------------------------------
# identify season of conceptions and week of conceptiosn
getSeason <- function(DATES) {
  WS <- as.Date("2012-12-21", format = "%Y-%m-%d") # Winter Solstice
  SE <- as.Date("2012-3-21",  format = "%Y-%m-%d") # Spring Equinox
  SS <- as.Date("2012-6-21",  format = "%Y-%m-%d") # Summer Solstice
  FE <- as.Date("2012-9-21",  format = "%Y-%m-%d") # Fall Equinox
  
  # Convert dates from any year to 2012 dates
  d <- as.Date(strftime(DATES, format="2012-%m-%d"))
  
  ifelse (d >= WS | d < SE, "Winter",
          ifelse (d >= SE & d < SS, "Spring",
                  ifelse (d >= SS & d < FE, "Summer", "Fall")))
}

# add season and month of conception
co_birth <- co_birth %>% mutate(season=getSeason(DOC), month=lubridate::month(DOC))

# add week of conception
co_birth <- co_birth %>% arrange(DOC)
co_birth <- co_birth %>% mutate(WOC=ceiling(as.numeric((DOC-min(DOC))/7)))


#--------------------------------------------------------------
# add elevation
co_elev <- co_birth %>% group_by(fipscoor, tract) %>% summarise(min_elev=min(elev_feet), max_elev=max(elev_feet))
co_birth <- left_join(x=co_birth, y=co_elev, by=c("fipscoor", "tract"))


#--------------------------------------------------------------
# get birth weight gestational age z score using 
# https://apps.cpeg-gcep.net/premZ_cpeg/
# this requaires linking each file
# missing bw cause errors in the fenton link

# flow: 1) create files; 2) manually link files; 3) load linked files; 4 merge linked files onto data
co_birth <- co_birth %>% filter(!is.na(BWGr))
co_birth <- co_birth[!duplicated(co_birth$vsid),]
for(y in unique(co_birth$YOC)){
  a <- co_birth %>% filter(YOC==y) %>% dplyr::select(vsid,EstGest,BWGr,Sex) %>% rename(weight=BWGr, sex=Sex, weeks=EstGest, id=vsid)
  write_csv(a,
            path=paste0("DataProcessing/FentonLink/forbwgaz",y,".csv"))
}

# this requires a manual link
bwgaz <- NULL
for(y in unique(co_birth$YOC)){
  bwgaz <- bind_rows(bwgaz,
                     read_csv(paste0("DataProcessing/FentonLink/out_forbwgaz",y,".csv"))
  )
}

# to load simulated data instead of linking with fenton use the line below
# bwgaz <- read_csv("data/fenton_simulated.csv")


#merge 
bwgaz <- bwgaz %>% dplyr::select(id, WZ) %>% rename(bwgaz = WZ, vsid=id) 
co_birth <- left_join(x=co_birth, y=bwgaz, by="vsid")



#--------------------------------------------------------------
# save file
save(co_birth, file="code/DataProcessing/AnalysisData/co_birth_master_analysis_file.rdata")

