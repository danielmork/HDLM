# options(echo=TRUE)
poll <- "Temp"
print(poll)

library(units)
library(rgdal)
library(sf)
library(tigris)
library(raster)
library(gstat)
library(dplyr)

# load census tract data
tracts <- tigris::tracts("CO", c("Larimer","Weld","Boulder","Denver","Arapahoe",
                                 "Jefferson","Adams","Douglas",
                                 "Broomfield","Elbert","El Paso","Pueblo","Fremont",
                                 "Clear Creek","Gilpin","Park County","Teller"
)
, cb = TRUE)

# load pollution data
air <- read.csv(paste0("code/DataProcessing/CO_EPAairdata/allyears_",poll,"_epa.txt"))
air$ID <- paste0(air$state,"_",air$site)
air$prec <- air[,poll]
air <- air[c("ID","prec","LONG","LAT","date")]
dsp <- sp::SpatialPoints(air[,c("LONG","LAT")], proj4string=sp::CRS("+proj=longlat +datum=NAD83"))
dsp <- sp::SpatialPointsDataFrame(dsp, air)

air <- sp::spTransform(dsp,  CRSobj=raster::crs(tracts))


# idw
dta_idw <- NULL
for(dt in unique(air$date)){

  d <- subset(air, date==dt)
  gs <- gstat::idw(formula=prec~1, d,newdata=tracts, idp = 2)
  
  dta_idw <- dplyr::bind_rows(dta_idw,
                       data.frame(date=dt,
                                  county=as.numeric(paste0(tracts$STATEFP,tracts$COUNTYFP,tracts$TRACTCE)),
                                  var=as.data.frame(gs)[,"var1.pred"])
  )
}

# save output dta_idw
save(dta_idw, file=paste0("code/DataProcessing/CO_EPAairdata/county_idw_",poll,".rda"))
