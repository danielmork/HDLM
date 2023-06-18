rm(list=ls())
gc()

library(tidyverse)

load(file="code/DataProcessing/AnalysisData/co_birth_master_analysis_file.rdata")




colnames(co_birth)[grep(toupper("cig"),toupper(colnames(co_birth)))]
table(co_birth$CigsPrePreg, useNA="always")
table(co_birth$CigsTri1)
table(co_birth$CigsTri2)
table(co_birth$CigsTri3)
table(co_birth$PrenatalCare)

#--------------------------------------------------------------
# some general cleaning and variable selctions
# each section after this creates an analysis file for a separate data analysis
dim(co_birth)
# removing any anomalies. The anomalies are either true or NA.  keep the NA.
co_birth <- co_birth %>%  filter(Plurality==1 ) %>%
  dplyr::select(DOC, bwgaz, BWGr, MatAge, Income, MEduc, Marital, PriorWeight, MotherHeight,
         PrenatalCare, methnic, mracebrg, fipscoor, tract, 
         Sex, YOC, season, month, WOC, elev_feet, min_elev, max_elev,
         CigsPrePreg, CigsTri1, CigsTri2, CigsTri3, EstGest)
co_birth <- co_birth %>% mutate(hispanic = ifelse(methnic>=200 & methnic<300,"Hispanic","NonHispanic"),
                                race = ifelse(mracebrg=="01", "white", ifelse(mracebrg=="02", "Black", ifelse(mracebrg=="03","AmInd", "AsianPI"))),
                                PriorWeight = ifelse(PriorWeight%in%c("?","999","NA"), NA, PriorWeight),
                                PriorWeight = as.numeric(PriorWeight),
                                Income=letters[Income+1],
                                MEduc=letters[MEduc+1]) 
co_birth <- co_birth %>% mutate(Income=recode(Income, a = "<15k", b = "15_24k", c="25_34k", d="35_49k", e="50_74k", f=">75k"),
                                MEduc=recode(MEduc, a = "lsHS", b = "lsHS", c = "HSdeg", d = "HSdeg", e = "AssocDeg", f = "CollegeDeg", g = "AdvDeg", h = "AdvDeg", i = "AdvDeg"),
                                CigsPrePreg = ifelse(CigsPrePreg%in%c("?",NA),NA,CigsPrePreg),CigsPrePreg=as.numeric(CigsPrePreg),
                                CigsTri1 = ifelse(CigsTri1%in%c("?",NA),NA,CigsTri1),CigsTri1=as.numeric(CigsTri1),
                                CigsTri2 = ifelse(CigsTri2%in%c("?",NA),NA,CigsTri2),CigsTri2=as.numeric(CigsTri2),
                                CigsTri3 = ifelse(CigsTri3%in%c("?",NA),NA,CigsTri3),CigsTri3=as.numeric(CigsTri3),
                                SmkPre = ifelse(is.na(CigsPrePreg),NA,c("N","Y")[1+1*(0<CigsPrePreg)] ),
                                SmkTri1 = ifelse(is.na(CigsTri1),NA,c("N","Y")[1+1*(0<CigsTri1)] ),
                                SmkTri2 = ifelse(is.na(CigsTri2),NA,c("N","Y")[1+1*(0<CigsTri2)] ),
                                SmkTri3 = ifelse(is.na(CigsTri3),NA,c("N","Y")[1+1*(0<CigsTri3)] ),
                                SmkAny= ifelse(is.na(SmkTri1) & is.na(SmkTri2) & is.na(SmkTri3),NA,ifelse(SmkTri1=="Y" | SmkTri2=="Y" | SmkTri3=="Y","Y","N")),
                                Marital2 = Marital,
                                Marital2 = ifelse(Marital=="MR","CM",Marital2),
                                Marital2 = ifelse(Marital=="D","DMSW",Marital2),
                                Marital2 = ifelse(Marital=="MS","DMSW",Marital2),
                                Marital2 = ifelse(Marital=="W","DMSW",Marital2),
                                Marital2 = ifelse(Marital=="U",NA,Marital2),
                                Marital2 = ifelse(Marital=="US",NA,Marital2))

# calculate BMI and clean height
co_birth <- co_birth %>% mutate(MotherHeight = ifelse(MotherHeight=="?", NA, 
                                                      ifelse(substring(MotherHeight,3,3)==":",
                                                             MotherHeight,
                                                             paste(substring(MotherHeight,1,2),substring(MotherHeight,3,4),sep=":"))),
                                MotherHeightIn=ifelse(is.na(MotherHeight),NA,as.numeric(substring(MotherHeight,1,2))*12+as.numeric(substring(MotherHeight,4,5))),
                                MotherBMI = (PriorWeight/2.2)/(MotherHeightIn*0.0254)^2)



#---------------------------------------------------------------------------------------------
## Data for Dan DLNM


dta <- readr::read_csv("code/DataProcessing/AnalysisData/FrontRangeExposure.csv")

dta2_exp<- dta %>% 
  dplyr::select(date,fips,starts_with("PM25_pred_wk"),starts_with("Temp_idw_wk")) %>%
  mutate(DOC=date,
         fipscoor = substring(as.character(fips),3,5),
         tract = substring(as.character(fips),6,11))
dim(dta2_exp)
head(dta2_exp)

co_birth %>% dplyr::select(DOC, fipscoor, tract)
dta2_exp %>% dplyr::select(DOC, fipscoor, tract)

dta2_exp <- dta2_exp %>% filter(DOC>=min(co_birth$DOC) & DOC<=max(co_birth$DOC))
dta2_all <- left_join(x=co_birth, y=dta2_exp, 
                      by=c("DOC","fipscoor","tract"))

#,"WOC"
dta_dlnm_dan <- dta2_all %>% dplyr::select(-c("fips","MotherHeight","DOC","WOC","date","methnic","mracebrg"))
dim(dta_dlnm_dan)
colnames(dta_dlnm_dan)
dta_dlnm_dan <- dta_dlnm_dan %>% filter(max_elev<=6000)

# this is a dataset that can be used for plotting a map
# no health data is included.
dta_dlnm_dan_map <- dta_dlnm_dan %>% dplyr::select(tract,PriorWeight, MotherHeightIn, MotherBMI, MatAge,
                                            Income , hispanic , race , MEduc , PrenatalCare ,
                                            SmkAny ,Marital2 , elev_feet,fipscoor, EstGest) %>%
  drop_na()
dim(dta_dlnm_dan_map)
dta_dlnm_dan_map <- dta_dlnm_dan_map %>% group_by(fipscoor,tract) %>% summarise(births=n())

# remove variables from main analysis data
# these variables should not be shared at the individual level (tract)
dta_dlnm_dan <- dta_dlnm_dan %>% dplyr::select(-c(max_elev,min_elev,tract, BWGr, season, CigsTri3, SmkPre, SmkTri1, SmkTri2, SmkTri3, Marital))


# create temperature trimester variables

dim(dta_dlnm_dan)

save(dta_dlnm_dan, file="data/CO_Birth_data.rda")
