# Load raw dataset
load("data/CO_Birth_data.rda")

pm_pred <- (as.matrix(dta_dlnm_dan[,grep("PM25_pred", colnames(dta_dlnm_dan))]))[,1:37]
temp_idw <- as.matrix(dta_dlnm_dan[,grep("Temp", colnames(dta_dlnm_dan))])[,1:37]
dat <- dta_dlnm_dan[,1:32]
rm(dta_dlnm_dan); gc()

# ---- * Additional covariates ----
dat$tempTri1 <- rowMeans(temp_idw[,1:13])
dat$tempTri2 <- rowMeans(temp_idw[,14:27])
dat$tempTri3 <- rowMeans(temp_idw[,28:37])
dat$Smk <- ifelse(is.na(dat$SmkAny), NA, "Never")
dat$Smk <- ifelse(dat$CigsPrePreg > 0, "Former", dat$Smk)
dat$Smk <- ifelse(dat$CigsTri1 > 0 | dat$CigsTri2 > 0, "CurLow", dat$Smk)
dat$Smk <- ifelse(dat$CigsTri1 > 10 | dat$CigsTri2 > 10, "CurHigh", dat$Smk)
dat$SmkOrd <- ifelse(dat$Smk == "Former", 2,
                     ifelse(dat$Smk == "CurLow", 3,
                            ifelse(dat$Smk == "CurHigh", 4, 1)))
dat$IncomeOrd <- ifelse(dat$Income == "<15k", 1,
                        ifelse(dat$Income == "15_24k", 2,
                               ifelse(dat$Income == "25_34k", 3,
                                      ifelse(dat$Income == "35_49k", 4,
                                             ifelse(dat$Income == "50_74k", 5, 6)))))
dat$MEducOrd <- ifelse(dat$MEduc == "lsHS", 1,
                       ifelse(dat$MEduc == "HSdeg", 2,
                              ifelse(dat$MEduc == "AssocDeg", 3, 
                                     ifelse(dat$MEduc == "CollegeDeg", 4, 5))))


# ---- * Complete cases ----
expCC <- which(complete.cases(pm_pred) & complete.cases(temp_idw))
cols <- c("MatAge", "MotherHeightIn", "PriorWeight", "MotherBMI",
          "Income", "race", "hispanic", "Sex",
          "MEduc", "Marital2", "PrenatalCare",
          "Smk", "fipscoor", "elev_feet",
          "month",  "YOC", "IncomeOrd", "MEducOrd", "SmkOrd", 
          "tempTri1", "tempTri2", "tempTri3")
datCC <- which(complete.cases(dat[, cols]))

# Combine with exposures complete cases
datCC <- intersect(datCC, expCC)
bwCC <- which(complete.cases(dat$bwgaz) & dat$EstGest >= 37)
dat <- dat[intersect(datCC, bwCC), ]
pm_pred <- pm_pred[intersect(datCC, bwCC),]
pm_sd <- sd(pm_pred)
pm_pred <- (pm_pred - mean(pm_pred)) / pm_sd
rm(datCC, bwCC, expCC, cols, temp_idw)

save(dat, pm_pred, pm_sd, 
     file = "bw_dat.rda", compress = "xz")

library(dlmtree)
# Fixed effect model
fixed.effect <- as.formula(bwgaz ~ MatAge + I(MatAge^2) +
                             MotherHeightIn + PriorWeight + MotherBMI +
                             Income + MEduc + Marital2 +  PrenatalCare + Smk +
                             race + hispanic + elev_feet + as.factor(fipscoor) + 
                             as.factor(month) + as.factor(YOC) +
                             tempTri1 + tempTri2 + tempTri3)

mod.list <- c("MatAge", "MotherBMI", "IncomeOrd", "MEducOrd",
              "race", "hispanic", "Marital2", "PrenatalCare", 
              "SmkOrd", "Sex")


# Model params
trees <- 20
burn <- 5000
iter <- 15000
thin <- 5

# Run models
for (model in 1:4) {
  if (model < 4) {
    type = ifelse(model == 1, "nested",
                  ifelse(model == 2, "shared", "gp"))
    
    bw <- dlmtree(formula = fixed.effect, 
                  data = dat,
                  tree.modifiers = mod.list, 
                  modifier.splits = 10,
                  exposure.data = pm_pred,
                  dlm.type = type,
                  n.trees = trees, n.burn = burn, n.iter = iter, n.thin = thin,
                  save.data = FALSE)
    
    save(bw, array.val, model, version, data.file, type,
         file = paste0("pm_bw_full_mod-", type, ".rda"), 
         compress = "xz")
    
    
    # TDLM
  } else if (model == 4) {
    type <- "tdlm"
    bw <- tdlnm(fixed.effect, data = dat, exposure.data = pm_pred, exposure.splits = 0,
                n.trees = trees, n.burn = burn, n.iter = iter, n.thin = thin)
    
    save(bw, array.val, model, version, data.file, type,
         file = paste0("/projects/dmork@colostate.edu/co_dlmtree/Out/",
                       "pm_bw_full_", type, ".rda"), 
         compress = "xz")
  }
}