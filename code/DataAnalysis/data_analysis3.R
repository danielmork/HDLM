library(dlmtree)

for (holdout.cat in 1:10) {
  for (model in 1:4) {
    
    
    ## ---- Load data ----
    load("bw_dat.rda")
    n <- nrow(dat)
    
    type = ifelse(model %in% c(1, 4), "nested",
                  ifelse(model == 2, "shared", "gp"))
    
    
    
    
    # Hold out 10% data
    n.holdout <- round(n * 0.1)
    samp.holdout <- function(n, n.holdout, i) {
      if (i == 1)
        return(1:n.holdout)
      if (i == 10)
        return((n.holdout * 9 + 1):n)
      return((n.holdout * (i-1) + 1):(n.holdout * i))
    }
    set.seed(43856)
    samp <- sample(n, n)
    
    for (i in 1:10) {
      subset <- (1:n)[-samp.holdout(n, n.holdout, i)]
      
      # check all levels are included in subset
      all.inc <- TRUE
      for (x in c("Income", "MEduc", "race", "hispanic",
                  "Marital2", "PrenatalCare", "Smk", "fipscoor",
                  "month", "YOC")) {
        if (length(setdiff(unique(dat[[x]]) , unique(dat[[x]][subset]))) > 0) {
          all.inc <- FALSE
          break;
        }
        
      }
      if (!all.inc)
        stop("missing categories")
    }
    holdout <- samp[samp.holdout(n, n.holdout, holdout.cat)]
    subset <- (1:n)[-holdout]
    
    hdat <- dat[holdout,]
    hexp <- pm_pred[holdout,]
    
    sdat <- dat[subset,]
    sexp <- pm_pred[subset,]
    rm(dat, pm_pred)
    
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
    burn <- 20
    iter <- 50
    thin <- 2
    
    cat("\nModel:", model, "Holdout:", holdout.cat, "\n")
    
    # Modifer models
    if (model < 4) {
      type = ifelse(model == 1, "nested",
                    ifelse(model == 2, "shared", "gp"))
      
      bw <- dlmtree(formula = fixed.effect,
                    data = sdat,
                    tree.modifiers = mod.list,
                    modifier.splits = 10,
                    exposure.data = sexp,
                    dlm.type = type,
                    n.trees = trees, n.burn = burn, n.iter = iter, n.thin = thin,
                    save.data = FALSE)
      pred <- predict(bw, hdat, hexp)
      oos.y.mspe <- mean((pred$y - hdat$bwgaz)^2)
      oos.y.cov <- mean((pred$y.lims[1,] < hdat$bwgaz) & (pred$y.lims[2,] > hdat$bwgaz))
      
      save(holdout, array.val, model, holdout.cat, type, bw,
           pred, oos.y.mspe, oos.y.cov,
           file = paste0("pm_bw_pred_mod-", type, "_h", holdout.cat, ".rda"))
      
      
      # TDLM
    } else if (model == 4) {
      type <- "nested"
      bw <-  dlmtree(formula = fixed.effect,
                     data = sdat,
                     tree.modifiers = mod.list,
                     modifier.splits = 10,
                     exposure.data = sexp,
                     dlm.type = type,
                     n.trees = trees, n.burn = burn, n.iter = iter, n.thin = thin,
                     fixed.tree.idx = list(1:nrow(sdat)),
                     save.data = FALSE)
      pred <- predict(bw, hdat, hexp, fixed.idx = list(1:nrow(hexp)))
      oos.y.mspe <- mean((pred$y - hdat$bwgaz)^2)
      oos.y.cov <- mean((pred$y.lims[1,] < hdat$bwgaz) & (pred$y.lims[2,] > hdat$bwgaz))
      
      save(holdout, array.val, model, holdout.cat, type, bw,
           pred, oos.y.mspe, oos.y.cov,
           file = paste0("pm_bw_pred_fixed-", type, "_h", holdout.cat, ".rda"))
    }
  }
}
quit()


#### Calculate holdout MSPE ####

library(tidyverse)
mspe <- data.frame()
# hdlm results
for (i in 1:10) {
  for (type in c("nested", "shared", "gp")) {
    f <- paste0("pm_bw_pred_mod-", type, "_h", i, ".rda")
    if (file.exists(f)) {
      load(f)
      mspe <- rbind.data.frame(mspe, 
                               data.frame("model" = "hdlm", 
                                          "type" = type, 
                                          "holdout" = i, 
                                          "mspe" = oos.y.mspe,
                                          "cov" = oos.y.cov,
                                          "f.c" = var(pred$fhat)/var(pred$ztg)))
    }
  }
}
# fixed tree results
for (i in 1:10) {
  f <- paste0("pm_bw_pred_fixed-nested", "_h", i, ".rda")
  if (file.exists(f)) {
    load(f)
    mspe <- rbind.data.frame(mspe, 
                             data.frame("model" = "dlm", 
                                        "type" = "fixed", 
                                        "holdout" = i, 
                                        "mspe" = oos.y.mspe,
                                        "cov" = oos.y.cov,
                                        "f.c" = var(pred$fhat)/var(pred$ztg)))
  }
}

options(pillar.sigfig = 7)
all <- mspe %>% group_by(holdout) %>% summarize(n=n()) %>% filter(n == 4)
mspe %>% #filter(holdout %in% all$holdout) %>% 
  group_by(type) %>%
  summarize(n(), mean(mspe), median(mspe), sd(mspe), "Fixed:Exp" = 1/mean(f.c)) %>% as.data.frame
mspe %>% #filter(holdout %in% all$holdout) %>% 
  group_by(model) %>%
  summarize(n(), mean(mspe), median(mspe), sd(mspe)) %>% as.data.frame

boxplot(mspe~type, data = mspe %>% filter(holdout %in% all$holdout))


mspe %>% pivot_wider(2, names_from = "type", values_from = mspe) %>%
  mutate(nested = nested / fixed, shared = shared / fixed, gp = gp / fixed) %>%
  select(nested, shared, gp) %>%# colMeans
  pivot_longer(cols = c("nested", "shared", "gp")) %>%
  ggplot() +
  geom_boxplot(aes(name, value), fill = "grey", outlier.size = 2) +
  theme_bw(base_size = 24)  +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  labs(x = "HDLM", y = "MSPE relative to treed DLM") +
  scale_x_discrete(limits = c("shared", "nested", "gp"),
                   labels = c("Shared", "Nested", "GP"))

