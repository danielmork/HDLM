library(tidyverse)
load("pm_bw_full_mod-shared.rda")
load("bw_dat.rda")

#### Modifier Selection ####
sort(colMeans(bw$modCount > 0));

# ---- Subgroup comparisons ----
pm_iqr <- IQR(pm_pred*pm_sd)
est <- estDLM(bw, dat, list("NonHispanic, BMI > 22.8" = which(dat$MotherBMI > 22.8 & dat$hispanic == "NonHispanic"),
                            "Hispanic, BMI > 22.8" = which(dat$MotherBMI > 22.8 & dat$hispanic == "Hispanic"),
                            "NonHispanic, BMI < 22.8" = which(dat$MotherBMI < 22.8 & dat$hispanic == "NonHispanic"),
                            "Hispanic, BMI < 22.8" = which(dat$MotherBMI < 22.8 & dat$hispanic == "Hispanic")))
# est <- estDLM(bw, dat, list("NonHispanic, Age >= 27" = which(dat$MatAge >= 27 & dat$hispanic == "NonHispanic"),
#                             "Hispanic, Age >= 27" = which(dat$MatAge >= 27 & dat$hispanic == "Hispanic"),
#                             "NonHispanic, Age < 27" = which(dat$MatAge < 27 & dat$hispanic == "NonHispanic"),
#                             "Hispanic, Age < 27" = which(dat$MatAge < 27 & dat$hispanic == "Hispanic")))
# est <- estDLM(bw, dat, 
#               list("Hispanic, AdvDeg" = which(dat$hispanic == "Hispanic" & dat$MEduc %in% c("AdvDeg")),
#                    "Hispanic, College" = which(dat$hispanic == "Hispanic" & dat$MEduc %in% c("CollegeDeg")),
#                    "Hispanic, Assoc" = which(dat$hispanic == "Hispanic" & dat$MEduc %in% c("AssocDeg")),
#                    "Hispanic, HS" = which(dat$hispanic == "Hispanic" & dat$MEduc %in% c("HSdeg")),
#                    "Hispanic, lsHS" = which(dat$hispanic == "Hispanic" & dat$MEduc %in% c("lsHS")),
#                    "Nonhispanic, AdvDeg" = which(dat$hispanic != "Hispanic" & dat$MEduc %in% c("AdvDeg")),
#                    "Nonhispanic, College" = which(dat$hispanic != "Hispanic" & dat$MEduc %in% c("CollegeDeg")),
#                    "Nonhispanic, Assoc" = which(dat$hispanic != "Hispanic" & dat$MEduc %in% c("AssocDeg")),
#                    "Nonhispanic, HS" = which(dat$hispanic != "Hispanic" & dat$MEduc %in% c("HSdeg")),
#                    "Nonhispanic, lsHS" = which(dat$hispanic != "Hispanic" & dat$MEduc %in% c("lsHS"))))
#### Create estDLM plot ####
plotDat <- do.call(rbind.data.frame,
                   lapply(names(est$groupIndex), function(n) {
                     data.frame(time = 1:bw$pExp,
                                mean = est$dlmMean[[n]],
                                BMIGrp = ifelse(str_detect(n, "BMI <"), "BMI < 22.8", "BMI \u2265 22.8"),
                                AgeGrp = ifelse(str_detect(n, "Age < 27"), "Age < 27", "Age \u2265 27"),
                                hisp = ifelse(str_detect(n, "Non"), "NonHispanic", "Hispanic"),
                                smk = ifelse(str_detect(n, "nSmk"), "Non Smoker", "Smoker"),
                                edu = ifelse(str_detect(n, "lsHS"), "<High School",
                                             ifelse(str_detect(n, "Assoc"), "Assoc",
                                                    ifelse(str_detect(n, "College"), "College",
                                                           ifelse(str_detect(n, "Adv"), "AdvDeg", "High School/GED")))),
                                lower = est$dlmCI[[n]][1,],
                                upper = est$dlmCI[[n]][2,],
                                group = n,
                                size = est$n[[n]])
                   }))
plotDat$edu <- factor(plotDat$edu, levels = c("<High School", "High School/GED", "Assoc", "College", "AdvDeg"))
ggplot(plotDat, aes(x = time, y = mean*pm_iqr/pm_sd, ymin = lower*pm_iqr/pm_sd, ymax = upper*pm_iqr/pm_sd)) +
  geom_hline(yintercept = 0, color = "red", size = 2) +
  geom_ribbon(fill = "grey") +
  geom_line(size = 2) +
  geom_hline(yintercept = 0, color = "red", size = 2, linetype = 3) +
  geom_text(aes(x = -Inf, y = Inf, vjust = 1.05, hjust = -0.05, label = paste0("n = ", size)), 
            size = 6) +
  facet_grid(hisp~BMIGrp) +
  theme_bw(base_size = 24) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  labs(x = "Week of Gestation",
       y = "Change in BWGAZ for\nIQR increase in log PM2.5")


# Individual Analyses
library(ggplot2)
library(viridis)
set.seed(3817)
idx.samp <- sample(nrow(dat), 5000)

sampDLM1 <- estDLM(bw, dat, as.list(idx.samp[1:1000]), verbose = F)
sampDLM2 <- estDLM(bw, dat, as.list(idx.samp[1001:2000]), verbose = F)
sampDLM3 <- estDLM(bw, dat, as.list(idx.samp[2001:3000]), verbose = F)
sampDLM4 <- estDLM(bw, dat, as.list(idx.samp[3001:4000]), verbose = F)
sampDLM5 <- estDLM(bw, dat, as.list(idx.samp[4001:5000]), verbose = F)

#### DLM effects ####
dlm.df <- do.call(rbind.data.frame,
                  lapply(1:1000, function(i) {
                    cbind.data.frame(dat[idx.samp[i], bw$modNames],
                                     data.frame(Obs = i,
                                                Time = 1:37, 
                                                Effect = sampDLM1$dlmMean[[i]] * pm_iqr / pm_sd))
                  }))
dlm.df$BMIGrp <- ifelse(dlm.df$MotherBMI < 22.8, "BMI < 22.8", "BMI \u2265 22.8")
dlm.df$AgeGrp <- ifelse(dlm.df$MatAge < 27, "Age < 27", "Age \u2265 27")
dlm.df$MEducOrd <- factor(dlm.df$MEducOrd, levels = c(1, 2, 3, 4, 5),
                          labels = c("<High School","High School/GED","Assoc","College","AdvDeg"))

ggplot(dlm.df) + 
  geom_hline(yintercept = 0, color = "red") +
  geom_line(aes(x = Time, y = Effect, group = Obs, color = MatAge), size = .5, alpha = .5) +
  facet_grid(hispanic~MEducOrd) + # change second variable to AgeGrp or MEducOrd
  scale_color_viridis(discrete=F) +
  theme_bw(base_size = 24) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.key.width = unit(2, "cm")) +
  labs(x = "Week of gestation",
       y = "Change in BWGAZ for\nIQR increase in log PM2.5", 
       color = "Mother's Age\n")

#### Cumulative effects ####
sampDLM1$ce <- lapply(1:1000, function(i) sum(sampDLM1$dlmMean[[i]]))
sampDLM2$ce <- lapply(1:1000, function(i) sum(sampDLM2$dlmMean[[i]]))
sampDLM3$ce <- lapply(1:1000, function(i) sum(sampDLM3$dlmMean[[i]]))
sampDLM4$ce <- lapply(1:1000, function(i) sum(sampDLM4$dlmMean[[i]]))
sampDLM5$ce <- lapply(1:1000, function(i) sum(sampDLM5$dlmMean[[i]]))
c.effect.df <- rbind.data.frame(do.call(rbind.data.frame,
                                        lapply(1:1000, function(i) {
                                          cbind.data.frame(dat[idx.samp[i], bw$modNames], Cumulative = sampDLM1$ce[[i]])
                                        })), 
                                do.call(rbind.data.frame,
                                        lapply(1:1000, function(i) {
                                          cbind.data.frame(dat[idx.samp[i+1000], bw$modNames], Cumulative = sampDLM2$ce[[i]])
                                        })),
                                do.call(rbind.data.frame,
                                        lapply(1:1000, function(i) {
                                          cbind.data.frame(dat[idx.samp[i+2000], bw$modNames], Cumulative = sampDLM3$ce[[i]])
                                        })),
                                do.call(rbind.data.frame,
                                        lapply(1:1000, function(i) {
                                          cbind.data.frame(dat[idx.samp[i+3000], bw$modNames], Cumulative = sampDLM4$ce[[i]])
                                        })),
                                do.call(rbind.data.frame,
                                        lapply(1:1000, function(i) {
                                          cbind.data.frame(dat[idx.samp[i+4000], bw$modNames], Cumulative = sampDLM5$ce[[i]])
                                        })))

c.effect.df$BMIGrp <- ifelse(c.effect.df$MotherBMI < 25, "Normal (BMI < 25)", "Overweight (BMI \u2265 25)")
c.effect.df$AgeGrp <- ifelse(c.effect.df$MatAge < 28, "Age < 28", "Age \u2265 28")
c.effect.df$MEducOrd <- factor(c.effect.df$MEducOrd, levels = c(1, 2, 3, 4, 5),
                               labels = c("<High School","High School/GED","Assoc","College","AdvDeg"))
c.effect.df$Cumulative <- c.effect.df$Cumulative * pm_iqr / pm_sd

ggplot(c.effect.df) +
  stat_summary_2d(aes(y = MatAge, x = MotherBMI, z = Cumulative), fun = "mean") +
  facet_grid(~hispanic) +
  scale_fill_viridis() +
  theme_bw(base_size = 30) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.key.width = unit(3, "cm")) +
  labs(y = "Mother's Age", x = "Body Mass Index", fill = "Avg. Cumulative Effect\n")

c.effect.df %>% group_by(hispanic) %>%
  summarize(min(Cumulative), max(Cumulative))



#### Modifier PIP results ####
sp <- cbind.data.frame(Rule = bw$termRules, bw$TreeStructs[,2:4])
sp <- sp[!duplicated(sp),]
splitRules <- lapply(strsplit(sp$Rule, "&", T), function(i) {
  sort(as.numeric(sapply(strsplit(i, ">=|<|\\[\\]|\\]\\[", perl = T), function(j) j[1])))
})
splitCount <- lapply(1:bw$mcmcIter, function(i) list())
treeMods <- lapply(1:bw$mcmcIter, function(i) rep(0, bw$nTrees))
for (i in 1:length(splitRules)) {
  it <- sp$Iter[i]
  tr <- sp$Tree[i]
  n <- length(splitRules[[i]])
  treeMods[[it]][tr+1] <- max(treeMods[[it]][tr+1], length(unique(splitRules[[i]])))
  if (n == 1) {
  } else if (n > 1) {
    for (s in 1:(n-1)) {
      for (e in (s+1):n) {
        splitCount[[it]][[paste0(bw$modNames[splitRules[[i]][s]+1], ".", 
                                 bw$modNames[splitRules[[i]][e]+1])]] <- T
      }
    }
  }
}
mean(do.call(rbind, treeMods)==4)
sc <- do.call(bind_rows, lapply(splitCount, cbind.data.frame))
sc.mean <- sort(colMeans(!is.na(sc)))
sc.mat <- data.frame()
for (i in 1:length(sc.mean)) {
  names <- sort(strsplit(names(sc.mean)[i], ".", T)[[1]])
  sc.mat <- rbind.data.frame(sc.mat,
                             data.frame("var1" = names[1], "var2" = names[2], "pip" = sc.mean[i]),
                             data.frame("var1" = names[2], "var2" = names[1], "pip" = sc.mean[i]))
}
sc.mat <- rbind.data.frame(sc.mat,
                           data.frame("var1" = "Sex", "var2" = "Sex", "pip" = 0),
                           data.frame("var1" = "hispanic", "var2" = "hispanic", "pip" = 0))
sc.mat$var1 <- factor(sc.mat$var1,
                      levels = c("PrenatalCare",
                                 "Marital2", "race", "Sex", "IncomeOrd", "SmkOrd", 
                                 "MEducOrd", "MatAge", "hispanic", "MotherBMI"),
                      labels = c("Prenatal",
                                 "Marital", "Race", "Sex", 
                                 "Income", "Smoking", 
                                 "Education", "Age", "Hispanic", "BMI"))
sc.mat$var2 <- factor(sc.mat$var2,
                      levels = c("PrenatalCare",
                                 "Marital2", "race", "Sex", "IncomeOrd", "SmkOrd", 
                                 "MEducOrd", "MatAge", "hispanic", "MotherBMI"),
                      labels = c("Prenatal",
                                 "Marital", "Race", "Sex", 
                                 "Income", "Smoking", 
                                 "Education", "Age", "Hispanic", "BMI"))
ggplot(sc.mat) +
  geom_tile(aes(x = var1, y = var2, fill = pip)) +
  scale_fill_viridis() +
  theme_bw(base_size = 30) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_discrete(expand = c(0, 0)) +
  labs(x = "", y = "", fill = "PIP") +
  theme(legend.position = "right", legend.key.height = unit(1.2, "cm"),
        legend.key.width = unit(.7, "cm"),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5)) +
  coord_fixed()



#### Continuous splits ####
sp2 <- bw$TreeStructs$Rule[!duplicated(bw$TreeStructs[,2:4])]
treeRules <- bw$TreeStructs %>% group_by(Iter, Tree) %>% 
  summarize(Rules = paste0(Rule, collapse = " & "))
splitRules2 <- table(do.call(c, lapply(strsplit(treeRules$Rules, " & ", T), unique)))
bmisp <- sort(splitRules2[str_detect(names(splitRules2), "MotherBMI") & 
                            str_detect(names(splitRules2), ">=")]) /
  sum(sort(splitRules2[str_detect(names(splitRules2), "MotherBMI") & 
                         str_detect(names(splitRules2), ">=")]))
agesp <- sort(splitRules2[str_detect(names(splitRules2), "MatAge") & 
                            str_detect(names(splitRules2), ">=")]) /
  sum(sort(splitRules2[str_detect(names(splitRules2), "MatAge") & 
                         str_detect(names(splitRules2), ">=")]))

ggplot() +
  geom_bar(data = data.frame(loc = as.numeric(sapply(strsplit(names(bmisp), " >= ", T), function(i) i[2])),
                             val = as.numeric(bmisp), grp = "BMI"), 
           mapping = aes(x = loc, y = val), stat = "identity", fill = "darkgreen", alpha = .5) +
  geom_bar(data = data.frame(loc = as.numeric(sapply(strsplit(names(agesp), " >= ", T), function(i) i[2])),
                             val = as.numeric(agesp), grp = "Age"), 
           mapping = aes(x = loc, y = val), stat = "identity", fill = "darkblue", alpha = .5) +
  facet_grid(~grp, scales = "free_x") +
  labs(x = "Split Location", y = "Density") +
  theme_bw(base_size = 24) +
  scale_y_continuous(expand = c(0, 0.005)) +
  scale_x_continuous(expand = c(0, 0.5)) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

