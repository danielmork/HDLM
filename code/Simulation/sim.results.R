# ---- Summarize Results ----
library(tidyr)
library(dplyr)
library(ggplot2)
library(tables)
library(kableExtra)

sims <- read.csv("output/sim_res_waic.txt")
sims <- sims %>% filter(trimws(model) %in% c("dlmtree:TDLMnested", "dlmtree:TDLMpairs",
                                             "dlmtree:GP",
                                             "dlmtree:SingleTDLM", "dlmtree:SingleGP",
                                             "dlmtree:TruthTDLM", "dlmtree:TruthGP")) %>%
  mutate(model = trimws(model))

sims %>% group_by(sim, error, model) %>% tally %>% print(n = 100)
# which(!(1:100 %in%
#           sort((sims %>% filter(sim == 2, error == 1,
#                                 model == "dlmtree:TruthGP") %>%
#                   select(sim.num) %>% unique %>% c)$sim.num)))

# ---- * RMSE, Cov, TP, FP, MSPE ----
sims %>%
  mutate(model = factor(trimws(model), levels = c("dlmtree:TDLMnested", "dlmtree:TDLMpairs",
                                          "dlmtree:GP",
                                          "dlmtree:SingleTDLM", "dlmtree:SingleGP",
                                          "dlmtree:TruthTDLM", "dlmtree:TruthGP"),
                        labels = c("Nested Tree HDLM", "Shared Tree HDLM",
                                   "Gaussian Process HDLM",
                                   "Treed DLM", "GP DLM",
                                   "True Subgroups TDLM", "True Subgroups GP"))) %>%
  filter(sim == 3, error == 25, model != "Gaussian Process HDLM"#, 
         # !str_detect(model, "True"),
  ) %>%
  group_by(sim.num) %>%
  mutate(min.mspe = min(oos.y.mspe)) %>% #select(model, sim.num, min.mspe,oos.y.mspe)
  mutate(min.waic = min(waic)) %>% #select(model, sim.num, min.mspe,oos.y.mspe)
  ungroup() %>%
  # filter(oos.y.mspe == min.mspe) %>%
  # filter(waic == min.waic) %>%
  group_by(model) %>%
  summarize(#name="MSPE Selected",
    rmse.effect = mean(rmse.effect)*100,
    cov.effect = mean(cov.effect),
    TP.effect = mean(TP.effect),
    FP.effect = mean(FP.effect),
    # "space" = "",
    rmse.noeffect = mean(rmse.noeffect)*100,
    cov.noeffect = mean(cov.noeffect),
    FP.noeffect = mean(FP.noeffect),
    # "space2" = "",
    mspe = mean(oos.y.mspe == min.mspe),
    waic = mean(waic == min.waic)) #%>%
  #oos.mspe = mean(oos.y.mspe)) %>%
  # kable(digits = 2, format = "latex", booktabs = T) %>%
  # kable_styling()

# ---- * Modifier inclusion----
sims %>%
  filter(trimws(model) %in% paste0("dlmtree:", c("GP", "TDLMnested", "TDLMpairs")),
         sim == 1, error == 1) %>%
  mutate(model = factor(trimws(model), 
                        levels = c("dlmtree:TDLMnested", "dlmtree:TDLMpairs", 
                                          "dlmtree:GP"),
                        labels = c("Nested Tree HDLM", "Shared Tree HDLM", 
                                   "Gaussian Process HDLM"))) %>%
  group_by(model) %>%
  summarize(inc.num = sprintf("%0.2f", mean(inc.mod_num)),
            inc.bin = sprintf("%0.2f", mean(inc.mod_bin)),
            inc.scale = sprintf("%0.2f", mean(inc.mod_scale)),
            inc.other_num = sprintf("%0.2f", mean(inc.other_num)),
            inc.other_bin = sprintf("%0.2f", mean(inc.other_bin)))#%>%#,
  # space = "",
  #inc.num.bin = sprintf("%0.2f", mean(prob.mod_num.mod_bin)),
  # inc.num.scale = mean(prob.mod_num.mod_scale),
  # inc.scale.scale = mean(prob.mod_scale.mod_scale),
  #inc.2mod.avg = sprintf("%0.2f", mean(prob.2mod.mean)),
  #inc.2mod.max = sprintf("%0.2f", mean(prob.2mod.max))) %>%
  # kable(digits = 2, format = "latex", booktabs = T) %>%
  # kable_styling()

# Cumulative effect
sims %>%
  group_by(sim, error, model) %>%
  summarize(cum.rmse.effect = mean(cum.rmse.effect),
            cum.rmse.noeffect = mean(cum.rmse.noeffect),
            cum.cov.effect = mean(cum.cov.effect),
            cum.cov.noeffect = mean(cum.cov.noeffect),
            cum.TP = mean(cum.TP),
            cum.FP = mean(cum.FP))


## MSPE / fixed
colnames(sims)
sims.mspe.long <- sims %>% 
  filter(sim == 3, error == 1) %>%
  pivot_wider(c("sim", "sim.num", "error"), 
              names_from = "model", values_from = "oos.y.mspe")
sims.mspe.long %>% mutate(nHDLM = `dlmtree:TDLMnested` / `dlmtree:SingleTDLM`,
                          sHDLM = `dlmtree:TDLMpairs` / `dlmtree:SingleTDLM`,
                          gHDLM = `dlmtree:GP` / `dlmtree:SingleTDLM`,
                          #trHDLM = `dlmtree:TruthTDLM` / `dlmtree:SingleTDLM`,
                          #tr2HDLM = `dlmtree:TruthGP` / `dlmtree:SingleTDLM`,
                          gDLM = `dlmtree:SingleGP` / `dlmtree:SingleTDLM`) %>%
  select(nHDLM, sHDLM, gHDLM,# trHDLM, tr2HDLM, 
         gDLM) %>%
  apply(2, mean)



## Boxplots
sims %>% 
  filter(sim == 2) %>%
  pivot_wider(c("sim", "sim.num", "error"), 
              names_from = "model", values_from = "rmse.effect") %>% 
  mutate(nHDLM = `dlmtree:TDLMnested` / `dlmtree:SingleTDLM`,
         sHDLM = `dlmtree:TDLMpairs` / `dlmtree:SingleTDLM`,
         gHDLM = `dlmtree:GP` / `dlmtree:SingleTDLM`,
         # trHDLM = `dlmtree:TruthTDLM` / `dlmtree:SingleTDLM`,
         # tr2HDLM = `dlmtree:TruthGP` / `dlmtree:SingleTDLM`,
         gDLM = `dlmtree:SingleGP` / `dlmtree:SingleTDLM`,
         tDLM = `dlmtree:SingleTDLM` / `dlmtree:SingleTDLM`) %>%
  select(sim, sim.num, error, nHDLM, sHDLM, gHDLM, tDLM, gDLM) %>%
  pivot_longer(4:8, names_to = "model", values_to = "val") %>%
  mutate(model = factor(model, levels = c("nHDLM", "sHDLM", "gHDLM", "tDLM", "gDLM"),
                        labels = c("Nested Tree HDLM", "Shared Tree HDLM", 
                                   "Gaussian Process HDLM", "Treed DLM", "GP DLM"))) %>%
  ggplot() +
  geom_boxplot(aes(x = as.factor(error), y = val, fill = model)) +
  theme_bw(base_size = 24) +
  labs(x = "Noise", y = "HDLM RMSE", fill = "Model") +
  theme(legend.position = "none", legend.key.height = unit(1.5, "cm"),
        legend.key.width = unit(1, "cm"))# +
scale_y_continuous(limits = c(0, 1))
