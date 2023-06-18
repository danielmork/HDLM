library(tidyverse)
library(BART)
library(dlmtree)



results <- NULL

for(j in 100){
  set.seed(j)

# -------------------------------- #
# generate data
# -------------------------------- #

# this is for a binomial model with covaraites so regenerate y to be only a function of exposrues
D <- sim.tdlmm(sim = 1, mean.p = 0.5, n =5000)

# new y
# use 1:10 signal to noise ratio of standard deviations
y <- D$exposures[[1]] %*% D$margDLM + sd(D$exposures[[1]] %*% D$margDLM)*rnorm(nrow(D$exposures[[1]]))*sqrt(10)
D$dat$y <- y

# get IQR for computation
iqr <- -diff(quantile(D$exposures[[1]],c(.75,.25)))

# -------------------------------- #
# fit TDLM
# -------------------------------- #

m1 <- tdlnm(y ~ 1, 
            data = D$dat, 
            exposure.data = D$exposures[[1]],
            exposure.splits = 0, # make this a DLM (as opposed to DLNM)
            family = "gaussian", # change to 'gaussian' for continuous response
            n.trees = 20, n.burn = 1000, n.iter = 2000, n.thin = 2)

s_m1 <- summary(m1)

# get estiamtes
est_tdlm <- data.frame(model="tdlm",mean=s_m1$matfit,lower=s_m1$cilower,upper=s_m1$ciupper)

# scale bu IQR
est_tdlm[,-1] <- iqr * est_tdlm[,-1]


# -------------------------------- #
# fit BART
# -------------------------------- #

fit <- wbart(
  x.train = as.matrix(D$exposures[[1]]),
  y.train = y,
  numcut=2000,
  nskip=1000
)


# place to stor bart results
est_bart <- est_tdlm 
est_bart$model <- "bart"
est_bart$mean <- est_bart$lower <- est_bart$upper <- NA

# construct contrast and save results
for(i in 1:ncol(D$exposures[[1]])){
  means <- colMeans(D$exposures[[1]])
  means2 <- rbind(means,means)
  means2[,i] <- c(quantile(D$exposures[[1]],c(.75,.25)))
  
  a <- predict(fit, newdata=means2)
  
  
  diff <- a[,1]-a[,2]
  
  est_bart[i,-1] <- c(mean(diff),c(quantile(diff,c(0.025,0.975))) )
  
}





# -------------------------------- #
# Summarize results
# -------------------------------- #

results <- rbind(results,
data.frame(
  
# RMSE
BART_rmse = sqrt(mean((est_bart[,2]-iqr*D$margDLM)^2)),
TDLM_rmse = sqrt(mean((est_tdlm[,2]-iqr*D$margDLM)^2)),

# CI width
BART_CI_width = mean(est_bart[,4]-est_bart[,3]),
TDLM_CI_width = mean(est_tdlm[,4]-est_tdlm[,3]),

# coverage
BART_CI_coverage = mean((est_bart[,4]>iqr*D$margDLM)*(est_bart[,3]<iqr*D$margDLM)),
TDLM_CI_coverage = mean((est_tdlm[,4]>iqr*D$margDLM)*(est_tdlm[,3]<iqr*D$margDLM)),

# identify window
BART_pr_window_true = mean((est_bart[which(D$margDLM!=0),3]>0)),
TDLM_pr_window_true = mean((est_tdlm[which(D$margDLM!=0),3]>0)),

# identify window
BART_pr_window_false = mean(
  c((est_bart[which(D$margDLM==0),3]>0)|(est_bart[which(D$margDLM==0),4]<0),
    (est_bart[which(D$margDLM!=0),4]<0)
)),
TDLM_pr_window_false = mean(
  c((est_tdlm[which(D$margDLM==0),3]>0)|(est_tdlm[which(D$margDLM==0),4]<0),
    (est_tdlm[which(D$margDLM!=0),4]<0)
  ))

)
)


print(j)
print(round(colMeans(results),3))

}



write.csv(results,"output/TDLM_vs_BART_sim_full_results.csv")
write.csv(colMeans(results),"output/TDLM_vs_BART_sim_summary_results.csv")




# -------------------------------- #
# Visualize results
# -------------------------------- #

est_tdlm$model <- toupper(est_tdlm$model)
est_bart$model <- toupper(est_bart$model)

est_tdlm$time <- 1:nrow(est_tdlm)
est_bart$time <- 1:nrow(est_bart)

true_total <- est_total <- bind_rows(est_tdlm,est_bart)
true_total$mean <- rep(iqr*D$margDLM,1)
true_total$lower <- true_total$upper <- NA

true_total$type <- "Simulated truth"
est_total$type <- "Estimated"



pdf("output/TDLM_vs_BART_figure.pdf", height=3.5, width=6)
est_total$model <- factor(est_total$model, levels = c("BART", "TDLM"), labels = c("BART", "Treed DLM"))
true_total$model <- factor(true_total$model, levels = c("BART", "TDLM"), labels = c("BART", "Treed DLM"))
ggplot(data=est_total) +
  geom_hline(yintercept = 0, color = "red", size = 1) +
  geom_ribbon(aes(x=time, y=mean, ymin=lower, ymax=upper), fill="grey") +
  geom_line(data = true_total, aes(x=time, y=mean), color="blue", linetype=2, size = 2) +
  geom_line(aes(x=time, y=mean, color=type, linetype=type), size = 2) +
  facet_wrap(~model, nrow = 2) +
  theme_bw(base_size = 24) +
  xlab("Week of gestation") +
  ylab("Exposure effect") + 
  scale_color_manual(values=c("black","blue")) +
  scale_x_continuous(minor_breaks = NULL, expand = c(0, 0)) + scale_y_continuous(minor_breaks = NULL) +
  theme(legend.title=element_blank(), panel.grid = element_blank(),
        legend.position = "none")


dev.off()




