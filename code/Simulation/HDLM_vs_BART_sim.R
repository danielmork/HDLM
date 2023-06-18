library(tidyverse)
library(BART)
library(dlmtree)



results <- NULL

for(j in 100){
  set.seed(j)

# -------------------------------- #
# generate data
# -------------------------------- #

# this is for a binomial model with covaraites so regenerate y to be only a function of exposures
D <- sim.tdlmm(sim = 1, mean.p = 0.5, n =5000)

# new y
# use 1:10 signal to noise ratio of standard deviations
dlf <- D$margDLM
modifier <- D$dat$b1
exp_eff <- (D$exposures[[1]] %*% dlf)
y <- exp_eff * modifier + sd(exp_eff)*rnorm(nrow(D$exposures[[1]]))*sqrt(10)
D$dat$y <- y

# get IQR for computation
iqr <- -diff(quantile(D$exposures[[1]],c(.75,.25)))

# -------------------------------- #
# fit TDLM
# -------------------------------- #

m1 <- dlmtree(y ~ .,
            data = D$dat[,c("y","b1")],
            tree.modifiers = "b1",
            exposure.data = D$exposures[[1]],
            exposure.splits = 0, # make this a DLM (as opposed to DLNM)
            family = "gaussian", # change to 'gaussian' for continuous response
            n.trees = 20, n.burn = 1000, n.iter = 2000, n.thin = 2)


est <- estDLM(object = m1, new.data = D$dat[,c("y","b1")],
              group.index=list("group1" = which(D$dat$b1 == 1),
                                           "group0" = which(D$dat$b1 == 0)),
              return.mcmc = TRUE)


diff <- est$mcmc$group1-est$mcmc$group0
# get estimates
est_tdlm <- data.frame(model="hdlm",
                       mean0=est$dlmMean$group0,
                       lower0=est$dlmCI$group0[1,],
                       upper0=est$dlmCI$group0[2,],
                       mean1=est$dlmMean$group1,
                       lower1=est$dlmCI$group1[1,],
                       upper1=est$dlmCI$group1[2,],
                       mean10=rowMeans(diff),
                       lower10=apply(diff,1,quantile,0.025),
                       upper10=apply(diff,1,quantile,0.975),
                       prdiff=rowMeans(diff!=0))

# scale bu IQR
est_tdlm[,-c(1,11)] <- iqr * est_tdlm[,-c(1,11)]


# -------------------------------- #
# fit BART
# -------------------------------- #

fit <- wbart(
  x.train = cbind(as.matrix(D$exposures[[1]]),modifier),
  y.train = y,
  numcut=2000,
  nskip=1000
)


# place to stor bart results
est_bart <- data.frame(model=rep("bart",37),
                       mean0 = NA,
                       lower0 = NA,
                       upper0 = NA,
                       mean1 = NA,
                       lower1 = NA,
                       upper1 = NA,
                       mean10 = NA,
                       lower10 = NA,
                       upper10 = NA,
                       prdiff=NA)

# construct contrast and save results
for(i in 1:ncol(D$exposures[[1]])){
  means <- colMeans(D$exposures[[1]])
  means2 <- rbind(means,means,means,means)
  means2[,i] <- rep(quantile(D$exposures[[1]],c(.75,.25)),2)
  # add modifier
  means2 <- cbind(means2,c(0,0,1,1))

  a <- predict(fit, newdata=means2)

  # difference in modifier=0
  diff0 <- a[,1]-a[,2]

  # difference in modifier=1
  diff1 <- a[,3]-a[,4]

  # difference between groups
  diff10 <- diff1 - diff0

  est_bart[i,2:4] <- c(mean(diff0),c(quantile(diff0,c(0.025,0.975))) )
  est_bart[i,5:7] <- c(mean(diff1),c(quantile(diff1,c(0.025,0.975))) )
  est_bart[i,8:10] <- c(mean(diff10),c(quantile(diff10,c(0.025,0.975))) )
  est_bart[i,11] <- mean((diff1 - diff0)!=0)
}





# -------------------------------- #
# Summarize results
# -------------------------------- #

results <- rbind(results,
data.frame(

# RMSE
  BART_rmse0 = sqrt(mean((est_bart[,2]-0)^2)),
  TDLM_rmse0 = sqrt(mean((est_tdlm[,2]-0)^2)),
  BART_rmse1 = sqrt(mean((est_bart[,5]-iqr*D$margDLM)^2)),
  TDLM_rmse1 = sqrt(mean((est_tdlm[,5]-iqr*D$margDLM)^2)),
  BART_rmse10 = sqrt(mean((est_bart[,8]-iqr*D$margDLM)^2)),
  TDLM_rmse10 = sqrt(mean((est_tdlm[,8]-iqr*D$margDLM)^2)),

# CI width
BART_CI_width0 = mean(est_bart[,4]-est_bart[,3]),
TDLM_CI_width0 = mean(est_tdlm[,4]-est_tdlm[,3]),
BART_CI_width1 = mean(est_bart[,7]-est_bart[,6]),
TDLM_CI_width1 = mean(est_tdlm[,7]-est_tdlm[,6]),
BART_CI_width10 = mean(est_bart[,10]-est_bart[,9]),
TDLM_CI_width10 = mean(est_tdlm[,10]-est_tdlm[,9]),

# coverage
BART_CI_coverage = mean((est_bart[,4]>0)*(est_bart[,3]<0)),
TDLM_CI_coverage = mean((est_tdlm[,4]>0)*(est_tdlm[,3]<0)),
BART_CI_coverage = mean((est_bart[,7]>iqr*D$margDLM)*(est_bart[,6]<iqr*D$margDLM)),
TDLM_CI_coverage = mean((est_tdlm[,7]>iqr*D$margDLM)*(est_tdlm[,6]<iqr*D$margDLM)),
BART_CI_coverage = mean((est_bart[,10]>iqr*D$margDLM)*(est_bart[,9]<iqr*D$margDLM)),
TDLM_CI_coverage = mean((est_tdlm[,10]>iqr*D$margDLM)*(est_tdlm[,9]<iqr*D$margDLM)),

# identify window
BART_pr_window_true = mean((est_bart[which(D$margDLM!=0),6]>0)),
TDLM_pr_window_true = mean((est_tdlm[which(D$margDLM!=0),6]>0)),


# probabilty of a difference in window
BART_pr_diff_true = mean(est_bart[which(D$margDLM!=0),11]),
TDLM_pr_diff_true = mean(est_tdlm[which(D$margDLM!=0),11]),


# probabilty of a difference in window
BART_CI_diff_true = mean(est_bart[which(D$margDLM!=0),9]>0),
TDLM_CI_diff_true = mean(est_tdlm[which(D$margDLM!=0),9]>0)


)
)


print(j)
print(round(colMeans(results),3))

}



write.csv(results,"output/HDLM_vs_BART_sim_full_results.csv")
write.csv(colMeans(results),"output/HDLM_vs_BART_sim_summary_results.csv")




# -------------------------------- #
# Visualize results
# -------------------------------- #

est_tdlm$model <- toupper(est_tdlm$model)
est_bart$model <- toupper(est_bart$model)

est_tdlm$time <- 1:nrow(est_tdlm)
est_bart$time <- 1:nrow(est_bart)

est_bart$model <- "BART"

plot_data <- NULL

for(i in 1:3){
  tmp_tdlm <- est_tdlm[,c(1,12,(2:4)+(i-1)*3)]
  tmp_tdlm$type <- c("Modifier = 0 (no effect)","Modifier = 1 (effect)","difference")[i]
  colnames(tmp_tdlm)[3:5] <- c("mean","lower","upper")
  if(i==1){
    tmp_tdlm$dlf <- 0
  }else{
    tmp_tdlm$dlf <- dlf
  }

  tmp_bart <- est_bart[,c(1,12,(2:4)+(i-1)*3)]
  tmp_bart$type <- c("Modifier = 0 (no effect)","Modifier = 1 (effect)","difference")[i]
  colnames(tmp_bart)[3:5] <- c("mean","lower","upper")
  if(i==1){
    tmp_bart$dlf <- 0
  }else{
    tmp_bart$dlf <- dlf
  }

  plot_data <-
  bind_rows(plot_data,
            tmp_tdlm,
            tmp_bart)
}






library(data.table)
setDT(plot_data)
plot_data$model <- factor(plot_data$model, labels = c("BART", "Nested Tree HDLM"))
#pdf("output/HDLM_vs_BART_figure.pdf", height=3, width=4)
ggplot(data=plot_data[type!="difference"]) +
  geom_hline(yintercept = 0, color = "red", size = 1) +
  geom_ribbon(aes(x=time, y=mean, ymin=lower, ymax=upper), fill="grey", color=NA) +
  geom_line(aes(x=time, y=mean), size = 2) +
  geom_line(aes(x=time, y=dlf), color="blue", linetype=2, size = 2) +
  facet_grid(model~type) +
  theme_bw(base_size = 24) +
  xlab("Week of gestation") +
  ylab("Exposure effect") +
  scale_color_manual(values=c("black","blue")) +
  scale_x_continuous(expand = c(0, 0)) +
  theme(legend.title=element_blank(),
        legend.position = "bottom", panel.grid = element_blank())


# dev.off()


