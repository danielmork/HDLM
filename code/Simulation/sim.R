# ---- Setup simulation ----
library(dlmtree)


# Setup simulation
# dir <- "/projects/dmork@colostate.edu/co_dlmtree/sim.dlmtree/"
dir <- "sim.res/"
source(file = "analyze.sim.dlmtree.R")
source(file = "sim.dlmtree.R")
file.path <- paste0(dir, "sim_res_waic.txt")
sim <- c("A", "B", "C")
n <- 5000
num.burn <- 5000
num.iter <- 10000
num.trees <- 20
num.thin <- 5

cat("\nSimulation:", sim.num, ", Effect:", sim[s], 
    ", Error:", error, ", Model:", model, "\n")

# Load exposure data
# load("/projects/dmork@colostate.edu/co_dlmtree/expList.rda")
load("expList.rda")
Lags <- ncol(expList[[1]])
exposure.data <- (expList[[1]] - mean(expList[[1]])) / sd(expList[[1]])
rm(expList)

set.seed(s * 1000 + sim.num)
D <- sim.dlmtree(sim[s], error, n * 2, exposure.data)
Doos <- D
Doos$dat <- D$dat[n + 1:n,]
Doos$exposure.dat <- D$exposure.dat[n + 1:n, ]
Doos$f <- D$f[n + 1:n]
Doos$c <- D$c[n + 1:n,]
Doos$fixedIdx <- lapply(D$fixedIdx, function(x) x[x > n] - n)
D$dat <- D$dat[1:n,]
D$exposure.dat <- D$exposure.dat[1:n, ]
D$f <- D$f[1:n]
D$c <- D$c[1:n,]
D$fixedIdx <- lapply(D$fixedIdx, function(x) x[x <= n])
  

out <- list()



# ---- dlmtree: TDLMnest ----
if (model == 1) {
t <- system.time({
m <- dlmtree(formula = y ~ ., data = D$dat, exposure.data = D$exposure.dat,
             tree.modifiers = "all", modifier.splits = 20, dlm.type = "tdlm2",
             n.trees = num.trees, n.burn = num.burn, n.iter = num.iter, n.thin = num.thin)
})[3]
res <- list(c("sim" = s, "sim.num" = sim.num, "error" = error, "n" = n, "time" = t,
              "model" = "dlmtree:TDLMnested",
              "mod.nodes" = mean(m$termNodesMod), 
              analyze.sim.dlmtree(m, D)))
pred <- predict(m, Doos$dat, Doos$exposure.dat, verbose = F)
res[[1]]$oos.f.mspe <- mean((Doos$f - pred$fhat)^2)
res[[1]]$oos.f.cov <- mean((pred$fhat.lims[1,] < Doos$f) & 
                        (pred$fhat.lims[2,] > Doos$f))
res[[1]]$oos.y.mspe <- mean((Doos$dat$y - pred$y)^2)
res[[1]]$oos.y.cov <- mean((pred$y.lims[1,] < Doos$dat$y) &
                             (pred$y.lims[2,] > Doos$dat$y))
res[[1]]$waic <- predict(m, D$dat, D$exposure.dat, 
                         type = "waic", outcome = D$dat$y)$waic
res
out <- c(out, res)

if (!file.exists(file.path))
  write(paste(names(out[[1]]), collapse = ", "), file.path)



} 




# ---- dlmtree: TDLMpair ----
if (model == 2) {
t <- system.time({
m <- dlmtree(formula = y ~ ., data = D$dat, exposure.data = D$exposure.dat,
             tree.modifiers = "all", modifier.splits = 20, dlm.type = "tdlm",
             n.trees = num.trees, n.burn = num.burn, n.iter = num.iter, n.thin = num.thin)
})[3]
res <- list(c("sim" = s, "sim.num" = sim.num, "error" = error, "n" = n, "time" = t,
              "model" = "dlmtree:TDLMpairs",
              "mod.nodes" = mean(m$termNodesMod), analyze.sim.dlmtree(m, D)))
pred <- predict(m, Doos$dat, Doos$exposure.dat, verbose = F)
res[[1]]$oos.f.mspe <- mean((Doos$f - pred$fhat)^2)
res[[1]]$oos.f.cov <- mean((pred$fhat.lims[1,] < Doos$f) & 
                             (pred$fhat.lims[2,] > Doos$f))
res[[1]]$oos.y.mspe <- mean((Doos$dat$y - pred$y)^2)
res[[1]]$oos.y.cov <- mean((pred$y.lims[1,] < Doos$dat$y) &
                             (pred$y.lims[2,] > Doos$dat$y))
res[[1]]$waic <- predict(m, D$dat, D$exposure.dat, 
                         type = "waic", outcome = D$dat$y)$waic
res
out <- c(out, res)


} 




# ---- dlmtree: GP ----
if (model == 3) {
t <- system.time({
m <- dlmtree(formula = y ~ ., data = D$dat, exposure.data = D$exposure.dat,
             tree.modifiers = "all", modifier.splits = 20, dlm.type = "gp",
             n.trees = num.trees, n.burn = num.burn, n.iter = num.iter, n.thin = num.thin)
})[3]
res <- list(c("sim" = s, "sim.num" = sim.num, "error" = error, "n" = n, "time" = t,
              "model" = "dlmtree:GP",
              "mod.nodes" = mean(m$termNodesMod), analyze.sim.dlmtree(m, D)))
pred <- predict(m, Doos$dat, Doos$exposure.dat, verbose = F)
res[[1]]$oos.f.mspe <- mean((Doos$f - pred$fhat)^2)
res[[1]]$oos.f.cov <- mean((pred$fhat.lims[1,] < Doos$f) & 
                             (pred$fhat.lims[2,] > Doos$f))
res[[1]]$oos.y.mspe <- mean((Doos$dat$y - pred$y)^2)
res[[1]]$oos.y.cov <- mean((pred$y.lims[1,] < Doos$dat$y) &
                             (pred$y.lims[2,] > Doos$dat$y))
res[[1]]$waic <- predict(m, D$dat, D$exposure.dat, 
                         type = "waic", outcome = D$dat$y)$waic
res
out <- c(out, res)


} 





# ---- dlmtree: Fixed TDLM ----
if (s < 3 && model == 4) {
t <- system.time({
m <- dlmtree(formula = y ~ ., data = D$dat, exposure.data = D$exposure.dat,
             tree.modifiers = "all", modifier.splits = 20, dlm.type = "tdlm2",
             fixed.tree.idx = D$fixedIdx,
             n.trees = num.trees, n.burn = num.burn, n.iter = num.iter, n.thin = num.thin)
})[3]
res <- list(c("sim" = s, "sim.num" = sim.num, "error" = error, "n" = n, "time" = t,
              "model" = "dlmtree:TruthTDLM",
              "mod.nodes" = 0, analyze.sim.dlmtree(m, D)))
pred <- predict(m, Doos$dat, Doos$exposure.dat, verbose = F,
                fixed.idx = Doos$fixedIdx)
res[[1]]$oos.f.mspe <- mean((Doos$f - pred$fhat)^2)
res[[1]]$oos.f.cov <- mean((pred$fhat.lims[1,] < Doos$f) & 
                             (pred$fhat.lims[2,] > Doos$f))
res[[1]]$oos.y.mspe <- Inf#mean((Doos$dat$y - pred$y)^2)
res[[1]]$oos.y.cov <- mean((pred$y.lims[1,] < Doos$dat$y) &
                             (pred$y.lims[2,] > Doos$dat$y))
res[[1]]$waic <- Inf
res
out <- c(out, res)


} 




# ---- dlmtree: Fixed GP ----
if (s < 3 && model == 5) {
t <- system.time({
  m <- dlmtree(formula = y ~ ., data = D$dat, exposure.data = D$exposure.dat,
               tree.modifiers = "all", modifier.splits = 20, dlm.type = "gp",
               fixed.tree.idx = D$fixedIdx,
               n.trees = num.trees, n.burn = num.burn, n.iter = num.iter, n.thin = num.thin)
})[3]
res <- list(c("sim" = s, "sim.num" = sim.num, "error" = error, "n" = n, "time" = t,
              "model" = "dlmtree:TruthGP",
              "mod.nodes" = 0, analyze.sim.dlmtree(m, D)))
pred <- predict(m, Doos$dat, Doos$exposure.dat, verbose = F,
                fixed.idx = Doos$fixedIdx)
res[[1]]$oos.f.mspe <- mean((Doos$f - pred$fhat)^2)
res[[1]]$oos.f.cov <- mean((pred$fhat.lims[1,] < Doos$f) & 
                             (pred$fhat.lims[2,] > Doos$f))
res[[1]]$oos.y.mspe <- Inf#mean((Doos$dat$y - pred$y)^2)
res[[1]]$oos.y.cov <- mean((pred$y.lims[1,] < Doos$dat$y) &
                             (pred$y.lims[2,] > Doos$dat$y))
res[[1]]$waic <- Inf
res
out <- c(out, res)
}

  
  



# ---- dlmtree: Single TDLM ----
if (model == 6) {
t <- system.time({
  m <- dlmtree(formula = y ~ ., data = D$dat, exposure.data = D$exposure.dat,
               tree.modifiers = "all", modifier.splits = 20, dlm.type = "tdlm2",
               fixed.tree.idx = list(1:n),
               n.trees = num.trees, n.burn = num.burn, n.iter = num.iter, n.thin = num.thin)
})[3]
res <- list(c("sim" = s, "sim.num" = sim.num, "error" = error, "n" = n, "time" = t,
              "model" = "dlmtree:SingleTDLM",
              "mod.nodes" = 0, analyze.sim.dlmtree(m, D)))
pred <- predict(m, Doos$dat, Doos$exposure.dat, verbose = F,
                fixed.idx = Doos$fixedIdx)
res[[1]]$oos.f.mspe <- mean((Doos$f - pred$fhat)^2)
res[[1]]$oos.f.cov <- mean((pred$fhat.lims[1,] < Doos$f) & 
                             (pred$fhat.lims[2,] > Doos$f))
res[[1]]$oos.y.mspe <- mean((Doos$dat$y - pred$y)^2)
res[[1]]$oos.y.cov <- mean((pred$y.lims[1,] < Doos$dat$y) &
                             (pred$y.lims[2,] > Doos$dat$y))
res[[1]]$waic <- predict(m, D$dat, D$exposure.dat, 
                         type = "waic", outcome = D$dat$y,
                         fixed.idx = list(1:n))$waic
res
out <- c(out, res)




} 





# ---- dlmtree: Single GP ----
if (model == 7) {
t <- system.time({
  m <- dlmtree(formula = y ~ ., data = D$dat, exposure.data = D$exposure.dat,
               tree.modifiers = "all", modifier.splits = 20, dlm.type = "gp",
               fixed.tree.idx = list(1:n),
               n.trees = num.trees, n.burn = num.burn, n.iter = num.iter, n.thin = num.thin)
})[3]
res <- list(c("sim" = s, "sim.num" = sim.num, "error" = error, "n" = n, "time" = t,
              "model" = "dlmtree:SingleGP",
              "mod.nodes" = 0, analyze.sim.dlmtree(m, D)))
pred <- predict(m, Doos$dat, Doos$exposure.dat, verbose = F,
                fixed.idx = Doos$fixedIdx)
res[[1]]$oos.f.mspe <- mean((Doos$f - pred$fhat)^2)
res[[1]]$oos.f.cov <- mean((pred$fhat.lims[1,] < Doos$f) & 
                             (pred$fhat.lims[2,] > Doos$f))
res[[1]]$oos.y.mspe <- mean((Doos$dat$y - pred$y)^2)
res[[1]]$oos.y.cov <- mean((pred$y.lims[1,] < Doos$dat$y) &
                             (pred$y.lims[2,] > Doos$dat$y))
res[[1]]$waic <- predict(m, D$dat, D$exposure.dat, 
                         type = "waic", outcome = D$dat$y,
                         fixed.idx = list(1:n))$waic
res
out <- c(out, res)
}






# ---- Write results to file ----
if (length(out) > 0) {
  f <- file(file.path, open = "a")
  for (i in 1:length(out))
    writeLines(paste(out[[i]], collapse = ", "), f)
  close(f)
}







