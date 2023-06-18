analyze.sim.dlmtree <- function(model, D, analyze.idx = 1:nrow(D$dat))
{
  mark <- ceiling(nrow(model$TreeStructs) / 42)
  cat(paste0("% complete\n[0--------25--------50--------75--------100]\n '"))
  
  draws <- lapply(1:model$mcmcIter, function(i) matrix(0.0, length(analyze.idx), model$pExp))
  # draws <- array(0.0, c(length(analyze.idx), model$pExp, model$mcmcIter))
  
  if (is.null(model$fixedIdx)) {
    mod <- model$Mo
    for (i in 1:nrow(model$TreeStructs)) {
      if (i %% mark == 0)
        cat("'")
      Iter <- model$TreeStructs$Iter[i]
      rule <- model$TreeStructs$Rule[i]
      if (rule == "") 
        idx <- 1:length(analyze.idx)
      else
        idx <- which(eval(parse(text = rule)))
      
      if (model$dlmType == "gp") {
        est <- matrix(rep(as.numeric(model$TreeStructs[i, -c(1:4)]), each = length(idx)), length(idx), model$pExp)
        draws[[Iter]][idx,] <- draws[[Iter]][idx,] + est
      } else {
        t <- model$TreeStructs$tmin[i]:model$TreeStructs$tmax[i]
        est <- model$TreeStructs$est[i]
        draws[[Iter]][idx, t] <- draws[[Iter]][idx, t] + est
      }
    }
    
  # Fixed tree
  } else {
    for (i in 1:nrow(model$TreeStructs)) {
      if (i %% mark == 0)
        cat("'")
      Iter <- model$TreeStructs$Iter[i]
      idx <- model$fixedIdx[[model$TreeStructs$fixedIdx[i] + 1]] + 1
      
      if (model$dlmType == "gp") {
        est <- matrix(rep(as.numeric(model$TreeStructs[i, -c(1:3)]), each = length(idx)), length(idx), model$pExp)
        draws[[Iter]][idx,] <- draws[[Iter]][idx,] + est
      } else {
        t <- model$TreeStructs$tmin[i]:model$TreeStructs$tmax[i]
        est <- model$TreeStructs$est[i]
        draws[[Iter]][idx, t] <- draws[[Iter]][idx, t] + est
      }
    }
  }
  
  draws <- array(do.call(c, draws), c(length(analyze.idx), model$pExp, model$mcmcIter))
  
  
  DLM <- sapply(1:model$pExp, function(t) {
    rowMeans(draws[,t,])
  })
  dlmLower <- sapply(1:model$pExp, function(t) {
    apply(draws[,t,], 1, quantile, probs = 0.025)
  })
  dlmUpper <- sapply(1:model$pExp, function(t) {
    apply(draws[,t,], 1, quantile, probs = 0.975)
  })
  cum <- sapply(1:length(analyze.idx), function(i) {
    mean(colSums(draws[i,,]))
  })
  cumLower <- sapply(1:length(analyze.idx), function(i) {
    quantile(colSums(draws[i,,]), probs = 0.025)
  })
  cumUpper <- sapply(1:length(analyze.idx), function(i) {
    quantile(colSums(draws[i,,]), probs = 0.975)
  })
  
  truth <- t(sapply(analyze.idx, function(i) D$dlmFun(D$dat[i,])))
  truth.cumulative <- rowSums(truth)
  idx.effect <- which(rowSums(truth) != 0)
  idx.noeffect <- setdiff(1:nrow(truth), idx.effect)
  
  out <- list()
  out$n.effect <- length(idx.effect)
  out$n.noeffect <- length(idx.noeffect)
  
  # DLM effect
  out$rmse.effect <- sqrt(mean(rowMeans((DLM[idx.effect,] - truth[idx.effect,])^2)))
  out$rmse.noeffect <- sqrt(mean(rowMeans((DLM[idx.noeffect,] - truth[idx.noeffect,])^2)))
  out$cov.effect <- mean(rowMeans(dlmLower[idx.effect,] < truth[idx.effect,] & 
                                    dlmUpper[idx.effect,] > truth[idx.effect,]))
  out$cov.noeffect <- mean(rowMeans(dlmLower[idx.noeffect,] < truth[idx.noeffect,] & 
                                    dlmUpper[idx.noeffect,] > truth[idx.noeffect,]))
  out$TP.effect <- mean(rowSums(dlmLower[idx.effect,] > 0 & truth[idx.effect,] > 0) /
                          rowSums(truth[idx.effect,] > 0))
  out$FP.effect <- mean(rowSums((dlmLower[idx.effect,] > 0 | dlmUpper[idx.effect,] < 0) & truth[idx.effect,] == 0) /
                          rowSums(truth[idx.effect,] == 0))
  out$FP.noeffect <- mean(rowSums((dlmLower[idx.noeffect,] > 0 | dlmUpper[idx.noeffect,] < 0) & truth[idx.noeffect,] == 0) /
                          rowSums(truth[idx.noeffect,] == 0))
  
  # DL function effect (cumulative effect)
  out$cum.rmse.effect <- sqrt(mean((cum[idx.effect] - truth.cumulative[idx.effect])^2))
  out$cum.rmse.noeffect <- sqrt(mean((cum[idx.noeffect] - truth.cumulative[idx.noeffect])^2))
  out$cum.cov.effect <- mean(cumLower[idx.effect] < truth.cumulative[idx.effect] &
                               cumUpper[idx.effect] > truth.cumulative[idx.effect])
  out$cum.cov.noeffect <- mean(cumLower[idx.noeffect] < truth.cumulative[idx.noeffect] &
                               cumUpper[idx.noeffect] > truth.cumulative[idx.noeffect])
  out$cum.TP <- mean(cumLower[idx.effect] > 0)
  out$cum.FP <- mean(cumLower[idx.noeffect] > 0 | cumUpper[idx.noeffect] < 0)
  
  # Modifier results
  if (!is.null(model$modCount)) {
    out$inc.mod_num <- mean(model$modCount[,"mod_num"] > 0)
    out$inc.mod_bin <- mean(model$modCount[,"mod_bin"] > 0)
    out$inc.mod_scale <- mean(model$modCount[,"mod_scale"] > 0)
    out$inc.other_num <- mean(model$modCount[,paste0("c", 1:5)] > 0)
    out$inc.other_bin <- mean(model$modCount[,paste0("b", 1:5)] > 0)
    out$inf.mod_num <- mean(model$modInf[,"mod_num"])
    out$inf.mod_bin <- mean(model$modInf[,"mod_bin"])
    out$inf.mod_scale <- mean(model$modInf[,"mod_scale"])
    out$inf.other_num <- mean(model$modInf[,paste0("c", 1:5)])
    out$inf.other_bin <- mean(model$modInf[,paste0("b", 1:5)])
    out$prob.mod_num <- mean(model$modProb[,"mod_num"])
    out$prob.mod_bin <- mean(model$modProb[,"mod_bin"])
    out$prob.mod_scale <- mean(model$modProb[,"mod_scale"])
    out$prob.other_num <- mean(model$modProb[,paste0("c", 1:5)])
    out$prob.other_bin <- mean(model$modProb[,paste0("b", 1:5)])
    
    # 2-way modifier interactions
    sp <- cbind.data.frame(Rule = model$termRules, model$TreeStructs[,2:4])
    sp <- sp[!duplicated(sp),]
    splitRules <- lapply(strsplit(sp$Rule, "&", T), function(i) {
      sort(as.numeric(sapply(strsplit(i, ">=|<|\\[\\]|\\]\\[", perl = T), function(j) j[1])))
    })
    splitCount <- lapply(1:model$mcmcIter, function(i) list())
    for (i in 1:length(splitRules)) {
      it <- sp$Iter[i]
      n <- length(splitRules[[i]])
      if (n == 1) {
      } else if (n > 1) {
        for (s in 1:(n-1)) {
          for (e in (s+1):n) {
            splitCount[[it]][[paste0(model$modNames[splitRules[[i]][s]+1], ".", 
                                     model$modNames[splitRules[[i]][e]+1])]] <- T
          }
        }
      }
    }
    sc <- do.call(dplyr::bind_rows, lapply(splitCount, cbind.data.frame))
    sc.mean <- sort(colMeans(!is.na(sc)))
    sc.mat <- data.frame()
    for (i in 1:length(sc.mean)) {
      names <- sort(strsplit(names(sc.mean)[i], ".", T)[[1]])
      sc.mat <- rbind.data.frame(sc.mat,
                                 data.frame("var1" = names[1], "var2" = names[2], "pip" = sc.mean[i]))
    }
    out$prob.mod_num.mod_bin <- sc.mat$pip[which(sc.mat$var1 == "mod_bin" & sc.mat$var2 == "mod_num")]
    out$prob.mod_num.mod_scale <- sc.mat$pip[which(sc.mat$var1 == "mod_num" & sc.mat$var2 == "mod_scale")]
    out$prob.mod_scale.mod_scale <- sc.mat$pip[which(sc.mat$var1 == "mod_scale" & sc.mat$var2 == "mod_scale")]
    out$prob.2mod.max <- max(sc.mat$pip[-which((sc.mat$var1 == "mod_num" & sc.mat$var2 == "mod_scale") |
                                                 (sc.mat$var1 == "mod_bin" & sc.mat$var2 == "mod_num") |
                                                 (sc.mat$var1 == "mod_scale" & sc.mat$var2 == "mod_scale"))])
    out$prob.2mod.mean <- mean(sc.mat$pip[-which((sc.mat$var1 == "mod_num" & sc.mat$var2 == "mod_scale") |
                                                 (sc.mat$var1 == "mod_bin" & sc.mat$var2 == "mod_num") |
                                                   (sc.mat$var1 == "mod_scale" & sc.mat$var2 == "mod_scale"))])
    
    
    
  } else {
    out$inc.mod_num <- 0
    out$inc.mod_bin <- 0
    out$inc.mod_scale <- 0
    out$inc.other_num <- 0
    out$inc.other_bin <- 0
    out$inf.mod_num <- 0
    out$inf.mod_bin <- 0
    out$inf.mod_scale <- 0
    out$inf.other_num <- 0
    out$inf.other_bin <- 0
    out$prob.mod_num <- 0
    out$prob.mod_bin <- 0
    out$prob.mod_scale <- 0
    out$prob.other_num <- 0
    out$prob.other_bin <- 0
    out$prob.mod_num.mod_bin <- 0
    out$prob.mod_num.mod_scale <- 0
    out$prob.mod_scale.mod_scale <- 0
    out$prob.2mod.max <- 0
    out$prob.2mod.mean <- 0
  }
  
  return(out)
}

