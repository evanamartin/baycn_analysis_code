############ load baycn posterior matrices into lists ############
bay_nc11_100_02_pm <- NULL
bay_nc11_100_05_pm <- NULL
bay_nc11_100_1_pm <- NULL

for (i in 1:M) {
  # load result
  filename <- paste("../baycn.nc11/bay_ge_N_100_nc11_run_", i, ".RData", sep = "")
  load (filename)    
  # add result to list
  bay_nc11_100_02_pm[[i]] <- bay_nc11_100_02@posteriorPM
  bay_nc11_100_05_pm[[i]] <- bay_nc11_100_05@posteriorPM
  bay_nc11_100_1_pm[[i]] <- bay_nc11_100_1@posteriorPM
}

bay_nc11_200_02_pm <- NULL
bay_nc11_200_05_pm <- NULL
bay_nc11_200_1_pm <- NULL

for (i in 1:M) {
  # load result
  filename <- paste("../baycn.nc11/bay_ge_N_200_nc11_run_", i, ".RData", sep = "")
  load (filename)    
  # add result to list
  bay_nc11_200_02_pm[[i]] <- bay_nc11_200_02@posteriorPM
  bay_nc11_200_05_pm[[i]] <- bay_nc11_200_05@posteriorPM
  bay_nc11_200_1_pm[[i]] <- bay_nc11_200_1@posteriorPM
}

bay_nc11_600_02_pm <- NULL
bay_nc11_600_05_pm <- NULL
bay_nc11_600_1_pm <- NULL

for (i in 1:M) {
  # load result
  filename <- paste("../baycn.nc11/bay_ge_N_600_nc11_run_", i, ".RData", sep = "")
  load (filename)    
  # add result to list
  bay_nc11_600_02_pm[[i]] <- bay_nc11_600_02@posteriorPM
  bay_nc11_600_05_pm[[i]] <- bay_nc11_600_05@posteriorPM
  bay_nc11_600_1_pm[[i]] <- bay_nc11_600_1@posteriorPM
}


############ calculate MSE2 ############
# expected probability matrices ------------------------------------------------

# The expected probabilities for topology NC11.
ep_nc11 <- matrix(c(0, 0.2, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0.8, 0, 0.4, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0.6, 0, 0.6, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0.4, 0, 0.8, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0.2, 0, 1, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 1, 0, 0.2, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0.8, 0, 0.4, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0.6, 0, 0.6, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0.4, 0, 0.8,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0.2, 0),
                  byrow = TRUE,
                  nrow = 11)


  # The MSE is calculated by subtracting the estimated edge probabilities (not
  # including the probability of the edge being absent) from the expected edge
  # probabilities.

# MSE2 calculates the mean squared difference between the estimated 
# and expected probabilistic adjacency matrix over all possible edges.
# For example, G2 has four nodes and four edges, but MSE2 is averaged 
# over 12, which is the total number of possible directed edges and 
# also the total number of off-diagonal elements.

bay_mse_nc11 <- matrix(0, nrow = M, ncol = 9)
colnames(bay_mse_nc11) <- c("MSE2_100_02", "MSE2_100_05", "MSE2_100_1", "MSE2_200_02", "MSE2_200_05", "MSE2_200_1", "MSE2_600_02", "MSE2_600_05", "MSE2_600_1")

nedges.nc11 <- 110

calcMSE2 <- function(pm1, pm2, nedges){
  return(sum((pm1 - pm2)^2) / nedges)
}
bay_mse_nc11[,1] <- unlist (lapply(bay_nc11_100_02_pm, calcMSE2, pm2=ep_nc11, nedges=nedges.nc11))
bay_mse_nc11[,2] <- unlist (lapply(bay_nc11_100_05_pm, calcMSE2, pm2=ep_nc11, nedges=nedges.nc11))
bay_mse_nc11[,3] <- unlist (lapply(bay_nc11_100_1_pm, calcMSE2, pm2=ep_nc11, nedges=nedges.nc11))

bay_mse_nc11[,4] <- unlist (lapply(bay_nc11_200_02_pm, calcMSE2, pm2=ep_nc11, nedges=nedges.nc11))
bay_mse_nc11[,5] <- unlist (lapply(bay_nc11_200_05_pm, calcMSE2, pm2=ep_nc11, nedges=nedges.nc11))
bay_mse_nc11[,6] <- unlist (lapply(bay_nc11_200_1_pm, calcMSE2, pm2=ep_nc11, nedges=nedges.nc11))

bay_mse_nc11[,7] <- unlist (lapply(bay_nc11_600_02_pm, calcMSE2, pm2=ep_nc11, nedges=nedges.nc11))
bay_mse_nc11[,8] <- unlist (lapply(bay_nc11_600_05_pm, calcMSE2, pm2=ep_nc11, nedges=nedges.nc11))
bay_mse_nc11[,9] <- unlist (lapply(bay_nc11_600_1_pm, calcMSE2, pm2=ep_nc11, nedges=nedges.nc11))

bay_mse_nc11_mean <- apply (bay_mse_nc11, 2, mean)
bay_mse_nc11_sd <- apply (bay_mse_nc11, 2, sd)


############ calculate precision and recall ############
# The true matrix for topology NC11.
true_nc11 <- matrix(c(0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0),
                    byrow = TRUE,
                    nrow = 11)

# Make sure the true matrix is symmetric
true_nc11 <- ifelse (true_nc11 + t(true_nc11), 1, 0)

bay_precision_nc11 <- matrix(0, nrow = M, ncol = 9)
colnames(bay_precision_nc11) <- c("Precision_100_02", "Precision_100_05", "Precision_100_1", "Precision_200_02", "Precision_200_05", "Precision_200_1", "Precision_600_02", "Precision_600_05", "Precision_600_1")

bay_recall_nc11 <- matrix(0, nrow = M, ncol = 9)
colnames(bay_recall_nc11) <- c("Recall_100_02", "Recall_100_05", "Recall_100_1", "Recall_200_02", "Recall_200_05", "Recall_200_1", "Recall_600_02", "Recall_600_05", "Recall_600_1")

# precision
bay_precision_nc11[,1] <- unlist (lapply(bay_nc11_100_02_pm, prerec, amTrue = true_nc11, cutoff = 0.5))[2*(1:M)-1]
bay_precision_nc11[,2] <- unlist (lapply(bay_nc11_100_05_pm, prerec, amTrue = true_nc11, cutoff = 0.5))[2*(1:M)-1]
bay_precision_nc11[,3] <- unlist (lapply(bay_nc11_100_1_pm, prerec, amTrue = true_nc11, cutoff = 0.5))[2*(1:M)-1]

bay_precision_nc11[,4] <- unlist (lapply(bay_nc11_200_02_pm, prerec, amTrue = true_nc11, cutoff = 0.5))[2*(1:M)-1]
bay_precision_nc11[,5] <- unlist (lapply(bay_nc11_200_05_pm, prerec, amTrue = true_nc11, cutoff = 0.5))[2*(1:M)-1]
bay_precision_nc11[,6] <- unlist (lapply(bay_nc11_200_1_pm, prerec, amTrue = true_nc11, cutoff = 0.5))[2*(1:M)-1]

bay_precision_nc11[,7] <- unlist (lapply(bay_nc11_600_02_pm, prerec, amTrue = true_nc11, cutoff = 0.5))[2*(1:M)-1]
bay_precision_nc11[,8] <- unlist (lapply(bay_nc11_600_05_pm, prerec, amTrue = true_nc11, cutoff = 0.5))[2*(1:M)-1]
bay_precision_nc11[,9] <- unlist (lapply(bay_nc11_600_1_pm, prerec, amTrue = true_nc11, cutoff = 0.5))[2*(1:M)-1]

# recall
bay_recall_nc11[,1] <- unlist (lapply(bay_nc11_100_02_pm, prerec, amTrue = true_nc11, cutoff = 0.5))[2*(1:M)]
bay_recall_nc11[,2] <- unlist (lapply(bay_nc11_100_05_pm, prerec, amTrue = true_nc11, cutoff = 0.5))[2*(1:M)]
bay_recall_nc11[,3] <- unlist (lapply(bay_nc11_100_1_pm, prerec, amTrue = true_nc11, cutoff = 0.5))[2*(1:M)]

bay_recall_nc11[,4] <- unlist (lapply(bay_nc11_200_02_pm, prerec, amTrue = true_nc11, cutoff = 0.5))[2*(1:M)]
bay_recall_nc11[,5] <- unlist (lapply(bay_nc11_200_05_pm, prerec, amTrue = true_nc11, cutoff = 0.5))[2*(1:M)]
bay_recall_nc11[,6] <- unlist (lapply(bay_nc11_200_1_pm, prerec, amTrue = true_nc11, cutoff = 0.5))[2*(1:M)]

bay_recall_nc11[,7] <- unlist (lapply(bay_nc11_600_02_pm, prerec, amTrue = true_nc11, cutoff = 0.5))[2*(1:M)]
bay_recall_nc11[,8] <- unlist (lapply(bay_nc11_600_05_pm, prerec, amTrue = true_nc11, cutoff = 0.5))[2*(1:M)]
bay_recall_nc11[,9] <- unlist (lapply(bay_nc11_600_1_pm, prerec, amTrue = true_nc11, cutoff = 0.5))[2*(1:M)]

############ add results to exisitng table ############
nc11_all <- read.delim("../grid_simulation_analysis/nc11_prerec_mse2_all_methods_table.tsv", header = TRUE, sep = "\t")

bay_merged <- data.frame(Method="baycn", 
                         N=rep (c(100, 200, 600), each=3), 
                         B=rep(c(0.2, 0.5, 1), 3), 
                         Precision.Mean=round(apply(bay_precision_nc11, 2, mean), digits = 2), 
                         Precision.SD=round(apply(bay_precision_nc11, 2, sd), digits = 2),
                         Recall.Mean=round(apply(bay_recall_nc11, 2, mean), digits = 2), 
                         Recall.SD=round(apply(bay_recall_nc11, 2, sd), digits = 2),
                         MSE.Mean=round(apply(bay_mse_nc11, 2, mean), digits = 2), 
                         MSE.SD=round(apply(bay_mse_nc11, 2, sd), digits = 2)
                         )

nc11_all_merged <- rbind (bay_merged, nc11_all)
write.table(nc11_all_merged, "nc11_prerec_mse2_all_methods_table_merged.tsv", col.names = TRUE, row.names = FALSE, sep = "\t", quote = FALSE)


############ plots for method comparison ############
nc11_all_merged <- read.delim("nc11_prerec_mse2_all_methods_table_merged.tsv", header = TRUE, row.names = FALSE, sep = "\t")

methods <- unique (nc11_all_merged$Method)
# "baycn", "mc3", "order", "partition", "bcdag", "scanBMA"
mycolors <- c("black", "orange", "green", "magenta", "blue", "pink")

# recall vs precision
plot (nc11_all_merged$Precision.Mean, nc11_all_merged$Recall.Mean, pch=16, cex=0.8, xlab="Precision", ylab="Recall")
abline (a = 0, b = 1)
for (i in 1:length (methods)){
  points (nc11_all_merged$Precision.Mean[which (nc11_all_merged$Method==methods[i])], nc11_all_merged$Recall.Mean[which (nc11_all_merged$Method==methods[i])], col=mycolors[i], pch=16, cex=0.8)
}

# MSE2 vs precision
plot (nc11_all_merged$Precision.Mean, nc11_all_merged$MSE.Mean, pch=16, cex=0.8, xlab="Precision", ylab="MSE2")
for (i in 1:length (methods)){
  points (nc11_all_merged$Precision.Mean[which (nc11_all_merged$Method==methods[i])], nc11_all_merged$MSE.Mean[which (nc11_all_merged$Method==methods[i])], col=mycolors[i], pch=16, cex=0.8)
}

# MSE2 vs recall
plot (nc11_all_merged$Recall.Mean, nc11_all_merged$MSE.Mean, pch=16, cex=0.8, xlab="Recall", ylab="MSE2")
for (i in 1:length (methods)){
  points (nc11_all_merged$Recall.Mean[which (nc11_all_merged$Method==methods[i])], nc11_all_merged$MSE.Mean[which (nc11_all_merged$Method==methods[i])], col=mycolors[i], pch=16, cex=0.8)
}



