# load the output from the four methods we compare with ------------------------

load("/Users/Evatar/Sync/Evan/classesUofI/BCB600/packages/baycn/simulations/bidag/ord_ge_N_200_pm.RData")
load("/Users/Evatar/Sync/Evan/classesUofI/BCB600/packages/baycn/simulations/bidag/par_ge_N_200_pm.RData")


# Adjacency matrices -----------------------------------------------------------

# Adjacency matrix with the true edges for toplogy G2.
am_g2_te <- matrix(c(0, 1, 1, 0,
                     1, 0, 0, 1,
                     1, 0, 0, 1,
                     0, 1, 1, 0),
                   byrow = TRUE,
                   nrow = 4)

# Adjacency matrix with the true edges for toplogy NC11.
am_nc11_te <- matrix(c(0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                       1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,
                       0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0,
                       0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0,
                       0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0,
                       0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0,
                       0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0,
                       0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0,
                       0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0,
                       0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1,
                       0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0),
                     byrow = TRUE,
                     nrow = 11)

# Undirected adjacency matrix with the true edges for the PC topology.
am_pc_te <- matrix(c(0, 1, 0, 0, 0, 1, 0, 1,
                     1, 0, 1, 0, 1, 0, 0, 0,
                     0, 1, 0, 0, 0, 0, 0, 0,
                     0, 0, 0, 0, 0, 0, 0, 0,
                     0, 1, 0, 0, 0, 1, 0, 1,
                     1, 0, 0, 0, 1, 0, 1, 0,
                     0, 0, 0, 0, 0, 1, 0, 0,
                     1, 0, 0, 0, 1, 0, 0, 0),
                   byrow = TRUE,
                   nrow = 8)

# expected probability matrices ------------------------------------------------

# The expected probabilities for topology G2.
ep_g2 <- matrix(c(0, 1/3, 1, 0,
                  2/3, 0, 0, 2/3,
                  0, 0, 0, 0,
                  0, 1/3, 1, 0),
                byrow = TRUE,
                nrow = 4)

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

# The expected probabilities for topology PC.
ep_pc <- matrix(c(0, 0.25, 0, 0, 0, 1, 0, 1,
                  0.75, 0, 0.75, 0, 0.75, 0, 0, 0,
                  0, 0.25, 0, 0, 0, 0, 0, 0,
                  0, 0, 0, 0, 0, 0, 0, 0,
                  0, 0.25, 0, 0, 0, 1, 0, 1,
                  0, 0, 0, 0, 0, 0, 1, 0,
                  0, 0, 0, 0, 0, 0, 0, 0,
                  0, 0, 0, 0, 0, 0, 0, 0),
                byrow = TRUE,
                nrow = 8)



# Calculate the mse for order MCMC ---------------------------------------------

# Topology G2 fc -----------------------

ord_mse_g2_200_02_fc <- vector(length = M)

for (e in 1:M) {

  # The MSE is calculated by subtracting the estimated edge probabilities (not
  # including the probability of the edge being absent) from the expected edge
  # probabilities.
  ord_mse_g2_200_02_fc[[e]] <- sum((ep_g2 - ord_g2_200_02_fc_pm[[e]])^2) / 12

}

ord_mse_g2_200_05_fc <- vector(length = M)

for (e in 1:M) {

  # The MSE is calculated by subtracting the estimated edge probabilities (not
  # including the probability of the edge being absent) from the expected edge
  # probabilities.
  ord_mse_g2_200_05_fc[[e]] <- sum((ep_g2 - ord_g2_200_05_fc_pm[[e]])^2) / 12

}

ord_mse_g2_200_1_fc <- vector(length = M)

for (e in 1:M) {

  # The MSE is calculated by subtracting the estimated edge probabilities (not
  # including the probability of the edge being absent) from the expected edge
  # probabilities.
  ord_mse_g2_200_1_fc[[e]] <- sum((ep_g2 - ord_g2_200_1_fc_pm[[e]])^2) / 12

}

#################################################
# orderMCMC
# G2
# 100
# 0.2
#################################################

ord_mse_g2_200_02 <- vector(length = M)

for (e in 1:M) {

  # Set the edge probabilities for the edges that are not the true edges to 0.
  # We do this step because BiDAG considers edges outside of the restricted
  # search space. To make the comparison to baycn more fair we will not consider
  # these probabilities when calculating the MSE.
  ord_g2_200_02_pm[[e]][am_g2_te == 0] <- 0

  # The MSE is calculated by subtracting the estimated edge probabilities (not
  # including the probability of the edge being absent) from the expected edge
  # probabilities.
  ord_mse_g2_200_02[[e]] <- sum((ep_g2 - ord_g2_200_02_pm[[e]])^2) / 8

}

#################################################
# orderMCMC
# G2
# 100
# 0.5
#################################################

ord_mse_g2_200_05 <- vector(length = M)

for (e in 1:M) {

  # Set the edge probabilities for the edges that are not the true edges to 0.
  # We do this step because BiDAG considers edges outside of the restricted
  # search space. To make the comparison to baycn more fair we will not consider
  # these probabilities when calculating the MSE.
  ord_g2_200_05_pm[[e]][am_g2_te == 0] <- 0

  # The MSE is calculated by subtracting the estimated edge probabilities (not
  # including the probability of the edge being absent) from the expected edge
  # probabilities.
  ord_mse_g2_200_05[[e]] <- sum((ep_g2 - ord_g2_200_05_pm[[e]])^2) / 8

}

#################################################
# orderMCMC
# G2
# 100
# 1
#################################################

ord_mse_g2_200_1 <- vector(length = M)

for (e in 1:M) {

  # Set the edge probabilities for the edges that are not the true edges to 0.
  # We do this step because BiDAG considers edges outside of the restricted
  # search space. To make the comparison to baycn more fair we will not consider
  # these probabilities when calculating the MSE.
  ord_g2_200_1_pm[[e]][am_g2_te == 0] <- 0

  # The MSE is calculated by subtracting the estimated edge probabilities (not
  # including the probability of the edge being absent) from the expected edge
  # probabilities.
  ord_mse_g2_200_1[[e]] <- sum((ep_g2 - ord_g2_200_1_pm[[e]])^2) / 8

}

################################################################################
# Topology NC11
################################################################################

#################################################
# orderMCMC
# NC11
# 100
# 0.2
#################################################

ord_mse_nc11_200_02 <- vector(length = M)

for (e in 1:M) {

  # Set the edge probabilities for the edges that are not the true edges to 0.
  # We do this step because BiDAG considers edges outside of the restricted
  # search space. To make the comparison to baycn more fair we will not consider
  # these probabilities when calculating the MSE.
  ord_nc11_200_02_pm[[e]][am_nc11_te == 0] <- 0

  # The MSE is calculated by subtracting the estimated edge probabilities (not
  # including the probability of the edge being absent) from the expected edge
  # probabilities.
  ord_mse_nc11_200_02[[e]] <- sum((ep_nc11 - ord_nc11_200_02_pm[[e]])^2) / 20

}

#################################################
# orderMCMC
# NC11
# 100
# 0.5
#################################################

ord_mse_nc11_200_05 <- vector(length = M)

for (e in 1:M) {

  # Set the edge probabilities for the edges that are not the true edges to 0.
  # We do this step because BiDAG considers edges outside of the restricted
  # search space. To make the comparison to baycn more fair we will not consider
  # these probabilities when calculating the MSE.
  ord_nc11_200_05_pm[[e]][am_nc11_te == 0] <- 0

  # The MSE is calculated by subtracting the estimated edge probabilities (not
  # including the probability of the edge being absent) from the expected edge
  # probabilities.
  ord_mse_nc11_200_05[[e]] <- sum((ep_nc11 - ord_nc11_200_05_pm[[e]])^2) / 20

}

#################################################
# orderMCMC
# NC11
# 100
# 1
#################################################

ord_mse_nc11_200_1 <- vector(length = M)

for (e in 1:M) {

  # Set the edge probabilities for the edges that are not the true edges to 0.
  # We do this step because BiDAG considers edges outside of the restricted
  # search space. To make the comparison to baycn more fair we will not consider
  # these probabilities when calculating the MSE.
  ord_nc11_200_1_pm[[e]][am_nc11_te == 0] <- 0

  # The MSE is calculated by subtracting the estimated edge probabilities (not
  # including the probability of the edge being absent) from the expected edge
  # probabilities.
  ord_mse_nc11_200_1[[e]] <- sum((ep_nc11 - ord_nc11_200_1_pm[[e]])^2) / 20

}

################################################################################
# Topology PC
################################################################################

#################################################
# orderMCMC
# PC
# 100
# 0.2
#################################################

ord_mse_pc_200_02 <- vector(length = M)

for (e in 1:M) {

  # Set the edge probabilities for the edges that are not the true edges to 0.
  # We do this step because BiDAG considers edges outside of the restricted
  # search space. To make the comparison to baycn more fair we will not consider
  # these probabilities when calculating the MSE.
  ord_pc_200_02_pm[[e]][am_pc_te == 0] <- 0

  # The MSE is calculated by subtracting the estimated edge probabilities (not
  # including the probability of the edge being absent) from the expected edge
  # probabilities.
  ord_mse_pc_200_02[[e]] <- sum((ep_pc - ord_pc_200_02_pm[[e]])^2) / 16

}

#################################################
# orderMCMC
# PC
# 100
# 0.5
#################################################

ord_mse_pc_200_05 <- vector(length = M)

for (e in 1:M) {

  # Set the edge probabilities for the edges that are not the true edges to 0.
  # We do this step because BiDAG considers edges outside of the restricted
  # search space. To make the comparison to baycn more fair we will not consider
  # these probabilities when calculating the MSE.
  ord_pc_200_05_pm[[e]][am_pc_te == 0] <- 0

  # The MSE is calculated by subtracting the estimated edge probabilities (not
  # including the probability of the edge being absent) from the expected edge
  # probabilities.
  ord_mse_pc_200_05[[e]] <- sum((ep_pc - ord_pc_200_05_pm[[e]])^2) / 16

}

#################################################
# orderMCMC
# PC
# 100
# 1
#################################################

ord_mse_pc_200_1 <- vector(length = M)

for (e in 1:M) {

  # Set the edge probabilities for the edges that are not the true edges to 0.
  # We do this step because BiDAG considers edges outside of the restricted
  # search space. To make the comparison to baycn more fair we will not consider
  # these probabilities when calculating the MSE.
  ord_pc_200_1_pm[[e]][am_pc_te == 0] <- 0

  # The MSE is calculated by subtracting the estimated edge probabilities (not
  # including the probability of the edge being absent) from the expected edge
  # probabilities.
  ord_mse_pc_200_1[[e]] <- sum((ep_pc - ord_pc_200_1_pm[[e]])^2) / 16

}

# Calculate the mse for partition MCMC -----------------------------------------

# Topology G2 fc -----------------------

par_mse_g2_200_02_fc <- vector(length = M)

for (e in 1:M) {

  # The MSE is calculated by subtracting the estimated edge probabilities (not
  # including the probability of the edge being absent) from the expected edge
  # probabilities.
  par_mse_g2_200_02_fc[[e]] <- sum((ep_g2 - par_g2_200_02_fc_pm[[e]])^2) / 12

}

par_mse_g2_200_05_fc <- vector(length = M)

for (e in 1:M) {

  # The MSE is calculated by subtracting the estimated edge probabilities (not
  # including the probability of the edge being absent) from the expected edge
  # probabilities.
  par_mse_g2_200_05_fc[[e]] <- sum((ep_g2 - par_g2_200_05_fc_pm[[e]])^2) / 12

}

par_mse_g2_200_1_fc <- vector(length = M)

for (e in 1:M) {

  # The MSE is calculated by subtracting the estimated edge probabilities (not
  # including the probability of the edge being absent) from the expected edge
  # probabilities.
  par_mse_g2_200_1_fc[[e]] <- sum((ep_g2 - par_g2_200_1_fc_pm[[e]])^2) / 12

}

################################################################################
# Topology G2
################################################################################

par_mse_g2_200_02 <- vector(length = M)

for (e in 1:M) {

  # Set the edge probabilities for the edges that are not the true edges to 0.
  # We do this step because BiDAG considers edges outside of the restricted
  # search space. To make the comparison to baycn more fair we will not consider
  # these probabilities when calculating the MSE.
  par_g2_200_02_pm[[e]][am_g2_te == 0] <- 0

  # The MSE is calculated by subtracting the estimated edge probabilities (not
  # including the probability of the edge being absent) from the expected edge
  # probabilities.
  par_mse_g2_200_02[[e]] <- sum((ep_g2 - par_g2_200_02_pm[[e]])^2) / 8

}

par_mse_g2_200_05 <- vector(length = M)

for (e in 1:M) {

  # Set the edge probabilities for the edges that are not the true edges to 0.
  # We do this step because BiDAG considers edges outside of the restricted
  # search space. To make the comparison to baycn more fair we will not consider
  # these probabilities when calculating the MSE.
  par_g2_200_05_pm[[e]][am_g2_te == 0] <- 0

  # The MSE is calculated by subtracting the estimated edge probabilities (not
  # including the probability of the edge being absent) from the expected edge
  # probabilities.
  par_mse_g2_200_05[[e]] <- sum((ep_g2 - par_g2_200_05_pm[[e]])^2) / 8

}

#################################################
# partitionMCMC
# G2
# 100
# 1
#################################################

par_mse_g2_200_1 <- vector(length = M)

for (e in 1:M) {

  # Set the edge probabilities for the edges that are not the true edges to 0.
  # We do this step because BiDAG considers edges outside of the restricted
  # search space. To make the comparison to baycn more fair we will not consider
  # these probabilities when calculating the MSE.
  par_g2_200_1_pm[[e]][am_g2_te == 0] <- 0

  # The MSE is calculated by subtracting the estimated edge probabilities (not
  # including the probability of the edge being absent) from the expected edge
  # probabilities.
  par_mse_g2_200_1[[e]] <- sum((ep_g2 - par_g2_200_1_pm[[e]])^2) / 8

}

################################################################################
# Topology NC11
################################################################################

#################################################
# partitionMCMC
# NC11
# 100
# 0.2
#################################################

par_mse_nc11_200_02 <- vector(length = M)

for (e in 1:M) {

  # Set the edge probabilities for the edges that are not the true edges to 0.
  # We do this step because BiDAG considers edges outside of the restricted
  # search space. To make the comparison to baycn more fair we will not consider
  # these probabilities when calculating the MSE.
  par_nc11_200_02_pm[[e]][am_nc11_te == 0] <- 0

  # The MSE is calculated by subtracting the estimated edge probabilities (not
  # including the probability of the edge being absent) from the expected edge
  # probabilities.
  par_mse_nc11_200_02[[e]] <- sum((ep_nc11 - par_nc11_200_02_pm[[e]])^2) / 20

}

#################################################
# partitionMCMC
# NC11
# 100
# 0.5
#################################################

par_mse_nc11_200_05 <- vector(length = M)

for (e in 1:M) {

  # Set the edge probabilities for the edges that are not the true edges to 0.
  # We do this step because BiDAG considers edges outside of the restricted
  # search space. To make the comparison to baycn more fair we will not consider
  # these probabilities when calculating the MSE.
  par_nc11_200_05_pm[[e]][am_nc11_te == 0] <- 0

  # The MSE is calculated by subtracting the estimated edge probabilities (not
  # including the probability of the edge being absent) from the expected edge
  # probabilities.
  par_mse_nc11_200_05[[e]] <- sum((ep_nc11 - par_nc11_200_05_pm[[e]])^2) / 20

}

#################################################
# partitionMCMC
# NC11
# 100
# 1
#################################################

par_mse_nc11_200_1 <- vector(length = M)

for (e in 1:M) {

  # Set the edge probabilities for the edges that are not the true edges to 0.
  # We do this step because BiDAG considers edges outside of the restricted
  # search space. To make the comparison to baycn more fair we will not consider
  # these probabilities when calculating the MSE.
  par_nc11_200_1_pm[[e]][am_nc11_te == 0] <- 0

  # The MSE is calculated by subtracting the estimated edge probabilities (not
  # including the probability of the edge being absent) from the expected edge
  # probabilities.
  par_mse_nc11_200_1[[e]] <- sum((ep_nc11 - par_nc11_200_1_pm[[e]])^2) / 20

}

################################################################################
# Topology PC
################################################################################

#################################################
# partitionMCMC
# PC
# 100
# 0.2
#################################################

par_mse_pc_200_02 <- vector(length = M)

for (e in 1:M) {

  # Set the edge probabilities for the edges that are not the true edges to 0.
  # We do this step because BiDAG considers edges outside of the restricted
  # search space. To make the comparison to baycn more fair we will not consider
  # these probabilities when calculating the MSE.
  par_pc_200_02_pm[[e]][am_pc_te == 0] <- 0

  # The MSE is calculated by subtracting the estimated edge probabilities (not
  # including the probability of the edge being absent) from the expected edge
  # probabilities.
  par_mse_pc_200_02[[e]] <- sum((ep_pc - par_pc_200_02_pm[[e]])^2) / 16

}

#################################################
# partitionMCMC
# PC
# 100
# 0.5
#################################################

par_mse_pc_200_05 <- vector(length = M)

for (e in 1:M) {

  # Set the edge probabilities for the edges that are not the true edges to 0.
  # We do this step because BiDAG considers edges outside of the restricted
  # search space. To make the comparison to baycn more fair we will not consider
  # these probabilities when calculating the MSE.
  par_pc_200_05_pm[[e]][am_pc_te == 0] <- 0

  # The MSE is calculated by subtracting the estimated edge probabilities (not
  # including the probability of the edge being absent) from the expected edge
  # probabilities.
  par_mse_pc_200_05[[e]] <- sum((ep_pc - par_pc_200_05_pm[[e]])^2) / 16

}

#################################################
# partitionMCMC
# PC
# 100
# 1
#################################################

par_mse_pc_200_1 <- vector(length = M)

for (e in 1:M) {

  # Set the edge probabilities for the edges that are not the true edges to 0.
  # We do this step because BiDAG considers edges outside of the restricted
  # search space. To make the comparison to baycn more fair we will not consider
  # these probabilities when calculating the MSE.
  par_pc_200_1_pm[[e]][am_pc_te == 0] <- 0

  # The MSE is calculated by subtracting the estimated edge probabilities (not
  # including the probability of the edge being absent) from the expected edge
  # probabilities.
  par_mse_pc_200_1[[e]] <- sum((ep_pc - par_pc_200_1_pm[[e]])^2) / 16

}

################################################################################
# Save the lists of MSE for each method.
################################################################################

save(ord_mse_g2_200_02_fc,
     ord_mse_g2_200_05_fc,
     ord_mse_g2_200_1_fc,
     ord_mse_g2_200_02,
     ord_mse_g2_200_05,
     ord_mse_g2_200_1,
     ord_mse_nc11_200_02,
     ord_mse_nc11_200_05,
     ord_mse_nc11_200_1,
     ord_mse_pc_200_02,
     ord_mse_pc_200_05,
     ord_mse_pc_200_1,
     par_mse_g2_200_02_fc,
     par_mse_g2_200_05_fc,
     par_mse_g2_200_1_fc,
     par_mse_g2_200_02,
     par_mse_g2_200_05,
     par_mse_g2_200_1,
     par_mse_nc11_200_02,
     par_mse_nc11_200_05,
     par_mse_nc11_200_1,
     par_mse_pc_200_02,
     par_mse_pc_200_05,
     par_mse_pc_200_1,
     file = '/Users/Evatar/Sync/Evan/classesUofI/BCB600/packages/baycn/simulations/bidag/mse2_bid_ge_N_200.RData')
