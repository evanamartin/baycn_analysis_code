# Load the output from MC3 and Gibbs -------------------------------------------

load("/Users/Evatar/Sync/Evan/classesUofI/BCB600/packages/baycn/simulations/structmcmc/gib_ge_N_600_pm.RData")
load("/Users/Evatar/Sync/Evan/classesUofI/BCB600/packages/baycn/simulations/structmcmc/mc3_ge_N_600_pm.RData")

# Expected probability matrices ------------------------------------------------

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

# Calculate the mse for all topologies for MC3 ---------------------------------

# Topology G2 fc -----------------------

mc3_mse_g2_600_02_fc <- vector(length = M)

for (e in 1:M) {

  # The MSE is calculated by subtracting the estimated edge probabilities (not
  # including the probability of the edge being absent) from the expected edge
  # probabilities.
  mc3_mse_g2_600_02_fc[[e]] <- sum((ep_g2 - mc3_g2_600_02_fc_pm[[e]])^2) / 12

}

mc3_mse_g2_600_05_fc <- vector(length = M)

for (e in 1:M) {

  # The MSE is calculated by subtracting the estimated edge probabilities (not
  # including the probability of the edge being absent) from the expected edge
  # probabilities.
  mc3_mse_g2_600_05_fc[[e]] <- sum((ep_g2 - mc3_g2_600_05_fc_pm[[e]])^2) / 12

}

mc3_mse_g2_600_1_fc <- vector(length = M)

for (e in 1:M) {

  # The MSE is calculated by subtracting the estimated edge probabilities (not
  # including the probability of the edge being absent) from the expected edge
  # probabilities.
  mc3_mse_g2_600_1_fc[[e]] <- sum((ep_g2 - mc3_g2_600_1_fc_pm[[e]])^2) / 12

}

# Topology G2 --------------------------

#################################################
# MC3
# G2
# 100
# 0.2
#################################################

mc3_mse_g2_600_02 <- vector(length = M)

for (e in 1:M) {

  # The MSE is calculated by subtracting the estimated edge probabilities (not
  # including the probability of the edge being absent) from the expected edge
  # probabilities.
  mc3_mse_g2_600_02[[e]] <- sum((ep_g2 - mc3_g2_600_02_pm[[e]])^2) / 8

}

#################################################
# MC3
# G2
# 100
# 0.5
#################################################

mc3_mse_g2_600_05 <- vector(length = M)

for (e in 1:M) {

  # The MSE is calculated by subtracting the estimated edge probabilities (not
  # including the probability of the edge being absent) from the expected edge
  # probabilities.
  mc3_mse_g2_600_05[[e]] <- sum((ep_g2 - mc3_g2_600_05_pm[[e]])^2) / 8

}

#################################################
# MC3
# G2
# 100
# 1
#################################################

mc3_mse_g2_600_1 <- vector(length = M)

for (e in 1:M) {

  # The MSE is calculated by subtracting the estimated edge probabilities (not
  # including the probability of the edge being absent) from the expected edge
  # probabilities.
  mc3_mse_g2_600_1[[e]] <- sum((ep_g2 - mc3_g2_600_1_pm[[e]])^2) / 8

}

################################################################################
# Topology NC11
################################################################################

#################################################
# MC3
# NC11
# 100
# 0.2
#################################################

mc3_mse_nc11_600_02 <- vector(length = M)

for (e in 1:M) {

  # The MSE is calculated by subtracting the estimated edge probabilities (not
  # including the probability of the edge being absent) from the expected edge
  # probabilities.
  mc3_mse_nc11_600_02[[e]] <- sum((ep_nc11 - mc3_nc11_600_02_pm[[e]])^2) / 20

}

#################################################
# MC3
# NC11
# 100
# 0.5
#################################################

mc3_mse_nc11_600_05 <- vector(length = M)

for (e in 1:M) {

  # The MSE is calculated by subtracting the estimated edge probabilities (not
  # including the probability of the edge being absent) from the expected edge
  # probabilities.
  mc3_mse_nc11_600_05[[e]] <- sum((ep_nc11 - mc3_nc11_600_05_pm[[e]])^2) / 20

}

#################################################
# MC3
# NC11
# 100
# 1
#################################################

mc3_mse_nc11_600_1 <- vector(length = M)

for (e in 1:M) {

  # The MSE is calculated by subtracting the estimated edge probabilities (not
  # including the probability of the edge being absent) from the expected edge
  # probabilities.
  mc3_mse_nc11_600_1[[e]] <- sum((ep_nc11 - mc3_nc11_600_1_pm[[e]])^2) / 20

}

################################################################################
# Topology PC
################################################################################

#################################################
# MC3
# PC
# 100
# 0.2
#################################################

mc3_mse_pc_600_02 <- vector(length = M)

for (e in 1:M) {

  # The MSE is calculated by subtracting the estimated edge probabilities (not
  # including the probability of the edge being absent) from the expected edge
  # probabilities.
  mc3_mse_pc_600_02[[e]] <- sum((ep_pc - mc3_pc_600_02_pm[[e]])^2) / 16

}

#################################################
# MC3
# PC
# 100
# 0.5
#################################################

mc3_mse_pc_600_05 <- vector(length = M)

for (e in 1:M) {

  # The MSE is calculated by subtracting the estimated edge probabilities (not
  # including the probability of the edge being absent) from the expected edge
  # probabilities.
  mc3_mse_pc_600_05[[e]] <- sum((ep_pc - mc3_pc_600_05_pm[[e]])^2) / 16

}

#################################################
# MC3
# PC
# 100
# 1
#################################################

mc3_mse_pc_600_1 <- vector(length = M)

for (e in 1:M) {

  # The MSE is calculated by subtracting the estimated edge probabilities (not
  # including the probability of the edge being absent) from the expected edge
  # probabilities.
  mc3_mse_pc_600_1[[e]] <- sum((ep_pc - mc3_pc_600_1_pm[[e]])^2) / 16

}

# Calculate the mse for all topologies for Gibbs -------------------------------

# Topology G2 fc -----------------------

gib_mse_g2_600_02_fc <- vector(length = M)

for (e in 1:M) {

  # The MSE is calculated by subtracting the estimated edge probabilities (not
  # including the probability of the edge being absent) from the expected edge
  # probabilities.
  gib_mse_g2_600_02_fc[[e]] <- sum((ep_g2 - gib_g2_600_02_fc_pm[[e]])^2) / 12

}

gib_mse_g2_600_05_fc <- vector(length = M)

for (e in 1:M) {

  # The MSE is calculated by subtracting the estimated edge probabilities (not
  # including the probability of the edge being absent) from the expected edge
  # probabilities.
  gib_mse_g2_600_05_fc[[e]] <- sum((ep_g2 - gib_g2_600_05_fc_pm[[e]])^2) / 12

}

gib_mse_g2_600_1_fc <- vector(length = M)

for (e in 1:M) {

  # The MSE is calculated by subtracting the estimated edge probabilities (not
  # including the probability of the edge being absent) from the expected edge
  # probabilities.
  gib_mse_g2_600_1_fc[[e]] <- sum((ep_g2 - gib_g2_600_1_fc_pm[[e]])^2) / 12

}

# Topology G2 --------------------------

#################################################
# gib
# G2
# 100
# 0.2
#################################################

gib_mse_g2_600_02 <- vector(length = M)

for (e in 1:M) {

  # The MSE is calculated by subtracting the estimated edge probabilities (not
  # including the probability of the edge being absent) from the expected edge
  # probabilities.
  gib_mse_g2_600_02[[e]] <- sum((ep_g2 - gib_g2_600_02_pm[[e]])^2) / 8

}

#################################################
# gib
# G2
# 100
# 0.5
#################################################

gib_mse_g2_600_05 <- vector(length = M)

for (e in 1:M) {

  # The MSE is calculated by subtracting the estimated edge probabilities (not
  # including the probability of the edge being absent) from the expected edge
  # probabilities.
  gib_mse_g2_600_05[[e]] <- sum((ep_g2 - gib_g2_600_05_pm[[e]])^2) / 8

}

#################################################
# gib
# G2
# 100
# 1
#################################################

gib_mse_g2_600_1 <- vector(length = M)

for (e in 1:M) {

  # The MSE is calculated by subtracting the estimated edge probabilities (not
  # including the probability of the edge being absent) from the expected edge
  # probabilities.
  gib_mse_g2_600_1[[e]] <- sum((ep_g2 - gib_g2_600_1_pm[[e]])^2) / 8

}

################################################################################
# Topology NC11
################################################################################

#################################################
# gib
# NC11
# 100
# 0.2
#################################################

gib_mse_nc11_600_02 <- vector(length = M)

for (e in 1:M) {

  # The MSE is calculated by subtracting the estimated edge probabilities (not
  # including the probability of the edge being absent) from the expected edge
  # probabilities.
  gib_mse_nc11_600_02[[e]] <- sum((ep_nc11 - gib_nc11_600_02_pm[[e]])^2) / 20

}

#################################################
# gib
# NC11
# 100
# 0.5
#################################################

gib_mse_nc11_600_05 <- vector(length = M)

for (e in 1:M) {

  # The MSE is calculated by subtracting the estimated edge probabilities (not
  # including the probability of the edge being absent) from the expected edge
  # probabilities.
  gib_mse_nc11_600_05[[e]] <- sum((ep_nc11 - gib_nc11_600_05_pm[[e]])^2) / 20

}

#################################################
# gib
# NC11
# 100
# 1
#################################################

gib_mse_nc11_600_1 <- vector(length = M)

for (e in 1:M) {

  # The MSE is calculated by subtracting the estimated edge probabilities (not
  # including the probability of the edge being absent) from the expected edge
  # probabilities.
  gib_mse_nc11_600_1[[e]] <- sum((ep_nc11 - gib_nc11_600_1_pm[[e]])^2) / 20

}

################################################################################
# Topology PC
################################################################################

#################################################
# gib
# PC
# 100
# 0.2
#################################################

gib_mse_pc_600_02 <- vector(length = M)

for (e in 1:M) {

  # The MSE is calculated by subtracting the estimated edge probabilities (not
  # including the probability of the edge being absent) from the expected edge
  # probabilities.
  gib_mse_pc_600_02[[e]] <- sum((ep_pc - gib_pc_600_02_pm[[e]])^2) / 16

}

#################################################
# gib
# PC
# 100
# 0.5
#################################################

gib_mse_pc_600_05 <- vector(length = M)

for (e in 1:M) {

  # The MSE is calculated by subtracting the estimated edge probabilities (not
  # including the probability of the edge being absent) from the expected edge
  # probabilities.
  gib_mse_pc_600_05[[e]] <- sum((ep_pc - gib_pc_600_05_pm[[e]])^2) / 16

}

#################################################
# gib
# PC
# 100
# 1
#################################################

gib_mse_pc_600_1 <- vector(length = M)

for (e in 1:M) {

  # The MSE is calculated by subtracting the estimated edge probabilities (not
  # including the probability of the edge being absent) from the expected edge
  # probabilities.
  gib_mse_pc_600_1[[e]] <- sum((ep_pc - gib_pc_600_1_pm[[e]])^2) / 16

}

################################################################################
# Save the lists of MSE for each method.
################################################################################

save(mc3_mse_g2_600_02_fc,
     mc3_mse_g2_600_05_fc,
     mc3_mse_g2_600_1_fc,
     mc3_mse_g2_600_02,
     mc3_mse_g2_600_05,
     mc3_mse_g2_600_1,
     mc3_mse_nc11_600_02,
     mc3_mse_nc11_600_05,
     mc3_mse_nc11_600_1,
     mc3_mse_pc_600_02,
     mc3_mse_pc_600_05,
     mc3_mse_pc_600_1,
     gib_mse_g2_600_02_fc,
     gib_mse_g2_600_05_fc,
     gib_mse_g2_600_1_fc,
     gib_mse_g2_600_02,
     gib_mse_g2_600_05,
     gib_mse_g2_600_1,
     gib_mse_nc11_600_02,
     gib_mse_nc11_600_05,
     gib_mse_nc11_600_1,
     gib_mse_pc_600_02,
     gib_mse_pc_600_05,
     gib_mse_pc_600_1,
     file = '/Users/Evatar/Sync/Evan/classesUofI/BCB600/packages/baycn/simulations/structmcmc/mse2_str_ge_N_600.RData')
