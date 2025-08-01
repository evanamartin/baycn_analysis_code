# load outputs

load("./ord_ge_N_200_pm.RData")
load("./par_ge_N_200_pm.RData")
# load("./gib_ge_N_200_pm.RData")
load("./mc3_ge_N_200_pm.RData")
load("./bay_ge_N_200_g2.RData")
# load("./bay_ge_N_200_nc11.RData")
load("./bay_ge_N_200_pc.RData")
load("./bcdag_ge_N_200.RData")

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

  # The MSE is calculated by subtracting the estimated edge probabilities (not
  # including the probability of the edge being absent) from the expected edge
  # probabilities.

# ------------------------------------------------------------------------------
# Calculate the mse for order MCMC ---------------------------------------------
# ------------------------------------------------------------------------------

# Topology G2 FC -----------------------

ord_mse_g2_200_02 <- vector(length = M)

for (e in 1:M) {
  ord_mse_g2_200_02[[e]] <- sum((ep_g2 - ord_g2_200_02_pm[[e]])^2) / 12
}

ord_mse_g2_200_05 <- vector(length = M)

for (e in 1:M) {
  ord_mse_g2_200_05[[e]] <- sum((ep_g2 - ord_g2_200_05_pm[[e]])^2) / 12
}

ord_mse_g2_200_1 <- vector(length = M)

for (e in 1:M) {
  ord_mse_g2_200_1[[e]] <- sum((ep_g2 - ord_g2_200_1_pm[[e]])^2) / 12
}


# Topology NC11 FC -----------------------

ord_mse_nc11_200_02 <- vector(length = M)

for (e in 1:M) {
  ord_mse_nc11_200_02[[e]] <- sum((ep_nc11 - ord_nc11_200_02_pm[[e]])^2) / 110
}

ord_mse_nc11_200_05 <- vector(length = M)

for (e in 1:M) {
  ord_mse_nc11_200_05[[e]] <- sum((ep_nc11 - ord_nc11_200_05_pm[[e]])^2) / 110
}

ord_mse_nc11_200_1 <- vector(length = M)

for (e in 1:M) {
  ord_mse_nc11_200_1[[e]] <- sum((ep_nc11 - ord_nc11_200_1_pm[[e]])^2) / 110
}

# Topology PC FC -----------------------

ord_mse_pc_200_02 <- vector(length = M)

for (e in 1:M) {
  ord_mse_pc_200_02[[e]] <- sum((ep_pc - ord_pc_200_02_pm[[e]])^2) / 56
}

ord_mse_pc_200_05 <- vector(length = M)

for (e in 1:M) {
  ord_mse_pc_200_05[[e]] <- sum((ep_pc - ord_pc_200_05_pm[[e]])^2) / 56
}

ord_mse_pc_200_1 <- vector(length = M)

for (e in 1:M) {
  ord_mse_pc_200_1[[e]] <- sum((ep_pc - ord_pc_200_1_pm[[e]])^2) / 56
}

# ------------------------------------------------------------------------------
# Calculate the mse for partition MCMC -----------------------------------------
# ------------------------------------------------------------------------------

# Topology G2 FC -----------------------

par_mse_g2_200_02 <- vector(length = M)

for (e in 1:M) {
  par_mse_g2_200_02[[e]] <- sum((ep_g2 - par_g2_200_02_pm[[e]])^2) / 12
}

par_mse_g2_200_05 <- vector(length = M)

for (e in 1:M) {
  par_mse_g2_200_05[[e]] <- sum((ep_g2 - par_g2_200_05_pm[[e]])^2) / 12
}

par_mse_g2_200_1 <- vector(length = M)

for (e in 1:M) {
  par_mse_g2_200_1[[e]] <- sum((ep_g2 - par_g2_200_1_pm[[e]])^2) / 12
}

# Topology NC11 FC -----------------------

par_mse_nc11_200_02 <- vector(length = M)

for (e in 1:M) {
  par_mse_nc11_200_02[[e]] <- sum((ep_nc11 - par_nc11_200_02_pm[[e]])^2) / 110
}

par_mse_nc11_200_05 <- vector(length = M)

for (e in 1:M) {
  par_mse_nc11_200_05[[e]] <- sum((ep_nc11 - par_nc11_200_05_pm[[e]])^2) / 110
}

par_mse_nc11_200_1 <- vector(length = M)

for (e in 1:M) {
  par_mse_nc11_200_1[[e]] <- sum((ep_nc11 - par_nc11_200_1_pm[[e]])^2) / 110
}

# Topology PC FC -----------------------

par_mse_pc_200_02 <- vector(length = M)

for (e in 1:M) {
  par_mse_pc_200_02[[e]] <- sum((ep_pc - par_pc_200_02_pm[[e]])^2) / 56
}

par_mse_pc_200_05 <- vector(length = M)

for (e in 1:M) {
  par_mse_pc_200_05[[e]] <- sum((ep_pc - par_pc_200_05_pm[[e]])^2) / 56
}

par_mse_pc_200_1 <- vector(length = M)

for (e in 1:M) {
  par_mse_pc_200_1[[e]] <- sum((ep_pc - par_pc_200_1_pm[[e]])^2) / 56
}

# ------------------------------------------------------------------------------
# Calculate the mse for all topologies for MC3 ---------------------------------
# ------------------------------------------------------------------------------

# Topology G2 FC -----------------------

mc3_mse_g2_200_02 <- vector(length = M)

for (e in 1:M) {
  mc3_mse_g2_200_02[[e]] <- sum((ep_g2 - mc3_g2_200_02_fc_pm[[e]])^2) / 12

}

mc3_mse_g2_200_05 <- vector(length = M)

for (e in 1:M) {
  mc3_mse_g2_200_05[[e]] <- sum((ep_g2 - mc3_g2_200_05_fc_pm[[e]])^2) / 12

}

mc3_mse_g2_200_1 <- vector(length = M)

for (e in 1:M) {
  mc3_mse_g2_200_1[[e]] <- sum((ep_g2 - mc3_g2_200_1_fc_pm[[e]])^2) / 12

}

# Topology NC11 FC -----------------------

mc3_mse_nc11_200_02 <- vector(length = M)

for (e in 1:M) {
  mc3_mse_nc11_200_02[[e]] <- sum((ep_nc11 - mc3_nc11_200_02_pm[[e]])^2) / 110

}

mc3_mse_nc11_200_05 <- vector(length = M)

for (e in 1:M) {
  mc3_mse_nc11_200_05[[e]] <- sum((ep_nc11 - mc3_nc11_200_05_pm[[e]])^2) / 110

}

mc3_mse_nc11_200_1 <- vector(length = M)

for (e in 1:M) {
  mc3_mse_nc11_200_1[[e]] <- sum((ep_nc11 - mc3_nc11_200_1_pm[[e]])^2) / 110

}

# Topology PC FC -----------------------

mc3_mse_pc_200_02 <- vector(length = M)

for (e in 1:M) {
  mc3_mse_pc_200_02[[e]] <- sum((ep_pc - mc3_pc_200_02_pm[[e]])^2) / 56

}

mc3_mse_pc_200_05 <- vector(length = M)

for (e in 1:M) {
  mc3_mse_pc_200_05[[e]] <- sum((ep_pc - mc3_pc_200_05_pm[[e]])^2) / 56

}

mc3_mse_pc_200_1 <- vector(length = M)

for (e in 1:M) {
  mc3_mse_pc_200_1[[e]] <- sum((ep_pc - mc3_pc_200_1_pm[[e]])^2) / 56

}

# ------------------------------------------------------------------------------
# Calculate the mse for all topologies for Gibbs -------------------------------
# ------------------------------------------------------------------------------

# SKIPPING: Did not run the Gibbs method

# ------------------------------------------------------------------------------
# Calculate the mse for all topologies for ScanBMA -----------------------------
# ------------------------------------------------------------------------------

# MSE2 was not calculated for scanBMA, since the posterior probabilities from scanBMA have a different interpretation from the MCMC methods

# ------------------------------------------------------------------------------
# Calculate the mse for all topologies for BCDAG -------------------------------
# ------------------------------------------------------------------------------

# Topology G2 FC -----------------------

bcdag_mse_g2_200_02 <- vector(length = M)

for (e in 1:M) {
  bcdag_mse_g2_200_02[[e]] <- sum((ep_g2 - mat_g2_200_02[[e]])^2) / 12
}

bcdag_mse_g2_200_05 <- vector(length = M)

for (e in 1:M) {
  bcdag_mse_g2_200_05[[e]] <- sum((ep_g2 - mat_g2_200_05[[e]])^2) / 12
}

bcdag_mse_g2_200_1 <- vector(length = M)

for (e in 1:M) {
  bcdag_mse_g2_200_1[[e]] <- sum((ep_g2 - mat_g2_200_1[[e]])^2) / 12
}

# Topology NC11 FC -----------------------

bcdag_mse_nc11_200_02 <- vector(length = M)

for (e in 1:M) {
  bcdag_mse_nc11_200_02[[e]] <- sum((ep_nc11 - mat_nc11_200_02[[e]])^2) / 110
}

bcdag_mse_nc11_200_05 <- vector(length = M)

for (e in 1:M) {
  bcdag_mse_nc11_200_05[[e]] <- sum((ep_nc11 - mat_nc11_200_05[[e]])^2) / 110
}

bcdag_mse_nc11_200_1 <- vector(length = M)

for (e in 1:M) {
  bcdag_mse_nc11_200_1[[e]] <- sum((ep_nc11 - mat_nc11_200_1[[e]])^2) / 110
}

# Topology PC FC -----------------------

bcdag_mse_pc_200_02 <- vector(length = M)

for (e in 1:M) {
  bcdag_mse_pc_200_02[[e]] <- sum((ep_pc - mat_pc_200_02[[e]])^2) / 56
}

bcdag_mse_pc_200_05 <- vector(length = M)

for (e in 1:M) {
  bcdag_mse_pc_200_05[[e]] <- sum((ep_pc - mat_pc_200_05[[e]])^2) / 56
}

bcdag_mse_pc_200_1 <- vector(length = M)

for (e in 1:M) {
  bcdag_mse_pc_200_1[[e]] <- sum((ep_pc - mat_pc_200_1[[e]])^2) / 56
}

# ------------------------------------------------------------------------------
# Calculate the mse for all topologies for baycn -------------------------------
# ------------------------------------------------------------------------------

# Topology G2 FC -----------------------

bay_mse_g2_200_02 <- vector(length = M)

for (e in 1:M) {
  bay_mse_g2_200_02[[e]] <- sum((ep_g2 - bay_g2_200_02[[e]]@posteriorPM)^2) / 12
}

bay_mse_g2_200_05 <- vector(length = M)

for (e in 1:M) {
  bay_mse_g2_200_05[[e]] <- sum((ep_g2 - bay_g2_200_05[[e]]@posteriorPM)^2) / 12
}

bay_mse_g2_200_1 <- vector(length = M)

for (e in 1:M) {
  bay_mse_g2_200_1[[e]] <- sum((ep_g2 - bay_g2_200_1[[e]]@posteriorPM)^2) / 12
}

# Topology NC11 FC -----------------------

# bay_mse_nc11_200_02 <- vector(length = M)
#
# for (e in 1:M) {
#   bay_mse_nc11_200_02[[e]] <- sum((ep_nc11 - bay_nc11_200_02[[e]]@posteriorPM)^2) / 110
# }
#
# bay_mse_nc11_200_05 <- vector(length = M)
#
# for (e in 1:M) {
#   bay_mse_nc11_200_05[[e]] <- sum((ep_nc11 - bay_nc11_200_05[[e]]@posteriorPM)^2) / 110
# }
#
# bay_mse_nc11_200_1 <- vector(length = M)
#
# for (e in 1:M) {
#   bay_mse_nc11_200_1[[e]] <- sum((ep_nc11 - bay_nc11_200_1[[e]]@posteriorPM)^2) / 110
# }

# Topology PC FC -----------------------

bay_mse_pc_200_02 <- vector(length = M)

for (e in 1:M) {
  bay_mse_pc_200_02[[e]] <- sum((ep_pc - bay_pc_200_02[[e]]@posteriorPM)^2) / 56
}

bay_mse_pc_200_05 <- vector(length = M)

for (e in 1:M) {
  bay_mse_pc_200_05[[e]] <- sum((ep_pc - bay_pc_200_05[[e]]@posteriorPM)^2) / 56
}

bay_mse_pc_200_1 <- vector(length = M)

for (e in 1:M) {
  bay_mse_pc_200_1[[e]] <- sum((ep_pc - bay_pc_200_1[[e]]@posteriorPM)^2) / 56
}


# ------------------------------------------------------------------------------
# Saving all variables
# ------------------------------------------------------------------------------


save(
     ord_mse_g2_200_02,
     ord_mse_g2_200_05,
     ord_mse_g2_200_1,
     ord_mse_nc11_200_02,
     ord_mse_nc11_200_05,
     ord_mse_nc11_200_1,
     ord_mse_pc_200_02,
     ord_mse_pc_200_05,
     ord_mse_pc_200_1,
     par_mse_g2_200_02,
     par_mse_g2_200_05,
     par_mse_g2_200_1,
     par_mse_nc11_200_02,
     par_mse_nc11_200_05,
     par_mse_nc11_200_1,
     par_mse_pc_200_02,
     par_mse_pc_200_05,
     par_mse_pc_200_1,
     mc3_mse_g2_200_02,
     mc3_mse_g2_200_05,
     mc3_mse_g2_200_1,
     mc3_mse_nc11_200_02,
     mc3_mse_nc11_200_05,
     mc3_mse_nc11_200_1,
     mc3_mse_pc_200_02,
     mc3_mse_pc_200_05,
     mc3_mse_pc_200_1,
     bcdag_mse_g2_200_02,
     bcdag_mse_g2_200_05,
     bcdag_mse_g2_200_1,
     bcdag_mse_nc11_200_02,
     bcdag_mse_nc11_200_05,
     bcdag_mse_nc11_200_1,
     bcdag_mse_pc_200_02,
     bcdag_mse_pc_200_05,
     bcdag_mse_pc_200_1,
     bay_mse_g2_200_02,
     bay_mse_g2_200_05,
     bay_mse_g2_200_1,
     # bay_mse_nc11_200_02,
     # bay_mse_nc11_200_05,
     # bay_mse_nc11_200_1,
     bay_mse_pc_200_02,
     bay_mse_pc_200_05,
     bay_mse_pc_200_1,
     file = './venkata_mse2_all_200.RData')
