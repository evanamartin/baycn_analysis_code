library(baycn)

load("./ord_ge_N_100_pm.RData")
load("./par_ge_N_100_pm.RData")
# load("./gib_ge_N_100_pm.RData")
load("./mc3_ge_N_100_pm.RData")
load("./bay_ge_N_100_g2.RData")
# load("./bay_ge_N_100_nc11.RData")
load("./bay_ge_N_100_pc.RData")
load("./bcdag_ge_N_100.RData")
load("./bma_ge_N_100_pm.RData")



# true prerec probability matrices ------------------------------------------------

# The true matrix for topology G2.
true_g2 <- matrix(c(0, 1, 1, 0,
                    1, 0, 0, 1,
                    1, 0, 0, 1,
                    0, 1, 1, 0),
                byrow = TRUE,
                nrow = 4)

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

# The true matrix for topology PC.
true_pc <- matrix(c(0, 1, 0, 0, 0, 1, 0, 1,
                    1, 0, 1, 0, 1, 0, 0, 0,
                    0, 1, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0,
                    0, 1, 0, 0, 0, 1, 0, 1,
                    0, 0, 0, 0, 0, 0, 1, 0,
                    0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0),
                byrow = TRUE,
                nrow = 8)

# Make the true matricies diagonal for the three graphs

true_g2 <- ifelse (true_g2 + t(true_g2), 1, 0)
true_nc11 <- ifelse (true_nc11 + t(true_nc11), 1, 0)
true_pc <- ifelse (true_pc + t(true_pc), 1, 0)


# ------------------------------------------------------------------------------
# Initialize lists to store prerec
# ------------------------------------------------------------------------------

# baycn -----------------------

bay_prerec_g2_100_02 <- vector(mode = "list", length = M)
bay_prerec_g2_100_05 <- vector(mode = "list", length = M)
bay_prerec_g2_100_1 <- vector(mode = "list", length = M)

bay_prerec_nc11_100_02 <- vector(mode = "list", length = M)
bay_prerec_nc11_100_05 <- vector(mode = "list", length = M)
bay_prerec_nc11_100_1 <- vector(mode = "list", length = M)

bay_prerec_pc_100_02 <- vector(mode = "list", length = M)
bay_prerec_pc_100_05 <- vector(mode = "list", length = M)
bay_prerec_pc_100_1 <- vector(mode = "list", length = M)

# order MCMC -----------------------

ord_prerec_g2_100_02 <- vector(mode = "list", length = M)
ord_prerec_g2_100_05 <- vector(mode = "list", length = M)
ord_prerec_g2_100_1 <- vector(mode = "list", length = M)

ord_prerec_nc11_100_02 <- vector(mode = "list", length = M)
ord_prerec_nc11_100_05 <- vector(mode = "list", length = M)
ord_prerec_nc11_100_1 <- vector(mode = "list", length = M)

ord_prerec_pc_100_02 <- vector(mode = "list", length = M)
ord_prerec_pc_100_05 <- vector(mode = "list", length = M)
ord_prerec_pc_100_1 <- vector(mode = "list", length = M)

# partition MCMC -----------------------

par_prerec_g2_100_02 <- vector(mode = "list", length = M)
par_prerec_g2_100_05 <- vector(mode = "list", length = M)
par_prerec_g2_100_1 <- vector(mode = "list", length = M)

par_prerec_nc11_100_02 <- vector(mode = "list", length = M)
par_prerec_nc11_100_05 <- vector(mode = "list", length = M)
par_prerec_nc11_100_1 <- vector(mode = "list", length = M)

par_prerec_pc_100_02 <- vector(mode = "list", length = M)
par_prerec_pc_100_05 <- vector(mode = "list", length = M)
par_prerec_pc_100_1 <- vector(mode = "list", length = M)

# mc3 -----------------------

mc3_prerec_g2_100_02 <- vector(mode = "list", length = M)
mc3_prerec_g2_100_05 <- vector(mode = "list", length = M)
mc3_prerec_g2_100_1 <- vector(mode = "list", length = M)

mc3_prerec_nc11_100_02 <- vector(mode = "list", length = M)
mc3_prerec_nc11_100_05 <- vector(mode = "list", length = M)
mc3_prerec_nc11_100_1 <- vector(mode = "list", length = M)

mc3_prerec_pc_100_02 <- vector(mode = "list", length = M)
mc3_prerec_pc_100_05 <- vector(mode = "list", length = M)
mc3_prerec_pc_100_1 <- vector(mode = "list", length = M)

# scanBMA -----------------------

bma_prerec_g2_100_02 <- vector(mode = "list", length = M)
bma_prerec_g2_100_05 <- vector(mode = "list", length = M)
bma_prerec_g2_100_1 <- vector(mode = "list", length = M)

bma_prerec_nc11_100_02 <- vector(mode = "list", length = M)
bma_prerec_nc11_100_05 <- vector(mode = "list", length = M)
bma_prerec_nc11_100_1 <- vector(mode = "list", length = M)

bma_prerec_pc_100_02 <- vector(mode = "list", length = M)
bma_prerec_pc_100_05 <- vector(mode = "list", length = M)
bma_prerec_pc_100_1 <- vector(mode = "list", length = M)

# bcdag -----------------------

bcdag_prerec_g2_100_02 <- vector(mode = "list", length = M)
bcdag_prerec_g2_100_05 <- vector(mode = "list", length = M)
bcdag_prerec_g2_100_1 <- vector(mode = "list", length = M)

bcdag_prerec_nc11_100_02 <- vector(mode = "list", length = M)
bcdag_prerec_nc11_100_05 <- vector(mode = "list", length = M)
bcdag_prerec_nc11_100_1 <- vector(mode = "list", length = M)

bcdag_prerec_pc_100_02 <- vector(mode = "list", length = M)
bcdag_prerec_pc_100_05 <- vector(mode = "list", length = M)
bcdag_prerec_pc_100_1 <- vector(mode = "list", length = M)




for (e in 1:M) {

  # baycn --------------------------
  print(e)
  print("baycn")

  bay_prerec_g2_100_02[[e]] <- prerec(amInferred = bay_g2_100_02[[e]]@posteriorPM, amTrue = true_g2, cutoff = 0.5)
  bay_prerec_g2_100_05[[e]] <- prerec(amInferred = bay_g2_100_05[[e]]@posteriorPM, amTrue = true_g2, cutoff = 0.5)
  bay_prerec_g2_100_1[[e]] <- prerec(amInferred = bay_g2_100_1[[e]]@posteriorPM, amTrue = true_g2, cutoff = 0.5)

  # bay_prerec_nc11_100_02[[e]] <- prerec(amInferred = bay_nc11_100_02[[e]]@posteriorPM, amTrue = true_nc11, cutoff = 0.5)
  # bay_prerec_nc11_100_05[[e]] <- prerec(amInferred = bay_nc11_100_05[[e]]@posteriorPM, amTrue = true_nc11, cutoff = 0.5)
  # bay_prerec_nc11_100_1[[e]] <- prerec(amInferred = bay_nc11_100_1[[e]]@posteriorPM, amTrue = true_nc11, cutoff = 0.5)

  bay_prerec_pc_100_02[[e]] <- prerec(amInferred = bay_pc_100_02[[e]]@posteriorPM, amTrue = true_pc, cutoff = 0.5)
  bay_prerec_pc_100_05[[e]] <- prerec(amInferred = bay_pc_100_05[[e]]@posteriorPM, amTrue = true_pc, cutoff = 0.5)
  bay_prerec_pc_100_1[[e]] <- prerec(amInferred = bay_pc_100_1[[e]]@posteriorPM, amTrue = true_pc, cutoff = 0.5)

  # order MCMC --------------------------
  print("order MCMC")

  ord_prerec_g2_100_02[[e]] <- prerec(amInferred = ord_g2_100_02_pm[[e]], amTrue = true_g2, cutoff = 0.5)
  ord_prerec_g2_100_05[[e]] <- prerec(amInferred = ord_g2_100_05_pm[[e]], amTrue = true_g2, cutoff = 0.5)
  ord_prerec_g2_100_1[[e]] <- prerec(amInferred = ord_g2_100_1_pm[[e]], amTrue = true_g2, cutoff = 0.5)

  ord_prerec_nc11_100_02[[e]] <- prerec(amInferred = ord_nc11_100_02_pm[[e]], amTrue = true_nc11, cutoff = 0.5)
  ord_prerec_nc11_100_05[[e]] <- prerec(amInferred = ord_nc11_100_05_pm[[e]], amTrue = true_nc11, cutoff = 0.5)
  ord_prerec_nc11_100_1[[e]] <- prerec(amInferred = ord_nc11_100_1_pm[[e]], amTrue = true_nc11, cutoff = 0.5)

  ord_prerec_pc_100_02[[e]] <- prerec(amInferred = ord_pc_100_02_pm[[e]], amTrue = true_pc, cutoff = 0.5)
  ord_prerec_pc_100_05[[e]] <- prerec(amInferred = ord_pc_100_05_pm[[e]], amTrue = true_pc, cutoff = 0.5)
  ord_prerec_pc_100_1[[e]] <- prerec(amInferred = ord_pc_100_1_pm[[e]], amTrue = true_pc, cutoff = 0.5)

  # partition MCMC --------------------------
  print("partition MCMC")

  par_prerec_g2_100_02[[e]] <- prerec(amInferred = par_g2_100_02_pm[[e]], amTrue = true_g2, cutoff = 0.5)
  par_prerec_g2_100_05[[e]] <- prerec(amInferred = par_g2_100_05_pm[[e]], amTrue = true_g2, cutoff = 0.5)
  par_prerec_g2_100_1[[e]] <- prerec(amInferred = par_g2_100_1_pm[[e]], amTrue = true_g2, cutoff = 0.5)

  par_prerec_nc11_100_02[[e]] <- prerec(amInferred = par_nc11_100_02_pm[[e]], amTrue = true_nc11, cutoff = 0.5)
  par_prerec_nc11_100_05[[e]] <- prerec(amInferred = par_nc11_100_05_pm[[e]], amTrue = true_nc11, cutoff = 0.5)
  par_prerec_nc11_100_1[[e]] <- prerec(amInferred = par_nc11_100_1_pm[[e]], amTrue = true_nc11, cutoff = 0.5)

  par_prerec_pc_100_02[[e]] <- prerec(amInferred = par_pc_100_02_pm[[e]], amTrue = true_pc, cutoff = 0.5)
  par_prerec_pc_100_05[[e]] <- prerec(amInferred = par_pc_100_05_pm[[e]], amTrue = true_pc, cutoff = 0.5)
  par_prerec_pc_100_1[[e]] <- prerec(amInferred = par_pc_100_1_pm[[e]], amTrue = true_pc, cutoff = 0.5)

  # MC3 --------------------------
  print("mc3")

  mc3_prerec_g2_100_02[[e]] <- prerec(amInferred = mc3_g2_100_02_fc_pm[[e]], amTrue = true_g2, cutoff = 0.5)
  mc3_prerec_g2_100_05[[e]] <- prerec(amInferred = mc3_g2_100_05_fc_pm[[e]], amTrue = true_g2, cutoff = 0.5)
  mc3_prerec_g2_100_1[[e]] <- prerec(amInferred = mc3_g2_100_1_fc_pm[[e]], amTrue = true_g2, cutoff = 0.5)

  mc3_prerec_nc11_100_02[[e]] <- prerec(amInferred = mc3_nc11_100_02_pm[[e]], amTrue = true_nc11, cutoff = 0.5)
  mc3_prerec_nc11_100_05[[e]] <- prerec(amInferred = mc3_nc11_100_05_pm[[e]], amTrue = true_nc11, cutoff = 0.5)
  mc3_prerec_nc11_100_1[[e]] <- prerec(amInferred = mc3_nc11_100_1_pm[[e]], amTrue = true_nc11, cutoff = 0.5)

  mc3_prerec_pc_100_02[[e]] <- prerec(amInferred = mc3_pc_100_02_pm[[e]], amTrue = true_pc, cutoff = 0.5)
  mc3_prerec_pc_100_05[[e]] <- prerec(amInferred = mc3_pc_100_05_pm[[e]], amTrue = true_pc, cutoff = 0.5)
  mc3_prerec_pc_100_1[[e]] <- prerec(amInferred = mc3_pc_100_1_pm[[e]], amTrue = true_pc, cutoff = 0.5)

  # scanBMA --------------------------
  print("scanBMA")

  bma_prerec_g2_100_02[[e]] <- prerec(amInferred = ppm_g2_100_02[[e]], amTrue = true_g2, cutoff = 0.5)
  bma_prerec_g2_100_05[[e]] <- prerec(amInferred = ppm_g2_100_05[[e]], amTrue = true_g2, cutoff = 0.5)
  bma_prerec_g2_100_1[[e]] <- prerec(amInferred = ppm_g2_100_1[[e]], amTrue = true_g2, cutoff = 0.5)

  bma_prerec_nc11_100_02[[e]] <- prerec(amInferred = ppm_nc11_100_02[[e]], amTrue = true_nc11, cutoff = 0.5)
  bma_prerec_nc11_100_05[[e]] <- prerec(amInferred = ppm_nc11_100_05[[e]], amTrue = true_nc11, cutoff = 0.5)
  bma_prerec_nc11_100_1[[e]] <- prerec(amInferred = ppm_nc11_100_1[[e]], amTrue = true_nc11, cutoff = 0.5)

  bma_prerec_pc_100_02[[e]] <- prerec(amInferred = ppm_pc_100_02[[e]], amTrue = true_pc, cutoff = 0.5)
  bma_prerec_pc_100_05[[e]] <- prerec(amInferred = ppm_pc_100_05[[e]], amTrue = true_pc, cutoff = 0.5)
  bma_prerec_pc_100_1[[e]] <- prerec(amInferred = ppm_pc_100_1[[e]], amTrue = true_pc, cutoff = 0.5)

  # bcdag --------------------------
  print("bcdag")

  bcdag_prerec_g2_100_02[[e]] <- prerec(amInferred = mat_g2_100_02[[e]], amTrue = true_g2, cutoff = 0.5)
  bcdag_prerec_g2_100_05[[e]] <- prerec(amInferred = mat_g2_100_05[[e]], amTrue = true_g2, cutoff = 0.5)
  bcdag_prerec_g2_100_1[[e]] <- prerec(amInferred = mat_g2_100_1[[e]], amTrue = true_g2, cutoff = 0.5)

  bcdag_prerec_nc11_100_02[[e]] <- prerec(amInferred = mat_nc11_100_02[[e]], amTrue = true_nc11, cutoff = 0.5)
  bcdag_prerec_nc11_100_05[[e]] <- prerec(amInferred = mat_nc11_100_05[[e]], amTrue = true_nc11, cutoff = 0.5)
  bcdag_prerec_nc11_100_1[[e]] <- prerec(amInferred = mat_nc11_100_1[[e]], amTrue = true_nc11, cutoff = 0.5)

  bcdag_prerec_pc_100_02[[e]] <- prerec(amInferred = mat_pc_100_02[[e]], amTrue = true_pc, cutoff = 0.5)
  bcdag_prerec_pc_100_05[[e]] <- prerec(amInferred = mat_pc_100_05[[e]], amTrue = true_pc, cutoff = 0.5)
  bcdag_prerec_pc_100_1[[e]] <- prerec(amInferred = mat_pc_100_1[[e]], amTrue = true_pc, cutoff = 0.5)

}

save(
     bay_prerec_g2_100_02,
     bay_prerec_g2_100_05,
     bay_prerec_g2_100_1,
     # bay_prerec_nc11_100_02,
     # bay_prerec_nc11_100_05,
     # bay_prerec_nc11_100_1,
     bay_prerec_pc_100_02,
     bay_prerec_pc_100_05,
     bay_prerec_pc_100_1,

     ord_prerec_g2_100_02,
     ord_prerec_g2_100_05,
     ord_prerec_g2_100_1,
     ord_prerec_nc11_100_02,
     ord_prerec_nc11_100_05,
     ord_prerec_nc11_100_1,
     ord_prerec_pc_100_02,
     ord_prerec_pc_100_05,
     ord_prerec_pc_100_1,

     par_prerec_g2_100_02,
     par_prerec_g2_100_05,
     par_prerec_g2_100_1,
     par_prerec_nc11_100_02,
     par_prerec_nc11_100_05,
     par_prerec_nc11_100_1,
     par_prerec_pc_100_02,
     par_prerec_pc_100_05,
     par_prerec_pc_100_1,

     mc3_prerec_g2_100_02,
     mc3_prerec_g2_100_05,
     mc3_prerec_g2_100_1,
     mc3_prerec_nc11_100_02,
     mc3_prerec_nc11_100_05,
     mc3_prerec_nc11_100_1,
     mc3_prerec_pc_100_02,
     mc3_prerec_pc_100_05,
     mc3_prerec_pc_100_1,

     bma_prerec_g2_100_02,
     bma_prerec_g2_100_05,
     bma_prerec_g2_100_1,
     bma_prerec_nc11_100_02,
     bma_prerec_nc11_100_05,
     bma_prerec_nc11_100_1,
     bma_prerec_pc_100_02,
     bma_prerec_pc_100_05,
     bma_prerec_pc_100_1,

     bcdag_prerec_g2_100_02,
     bcdag_prerec_g2_100_05,
     bcdag_prerec_g2_100_1,
     bcdag_prerec_nc11_100_02,
     bcdag_prerec_nc11_100_05,
     bcdag_prerec_nc11_100_1,
     bcdag_prerec_pc_100_02,
     bcdag_prerec_pc_100_05,
     bcdag_prerec_pc_100_1,

     file = "./venkata_prerec_all_100.RData"
)
