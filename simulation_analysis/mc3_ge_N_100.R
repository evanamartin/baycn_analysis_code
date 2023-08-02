library (parental,
         lib = '/mnt/lfs2/mart9986/Rpackages/')
library (structmcmc,
         lib = '/mnt/lfs2/mart9986/Rpackages/')

load('/mnt/lfs2/mart9986/data/data_ge_N_100.RData')

# Constrained matrix for topology G2. This will be used for the posterior
# function in the structmcmc package
cm_g2 <- matrix(c(0, 0, 0, -1,
                  0, 0, -1, 0,
                  0, -1, 0, 0,
                  -1, 0, 0, 0),
                byrow = TRUE,
                nrow = 4)

# Constrained matrix for topology NC11. This will be used for the posterior
# function in the structmcmc package
cm_nc11 <- matrix(c(0, 0, -1, -1, -1, -1, -1, -1, -1, -1, -1,
                    0, 0, 0, -1, -1, -1, -1, -1, -1, -1, -1,
                    -1, 0, 0, 0, -1, -1, -1, -1, -1, -1, -1,
                    -1, -1, 0, 0, 0, -1, -1, -1, -1, -1, -1,
                    -1, -1, -1, 0, 0, 0, -1, -1, -1, -1, -1,
                    -1, -1, -1, -1, 0, 0, 0, -1, -1, -1, -1,
                    -1, -1, -1, -1, -1, 0, 0, 0, -1, -1, -1,
                    -1, -1, -1, -1, -1, -1, 0, 0, 0, -1, -1,
                    -1, -1, -1, -1, -1, -1, -1, 0, 0, 0, -1,
                    -1, -1, -1, -1, -1, -1, -1, -1, 0, 0, 0,
                    -1, -1, -1, -1, -1, -1, -1, -1, -1, 0, 0),
                  byrow = TRUE,
                  nrow = 11)

# Constrained matrix for topology PC. This will be used for the posterior
# function in the structmcmc package
cm_pc <- matrix(c(0, 0, -1, -1, -1, 0, -1, 0,
                  0, 0, 0, -1, 0, -1, -1, -1,
                  -1, 0, 0, -1, -1, -1, -1, -1,
                  -1, -1, -1, 0, -1, -1, -1, -1,
                  -1, 0, -1, -1, 0, 0, -1, 0,
                  0, -1, -1, -1, 0, 0, 0, -1,
                  -1, -1, -1, -1, -1, 0, 0, -1,
                  0, -1, -1, -1, 0, -1, -1, 0),
                byrow = TRUE,
                nrow = 8)

# Initialize each list for the gibbs output to full length.
mc3_g2_100_02_fc <- vector(mode = 'list',
                           length = M)

mc3_g2_100_05_fc <- vector(mode = 'list',
                           length = M)

mc3_g2_100_1_fc <- vector(mode = 'list',
                          length = M)

mc3_g2_100_02 <- vector(mode = 'list',
                        length = M)

mc3_g2_100_05 <- vector(mode = 'list',
                        length = M)

mc3_g2_100_1 <- vector(mode = 'list',
                       length = M)

mc3_nc11_100_02 <- vector(mode = 'list',
                          length = M)

mc3_nc11_100_05 <- vector(mode = 'list',
                          length = M)

mc3_nc11_100_1 <- vector(mode = 'list',
                         length = M)

mc3_pc_100_02 <- vector(mode = 'list',
                        length = M)

mc3_pc_100_05 <- vector(mode = 'list',
                        length = M)

mc3_pc_100_1 <- vector(mode = 'list',
                       length = M)

# Initialize the probability matrix lists to full length.
mc3_g2_100_02_fc_pm <- vector(mode = 'list',
                              length = M)

mc3_g2_100_05_fc_pm <- vector(mode = 'list',
                              length = M)

mc3_g2_100_1_fc_pm <- vector(mode = 'list',
                             length = M)

mc3_g2_100_02_pm <- vector(mode = 'list',
                           length = M)

mc3_g2_100_05_pm <- vector(mode = 'list',
                           length = M)

mc3_g2_100_1_pm <- vector(mode = 'list',
                          length = M)

mc3_nc11_100_02_pm <- vector(mode = 'list',
                             length = M)

mc3_nc11_100_05_pm <- vector(mode = 'list',
                             length = M)

mc3_nc11_100_1_pm <- vector(mode = 'list',
                            length = M)

mc3_pc_100_02_pm <- vector(mode = 'list',
                           length = M)

mc3_pc_100_05_pm <- vector(mode = 'list',
                           length = M)

mc3_pc_100_1_pm <- vector(mode = 'list',
                          length = M)

# Initialize the time lists to full length.
mc3_g2_100_02_fc_time <- vector(mode = 'list',
                                length = M)

mc3_g2_100_05_fc_time <- vector(mode = 'list',
                                length = M)

mc3_g2_100_1_fc_time <- vector(mode = 'list',
                               length = M)

mc3_g2_100_02_time <- vector(mode = 'list',
                             length = M)

mc3_g2_100_05_time <- vector(mode = 'list',
                             length = M)

mc3_g2_100_1_time <- vector(mode = 'list',
                            length = M)

mc3_nc11_100_02_time <- vector(mode = 'list',
                               length = M)

mc3_nc11_100_05_time <- vector(mode = 'list',
                               length = M)

mc3_nc11_100_1_time <- vector(mode = 'list',
                              length = M)

mc3_pc_100_02_time <- vector(mode = 'list',
                             length = M)

mc3_pc_100_05_time <- vector(mode = 'list',
                             length = M)

mc3_pc_100_1_time <- vector(mode = 'list',
                            length = M)

set.seed(101)

# Loop through each combination of signal strength and sample size for all
# topologies.
for (e in 1:M) {

  #################################################
  # G2 FULLY CONNECTED
  #################################################

  starttime <- Sys.time()
  mc3_g2_100_02_fc[[e]] <- posterior(data = data_g2_100_02[[e]],
                                     method = 'mc3',
                                     logScoreFUN = logScoreNormalFUN(),
                                     nSamples = 30000,
                                     nBurnin = 6000,
                                     verbose = FALSE)
  endtime <- Sys.time()
  mc3_g2_100_02_fc_time[[e]] <- endtime - starttime
  mc3_g2_100_02_fc_pm[[e]] <- ep(mc3_g2_100_02_fc[[e]])

  starttime <- Sys.time()
  mc3_g2_100_05_fc[[e]] <- posterior(data = data_g2_100_05[[e]],
                                     method = 'mc3',
                                     logScoreFUN = logScoreNormalFUN(),
                                     nSamples = 30000,
                                     nBurnin = 6000,
                                     verbose = FALSE)
  endtime <- Sys.time()
  mc3_g2_100_05_fc_time[[e]] <- endtime - starttime
  mc3_g2_100_05_fc_pm[[e]] <- ep(mc3_g2_100_05_fc[[e]])

  starttime <- Sys.time()
  mc3_g2_100_1_fc[[e]] <- posterior(data = data_g2_100_1[[e]],
                                    method = 'mc3',
                                    logScoreFUN = logScoreNormalFUN(),
                                    nSamples = 30000,
                                    nBurnin = 6000,
                                    verbose = FALSE)
  endtime <- Sys.time()
  mc3_g2_100_1_fc_time[[e]] <- endtime - starttime
  mc3_g2_100_1_fc_pm[[e]] <- ep(mc3_g2_100_1_fc[[e]])

  print('g2 fully connected')
  print(Sys.time())

  #################################################
  # G2 TRUE EDGES
  #################################################

  starttime <- Sys.time()
  mc3_g2_100_02[[e]] <- posterior(data = data_g2_100_02[[e]],
                                  method = 'mc3',
                                  logScoreFUN = logScoreNormalFUN(),
                                  constraint = cm_g2,
                                  nSamples = 30000,
                                  nBurnin = 6000,
                                  verbose = FALSE)
  endtime <- Sys.time()
  mc3_g2_100_02_time[[e]] <- endtime - starttime
  mc3_g2_100_02_pm[[e]] <- ep(mc3_g2_100_02[[e]])

  starttime <- Sys.time()
  mc3_g2_100_05[[e]] <- posterior(data = data_g2_100_05[[e]],
                                  method = 'mc3',
                                  logScoreFUN = logScoreNormalFUN(),
                                  constraint = cm_g2,
                                  nSamples = 30000,
                                  nBurnin = 6000,
                                  verbose = FALSE)
  endtime <- Sys.time()
  mc3_g2_100_05_time[[e]] <- endtime - starttime
  mc3_g2_100_05_pm[[e]] <- ep(mc3_g2_100_05[[e]])

  starttime <- Sys.time()
  mc3_g2_100_1[[e]] <- posterior(data = data_g2_100_1[[e]],
                                 method = 'mc3',
                                 logScoreFUN = logScoreNormalFUN(),
                                 constraint = cm_g2,
                                 nSamples = 30000,
                                 nBurnin = 6000,
                                 verbose = FALSE)
  endtime <- Sys.time()
  mc3_g2_100_1_time[[e]] <- endtime - starttime
  mc3_g2_100_1_pm[[e]] <- ep(mc3_g2_100_1[[e]])

  print('g2 true edges')
  print(Sys.time())

  #################################################
  # NC11
  #################################################

  starttime <- Sys.time()
  mc3_nc11_100_02[[e]] <- posterior(data = data_nc11_100_02[[e]],
                                    method = 'mc3',
                                    logScoreFUN = logScoreNormalFUN(),
                                    constraint = cm_nc11,
                                    nSamples = 50000,
                                    nBurnin = 10000,
                                    verbose = FALSE)
  endtime <- Sys.time()
  mc3_nc11_100_02_time[[e]] <- endtime - starttime
  mc3_nc11_100_02_pm[[e]] <- ep(mc3_nc11_100_02[[e]])

  starttime <- Sys.time()
  mc3_nc11_100_05[[e]] <- posterior(data = data_nc11_100_05[[e]],
                                    method = 'mc3',
                                    logScoreFUN = logScoreNormalFUN(),
                                    constraint = cm_nc11,
                                    nSamples = 50000,
                                    nBurnin = 10000,
                                    verbose = FALSE)
  endtime <- Sys.time()
  mc3_nc11_100_05_time[[e]] <- endtime - starttime
  mc3_nc11_100_05_pm[[e]] <- ep(mc3_nc11_100_05[[e]])

  starttime <- Sys.time()
  mc3_nc11_100_1[[e]] <- posterior(data = data_nc11_100_1[[e]],
                                   method = 'mc3',
                                   logScoreFUN = logScoreNormalFUN(),
                                   constraint = cm_nc11,
                                   nSamples = 50000,
                                   nBurnin = 10000,
                                   verbose = FALSE)
  endtime <- Sys.time()
  mc3_nc11_100_1_time[[e]] <- endtime - starttime
  mc3_nc11_100_1_pm[[e]] <- ep(mc3_nc11_100_1[[e]])

  print('nc11')
  print(Sys.time())

  #################################################
  # PC
  #################################################

  starttime <- Sys.time()
  mc3_pc_100_02[[e]] <- posterior(data = data_pc_100_02[[e]],
                                  method = 'mc3',
                                  logScoreFUN = logScoreNormalFUN(),
                                  constraint = cm_pc,
                                  nSamples = 50000,
                                  nBurnin = 10000,
                                  verbose = FALSE)
  endtime <- Sys.time()
  mc3_pc_100_02_time[[e]] <- endtime - starttime
  mc3_pc_100_02_pm[[e]] <- ep(mc3_pc_100_02[[e]])

  starttime <- Sys.time()
  mc3_pc_100_05[[e]] <- posterior(data = data_pc_100_05[[e]],
                                  method = 'mc3',
                                  logScoreFUN = logScoreNormalFUN(),
                                  constraint = cm_pc,
                                  nSamples = 50000,
                                  nBurnin = 10000,
                                  verbose = FALSE)
  endtime <- Sys.time()
  mc3_pc_100_05_time[[e]] <- endtime - starttime
  mc3_pc_100_05_pm[[e]] <- ep(mc3_pc_100_05[[e]])

  starttime <- Sys.time()
  mc3_pc_100_1[[e]] <- posterior(data = data_pc_100_1[[e]],
                                 method = 'mc3',
                                 logScoreFUN = logScoreNormalFUN(),
                                 constraint = cm_pc,
                                 nSamples = 50000,
                                 nBurnin = 10000,
                                 verbose = FALSE)
  endtime <- Sys.time()
  mc3_pc_100_1_time[[e]] <- endtime - starttime
  mc3_pc_100_1_pm[[e]] <- ep(mc3_pc_100_1[[e]])

  print('pc')
  print(e)
  print(Sys.time())

  save(M,
       mc3_g2_100_02_fc,
       mc3_g2_100_05_fc,
       mc3_g2_100_1_fc,
       mc3_g2_100_02,
       mc3_g2_100_05,
       mc3_g2_100_1,
       mc3_nc11_100_02,
       mc3_nc11_100_05,
       mc3_nc11_100_1,
       mc3_pc_100_02,
       mc3_pc_100_05,
       mc3_pc_100_1,
       file = '/mnt/lfs2/mart9986/structmcmc/mc3_ge_N_100.RData')

  save(M,
       mc3_g2_100_02_fc_pm,
       mc3_g2_100_05_fc_pm,
       mc3_g2_100_1_fc_pm,
       mc3_g2_100_02_pm,
       mc3_g2_100_05_pm,
       mc3_g2_100_1_pm,
       mc3_nc11_100_02_pm,
       mc3_nc11_100_05_pm,
       mc3_nc11_100_1_pm,
       mc3_pc_100_02_pm,
       mc3_pc_100_05_pm,
       mc3_pc_100_1_pm,
       file = '/mnt/lfs2/mart9986/structmcmc/mc3_ge_N_100_pm.RData')

  save(M,
       mc3_g2_100_02_fc_time,
       mc3_g2_100_05_fc_time,
       mc3_g2_100_1_fc_time,
       mc3_g2_100_02_time,
       mc3_g2_100_05_time,
       mc3_g2_100_1_time,
       mc3_nc11_100_02_time,
       mc3_nc11_100_05_time,
       mc3_nc11_100_1_time,
       mc3_pc_100_02_time,
       mc3_pc_100_05_time,
       mc3_pc_100_1_time,
       file = '/mnt/lfs2/mart9986/structmcmc/mc3_ge_N_100_time.RData')

}
