library (parental,
)
library (structmcmc,
)

load('./data_ge_N_200.RData')

# Constrained matrices removed. Default behavior is to model Fully Connected

# ---------------------------------

# Initialize each list for the gibbs output to full length.
mc3_g2_200_02_fc <- vector(mode = 'list',
                           length = M)

mc3_g2_200_05_fc <- vector(mode = 'list',
                           length = M)

mc3_g2_200_1_fc <- vector(mode = 'list',
                          length = M)

mc3_g2_200_02 <- vector(mode = 'list',
                        length = M)

mc3_g2_200_05 <- vector(mode = 'list',
                        length = M)

mc3_g2_200_1 <- vector(mode = 'list',
                       length = M)

mc3_nc11_200_02 <- vector(mode = 'list',
                          length = M)

mc3_nc11_200_05 <- vector(mode = 'list',
                          length = M)

mc3_nc11_200_1 <- vector(mode = 'list',
                         length = M)

mc3_pc_200_02 <- vector(mode = 'list',
                        length = M)

mc3_pc_200_05 <- vector(mode = 'list',
                        length = M)

mc3_pc_200_1 <- vector(mode = 'list',
                       length = M)

# Initialize the probability matrix lists to full length.
mc3_g2_200_02_fc_pm <- vector(mode = 'list',
                              length = M)

mc3_g2_200_05_fc_pm <- vector(mode = 'list',
                              length = M)

mc3_g2_200_1_fc_pm <- vector(mode = 'list',
                             length = M)

mc3_g2_200_02_pm <- vector(mode = 'list',
                           length = M)

mc3_g2_200_05_pm <- vector(mode = 'list',
                           length = M)

mc3_g2_200_1_pm <- vector(mode = 'list',
                          length = M)

mc3_nc11_200_02_pm <- vector(mode = 'list',
                             length = M)

mc3_nc11_200_05_pm <- vector(mode = 'list',
                             length = M)

mc3_nc11_200_1_pm <- vector(mode = 'list',
                            length = M)

mc3_pc_200_02_pm <- vector(mode = 'list',
                           length = M)

mc3_pc_200_05_pm <- vector(mode = 'list',
                           length = M)

mc3_pc_200_1_pm <- vector(mode = 'list',
                          length = M)

# Initialize the time lists to full length.
mc3_g2_200_02_fc_time <- vector(mode = 'list',
                                length = M)

mc3_g2_200_05_fc_time <- vector(mode = 'list',
                                length = M)

mc3_g2_200_1_fc_time <- vector(mode = 'list',
                               length = M)

mc3_g2_200_02_time <- vector(mode = 'list',
                             length = M)

mc3_g2_200_05_time <- vector(mode = 'list',
                             length = M)

mc3_g2_200_1_time <- vector(mode = 'list',
                            length = M)

mc3_nc11_200_02_time <- vector(mode = 'list',
                               length = M)

mc3_nc11_200_05_time <- vector(mode = 'list',
                               length = M)

mc3_nc11_200_1_time <- vector(mode = 'list',
                              length = M)

mc3_pc_200_02_time <- vector(mode = 'list',
                             length = M)

mc3_pc_200_05_time <- vector(mode = 'list',
                             length = M)

mc3_pc_200_1_time <- vector(mode = 'list',
                            length = M)

set.seed(201)

# Loop through each combination of signal strength and sample size for all
# topologies.
for (e in 1:M) {

  #################################################
  # G2 FULLY CONNECTED
  #################################################

  starttime <- Sys.time()
  mc3_g2_200_02_fc[[e]] <- posterior(data = data_g2_200_02[[e]],
                                     method = 'mc3',
                                     logScoreFUN = logScoreNormalFUN(),
                                     nSamples = 30000,
                                     nBurnin = 6000,
                                     verbose = FALSE)
  endtime <- Sys.time()
  mc3_g2_200_02_fc_time[[e]] <- endtime - starttime
  mc3_g2_200_02_fc_pm[[e]] <- ep(mc3_g2_200_02_fc[[e]])

  starttime <- Sys.time()
  mc3_g2_200_05_fc[[e]] <- posterior(data = data_g2_200_05[[e]],
                                     method = 'mc3',
                                     logScoreFUN = logScoreNormalFUN(),
                                     nSamples = 30000,
                                     nBurnin = 6000,
                                     verbose = FALSE)
  endtime <- Sys.time()
  mc3_g2_200_05_fc_time[[e]] <- endtime - starttime
  mc3_g2_200_05_fc_pm[[e]] <- ep(mc3_g2_200_05_fc[[e]])

  starttime <- Sys.time()
  mc3_g2_200_1_fc[[e]] <- posterior(data = data_g2_200_1[[e]],
                                    method = 'mc3',
                                    logScoreFUN = logScoreNormalFUN(),
                                    nSamples = 30000,
                                    nBurnin = 6000,
                                    verbose = FALSE)
  endtime <- Sys.time()
  mc3_g2_200_1_fc_time[[e]] <- endtime - starttime
  mc3_g2_200_1_fc_pm[[e]] <- ep(mc3_g2_200_1_fc[[e]])

  print(Sys.time())

  #################################################
  # NC11
  #################################################

  starttime <- Sys.time()
  mc3_nc11_200_02[[e]] <- posterior(data = data_nc11_200_02[[e]],
                                    method = 'mc3',
                                    logScoreFUN = logScoreNormalFUN(),
                                    nSamples = 50000,
                                    nBurnin = 10000,
                                    verbose = FALSE)
  endtime <- Sys.time()
  mc3_nc11_200_02_time[[e]] <- endtime - starttime
  mc3_nc11_200_02_pm[[e]] <- ep(mc3_nc11_200_02[[e]])

  starttime <- Sys.time()
  mc3_nc11_200_05[[e]] <- posterior(data = data_nc11_200_05[[e]],
                                    method = 'mc3',
                                    logScoreFUN = logScoreNormalFUN(),
                                    nSamples = 50000,
                                    nBurnin = 10000,
                                    verbose = FALSE)
  endtime <- Sys.time()
  mc3_nc11_200_05_time[[e]] <- endtime - starttime
  mc3_nc11_200_05_pm[[e]] <- ep(mc3_nc11_200_05[[e]])

  starttime <- Sys.time()
  mc3_nc11_200_1[[e]] <- posterior(data = data_nc11_200_1[[e]],
                                   method = 'mc3',
                                   logScoreFUN = logScoreNormalFUN(),
                                   nSamples = 50000,
                                   nBurnin = 10000,
                                   verbose = FALSE)
  endtime <- Sys.time()
  mc3_nc11_200_1_time[[e]] <- endtime - starttime
  mc3_nc11_200_1_pm[[e]] <- ep(mc3_nc11_200_1[[e]])

  print('nc11')
  print(Sys.time())

  #################################################
  # PC
  #################################################

  starttime <- Sys.time()
  mc3_pc_200_02[[e]] <- posterior(data = data_pc_200_02[[e]],
                                  method = 'mc3',
                                  logScoreFUN = logScoreNormalFUN(),
                                  nSamples = 50000,
                                  nBurnin = 10000,
                                  verbose = FALSE)
  endtime <- Sys.time()
  mc3_pc_200_02_time[[e]] <- endtime - starttime
  mc3_pc_200_02_pm[[e]] <- ep(mc3_pc_200_02[[e]])

  starttime <- Sys.time()
  mc3_pc_200_05[[e]] <- posterior(data = data_pc_200_05[[e]],
                                  method = 'mc3',
                                  logScoreFUN = logScoreNormalFUN(),
                                  nSamples = 50000,
                                  nBurnin = 10000,
                                  verbose = FALSE)
  endtime <- Sys.time()
  mc3_pc_200_05_time[[e]] <- endtime - starttime
  mc3_pc_200_05_pm[[e]] <- ep(mc3_pc_200_05[[e]])

  starttime <- Sys.time()
  mc3_pc_200_1[[e]] <- posterior(data = data_pc_200_1[[e]],
                                 method = 'mc3',
                                 logScoreFUN = logScoreNormalFUN(),
                                 nSamples = 50000,
                                 nBurnin = 10000,
                                 verbose = FALSE)
  endtime <- Sys.time()
  mc3_pc_200_1_time[[e]] <- endtime - starttime
  mc3_pc_200_1_pm[[e]] <- ep(mc3_pc_200_1[[e]])

  print('pc')
  print(e)
  print(Sys.time())

  save(M,
       mc3_g2_200_02_fc,
       mc3_g2_200_05_fc,
       mc3_g2_200_1_fc,
       mc3_g2_200_02,
       mc3_g2_200_05,
       mc3_g2_200_1,
       mc3_nc11_200_02,
       mc3_nc11_200_05,
       mc3_nc11_200_1,
       mc3_pc_200_02,
       mc3_pc_200_05,
       mc3_pc_200_1,
       file = './mc3_ge_N_200.RData')

  save(M,
       mc3_g2_200_02_fc_pm,
       mc3_g2_200_05_fc_pm,
       mc3_g2_200_1_fc_pm,
       mc3_g2_200_02_pm,
       mc3_g2_200_05_pm,
       mc3_g2_200_1_pm,
       mc3_nc11_200_02_pm,
       mc3_nc11_200_05_pm,
       mc3_nc11_200_1_pm,
       mc3_pc_200_02_pm,
       mc3_pc_200_05_pm,
       mc3_pc_200_1_pm,
       file = './mc3_ge_N_200_pm.RData')

  save(M,
       mc3_g2_200_02_fc_time,
       mc3_g2_200_05_fc_time,
       mc3_g2_200_1_fc_time,
       mc3_g2_200_02_time,
       mc3_g2_200_05_time,
       mc3_g2_200_1_time,
       mc3_nc11_200_02_time,
       mc3_nc11_200_05_time,
       mc3_nc11_200_1_time,
       mc3_pc_200_02_time,
       mc3_pc_200_05_time,
       mc3_pc_200_1_time,
       file = './mc3_ge_N_200_time.RData')

}
