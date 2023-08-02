library (BiDAG,
         lib = '/mnt/lfs2/mart9986/Rpackages')

# load the data
load('/mnt/lfs2/mart9986/data/data_ge_N_200.RData')

# Undirected adjacency matrix with the true edges for topology G2.
am_g2 <- matrix(c(0, 1, 1, 0,
                  1, 0, 0, 1,
                  1, 0, 0, 1,
                  0, 1, 1, 0),
                byrow = TRUE,
                nrow = 4)

# Undirected adjacency matrix with the true edges for toplogy NC11.
am_nc11 <- matrix(c(0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,
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
am_pc <- matrix(c(0, 1, 0, 0, 0, 1, 0, 1,
                  1, 0, 1, 0, 1, 0, 0, 0,
                  0, 1, 0, 0, 0, 0, 0, 0,
                  0, 0, 0, 0, 0, 0, 0, 0,
                  0, 1, 0, 0, 0, 1, 0, 1,
                  1, 0, 0, 0, 1, 0, 1, 0,
                  0, 0, 0, 0, 0, 1, 0, 0,
                  1, 0, 0, 0, 1, 0, 0, 0),
                byrow = TRUE,
                nrow = 8)

# Initiate lists for the output of each method to full length.
ord_g2_200_02_fc <- vector(mode = 'list',
                           length = M)

par_g2_200_02_fc <- vector(mode = 'list',
                           length = M)

ord_g2_200_05_fc <- vector(mode = 'list',
                           length = M)

par_g2_200_05_fc <- vector(mode = 'list',
                           length = M)

ord_g2_200_1_fc <- vector(mode = 'list',
                          length = M)

par_g2_200_1_fc <- vector(mode = 'list',
                          length = M)

ord_g2_200_02 <- vector(mode = 'list',
                        length = M)

par_g2_200_02 <- vector(mode = 'list',
                        length = M)

ord_g2_200_05 <- vector(mode = 'list',
                        length = M)

par_g2_200_05 <- vector(mode = 'list',
                        length = M)

ord_g2_200_1 <- vector(mode = 'list',
                       length = M)

par_g2_200_1 <- vector(mode = 'list',
                       length = M)

ord_nc11_200_02 <- vector(mode = 'list',
                          length = M)

par_nc11_200_02 <- vector(mode = 'list',
                          length = M)

ord_nc11_200_05 <- vector(mode = 'list',
                          length = M)

par_nc11_200_05 <- vector(mode = 'list',
                          length = M)

ord_nc11_200_1 <- vector(mode = 'list',
                         length = M)

par_nc11_200_1 <- vector(mode = 'list',
                         length = M)

ord_pc_200_02 <- vector(mode = 'list',
                        length = M)

par_pc_200_02 <- vector(mode = 'list',
                        length = M)

ord_pc_200_05 <- vector(mode = 'list',
                        length = M)

par_pc_200_05 <- vector(mode = 'list',
                        length = M)

ord_pc_200_1 <- vector(mode = 'list',
                       length = M)

par_pc_200_1 <- vector(mode = 'list',
                       length = M)

# Initialize the lists for the probability matrix to full length.
ord_g2_200_02_fc_pm <- vector(mode = 'list',
                              length = M)

par_g2_200_02_fc_pm <- vector(mode = 'list',
                              length = M)

ord_g2_200_05_fc_pm <- vector(mode = 'list',
                              length = M)

par_g2_200_05_fc_pm <- vector(mode = 'list',
                              length = M)

ord_g2_200_1_fc_pm <- vector(mode = 'list',
                             length = M)

par_g2_200_1_fc_pm <- vector(mode = 'list',
                             length = M)

ord_g2_200_02_pm <- vector(mode = 'list',
                           length = M)

par_g2_200_02_pm <- vector(mode = 'list',
                           length = M)

ord_g2_200_05_pm <- vector(mode = 'list',
                           length = M)

par_g2_200_05_pm <- vector(mode = 'list',
                           length = M)

ord_g2_200_1_pm <- vector(mode = 'list',
                          length = M)

par_g2_200_1_pm <- vector(mode = 'list',
                          length = M)

ord_nc11_200_02_pm <- vector(mode = 'list',
                             length = M)

par_nc11_200_02_pm <- vector(mode = 'list',
                             length = M)

ord_nc11_200_05_pm <- vector(mode = 'list',
                             length = M)

par_nc11_200_05_pm <- vector(mode = 'list',
                             length = M)

ord_nc11_200_1_pm <- vector(mode = 'list',
                            length = M)

par_nc11_200_1_pm <- vector(mode = 'list',
                            length = M)

ord_pc_200_02_pm <- vector(mode = 'list',
                           length = M)

par_pc_200_02_pm <- vector(mode = 'list',
                           length = M)

ord_pc_200_05_pm <- vector(mode = 'list',
                           length = M)

par_pc_200_05_pm <- vector(mode = 'list',
                           length = M)

ord_pc_200_1_pm <- vector(mode = 'list',
                          length = M)

par_pc_200_1_pm <- vector(mode = 'list',
                          length = M)

# Initialize the time lists to full length.
ord_g2_200_02_fc_time <- vector(mode = 'list',
                                length = M)

par_g2_200_02_fc_time <- vector(mode = 'list',
                                length = M)

ord_g2_200_05_fc_time <- vector(mode = 'list',
                                length = M)

par_g2_200_05_fc_time <- vector(mode = 'list',
                                length = M)

ord_g2_200_1_fc_time <- vector(mode = 'list',
                               length = M)

par_g2_200_1_fc_time <- vector(mode = 'list',
                               length = M)

ord_g2_200_02_time <- vector(mode = 'list',
                             length = M)

par_g2_200_02_time <- vector(mode = 'list',
                             length = M)

ord_g2_200_05_time <- vector(mode = 'list',
                             length = M)

par_g2_200_05_time <- vector(mode = 'list',
                             length = M)

ord_g2_200_1_time <- vector(mode = 'list',
                            length = M)

par_g2_200_1_time <- vector(mode = 'list',
                            length = M)

ord_nc11_200_02_time <- vector(mode = 'list',
                               length = M)

par_nc11_200_02_time <- vector(mode = 'list',
                               length = M)

ord_nc11_200_05_time <- vector(mode = 'list',
                               length = M)

par_nc11_200_05_time <- vector(mode = 'list',
                               length = M)

ord_nc11_200_1_time <- vector(mode = 'list',
                              length = M)

par_nc11_200_1_time <- vector(mode = 'list',
                              length = M)

ord_pc_200_02_time <- vector(mode = 'list',
                             length = M)

par_pc_200_02_time <- vector(mode = 'list',
                             length = M)

ord_pc_200_05_time <- vector(mode = 'list',
                             length = M)

par_pc_200_05_time <- vector(mode = 'list',
                             length = M)

ord_pc_200_1_time <- vector(mode = 'list',
                            length = M)

par_pc_200_1_time <- vector(mode = 'list',
                            length = M)

set.seed(201)

# Loop through each combination of signal strength and sample size for all
# topologies.
for (e in 1:M) {

  #################################################
  # ORDER AND PARTITION
  # G2 FULLY CONNECTED, TRUE EDGES
  # 200
  # 0.2
  #################################################

  score_g2_200_02 <- scoreparameters(4,
                                     "bge",
                                     data_g2_200_02[[e]])

  starttime <- Sys.time()
  ord_g2_200_02_fc[[e]] <- orderMCMC(iterations = 30000,
                                     n = 4,
                                     scoreparam = score_g2_200_02,
                                     chainout = TRUE)
  endtime <- Sys.time()
  ord_g2_200_02_fc_time[[e]] <- endtime - starttime
  ord_g2_200_02_fc_pm[[e]] <- edges.posterior(ord_g2_200_02_fc[[e]]$c$i,
                                              burnin = 0.2)

  starttime <- Sys.time()
  par_g2_200_02_fc[[e]] <- partitionMCMC(iterations = 30000,
                                         n = 4,
                                         scoreparam = score_g2_200_02,
                                         verbose = FALSE)
  endtime <- Sys.time()
  par_g2_200_02_fc_time[[e]] <- endtime - starttime
  par_g2_200_02_fc_pm[[e]] <- edges.posterior(par_g2_200_02_fc[[e]]$c$i,
                                              burnin = 0.2)

  starttime <- Sys.time()
  ord_g2_200_02[[e]] <- orderMCMC(iterations = 30000,
                                  n = 4,
                                  scoreparam = score_g2_200_02,
                                  startspace = am_g2,
                                  chainout = TRUE)
  endtime <- Sys.time()
  ord_g2_200_02_time[[e]] <- endtime - starttime
  ord_g2_200_02_pm[[e]] <- edges.posterior(ord_g2_200_02[[e]]$c$i,
                                           burnin = 0.2)

  starttime <- Sys.time()
  par_g2_200_02[[e]] <- partitionMCMC(iterations = 30000,
                                      n = 4,
                                      scoreparam = score_g2_200_02,
                                      startspace = am_g2,
                                      verbose = FALSE)
  endtime <- Sys.time()
  par_g2_200_02_time[[e]] <- endtime - starttime
  par_g2_200_02_pm[[e]] <- edges.posterior(par_g2_200_02[[e]]$c$i,
                                           burnin = 0.2)


  #################################################
  # ORDER AND PARTITION
  # G2 FULLY CONNECTED, TRUE EDGES
  # 200
  # 0.5
  #################################################

  score_g2_200_05 <- scoreparameters(4,
                                     "bge",
                                     data_g2_200_05[[e]])

  starttime <- Sys.time()
  ord_g2_200_05_fc[[e]] <- orderMCMC(iterations = 30000,
                                     n = 4,
                                     scoreparam = score_g2_200_05,
                                     chainout = TRUE)
  endtime <- Sys.time()
  ord_g2_200_05_fc_time[[e]] <- endtime - starttime
  ord_g2_200_05_fc_pm[[e]] <- edges.posterior(ord_g2_200_05_fc[[e]]$c$i,
                                              burnin = 0.2)

  starttime <- Sys.time()
  par_g2_200_05_fc[[e]] <- partitionMCMC(iterations = 30000,
                                         n = 4,
                                         scoreparam = score_g2_200_05,
                                         verbose = FALSE)
  endtime <- Sys.time()
  par_g2_200_05_fc_time[[e]] <- endtime - starttime
  par_g2_200_05_fc_pm[[e]] <- edges.posterior(par_g2_200_05_fc[[e]]$c$i,
                                              burnin = 0.2)

  starttime <- Sys.time()
  ord_g2_200_05[[e]] <- orderMCMC(iterations = 30000,
                                  n = 4,
                                  scoreparam = score_g2_200_05,
                                  startspace = am_g2,
                                  chainout = TRUE)
  endtime <- Sys.time()
  ord_g2_200_05_time[[e]] <- endtime - starttime
  ord_g2_200_05_pm[[e]] <- edges.posterior(ord_g2_200_05[[e]]$c$i,
                                           burnin = 0.2)

  starttime <- Sys.time()
  par_g2_200_05[[e]] <- partitionMCMC(iterations = 30000,
                                      n = 4,
                                      scoreparam = score_g2_200_05,
                                      startspace = am_g2,
                                      verbose = FALSE)
  endtime <- Sys.time()
  par_g2_200_05_time[[e]] <- endtime - starttime
  par_g2_200_05_pm[[e]] <- edges.posterior(par_g2_200_05[[e]]$c$i,
                                           burnin = 0.2)

  #################################################
  # ORDER AND PARTITION
  # G2 FULLY CONNECTED, TRUE EDGES
  # 200
  # 1
  #################################################

  score_g2_200_1 <- scoreparameters(4,
                                    "bge",
                                    data_g2_200_1[[e]])

  starttime <- Sys.time()
  ord_g2_200_1_fc[[e]] <- orderMCMC(iterations = 30000,
                                    n = 4,
                                    scoreparam = score_g2_200_1,
                                    chainout = TRUE)
  endtime <- Sys.time()
  ord_g2_200_1_fc_time[[e]] <- endtime - starttime
  ord_g2_200_1_fc_pm[[e]] <- edges.posterior(ord_g2_200_1_fc[[e]]$c$i,
                                             burnin = 0.2)

  starttime <- Sys.time()
  par_g2_200_1_fc[[e]] <- partitionMCMC(iterations = 30000,
                                        n = 4,
                                        scoreparam = score_g2_200_1,
                                        verbose = FALSE)
  endtime <- Sys.time()
  par_g2_200_1_fc_time[[e]] <- endtime - starttime
  par_g2_200_1_fc_pm[[e]] <- edges.posterior(par_g2_200_1_fc[[e]]$c$i,
                                             burnin = 0.2)

  starttime <- Sys.time()
  ord_g2_200_1[[e]] <- orderMCMC(iterations = 30000,
                                 n = 4,
                                 scoreparam = score_g2_200_1,
                                 startspace = am_g2,
                                 chainout = TRUE)
  endtime <- Sys.time()
  ord_g2_200_1_time[[e]] <- endtime - starttime
  ord_g2_200_1_pm[[e]] <- edges.posterior(ord_g2_200_1[[e]]$c$i,
                                          burnin = 0.2)

  starttime <- Sys.time()
  par_g2_200_1[[e]] <- partitionMCMC(iterations = 30000,
                                     n = 4,
                                     scoreparam = score_g2_200_1,
                                     startspace = am_g2,
                                     verbose = FALSE)
  endtime <- Sys.time()
  par_g2_200_1_time[[e]] <- endtime - starttime
  par_g2_200_1_pm[[e]] <- edges.posterior(par_g2_200_1[[e]]$c$i,
                                          burnin = 0.2)

  #################################################
  # ORDER AND PARTITION
  # NC11
  # 200
  # 0.2
  #################################################

  score_nc11_200_02 <- scoreparameters(11,
                                       "bge",
                                       data_nc11_200_02[[e]])

  starttime <- Sys.time()
  ord_nc11_200_02[[e]] <- orderMCMC(iterations = 30000,
                                    n = 11,
                                    scoreparam = score_nc11_200_02,
                                    startspace = am_nc11,
                                    chainout = TRUE)
  endtime <- Sys.time()
  ord_nc11_200_02_time[[e]] <- endtime - starttime
  ord_nc11_200_02_pm[[e]] <- edges.posterior(ord_nc11_200_02[[e]]$c$i,
                                             burnin = 0.2)

  starttime <- Sys.time()
  par_nc11_200_02[[e]] <- partitionMCMC(iterations = 30000,
                                        n = 11,
                                        scoreparam = score_nc11_200_02,
                                        startspace = am_nc11,
                                        verbose = FALSE)
  endtime <- Sys.time()
  par_nc11_200_02_time[[e]] <- endtime - starttime
  par_nc11_200_02_pm[[e]] <- edges.posterior(par_nc11_200_02[[e]]$c$i,
                                             burnin = 0.2)

  #################################################
  # ORDER AND PARTITION
  # NC11
  # 200
  # 0.5
  #################################################

  score_nc11_200_05 <- scoreparameters(11,
                                       "bge",
                                       data_nc11_200_05[[e]])

  starttime <- Sys.time()
  ord_nc11_200_05[[e]] <- orderMCMC(iterations = 30000,
                                    n = 11,
                                    scoreparam = score_nc11_200_05,
                                    startspace = am_nc11,
                                    chainout = TRUE)
  endtime <- Sys.time()
  ord_nc11_200_05_time[[e]] <- endtime - starttime
  ord_nc11_200_05_pm[[e]] <- edges.posterior(ord_nc11_200_05[[e]]$c$i,
                                             burnin = 0.2)

  starttime <- Sys.time()
  par_nc11_200_05[[e]] <- partitionMCMC(iterations = 30000,
                                        n = 11,
                                        scoreparam = score_nc11_200_05,
                                        startspace = am_nc11,
                                        verbose = FALSE)
  endtime <- Sys.time()
  par_nc11_200_05_time[[e]] <- endtime - starttime
  par_nc11_200_05_pm[[e]] <- edges.posterior(par_nc11_200_05[[e]]$c$i,
                                             burnin = 0.2)

  #################################################
  # ORDER AND PARTITION
  # NC11
  # 200
  # 1
  #################################################

  score_nc11_200_1 <- scoreparameters(11,
                                      "bge",
                                      data_nc11_200_1[[e]])

  starttime <- Sys.time()
  ord_nc11_200_1[[e]] <- orderMCMC(iterations = 30000,
                                   n = 11,
                                   scoreparam = score_nc11_200_1,
                                   startspace = am_nc11,
                                   chainout = TRUE)
  endtime <- Sys.time()
  ord_nc11_200_1_time[[e]] <- endtime - starttime
  ord_nc11_200_1_pm[[e]] <- edges.posterior(ord_nc11_200_1[[e]]$c$i,
                                            burnin = 0.2)

  starttime <- Sys.time()
  par_nc11_200_1[[e]] <- partitionMCMC(iterations = 30000,
                                       n = 11,
                                       scoreparam = score_nc11_200_1,
                                       startspace = am_nc11,
                                       verbose = FALSE)
  endtime <- Sys.time()
  par_nc11_200_1_time[[e]] <- endtime - starttime
  par_nc11_200_1_pm[[e]] <- edges.posterior(par_nc11_200_1[[e]]$c$i,
                                            burnin = 0.2)

  #################################################
  # ORDER AND PARTITION
  # PC
  # 200
  # 0.2
  #################################################

  score_pc_200_02 <- scoreparameters(8,
                                     "bge",
                                     data_pc_200_02[[e]])

  starttime <- Sys.time()
  ord_pc_200_02[[e]] <- orderMCMC(iterations = 30000,
                                  n = 8,
                                  scoreparam = score_pc_200_02,
                                  startspace = am_pc,
                                  chainout = TRUE)
  endtime <- Sys.time()
  ord_pc_200_02_time[[e]] <- endtime - starttime
  ord_pc_200_02_pm[[e]] <- edges.posterior(ord_pc_200_02[[e]]$c$i,
                                           burnin = 0.2)

  starttime <- Sys.time()
  par_pc_200_02[[e]] <- partitionMCMC(iterations = 30000,
                                      n = 8,
                                      scoreparam = score_pc_200_02,
                                      startspace = am_pc,
                                      verbose = FALSE)
  endtime <- Sys.time()
  par_pc_200_02_time[[e]] <- endtime - starttime
  par_pc_200_02_pm[[e]] <- edges.posterior(par_pc_200_02[[e]]$c$i,
                                           burnin = 0.2)

  #################################################
  # ORDER AND PARTITION
  # PC
  # 200
  # 0.5
  #################################################

  score_pc_200_05 <- scoreparameters(8,
                                     "bge",
                                     data_pc_200_05[[e]])

  starttime <- Sys.time()
  ord_pc_200_05[[e]] <- orderMCMC(iterations = 30000,
                                  n = 8,
                                  scoreparam = score_pc_200_05,
                                  startspace = am_pc,
                                  chainout = TRUE)
  endtime <- Sys.time()
  ord_pc_200_05_time[[e]] <- endtime - starttime
  ord_pc_200_05_pm[[e]] <- edges.posterior(ord_pc_200_05[[e]]$c$i,
                                           burnin = 0.2)

  starttime <- Sys.time()
  par_pc_200_05[[e]] <- partitionMCMC(iterations = 30000,
                                      n = 8,
                                      scoreparam = score_pc_200_05,
                                      startspace = am_pc,
                                      verbose = FALSE)
  endtime <- Sys.time()
  par_pc_200_05_time[[e]] <- endtime - starttime
  par_pc_200_05_pm[[e]] <- edges.posterior(par_pc_200_05[[e]]$c$i,
                                           burnin = 0.2)

  #################################################
  # ORDER AND PARTITION
  # PC
  # 200
  # 1
  #################################################

  score_pc_200_1 <- scoreparameters(8,
                                    "bge",
                                    data_pc_200_1[[e]])

  starttime <- Sys.time()
  ord_pc_200_1[[e]] <- orderMCMC(iterations = 30000,
                                 n = 8,
                                 scoreparam = score_pc_200_1,
                                 startspace = am_pc,
                                 chainout = TRUE)
  endtime <- Sys.time()
  ord_pc_200_1_time[[e]] <- endtime - starttime
  ord_pc_200_1_pm[[e]] <- edges.posterior(ord_pc_200_1[[e]]$c$i,
                                          burnin = 0.2)

  starttime <- Sys.time()
  par_pc_200_1[[e]] <- partitionMCMC(iterations = 30000,
                                     n = 8,
                                     scoreparam = score_pc_200_1,
                                     startspace = am_pc,
                                     verbose = FALSE)
  endtime <- Sys.time()
  par_pc_200_1_time[[e]] <- endtime - starttime
  par_pc_200_1_pm[[e]] <- edges.posterior(par_pc_200_1[[e]]$c$i,
                                          burnin = 0.2)

  print(e)
  print(Sys.time())

  save(M,
       ord_g2_200_02_fc,
       ord_g2_200_02,
       ord_g2_200_05_fc,
       ord_g2_200_05,
       ord_g2_200_1_fc,
       ord_g2_200_1,
       ord_nc11_200_02,
       ord_nc11_200_05,
       ord_nc11_200_1,
       ord_pc_200_02,
       ord_pc_200_05,
       ord_pc_200_1,
       file = '/mnt/lfs2/mart9986/bidag/ord_ge_N_200.RData')

  save(M,
       par_g2_200_02_fc,
       par_g2_200_02,
       par_g2_200_05_fc,
       par_g2_200_05,
       par_g2_200_1_fc,
       par_g2_200_1,
       par_nc11_200_02,
       par_nc11_200_05,
       par_nc11_200_1,
       par_pc_200_02,
       par_pc_200_05,
       par_pc_200_1,
       file = '/mnt/lfs2/mart9986/bidag/par_ge_N_200.RData')

  save(M,
       ord_g2_200_02_fc_pm,
       ord_g2_200_02_pm,
       ord_g2_200_05_fc_pm,
       ord_g2_200_05_pm,
       ord_g2_200_1_fc_pm,
       ord_g2_200_1_pm,
       ord_nc11_200_02_pm,
       ord_nc11_200_05_pm,
       ord_nc11_200_1_pm,
       ord_pc_200_02_pm,
       ord_pc_200_05_pm,
       ord_pc_200_1_pm,
       file = '/mnt/lfs2/mart9986/bidag/ord_ge_N_200_pm.RData')

  save(M,
       par_g2_200_02_fc_pm,
       par_g2_200_02_pm,
       par_g2_200_05_fc_pm,
       par_g2_200_05_pm,
       par_g2_200_1_fc_pm,
       par_g2_200_1_pm,
       par_nc11_200_02_pm,
       par_nc11_200_05_pm,
       par_nc11_200_1_pm,
       par_pc_200_02_pm,
       par_pc_200_05_pm,
       par_pc_200_1_pm,
       file = '/mnt/lfs2/mart9986/bidag/par_ge_N_200_pm.RData')

  save(M,
       ord_g2_200_02_fc_time,
       ord_g2_200_02_time,
       ord_g2_200_05_fc_time,
       ord_g2_200_05_time,
       ord_g2_200_1_fc_time,
       ord_g2_200_1_time,
       ord_nc11_200_02_time,
       ord_nc11_200_05_time,
       ord_nc11_200_1_time,
       ord_pc_200_02_time,
       ord_pc_200_05_time,
       ord_pc_200_1_time,
       file = '/mnt/lfs2/mart9986/bidag/ord_ge_N_200_time.RData')

  save(M,
       par_g2_200_02_fc_time,
       par_g2_200_02_time,
       par_g2_200_05_fc_time,
       par_g2_200_05_time,
       par_g2_200_1_fc_time,
       par_g2_200_1_time,
       par_nc11_200_02_time,
       par_nc11_200_05_time,
       par_nc11_200_1_time,
       par_pc_200_02_time,
       par_pc_200_05_time,
       par_pc_200_1_time,
       file = '/mnt/lfs2/mart9986/bidag/par_ge_N_200_time.RData')

}
