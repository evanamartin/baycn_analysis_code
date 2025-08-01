library (baycn)

# Load the data
load('./data_ge_N_600.RData')

# Adjacency matrices -----------------------------------------------------------

# Adjacency matrix with the all edges for topology G2.
am_g2 <- matrix(c(0, 1, 1, 1,
                  1, 0, 1, 1,
                  1, 1, 0, 1,
                  1, 1, 1, 0),
                byrow = TRUE,
                nrow = 4)

# Adjacency matrix with all possible edges for topology G2.
am_g2_fc <- matrix(c(0, 1, 1, 1,
                     1, 0, 1, 1,
                     1, 1, 0, 1,
                     1, 1, 1, 0),
                   byrow = TRUE,
                   nrow = 4)

# Adjacency matrix with the all edges for topology H2.
am_h2 <- matrix(c(0, 1, 1, 1, 1,
                  1, 0, 1, 1, 1,
                  1, 1, 0, 1, 1,
                  1, 1, 1, 0, 1,
                  1, 1, 1, 1, 0),
                byrow = TRUE,
                nrow = 5)

# Adjacency matrix with the all edges for topology M1.
am_m1 <- matrix(c(0, 1, 1,
                  1, 0, 1,
                  1, 1, 0),
                byrow = TRUE,
                nrow = 3)

# Adjacency matrix with the all edges for topology M2.
am_m2 <- matrix(c(0, 1, 1,
                  1, 0, 1,
                  1, 1, 0),
                byrow = TRUE,
                nrow = 3)

# Adjacency matrix with the all edges for the multi-parent topology.
am_mp <- matrix(c(0, 1, 1, 1,
                  1, 0, 1, 1,
                  1, 1, 0, 1,
                  1, 1, 1, 0),
                byrow = TRUE,
                nrow = 4)

# Adjacency matrix with the all edges for topology NC11.
am_nc11 <- matrix(c(0,  1,  1,  1,  1,  1,  1,  1,  1,   1,   1,
                    1,  0,  1,  1,  1,  1,  1,  1,  1,   1,   1,
                    1,  1,  0,  1,  1,  1,  1,  1,  1,   1,   1,
                    1,  1,  1,  0,  1,  1,  1,  1,  1,   1,   1,
                    1,  1,  1,  1,  0,  1,  1,  1,  1,   1,   1,
                    1,  1,  1,  1,  1,  0,  1,  1,  1,   1,   1,
                    1,  1,  1,  1,  1,  1,  0,  1,  1,   1,   1,
                    1,  1,  1,  1,  1,  1,  1,  0,  1,   1,   1,
                    1,  1,  1,  1,  1,  1,  1,  1,  0,   1,   1,
                    1,  1,  1,  1,  1,  1,  1,  1,  1,   0,   1,
                    1,  1,  1,  1,  1,  1,  1,  1,  1,   1,   0),
                  byrow = TRUE,
                  nrow = 11)

# Adjacency matrix with the all edges for topology PC.
am_pc <- matrix(c(0, 1, 1, 1, 1, 1, 1, 1,
                  1, 0, 1, 1, 1, 1, 1, 1,
                  1, 1, 0, 1, 1, 1, 1, 1,
                  1, 1, 1, 0, 1, 1, 1, 1,
                  1, 1, 1, 1, 0, 1, 1, 1,
                  1, 1, 1, 1, 1, 0, 1, 1,
                  1, 1, 1, 1, 1, 1, 0, 1,
                  1, 1, 1, 1, 1, 1, 1, 0),
                byrow = TRUE,
                nrow = 8)


# Initialize the lists to full length. -----------------------------------------

# 0.2 -------------------------------------------

bay_g2_600_02 <- vector(mode = 'list',
                        length = M)

bay_g2_600_02_fc <- vector(mode = 'list',
                           length = M)

bay_h2_600_02 <- vector(mode = 'list',
                        length = M)

bay_m1_600_02 <- vector(mode = 'list',
                        length = M)

bay_m2_600_02 <- vector(mode = 'list',
                        length = M)

bay_mp_600_02 <- vector(mode = 'list',
                        length = M)

bay_nc11_600_02 <- vector(mode = 'list',
                          length = M)

bay_pc_600_02 <- vector(mode = 'list',
                        length = M)

# 0.5 -------------------------------------------

bay_g2_600_05 <- vector(mode = 'list',
                        length = M)

bay_g2_600_05_fc <- vector(mode = 'list',
                           length = M)

bay_h2_600_05 <- vector(mode = 'list',
                        length = M)

bay_m1_600_05 <- vector(mode = 'list',
                        length = M)

bay_m2_600_05 <- vector(mode = 'list',
                        length = M)

bay_mp_600_05 <- vector(mode = 'list',
                        length = M)

bay_nc11_600_05 <- vector(mode = 'list',
                          length = M)

bay_pc_600_05 <- vector(mode = 'list',
                        length = M)

# 1 ---------------------------------------------

bay_g2_600_1 <- vector(mode = 'list',
                       length = M)

bay_g2_600_1_fc <- vector(mode = 'list',
                          length = M)

bay_h2_600_1 <- vector(mode = 'list',
                       length = M)

bay_m1_600_1 <- vector(mode = 'list',
                       length = M)

bay_m2_600_1 <- vector(mode = 'list',
                       length = M)

bay_mp_600_1 <- vector(mode = 'list',
                       length = M)

bay_nc11_600_1 <- vector(mode = 'list',
                         length = M)

bay_pc_600_1 <- vector(mode = 'list',
                       length = M)

# Run baycn on each data set. --------------------------------------------------

set.seed(622)

# Loop through each combination of signal strength and sample size for all
# topologies.
for (e in 1:M) {

  # 0.2 -------------------------------------------

  bay_g2_600_02[[e]] <- mhEdge(adjMatrix = am_g2,
                               burnIn = 0.2,
                               data = data_g2_600_02[[e]],
                               iterations = 30000,
                               nGV = 0,
                               pmr = FALSE,
                               prior = c(0.05,
                                         0.05,
                                         0.9),
                               thinTo = 200,
                               progress = FALSE)

  bay_g2_600_02_fc[[e]] <- mhEdge(adjMatrix = am_g2_fc,
                                  burnIn = 0.2,
                                  data = data_g2_600_02[[e]],
                                  iterations = 30000,
                                  nGV = 0,
                                  pmr = FALSE,
                                  prior = c(0.05,
                                            0.05,
                                            0.9),
                                  thinTo = 200,
                                  progress = FALSE)

  bay_h2_600_02[[e]] <- mhEdge(adjMatrix = am_h2,
                               burnIn = 0.2,
                               data = data_h2_600_02[[e]],
                               iterations = 30000,
                               nGV = 0,
                               pmr = FALSE,
                               prior = c(0.05,
                                         0.05,
                                         0.9),
                               thinTo = 200,
                               progress = FALSE)

  bay_m1_600_02[[e]] <- mhEdge(adjMatrix = am_m1,
                               burnIn = 0.2,
                               data = data_m1_600_02[[e]],
                               iterations = 30000,
                               nGV = 0,
                               pmr = FALSE,
                               prior = c(0.05,
                                         0.05,
                                         0.9),
                               thinTo = 200,
                               progress = FALSE)

  bay_m2_600_02[[e]] <- mhEdge(adjMatrix = am_m2,
                               burnIn = 0.2,
                               data = data_m2_600_02[[e]],
                               iterations = 30000,
                               nGV = 0,
                               pmr = FALSE,
                               prior = c(0.05,
                                         0.05,
                                         0.9),
                               thinTo = 200,
                               progress = FALSE)

  bay_mp_600_02[[e]] <- mhEdge(adjMatrix = am_mp,
                               burnIn = 0.2,
                               data = data_mp_600_02[[e]],
                               iterations = 30000,
                               nGV = 0,
                               pmr = FALSE,
                               prior = c(0.05,
                                         0.05,
                                         0.9),
                               thinTo = 200,
                               progress = FALSE)

  bay_nc11_600_02[[e]] <- mhEdge(adjMatrix = am_nc11,
                                 burnIn = 0.2,
                                 data = data_nc11_600_02[[e]],
                                 iterations = 50000,
                                 nGV = 0,
                                 pmr = FALSE,
                                 prior = c(0.05,
                                           0.05,
                                           0.9),
                                 thinTo = 200,
                                 progress = FALSE)

  bay_pc_600_02[[e]] <- mhEdge(adjMatrix = am_pc,
                               burnIn = 0.2,
                               data = data_pc_600_02[[e]],
                               iterations = 50000,
                               nGV = 0,
                               pmr = FALSE,
                               prior = c(0.05,
                                         0.05,
                                         0.9),
                               thinTo = 200,
                               progress = FALSE)

  # 0.5 -------------------------------------------

  bay_g2_600_05[[e]] <- mhEdge(adjMatrix = am_g2,
                               burnIn = 0.2,
                               data = data_g2_600_05[[e]],
                               iterations = 30000,
                               nGV = 0,
                               pmr = FALSE,
                               prior = c(0.05,
                                         0.05,
                                         0.9),
                               thinTo = 200,
                               progress = FALSE)

  bay_g2_600_05_fc[[e]] <- mhEdge(adjMatrix = am_g2_fc,
                                  burnIn = 0.2,
                                  data = data_g2_600_05[[e]],
                                  iterations = 30000,
                                  nGV = 0,
                                  pmr = FALSE,
                                  prior = c(0.05,
                                            0.05,
                                            0.9),
                                  thinTo = 200,
                                  progress = FALSE)

  bay_h2_600_05[[e]] <- mhEdge(adjMatrix = am_h2,
                               burnIn = 0.2,
                               data = data_h2_600_05[[e]],
                               iterations = 30000,
                               nGV = 0,
                               pmr = FALSE,
                               prior = c(0.05,
                                         0.05,
                                         0.9),
                               thinTo = 200,
                               progress = FALSE)

  bay_m1_600_05[[e]] <- mhEdge(adjMatrix = am_m1,
                               burnIn = 0.2,
                               data = data_m1_600_05[[e]],
                               iterations = 30000,
                               nGV = 0,
                               pmr = FALSE,
                               prior = c(0.05,
                                         0.05,
                                         0.9),
                               thinTo = 200,
                               progress = FALSE)

  bay_m2_600_05[[e]] <- mhEdge(adjMatrix = am_m2,
                               burnIn = 0.2,
                               data = data_m2_600_05[[e]],
                               iterations = 30000,
                               nGV = 0,
                               pmr = FALSE,
                               prior = c(0.05,
                                         0.05,
                                         0.9),
                               thinTo = 200,
                               progress = FALSE)

  bay_mp_600_05[[e]] <- mhEdge(adjMatrix = am_mp,
                               burnIn = 0.2,
                               data = data_mp_600_05[[e]],
                               iterations = 30000,
                               nGV = 0,
                               pmr = FALSE,
                               prior = c(0.05,
                                         0.05,
                                         0.9),
                               thinTo = 200,
                               progress = FALSE)

  bay_nc11_600_05[[e]] <- mhEdge(adjMatrix = am_nc11,
                                 burnIn = 0.2,
                                 data = data_nc11_600_05[[e]],
                                 iterations = 50000,
                                 nGV = 0,
                                 pmr = FALSE,
                                 prior = c(0.05,
                                           0.05,
                                           0.9),
                                 thinTo = 200,
                                 progress = FALSE)

  bay_pc_600_05[[e]] <- mhEdge(adjMatrix = am_pc,
                               burnIn = 0.2,
                               data = data_pc_600_05[[e]],
                               iterations = 50000,
                               nGV = 0,
                               pmr = FALSE,
                               prior = c(0.05,
                                         0.05,
                                         0.9),
                               thinTo = 200,
                               progress = FALSE)

  # 1 ---------------------------------------------

  bay_g2_600_1[[e]] <- mhEdge(adjMatrix = am_g2,
                              burnIn = 0.2,
                              data = data_g2_600_1[[e]],
                              iterations = 30000,
                              nGV = 0,
                              pmr = FALSE,
                              prior = c(0.05,
                                        0.05,
                                        0.9),
                              thinTo = 200,
                              progress = FALSE)

  bay_g2_600_1_fc[[e]] <- mhEdge(adjMatrix = am_g2_fc,
                                 burnIn = 0.2,
                                 data = data_g2_600_1[[e]],
                                 iterations = 30000,
                                 nGV = 0,
                                 pmr = FALSE,
                                 prior = c(0.05,
                                           0.05,
                                           0.9),
                                 thinTo = 200,
                                 progress = FALSE)

  bay_h2_600_1[[e]] <- mhEdge(adjMatrix = am_h2,
                              burnIn = 0.2,
                              data = data_h2_600_1[[e]],
                              iterations = 30000,
                              nGV = 0,
                              pmr = FALSE,
                              prior = c(0.05,
                                        0.05,
                                        0.9),
                              thinTo = 200,
                              progress = FALSE)

  bay_m1_600_1[[e]] <- mhEdge(adjMatrix = am_m1,
                              burnIn = 0.2,
                              data = data_m1_600_1[[e]],
                              iterations = 30000,
                              nGV = 0,
                              pmr = FALSE,
                              prior = c(0.05,
                                        0.05,
                                        0.9),
                              thinTo = 200,
                              progress = FALSE)

  bay_m2_600_1[[e]] <- mhEdge(adjMatrix = am_m2,
                              burnIn = 0.2,
                              data = data_m2_600_1[[e]],
                              iterations = 30000,
                              nGV = 0,
                              pmr = FALSE,
                              prior = c(0.05,
                                        0.05,
                                        0.9),
                              thinTo = 200,
                              progress = FALSE)

  bay_mp_600_1[[e]] <- mhEdge(adjMatrix = am_mp,
                              burnIn = 0.2,
                              data = data_mp_600_1[[e]],
                              iterations = 30000,
                              nGV = 0,
                              pmr = FALSE,
                              prior = c(0.05,
                                        0.05,
                                        0.9),
                              thinTo = 200,
                              progress = FALSE)

  bay_nc11_600_1[[e]] <- mhEdge(adjMatrix = am_nc11,
                                burnIn = 0.2,
                                data = data_nc11_600_1[[e]],
                                iterations = 50000,
                                nGV = 0,
                                pmr = FALSE,
                                prior = c(0.05,
                                          0.05,
                                          0.9),
                                thinTo = 200,
                                progress = FALSE)

  bay_pc_600_1[[e]] <- mhEdge(adjMatrix = am_pc,
                              burnIn = 0.2,
                              data = data_pc_600_1[[e]],
                              iterations = 50000,
                              nGV = 0,
                              pmr = FALSE,
                              prior = c(0.05,
                                        0.05,
                                        0.9),
                              thinTo = 200,
                              progress = FALSE)

  print(e)
  print(Sys.time())

  save(M,
       bay_g2_600_02,
       bay_g2_600_02_fc,
       bay_h2_600_02,
       bay_m1_600_02,
       bay_m2_600_02,
       bay_mp_600_02,
       bay_nc11_600_02,
       bay_pc_600_02,
       bay_g2_600_05,
       bay_g2_600_05_fc,
       bay_h2_600_05,
       bay_m1_600_05,
       bay_m2_600_05,
       bay_mp_600_05,
       bay_nc11_600_05,
       bay_pc_600_05,
       bay_g2_600_1,
       bay_g2_600_1_fc,
       bay_h2_600_1,
       bay_m1_600_1,
       bay_m2_600_1,
       bay_mp_600_1,
       bay_nc11_600_1,
       bay_pc_600_1,
       file = './bay_ge_N_600.RData')

}
