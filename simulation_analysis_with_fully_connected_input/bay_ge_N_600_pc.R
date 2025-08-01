library (baycn)

# Load the data
load('./data_ge_N_600.RData')

# Adjacency matrices -----------------------------------------------------------

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

bay_pc_600_02 <- vector(mode = 'list',
                        length = M)

# 0.5 -------------------------------------------

bay_pc_600_05 <- vector(mode = 'list',
                        length = M)

# 1 ---------------------------------------------

bay_pc_600_1 <- vector(mode = 'list',
                       length = M)

# Run baycn on each data set. --------------------------------------------------

set.seed(622)

# Loop through each combination of signal strength and sample size for all
# topologies.
for (e in 1:M) {

  # 0.2 -------------------------------------------

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
       bay_pc_600_02,
       bay_pc_600_05,
       bay_pc_600_1,
       file = './bay_ge_N_600_pc.RData')

}
