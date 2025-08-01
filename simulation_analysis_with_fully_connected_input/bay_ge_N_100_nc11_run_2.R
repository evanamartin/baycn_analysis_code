library (baycn)

# Load the data
load('./data_ge_N_100.RData')

# Adjacency matrices -----------------------------------------------------------

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

# Initialize the lists to full length. -----------------------------------------

# 0.2 -------------------------------------------

bay_nc11_100_02 <- vector(mode = 'list',
                          length = M)

# 0.5 -------------------------------------------

bay_nc11_100_05 <- vector(mode = 'list',
                          length = M)

# 1 ---------------------------------------------

bay_nc11_100_1 <- vector(mode = 'list',
                         length = M)

# Generate the inputs for and run `cycleFndr` ----------------------------------

# Get the coordinates of the non zero elements in the adjacency matrix.
coord <- coordinates(adjMatrix = am_nc11)

# Determine the number of edges in the network
nEdges <- dim(coord)[2]

# Generate a library of potential cycles. This is used as input into the mhEdge function
cf <- cycleFndr(adjMatrix = am_nc11,
                  nEdges = nEdges,
                  nCPh = 0,
                  nGV = 0,
                  pmr = FALSE,
                  position = coord,
                  threads = 12
)

# Run baycn on each data set. --------------------------------------------------

set.seed(122)

# Loop through each combination of signal strength and sample size for all
# topologies.
for (e in 2:2) {

  # 0.2 -------------------------------------------

  bay_nc11_100_02[[e]] <- mhEdge(adjMatrix = am_nc11,
                                 burnIn = 0.2,
                                 data = data_nc11_100_02[[e]],
                                 iterations = 50000,
                                 nGV = 0,
                                 pmr = FALSE,
                                 prior = c(0.05,
                                           0.05,
                                           0.9),
                                 thinTo = 200,
                                 progress = FALSE,
                                 inpCf = cf)

  # 0.5 -------------------------------------------

  bay_nc11_100_05[[e]] <- mhEdge(adjMatrix = am_nc11,
                                 burnIn = 0.2,
                                 data = data_nc11_100_05[[e]],
                                 iterations = 50000,
                                 nGV = 0,
                                 pmr = FALSE,
                                 prior = c(0.05,
                                           0.05,
                                           0.9),
                                 thinTo = 200,
                                 progress = FALSE,
                                 inpCf = cf)
  # 1 ---------------------------------------------

  bay_nc11_100_1[[e]] <- mhEdge(adjMatrix = am_nc11,
                                burnIn = 0.2,
                                data = data_nc11_100_1[[e]],
                                iterations = 50000,
                                nGV = 0,
                                pmr = FALSE,
                                prior = c(0.05,
                                          0.05,
                                          0.9),
                                thinTo = 200,
                                progress = FALSE,
                                 inpCf = cf)

  print(e)
  print(Sys.time())

  save(M,
       bay_nc11_100_02,
       bay_nc11_100_05,
       bay_nc11_100_1,
       file = './bay_ge_N_100_nc11_run_2.RData')

}
