# Re-running simulation
# Using fully connected graphs as the initial graph
# library(doParallel)
# library(gtools)
library(baycn)

# Load the data
load('./data_ge_N_600.RData')
d <- 11

# Generate adjacency matrix for a fully-connect graph
fullycon_pc <- matrix (1, nrow = d, ncol = d)
diag(fullycon_pc) <- rep(0, d)

print(fullycon_pc)

# Get the coordinates of the non zero elements in the adjacency matrix.
coord <- coordinates(adjMatrix = fullycon_pc)

# Determine the number of edges in the network
nEdges <- dim(coord)[2]

# Generate a library of potential cycles. This is used as input into the mhEdge function
cf <- cycleFndr(adjMatrix = fullycon_pc,
                  nEdges = nEdges,
                  nCPh = 0,
                  nGV = 0,
                  pmr = FALSE,
                  position = coord,
                  threads = 12
)

e <- 1
# about 10 mins
mh_pc_fullycon <- mhEdge(adjMatrix = fullycon_pc,
                         burnIn = 0.2,
                         data = data_nc11_600_02[[e]],
                         iterations = 50000,
                         nGV = 0,
                         pmr = FALSE,
                         prior = c(0.05,
                                   0.05,
                                   0.9),
                         thinTo = 200,
                         progress = TRUE,
                         inpCf = cf
)
print("Completed mhEdge function")
print(mh_pc_fullycon@posteriorPM)
# mse (posterior = list (mh_pc_fullycon), expected = ep_pc, type = "gmse")
# mse (posterior = list (mh_pc_fullycon), expected = ep_pc, type = "emse")
