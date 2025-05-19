# Re-running simulation
# Using fully connected graphs as the initial graph

# Load the data
load('/mnt/lfs2/mart9986/data/data_ge_N_600.RData')

# Generate adjacency matrix for a fully-connect graph
fullycon_pc <- matrix (1, nrow = 8, ncol = 8)

e <- 1
# about 10 mins
mh_pc_fullycon <- mhEdge(adjMatrix = fullycon_pc,
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

mse (posterior = list (mh_pc_fullycon), expected = ep_pc, type = "gmse")
mse (posterior = list (mh_pc_fullycon), expected = ep_pc, type = "emse")