# Adjacency matrices -----------------------------------------------------------

# M1 -----------------------------------

# Undirected adjacency matrix (true edges) for M1.
am_m1 <- matrix(c(0, 1, 0,
                  1, 0, 1,
                  0, 1, 0),
                byrow = TRUE,
                nrow = 3)

# Fully connected adjacency matrix for M1.
am_m1_fc <- matrix(c(0, 1, 1,
                     1, 0, 1,
                     1, 1, 0),
                   byrow = TRUE,
                   nrow = 3)

# M2 -----------------------------------

# Undirected adjacency matrix (true edges) for M2.
am_m2 <- matrix(c(0, 1, 0,
                  1, 0, 1,
                  0, 1, 0),
                byrow = TRUE,
                nrow = 3)

# Fully connected adjacency matrix for M2.
am_m2_fc <- matrix(c(0, 1, 1,
                     1, 0, 1,
                     1, 1, 0),
                   byrow = TRUE,
                   nrow = 3)

# M3 -----------------------------------

# Undirected adjacency matrix (true edges) for M3.
am_m3 <- matrix(c(0, 1, 1,
                  1, 0, 0,
                  1, 0, 0),
                byrow = TRUE,
                nrow = 3)

# M4 -----------------------------------

# Undirected adjacency matrix (true edges) for M4.
am_m4 <- matrix(c(0, 1, 1,
                  1, 0, 1,
                  1, 1, 0),
                byrow = TRUE,
                nrow = 3)

# Multi-parent -------------------------

# Undirected adjacency matrix (true edges) for the multi-parent topology.
am_mp <- matrix(c(0, 0, 0, 1,
                  0, 0, 0, 1,
                  0, 0, 0, 1,
                  1, 1, 1, 0),
                byrow = TRUE,
                nrow = 4)

# GN4 ----------------------------------

am_gn4 <- matrix(c(0, 1, 1, 0,
                   1, 0, 0, 1,
                   1, 0, 0, 1,
                   0, 1, 1, 0),
                 byrow = TRUE,
                 nrow = 4)

# Layer --------------------------------

# Undirected adjacency matrix (true edges) for the layer topology.
am_layer <- matrix(c(0, 1, 1, 1, 0, 0, 0, 0,
                     1, 0, 0, 0, 1, 1, 0, 0,
                     1, 0, 0, 0, 0, 1, 1, 0,
                     1, 0, 0, 0, 0, 0, 0, 1,
                     0, 1, 0, 0, 0, 0, 0, 0,
                     0, 1, 1, 0, 0, 0, 0, 0,
                     0, 0, 1, 0, 0, 0, 0, 0,
                     0, 0, 0, 1, 0, 0, 0, 0),
                   byrow = TRUE,
                   nrow = 8)

# Clinical phenotype -------------------

# Adjacency matrix for cph graphs with all three nodes as parents to W.
# There are four nodes in the network including W.
am_cph_4_mp <- matrix(c(0, 0, 0, 1,
                        0, 0, 0, 1,
                        0, 0, 0, 1,
                        0, 0, 0, 0),
                      byrow = TRUE,
                      nrow = 4)

# Adjacency matrix for cph graphs with all four nodes as parents to W.
# There are five nodes in the network including W.
am_cph_5_mp <- matrix(c(0, 0, 0, 0, 1,
                        0, 0, 0, 0, 1,
                        0, 0, 0, 0, 1,
                        0, 0, 0, 0, 1,
                        0, 0, 0, 0, 0),
                      byrow = TRUE,
                      nrow = 5)

# Adjacency matrix for cph graphs with all five nodes as parents to W.
# There are six nodes in the network including W.
am_cph_6_mp <- matrix(c(0, 0, 0, 0, 0, 1,
                        0, 0, 0, 0, 0, 1,
                        0, 0, 0, 0, 0, 1,
                        0, 0, 0, 0, 0, 1,
                        0, 0, 0, 0, 0, 1,
                        0, 0, 0, 0, 0, 0),
                      byrow = TRUE,
                      nrow = 6)

# Adjacency matrix for cph graphs with first four nodes as parents to W.
# There are six nodes in the network including W.
am_cph_6_1_mp <- matrix(c(0, 0, 0, 0, 0, 1,
                          0, 0, 0, 0, 0, 1,
                          0, 0, 0, 0, 0, 1,
                          0, 0, 0, 0, 0, 1,
                          0, 0, 0, 0, 0, 0,
                          0, 0, 0, 0, 0, 0),
                        byrow = TRUE,
                        nrow = 6)

# Adjacency matrix for cph graphs with last four nodes as parents to W.
# There are six nodes in the network including W.
am_cph_6_2_mp <- matrix(c(0, 0, 0, 0, 0, 0,
                          0, 0, 0, 0, 0, 1,
                          0, 0, 0, 0, 0, 1,
                          0, 0, 0, 0, 0, 1,
                          0, 0, 0, 0, 0, 1,
                          0, 0, 0, 0, 0, 0),
                        byrow = TRUE,
                        nrow = 6)

# Adjacency matrix for cph graphs with all six nodes as parents to W.
# There are seven nodes in the network including W.
am_cph_7_mp <- matrix(c(0, 0, 0, 0, 0, 0, 1,
                        0, 0, 0, 0, 0, 0, 1,
                        0, 0, 0, 0, 0, 0, 1,
                        0, 0, 0, 0, 0, 0, 1,
                        0, 0, 0, 0, 0, 0, 1,
                        0, 0, 0, 0, 0, 0, 1,
                        0, 0, 0, 0, 0, 0, 0),
                      byrow = TRUE,
                      nrow = 7)

# Adjacency matrix for cph graphs with first five nodes as parents to W.
# There are seven nodes in the network including W.
am_cph_7_1_mp <- matrix(c(0, 0, 0, 0, 0, 0, 1,
                          0, 0, 0, 0, 0, 0, 1,
                          0, 0, 0, 0, 0, 0, 1,
                          0, 0, 0, 0, 0, 0, 1,
                          0, 0, 0, 0, 0, 0, 1,
                          0, 0, 0, 0, 0, 0, 0,
                          0, 0, 0, 0, 0, 0, 0),
                        byrow = TRUE,
                        nrow = 7)

# Adjacency matrix for cph graphs with all seven nodes as parents to W.
# There are eight nodes in the network including W.
am_cph_8_mp <- matrix(c(0, 0, 0, 0, 0, 0, 0, 1,
                        0, 0, 0, 0, 0, 0, 0, 1,
                        0, 0, 0, 0, 0, 0, 0, 1,
                        0, 0, 0, 0, 0, 0, 0, 1,
                        0, 0, 0, 0, 0, 0, 0, 1,
                        0, 0, 0, 0, 0, 0, 0, 1,
                        0, 0, 0, 0, 0, 0, 0, 1,
                        0, 0, 0, 0, 0, 0, 0, 0),
                      byrow = TRUE,
                      nrow = 8)

# Adjacency matrix for cph graphs with all eight nodes as parents to W.
# There are nine nodes in the network including W.
am_cph_9_mp <- matrix(c(0, 0, 0, 0, 0, 0, 0, 0, 1,
                        0, 0, 0, 0, 0, 0, 0, 0, 1,
                        0, 0, 0, 0, 0, 0, 0, 0, 1,
                        0, 0, 0, 0, 0, 0, 0, 0, 1,
                        0, 0, 0, 0, 0, 0, 0, 0, 1,
                        0, 0, 0, 0, 0, 0, 0, 0, 1,
                        0, 0, 0, 0, 0, 0, 0, 0, 1,
                        0, 0, 0, 0, 0, 0, 0, 0, 1,
                        0, 0, 0, 0, 0, 0, 0, 0, 0),
                      byrow = TRUE,
                      nrow = 9)

# Star ---------------------------------

# Undirected adjacency matrix (true edges) for the star topology.
am_star <- matrix(c(0, 1, 0, 0, 0, 0,
                    1, 0, 1, 1, 1, 1,
                    0, 1, 0, 0, 0, 0,
                    0, 1, 0, 0, 0, 0,
                    0, 1, 0, 0, 0, 0,
                    0, 1, 0, 0, 0, 0),
                  byrow = TRUE,
                  nrow = 6)
