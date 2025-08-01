# Load libraries ---------------------------------------------------------------
library("RcppArmadillo", lib="~/bin/r.packages/3.2.3") 
library ("networkBMA", lib="~/bin/r.packages/3.2.3")

# Load data --------------------------------------------------------------------

load("./data_ge_N_100.RData")

# am_bma is a function to create an "adjacency" matrix from the scanBMA output.
am_bma <- function (bma) {

  am <- matrix(0,
               nrow = length(bma),
               ncol = length(bma))

  # Loop through each node in bma to extract the parent nodes.
  for (e in 1:length(bma)) {

    # Create a counter to subset the poterior probability vector from the bma
    # output. This vector is length(bma) - 1 because a node cannot be the parent
    # of itself.
    counter <- 1

    # Loop through each potential parent of the current node.
    for (v in 1:length(bma)) {

      # Check if the current parent v is the current node e (i.e., the diagonal
      # of the adjacency matrix).
      if (e != v) {

        # Add the posterior probability of the current parent to the column of
        # the adjacency matrix corresponding to the current node.
        am[v, e] <- bma[[e]]$probne0[[counter]] / 100

        # Increase the counter by one.
        counter <- counter + 1

      }

    }

  }

  return (am)

}

# Load adjacency matrices ------------------------------------------------------

source ('./adjacency_matrices.R')

am_gn4 <- matrix(c(0, 1, 1, 0,
                   1, 0, 0, 1,
                   1, 0, 0, 1,
                   0, 1, 1, 0),
                 byrow = TRUE,
                 nrow = 4)

# Adjacency matrix with the true edges for topology PC.
am_gn8 <- matrix(c(0, 1, 0, 0, 0, 1, 0, 1,
                  0, 0, 1, 0, 1, 0, 0, 0,
                  0, 0, 0, 0, 0, 0, 0, 0,
                  0, 0, 0, 0, 0, 0, 0, 0,
                  0, 0, 0, 0, 0, 1, 0, 1,
                  0, 0, 0, 0, 0, 0, 1, 0,
                  0, 0, 0, 0, 0, 0, 0, 0,
                  0, 0, 0, 0, 0, 0, 0, 0),
                byrow = TRUE,
                nrow = 8) 

# Adjacency matrix with the true edges for topology NC11.
am_gn11 <- matrix(c(0,  1,  0,  0,  0,  0,  0,  0,  0,   0,   0,
                    1,  0,  1,  0,  0,  0,  0,  0,  0,   0,   0,
                    0,  1,  0,  1,  0,  0,  0,  0,  0,   0,   0,
                    0,  0,  1,  0,  1,  0,  0,  0,  0,   0,   0,
                    0,  0,  0,  1,  0,  1,  0,  0,  0,   0,   0,
                    0,  0,  0,  0,  0,  0,  0,  0,  0,   0,   0,
                    0,  0,  0,  0,  0,  1,  0,  1,  0,   0,   0,
                    0,  0,  0,  0,  0,  0,  1,  0,  1,   0,   0,
                    0,  0,  0,  0,  0,  0,  0,  1,  0,   1,   0,
                    0,  0,  0,  0,  0,  0,  0,  0,  1,   0,   1,
                    0,  0,  0,  0,  0,  0,  0,  0,  0,   1,   0),
                  byrow = TRUE,
                  nrow = 11)


# Preliminaries ----------------------------------------------------------------

# The following function will be used on all calls to the scanBMA function
# regardless of which topology is the input.
# Use the default settings (from the networkBMA vignette) for setting the value
# for Occam's window and whether to use Zellner's g prior or BIC.
control <- ScanBMAcontrol(OR = 20,
                          useg = TRUE,
                          gCtrl = gControl(optimize = FALSE,
                                           g0 = 20))

# Initialize lists to full length ----------------------------------------------

# Topology G2 --------------------------

# Create a list to hold the output from each node in G2.
bma_g2_100_02 <- vector(mode = 'list',
                        length = M)

bma_g2_100_05 <- vector(mode = 'list',
                        length = M)

bma_g2_100_1 <- vector(mode = 'list',
                       length = M)

# Create a list to hold the adjacency matrices created from the bma output.
ppm_g2_100_02 <- vector(mode = 'list',
                        length = M)

ppm_g2_100_05 <- vector(mode = 'list',
                        length = M)

ppm_g2_100_1 <- vector(mode = 'list',
                       length = M)

# Create lists to for the precision and power.
bma_prerec_g2_100_02 <- vector(mode = 'list',
                               length = M)

bma_prerec_g2_100_05 <- vector(mode = 'list',
                               length = M)

bma_prerec_g2_100_1 <- vector(mode = 'list',
                              length = M)

# Create a list to hold the runtime for each data set.
times_g2_100_02 <- vector(mode = 'list',
                          length = M)

times_g2_100_05 <- vector(mode = 'list',
                          length = M)

times_g2_100_1 <- vector(mode = 'list',
                         length = M)

# Topology NC11  --------------------------

# Create a list to hold the output from each node in G2.
bma_nc11_100_02 <- vector(mode = 'list',
                        length = M)

bma_nc11_100_05 <- vector(mode = 'list',
                        length = M)

bma_nc11_100_1 <- vector(mode = 'list',
                       length = M)

# Create a list to hold the adjacency matrices created from the bma output.
ppm_nc11_100_02 <- vector(mode = 'list',
                        length = M)

ppm_nc11_100_05 <- vector(mode = 'list',
                        length = M)

ppm_nc11_100_1 <- vector(mode = 'list',
                       length = M)

# Create lists to for the precision and power.
bma_prerec_nc11_100_02 <- vector(mode = 'list',
                               length = M)

bma_prerec_nc11_100_05 <- vector(mode = 'list',
                               length = M)

bma_prerec_nc11_100_1 <- vector(mode = 'list',
                              length = M)

# Create a list to hold the runtime for each data set.
times_nc11_100_02 <- vector(mode = 'list',
                          length = M)

times_nc11_100_05 <- vector(mode = 'list',
                          length = M)

times_nc11_100_1 <- vector(mode = 'list',
                         length = M)

# Topology PC --------------------------

# Create a list to hold the output from each node in G2.
bma_pc_100_02 <- vector(mode = 'list',
                        length = M)

bma_pc_100_05 <- vector(mode = 'list',
                        length = M)

bma_pc_100_1 <- vector(mode = 'list',
                       length = M)

# Create a list to hold the adjacency matrices created from the bma output.
ppm_pc_100_02 <- vector(mode = 'list',
                        length = M)

ppm_pc_100_05 <- vector(mode = 'list',
                        length = M)

ppm_pc_100_1 <- vector(mode = 'list',
                       length = M)

# Create lists to for the precision and power.
bma_prerec_pc_100_02 <- vector(mode = 'list',
                               length = M)

bma_prerec_pc_100_05 <- vector(mode = 'list',
                               length = M)

bma_prerec_pc_100_1 <- vector(mode = 'list',
                              length = M)

# Create a list to hold the runtime for each data set.
times_pc_100_02 <- vector(mode = 'list',
                          length = M)

times_pc_100_05 <- vector(mode = 'list',
                          length = M)

times_pc_100_1 <- vector(mode = 'list',
                         length = M)



# Run ScanBMA ------------------------------------------------------------------

for (e in 1:M) {

  print("ScanBMA (100) Loop:")
  print(e)

  # Topology G2 --------------------------

  # ScanBMA must be applied to each gene individually.
  # Create a sublist to hold the output for each node.
  bma_g2_100_02[[e]] <- vector(mode = 'list',
                               length = dim(data_g2_100_02[[1]])[2])

  bma_g2_100_05[[e]] <- vector(mode = 'list',
                               length = dim(data_g2_100_05[[1]])[2])

  bma_g2_100_1[[e]] <- vector(mode = 'list',
                              length = dim(data_g2_100_1[[1]])[2])

  begin_time <- Sys.time()
  # Apply ScanBMA to each node in the network.
  for (v in 1:dim(data_g2_100_02[[1]])[2]) {

    bma_g2_100_02[[e]][[v]] <- ScanBMA(x = data_g2_100_02[[e]][, -v],
                                       y = data_g2_100_02[[e]][, v],
                                       prior.prob = 0.1,
                                       control = control)

  }
  end_time <- Sys.time()

  # Calculate the runtime for each graph.
  times_g2_100_02[[e]] <- end_time - begin_time

  begin_time <- Sys.time()
  # Apply ScanBMA to each node in the network.
  for (v in 1:dim(data_g2_100_05[[1]])[2]) {

    bma_g2_100_05[[e]][[v]] <- ScanBMA(x = data_g2_100_05[[e]][, -v],
                                       y = data_g2_100_05[[e]][, v],
                                       prior.prob = 0.1,
                                       control = control)

  }
  end_time <- Sys.time()

  # Calculate the runtime for each graph.
  times_g2_100_05[[e]] <- end_time - begin_time

  begin_time <- Sys.time()
  # Apply ScanBMA to each node in the network.
  for (v in 1:dim(data_g2_100_1[[1]])[2]) {

    bma_g2_100_1[[e]][[v]] <- ScanBMA(x = data_g2_100_1[[e]][, -v],
                                      y = data_g2_100_1[[e]][, v],
                                      prior.prob = 0.1,
                                      control = control)

  }
  end_time <- Sys.time()

  # Calculate the runtime for each graph.
  times_g2_100_1[[e]] <- end_time - begin_time


  # Topology NC11 --------------------------

  # ScanBMA must be applied to each gene individually.
  # Create a sublist to hold the output for each node.
  bma_nc11_100_02[[e]] <- vector(mode = 'list',
                               length = dim(data_nc11_100_02[[1]])[2])

  bma_nc11_100_05[[e]] <- vector(mode = 'list',
                               length = dim(data_nc11_100_05[[1]])[2])

  bma_nc11_100_1[[e]] <- vector(mode = 'list',
                              length = dim(data_nc11_100_1[[1]])[2])

  begin_time <- Sys.time()
  # Apply ScanBMA to each node in the network.
  for (v in 1:dim(data_nc11_100_02[[1]])[2]) {

    bma_nc11_100_02[[e]][[v]] <- ScanBMA(x = data_nc11_100_02[[e]][, -v],
                                       y = data_nc11_100_02[[e]][, v],
                                       prior.prob = 0.1,
                                       control = control)

  }
  end_time <- Sys.time()

  # Calculate the runtime for each graph.
  times_nc11_100_02[[e]] <- end_time - begin_time

  begin_time <- Sys.time()
  # Apply ScanBMA to each node in the network.
  for (v in 1:dim(data_nc11_100_05[[1]])[2]) {

    bma_nc11_100_05[[e]][[v]] <- ScanBMA(x = data_nc11_100_05[[e]][, -v],
                                       y = data_nc11_100_05[[e]][, v],
                                       prior.prob = 0.1,
                                       control = control)

  }
  end_time <- Sys.time()

  # Calculate the runtime for each graph.
  times_nc11_100_05[[e]] <- end_time - begin_time

  begin_time <- Sys.time()
  # Apply ScanBMA to each node in the network.
  for (v in 1:dim(data_nc11_100_1[[1]])[2]) {

    bma_nc11_100_1[[e]][[v]] <- ScanBMA(x = data_nc11_100_1[[e]][, -v],
                                      y = data_nc11_100_1[[e]][, v],
                                      prior.prob = 0.1,
                                      control = control)

  }
  end_time <- Sys.time()

  # Calculate the runtime for each graph.
  times_nc11_100_1[[e]] <- end_time - begin_time

  # Topology PC --------------------------

  # ScanBMA must be applied to each gene individually.
  # Create a sublist to hold the output for each node.
  bma_pc_100_02[[e]] <- vector(mode = 'list',
                               length = dim(data_pc_100_02[[1]])[2])

  bma_pc_100_05[[e]] <- vector(mode = 'list',
                               length = dim(data_pc_100_05[[1]])[2])

  bma_pc_100_1[[e]] <- vector(mode = 'list',
                              length = dim(data_pc_100_1[[1]])[2])

  begin_time <- Sys.time()
  # Apply ScanBMA to each node in the network.
  for (v in 1:dim(data_pc_100_02[[1]])[2]) {

    bma_pc_100_02[[e]][[v]] <- ScanBMA(x = data_pc_100_02[[e]][, -v],
                                       y = data_pc_100_02[[e]][, v],
                                       prior.prob = 0.1,
                                       control = control)
    print(is.na(data_pc_100_02[[e]][, v]))


  }
  end_time <- Sys.time()

  # Calculate the runtime for each graph.
  times_pc_100_02[[e]] <- end_time - begin_time

  begin_time <- Sys.time()
  # Apply ScanBMA to each node in the network.
  for (v in 1:dim(data_pc_100_05[[1]])[2]) {

    bma_pc_100_05[[e]][[v]] <- ScanBMA(x = data_pc_100_05[[e]][, -v],
                                       y = data_pc_100_05[[e]][, v],
                                       prior.prob = 0.1,
                                       control = control)

  }
  end_time <- Sys.time()

  # Calculate the runtime for each graph.
  times_pc_100_05[[e]] <- end_time - begin_time

  begin_time <- Sys.time()
  # Apply ScanBMA to each node in the network.
  for (v in 1:dim(data_pc_100_1[[1]])[2]) {

    bma_pc_100_1[[e]][[v]] <- ScanBMA(x = data_pc_100_1[[e]][, -v],
                                      y = data_pc_100_1[[e]][, v],
                                      prior.prob = 0.1,
                                      control = control)

  }
  end_time <- Sys.time()

  # Calculate the runtime for each graph.
  times_pc_100_1[[e]] <- end_time - begin_time

}

# Calculate the precision and recall for each topology -------------------------

for (e in 1:M) {

  # Topology G2 --------------------------

  # Create posterior probability adjacency matrices from bma output.
  ppm_g2_100_02[[e]] <- am_bma(bma_g2_100_02[[e]])
  ppm_g2_100_05[[e]] <- am_bma(bma_g2_100_05[[e]])
  ppm_g2_100_1[[e]] <- am_bma(bma_g2_100_1[[e]])

  # Calculate the precision and power from the adjacency matrices.
  # bma_prerec_g2_100_02[[e]] <- prerec(amInferred = ppm_g2_100_02[[e]],
  #                                     amTrue = am_gn4,
  #                                     cutoff = 0.5)
  #
  # bma_prerec_g2_100_05[[e]] <- prerec(amInferred = ppm_g2_100_05[[e]],
  #                                     amTrue = am_gn4,
  #                                     cutoff = 0.5)
  #
  # bma_prerec_g2_100_1[[e]] <- prerec(amInferred = ppm_g2_100_1[[e]],
  #                                    amTrue = am_gn4,
  #                                    cutoff = 0.5)
  #
  # Topology NC11 --------------------------

  # Create posterior probability adjacency matrices from bma output.
  ppm_nc11_100_02[[e]] <- am_bma(bma_nc11_100_02[[e]])
  ppm_nc11_100_05[[e]] <- am_bma(bma_nc11_100_05[[e]])
  ppm_nc11_100_1[[e]] <- am_bma(bma_nc11_100_1[[e]])

  # Calculate the precision and power from the adjacency matrices.
  # bma_prerec_nc11_100_02[[e]] <- prerec(amInferred = ppm_nc11_100_02[[e]],
  #                                     amTrue = am_gn11,
  #                                     cutoff = 0.5)
  #
  # bma_prerec_nc11_100_05[[e]] <- prerec(amInferred = ppm_nc11_100_05[[e]],
  #                                     amTrue = am_gn11,
  #                                     cutoff = 0.5)
  #
  # bma_prerec_nc11_100_1[[e]] <- prerec(amInferred = ppm_nc11_100_1[[e]],
  #                                    amTrue = am_gn11,
  #                                    cutoff = 0.5)
  #
  # Topology PC --------------------------

  # Create posterior probability adjacency matrices from bma output.
  ppm_pc_100_02[[e]] <- am_bma(bma_pc_100_02[[e]])
  ppm_pc_100_05[[e]] <- am_bma(bma_pc_100_05[[e]])
  ppm_pc_100_1[[e]] <- am_bma(bma_pc_100_1[[e]])

  # Calculate the precision and power from the adjacency matrices.
  # bma_prerec_pc_100_02[[e]] <- prerec(amInferred = ppm_pc_100_02[[e]],
  #                                     amTrue = am_gn8,
  #                                     cutoff = 0.5)
  #
  # bma_prerec_pc_100_05[[e]] <- prerec(amInferred = ppm_pc_100_05[[e]],
  #                                     amTrue = am_gn8,
  #                                     cutoff = 0.5)
  #
  # bma_prerec_pc_100_1[[e]] <- prerec(amInferred = ppm_pc_100_1[[e]],
  #                                    amTrue = am_gn8,
  #                                    cutoff = 0.5)
  #
}

save(
     bma_g2_100_02,
     bma_g2_100_05,
     bma_g2_100_1,
     bma_nc11_100_02,
     bma_nc11_100_05,
     bma_nc11_100_1,
     bma_pc_100_02,
     bma_pc_100_05,
     bma_pc_100_1,
     ppm_g2_100_02,
     ppm_g2_100_05,
     ppm_g2_100_1,
     ppm_nc11_100_02,
     ppm_nc11_100_05,
     ppm_nc11_100_1,
     ppm_pc_100_02,
     ppm_pc_100_05,
     ppm_pc_100_1,
     file = './bma_ge_N_100_pm.RData')

# save(
#      bma_prerec_g2_100_02,
#      bma_prerec_g2_100_05,
#      bma_prerec_g2_100_1,
#      bma_prerec_nc11_100_02,
#      bma_prerec_nc11_100_05,
#      bma_prerec_nc11_100_1,
#      bma_prerec_pc_100_02,
#      bma_prerec_pc_100_05,
#      bma_prerec_pc_100_1,
#      file = './bma_prerec_ge_N_100.RData')
