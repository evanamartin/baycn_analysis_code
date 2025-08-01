library (baycn)

data ("geuvadis")

# adjacency matrix for graphs with three nodes.
am_3 <- matrix(c(0, 1, 1,
                 1, 0, 1,
                 1, 1, 0),
               nrow = 3,
               byrow = TRUE)

# adjacency matrix for graphs with three nodes.
am_4 <- matrix(c(0, 1, 1, 1,
                 1, 0, 1, 1,
                 1, 1, 0, 1,
                 1, 1, 1, 0),
               nrow = 4,
               byrow = TRUE)

# get the names of the data sets
geuvadis_names <- names(geuvadis)

# create a list to hold the output of baycn
bay_geuvadis <- vector(mode = 'list',
                       length = 46)

# name the baycn output according to the data set names
names(bay_geuvadis) <- geuvadis_names

set.seed(1020)

for (e in 1:46) {

  if (dim(geuvadis[[e]])[2] == 3) {

    bay_geuvadis[[e]] <- mhEdge(adjMatrix = am_3,
                                burnIn = 0.2,
                                data = geuvadis[[e]],
                                iterations = 30000,
                                nGV = 1,
                                pmr = TRUE,
                                prior = c(0.05,
                                          0.05,
                                          0.9),
                                progress = FALSE,
                                thinTo = 200)

  } else {

    bay_geuvadis[[e]] <- mhEdge(adjMatrix = am_4,
                                burnIn = 0.2,
                                data = geuvadis[[e]],
                                iterations = 30000,
                                nGV = 1,
                                pmr = TRUE,
                                prior = c(0.05,
                                          0.05,
                                          0.9),
                                progress = FALSE,
                                thinTo = 200)

  }

  print(e)
  print(Sys.time())

  save(bay_geuvadis,
       file = '/mnt/lfs2/mart9986/baycn/bay_geuvadis.RData')

}
