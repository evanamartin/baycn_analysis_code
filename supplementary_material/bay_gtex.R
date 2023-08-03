library (baycn,
         lib = '/mnt/lfs2/mart9986/Rpackages')

load('/mnt/lfs2/mart9986/data/data_gtex.RData')

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
gtex_names <- names(data_gtex)

# create a list to hold the output of baycn
bay_gtex <- vector(mode = 'list',
                   length = 46)

# name the baycn output according to the data set names
names(bay_gtex) <- gtex_names

set.seed(2045)

for (e in 1:46) {

  if (dim(data_gtex[[e]])[2] == 3) {

    bay_gtex[[e]] <- mhEdge(adjMatrix = am_3,
                            burnIn = 0.2,
                            data = data_gtex[[e]],
                            iterations = 30000,
                            nGV = 1,
                            pmr = TRUE,
                            prior = c(0.05,
                                      0.05,
                                      0.9),
                            progress = FALSE,
                            thinTo = 200)

  } else {

    bay_gtex[[e]] <- mhEdge(adjMatrix = am_4,
                            burnIn = 0.2,
                            data = data_gtex[[e]],
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

  save(bay_gtex,
       file = '/mnt/lfs2/mart9986/baycn/bay_gtex.RData')

}
