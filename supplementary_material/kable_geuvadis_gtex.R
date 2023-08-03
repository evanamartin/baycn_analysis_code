# Read in the geuvadis and gtex data
load('~/Sync/Evan/classesUofI/BCB600/baycn/geuvadis_gtex/bay_geuvadis.RData')
load('~/Sync/Evan/classesUofI/BCB600/baycn/geuvadis_gtex/bay_geuvadis_pcs.RData')
load('~/Sync/Evan/classesUofI/BCB600/baycn/geuvadis_gtex/bay_gtex.RData')

# Create booktabs tables for GEUVADIS and GTEx ---------------------------------

# Loop through each of the 46 data sets and create a booktabs table with
# GEUVADIS output in the first three columns and GTEx output in the last three
# columns.
for (e in 1:46) {

  print(names(bay_geuvadis)[[e]])

  print(kableExtra::kable(format = 'latex',
                          booktabs = TRUE,
                          cbind(bay_geuvadis[[e]]@posteriorES,
                                bay_gtex[[e]]@posteriorES[, 2:4])))

}

# Create booktabs table for GEUVADIS with PCs ----------------------------------

# Q8
kableExtra::kable(format = 'latex',
                  booktabs = TRUE,
                  bay_Q8_pc@posteriorES)

# Q21
kableExtra::kable(format = 'latex',
                  booktabs = TRUE,
                  bay_Q21_pc@posteriorES)

# Q23
kableExtra::kable(format = 'latex',
                  booktabs = TRUE,
                  bay_Q23_pc@posteriorES)

# Q37
kableExtra::kable(format = 'latex',
                  booktabs = TRUE,
                  bay_Q37_pc@posteriorES)

# Q50
kableExtra::kable(format = 'latex',
                  booktabs = TRUE,
                  bay_Q50_pc@posteriorES)
