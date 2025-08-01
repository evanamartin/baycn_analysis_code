library(BCDAG)

# load the data
load('./data_ge_N_100.RData')

# Initialize model parameters
# a is the shape parameter
w = 0.05
n = 100
a_g2 = 4
a_nc11 = 11 
a_pc = 8
U_g2 = diag(1,a_g2)/n
U_nc11 = diag(1,a_nc11)/n
U_pc = diag(1,a_pc)/n

# Initialize S and burn for the three graphs: g2, nc11, pc
S_g2 <- 30000
burn_g2 <- 6000

S_nc11 <- 50000
burn_nc11 <- 10000

S_pc <- 50000
burn_pc <- 10000

# Initiate lists for the output of each method to full length.

# Graph G2
# out_ will he the output of the learn_DAG()
out_g2_100_02 <- vector(mode="list", length=M)
out_g2_100_05 <- vector(mode="list", length=M)
out_g2_100_1 <- vector(mode="list", length=M)
out_g2_100_02_time <- vector(mode="list", length=M)
out_g2_100_05_time <- vector(mode="list", length=M)
out_g2_100_1_time <- vector(mode="list", length=M)

# mat_ will the edge probabilities
mat_g2_100_02 <- vector(mode="list", length=M)
mat_g2_100_05 <- vector(mode="list", length=M)
mat_g2_100_1 <- vector(mode="list", length=M)
mat_g2_100_02_time <- vector(mode="list", length=M)
mat_g2_100_05_time <- vector(mode="list", length=M)
mat_g2_100_1_time <- vector(mode="list", length=M)

# Graph G2
out_nc11_100_02 <- vector(mode="list", length=M)
out_nc11_100_05 <- vector(mode="list", length=M)
out_nc11_100_1 <- vector(mode="list", length=M)
out_nc11_100_02_time <- vector(mode="list", length=M)
out_nc11_100_05_time <- vector(mode="list", length=M)
out_nc11_100_1_time <- vector(mode="list", length=M)

mat_nc11_100_02 <- vector(mode="list", length=M)
mat_nc11_100_05 <- vector(mode="list", length=M)
mat_nc11_100_1 <- vector(mode="list", length=M)
mat_nc11_100_02_time <- vector(mode="list", length=M)
mat_nc11_100_05_time <- vector(mode="list", length=M)
mat_nc11_100_1_time <- vector(mode="list", length=M)

# Graph G2
out_pc_100_02 <- vector(mode="list", length=M)
out_pc_100_05 <- vector(mode="list", length=M)
out_pc_100_1 <- vector(mode="list", length=M)
out_pc_100_02_time <- vector(mode="list", length=M)
out_pc_100_05_time <- vector(mode="list", length=M)
out_pc_100_1_time <- vector(mode="list", length=M)

mat_pc_100_02 <- vector(mode="list", length=M)
mat_pc_100_05 <- vector(mode="list", length=M)
mat_pc_100_1 <- vector(mode="list", length=M)
mat_pc_100_02_time <- vector(mode="list", length=M)
mat_pc_100_05_time <- vector(mode="list", length=M)
mat_pc_100_1_time <- vector(mode="list", length=M)


# Loop through each combination of signal strength and sample size for all topologies
for (e in 1:M) {

  ####################################
  # G2 
  ####################################

  starttime <- Sys.time()
  out_g2_100_02[[e]] = learn_DAG(S=S_g2, burn=burn_g2, data=data_g2_100_02[[e]], a=a_g2, U=U_g2, w=w, verbose=FALSE)
  endtime <- Sys.time()

  mat_g2_100_02[[e]] = get_edgeprobs(out_g2_100_02[[e]])
  out_g2_100_02_time[[e]] = endtime - starttime

  starttime <- Sys.time()
  out_g2_100_05[[e]] = learn_DAG(S=S_g2, burn=burn_g2, data=data_g2_100_05[[e]], a=a_g2, U=U_g2, w=w, verbose=FALSE)
  endtime <- Sys.time()

  mat_g2_100_05[[e]] = get_edgeprobs(out_g2_100_05[[e]])
  out_g2_100_05_time[[e]] = endtime - starttime

  starttime <- Sys.time()
  out_g2_100_1[[e]] = learn_DAG(S=S_g2, burn=burn_g2, data=data_g2_100_1[[e]], a=a_g2, U=U_g2, w=w, verbose=FALSE)
  endtime <- Sys.time()

  mat_g2_100_1[[e]] = get_edgeprobs(out_g2_100_1[[e]])
  out_g2_100_1_time[[e]] = endtime - starttime

  ####################################
  # NC11 
  ####################################

  starttime <- Sys.time()
  out_nc11_100_02[[e]] = learn_DAG(S=S_nc11, burn=burn_nc11, data=data_nc11_100_02[[e]], a=a_nc11, U=U_nc11, w=w, verbose=FALSE)
  endtime <- Sys.time()

  mat_nc11_100_02[[e]] = get_edgeprobs(out_nc11_100_02[[e]])
  out_nc11_100_02_time[[e]] = endtime - starttime

  starttime <- Sys.time()
  out_nc11_100_05[[e]] = learn_DAG(S=S_nc11, burn=burn_nc11, data=data_nc11_100_05[[e]], a=a_nc11, U=U_nc11, w=w, verbose=FALSE)
  endtime <- Sys.time()

  mat_nc11_100_05[[e]] = get_edgeprobs(out_nc11_100_05[[e]])
  out_nc11_100_05_time[[e]] = endtime - starttime

  starttime <- Sys.time()
  out_nc11_100_1[[e]] = learn_DAG(S=S_nc11, burn=burn_nc11, data=data_nc11_100_1[[e]], a=a_nc11, U=U_nc11, w=w, verbose=FALSE)
  endtime <- Sys.time()

  mat_nc11_100_1[[e]] = get_edgeprobs(out_nc11_100_1[[e]])
  out_nc11_100_1_time[[e]] = endtime - starttime

  ####################################
  # PC 
  ####################################

  starttime <- Sys.time()
  out_pc_100_02[[e]] = learn_DAG(S=S_pc, burn=burn_pc, data=data_pc_100_02[[e]], a=a_pc, U=U_pc, w=w, verbose=FALSE)
  endtime <- Sys.time()

  mat_pc_100_02[[e]] = get_edgeprobs(out_pc_100_02[[e]])
  out_pc_100_02_time[[e]] = endtime - starttime

  starttime <- Sys.time()
  out_pc_100_05[[e]] = learn_DAG(S=S_pc, burn=burn_pc, data=data_pc_100_05[[e]], a=a_pc, U=U_pc, w=w, verbose=FALSE)
  endtime <- Sys.time()

  mat_pc_100_05[[e]] = get_edgeprobs(out_pc_100_05[[e]])
  out_pc_100_05_time[[e]] = endtime - starttime

  starttime <- Sys.time()
  out_pc_100_1[[e]] = learn_DAG(S=S_pc, burn=burn_pc, data=data_pc_100_1[[e]], a=a_pc, U=U_pc, w=w, verbose=FALSE)
  endtime <- Sys.time()

  mat_pc_100_1[[e]] = get_edgeprobs(out_pc_100_1[[e]])
  out_pc_100_1_time[[e]] = endtime - starttime

}


save(M,
     # out_g2_100_02,
     # out_g2_100_05,
     # out_g2_100_1,
     # out_nc11_100_02,
     # out_nc11_100_05,
     # out_nc11_100_1,
     # out_pc_100_02,
     # out_pc_100_05,
     # out_pc_100_1,
     mat_g2_100_02,
     mat_g2_100_05,
     mat_g2_100_1,
     mat_nc11_100_02,
     mat_nc11_100_05,
     mat_nc11_100_1,
     mat_pc_100_02,
     mat_pc_100_05,
     mat_pc_100_1,
     file = "./bcdag_ge_N_100.RData"
)

save(M,
     out_g2_100_02_time,
     out_g2_100_05_time,
     out_g2_100_1_time,
     out_nc11_100_02_time,
     out_nc11_100_05_time,
     out_nc11_100_1_time,
     out_pc_100_02_time,
     out_pc_100_05_time,
     out_pc_100_1_time,
     file = "./bcdag_ge_N_100_time.RData"
)
