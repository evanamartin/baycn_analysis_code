load("./venkata_mse2_all_100.RData")
load("./venkata_mse2_all_200.RData")
load("./venkata_mse2_all_600.RData")
load("./venkata_prerec_all_100.RData")
load("./venkata_prerec_all_200.RData")
load("./venkata_prerec_all_600.RData")

M = 25

# Returns the mean then the std
getPrecisionValues <- function(results) {
    tmpList = rep(NA, M)
    for (e in 1:M) {
      tmpList[e] <- results[[e]]$precision
    }
    return (round(c(mean(tmpList), sd(tmpList)), 2))
}

# Returns the mean then the std
getRecallValues <- function(results) {
    tmpList = rep(NA, M)
    for (e in 1:M) {
      tmpList[e] <- results[[e]]$recall
    }
    return (round(c(mean(tmpList), sd(tmpList)), 2))
}

# Returns the mean then the std
getMSEValues <- function(results) {
    tmpList = rep(NA, M)
    for (e in 1:M) {
      tmpList[e] <- results[[e]]
    }
    return (round(c(mean(tmpList), sd(tmpList)), 2))
}

createRow <- function(method, n, b, prerec, mse) {
  if (typeof(mse) == "NULL") {
    return (data.frame(
               Method = method,
               N = n, 
               B = b, 
               Precision.Mean = getPrecisionValues(prerec)[1],
               Precision.SD = getPrecisionValues(prerec)[2],
               Recall.Mean = getRecallValues(prerec)[1],
               Recall.SD = getRecallValues(prerec)[2],
               MSE.Mean = "-",
               MSE.SD = "-"
              )
            )
  }

  return (data.frame(
             Method = method,
             N = n, 
             B = b, 
             Precision.Mean = getPrecisionValues(prerec)[1],
             Precision.SD = getPrecisionValues(prerec)[2],
             Recall.Mean = getRecallValues(prerec)[1],
             Recall.SD = getRecallValues(prerec)[2],
             MSE.Mean = getMSEValues(mse)[1],
             MSE.SD = getMSEValues(mse)[2]
            )
          )
}

df <- rbind(
  # baycn pc ----------------------------------------------------------
  createRow("baycn", 100, 0.2, bay_prerec_pc_100_02, bay_mse_pc_100_02),
  createRow("baycn", 100, 0.5, bay_prerec_pc_100_05, bay_mse_pc_100_05),
  createRow("baycn", 100, 1,   bay_prerec_pc_100_1,  bay_mse_pc_100_1) ,
  createRow("baycn", 200, 0.2, bay_prerec_pc_200_02, bay_mse_pc_200_02),
  createRow("baycn", 200, 0.5, bay_prerec_pc_200_05, bay_mse_pc_200_05),
  createRow("baycn", 200, 1,   bay_prerec_pc_200_1,  bay_mse_pc_200_1) ,
  createRow("baycn", 600, 0.2, bay_prerec_pc_600_02, bay_mse_pc_600_02),
  createRow("baycn", 600, 0.5, bay_prerec_pc_600_05, bay_mse_pc_600_05),
  createRow("baycn", 600, 1,   bay_prerec_pc_600_1,  bay_mse_pc_600_1) ,

  # mc3 pc ----------------------------------------------------------
  createRow("mc3", 100, 0.2, mc3_prerec_pc_100_02, mc3_mse_pc_100_02),
  createRow("mc3", 100, 0.5, mc3_prerec_pc_100_05, mc3_mse_pc_100_05),
  createRow("mc3", 100, 1,   mc3_prerec_pc_100_1,  mc3_mse_pc_100_1) ,
  createRow("mc3", 200, 0.2, mc3_prerec_pc_200_02, mc3_mse_pc_200_02),
  createRow("mc3", 200, 0.5, mc3_prerec_pc_200_05, mc3_mse_pc_200_05),
  createRow("mc3", 200, 1,   mc3_prerec_pc_200_1,  mc3_mse_pc_200_1) ,
  createRow("mc3", 600, 0.2, mc3_prerec_pc_600_02, mc3_mse_pc_600_02),
  createRow("mc3", 600, 0.5, mc3_prerec_pc_600_05, mc3_mse_pc_600_05),
  createRow("mc3", 600, 1,   mc3_prerec_pc_600_1,  mc3_mse_pc_600_1) ,

  # order pc ----------------------------------------------------------
  createRow("order", 100, 0.2, ord_prerec_pc_100_02, ord_mse_pc_100_02),
  createRow("order", 100, 0.5, ord_prerec_pc_100_05, ord_mse_pc_100_05),
  createRow("order", 100, 1,   ord_prerec_pc_100_1,  ord_mse_pc_100_1) ,
  createRow("order", 200, 0.2, ord_prerec_pc_200_02, ord_mse_pc_200_02),
  createRow("order", 200, 0.5, ord_prerec_pc_200_05, ord_mse_pc_200_05),
  createRow("order", 200, 1,   ord_prerec_pc_200_1,  ord_mse_pc_200_1) ,
  createRow("order", 600, 0.2, ord_prerec_pc_600_02, ord_mse_pc_600_02),
  createRow("order", 600, 0.5, ord_prerec_pc_600_05, ord_mse_pc_600_05),
  createRow("order", 600, 1,   ord_prerec_pc_600_1,  ord_mse_pc_600_1) ,

  # partition pc ----------------------------------------------------------
  createRow("partition", 100, 0.2, par_prerec_pc_100_02, par_mse_pc_100_02),
  createRow("partition", 100, 0.5, par_prerec_pc_100_05, par_mse_pc_100_05),
  createRow("partition", 100, 1,   par_prerec_pc_100_1,  par_mse_pc_100_1) ,
  createRow("partition", 200, 0.2, par_prerec_pc_200_02, par_mse_pc_200_02),
  createRow("partition", 200, 0.5, par_prerec_pc_200_05, par_mse_pc_200_05),
  createRow("partition", 200, 1,   par_prerec_pc_200_1,  par_mse_pc_200_1) ,
  createRow("partition", 600, 0.2, par_prerec_pc_600_02, par_mse_pc_600_02),
  createRow("partition", 600, 0.5, par_prerec_pc_600_05, par_mse_pc_600_05),
  createRow("partition", 600, 1,   par_prerec_pc_600_1,  par_mse_pc_600_1) ,

  # bcdag pc ----------------------------------------------------------
  createRow("bcdag", 100, 0.2, bcdag_prerec_pc_100_02, bcdag_mse_pc_100_02),
  createRow("bcdag", 100, 0.5, bcdag_prerec_pc_100_05, bcdag_mse_pc_100_05),
  createRow("bcdag", 100, 1,   bcdag_prerec_pc_100_1,  bcdag_mse_pc_100_1) ,
  createRow("bcdag", 200, 0.2, bcdag_prerec_pc_200_02, bcdag_mse_pc_200_02),
  createRow("bcdag", 200, 0.5, bcdag_prerec_pc_200_05, bcdag_mse_pc_200_05),
  createRow("bcdag", 200, 1,   bcdag_prerec_pc_200_1,  bcdag_mse_pc_200_1) ,
  createRow("bcdag", 600, 0.2, bcdag_prerec_pc_600_02, bcdag_mse_pc_600_02),
  createRow("bcdag", 600, 0.5, bcdag_prerec_pc_600_05, bcdag_mse_pc_600_05),
  createRow("bcdag", 600, 1,   bcdag_prerec_pc_600_1,  bcdag_mse_pc_600_1) ,

  # scanBMA pc ----------------------------------------------------------
  createRow("scanBMA", 100, 0.2, bma_prerec_pc_100_02, NULL),
  createRow("scanBMA", 100, 0.5, bma_prerec_pc_100_05, NULL),
  createRow("scanBMA", 100, 1,   bma_prerec_pc_100_1,  NULL) ,
  createRow("scanBMA", 200, 0.2, bma_prerec_pc_200_02, NULL),
  createRow("scanBMA", 200, 0.5, bma_prerec_pc_200_05, NULL),
  createRow("scanBMA", 200, 1,   bma_prerec_pc_200_1,  NULL) ,
  createRow("scanBMA", 600, 0.2, bma_prerec_pc_600_02, NULL),
  createRow("scanBMA", 600, 0.5, bma_prerec_pc_600_05, NULL),
  createRow("scanBMA", 600, 1,   bma_prerec_pc_600_1,  NULL) 
)

write.table(df, quote = FALSE, file = "./pc_prerec_mse2_all_methods_table.tsv", sep = "\t", row.names = FALSE)
