########### read performance evaluation ###########
# G2
g2_all <- read.delim("grid_simulation_analysis/g2_prerec_mse2_all_methods_table.tsv", header = TRUE, sep = "\t", na.strings = "-")

# pc
pc_all <- read.delim("grid_simulation_analysis/pc_prerec_mse2_all_methods_table.tsv", header = TRUE, sep = "\t", na.strings = "-")

# nc11
nc11_all <- read.delim("nc11_mse2_prerec_all/nc11_prerec_mse2_all_methods_table_merged.tsv", header = TRUE, sep = "\t", na.strings = "-")


########### visualization ###########
methods <- c(setdiff(unique(nc11_all$Method), "baycn"), "baycn")
# "baycn", "mc3", "order", "partition", "bcdag", "scanBMA"
mycolors <- c("orange", "green", "magenta", "blue", "pink", "black")

`%||%` <- function(a, b) {
  if (!is.null(a)) a else b
}

plot_with_SD_bars <- function(data, xvar, yvar, xsdvar, ysdvar, 
                                 method_col = "Method", methods = NULL, 
                                 colors = NULL, pch = 16, cex = 0.8, 
                                 xlab = NULL, ylab = NULL, diagonal=FALSE, ...) {
  
  # Map each method to color
  method_colors <- setNames(colors, methods)
  
  # Plot base
  plot(data[[xvar]], data[[yvar]], type = "n",
       xlab = xlab %||% xvar, ylab = ylab %||% yvar, ...)
  if (diagonal){
    abline(a = 0, b = 1, lty=2)
  }
  
  for (m in methods) {
    idx <- which(data[[method_col]] == m)
    x <- data[[xvar]][idx]
    y <- data[[yvar]][idx]
    xsd <- data[[xsdvar]][idx]
    ysd <- data[[ysdvar]][idx]
    col <- method_colors[[m]]
    
    # Error bars
    arrows(x - xsd, y, x + xsd, y, angle = 90, code = 3, length = 0.02, col = col)
    arrows(x, y - ysd, x, y + ysd, angle = 90, code = 3, length = 0.02, col = col)
    
    # Points
    points(x, y, col = col, pch = pch, cex = cex)
  }
}

#### NC11
# recall vs precision
plot_with_SD_bars(nc11_all, 
                     xvar = "Precision.Mean", yvar = "Recall.Mean", 
                     xsdvar = "Precision.SD", ysdvar = "Recall.SD",
                     methods = methods,
                     colors = mycolors,
                     xlab = "Precision", ylab = "Recall", diagonal = TRUE, xlim=c(0, 1), ylim=c(0,1))

# MSE2 vs precision
plot_with_SD_bars(nc11_all, 
                  xvar = "Precision.Mean", yvar = "MSE.Mean", 
                  xsdvar = "Precision.SD", ysdvar = "MSE.SD",
                  methods = methods,
                  colors = mycolors,
                  xlab = "Precision", ylab = "MSE2", xlim=c(0, 1), ylim=c(0, 0.1))

# MSE2 vs recall
plot_with_SD_bars(nc11_all, 
                  xvar = "Recall.Mean", yvar = "MSE.Mean", 
                  xsdvar = "Recall.SD", ysdvar = "MSE.SD",
                  methods = methods,
                  colors = mycolors,
                  xlab = "Recall", ylab = "MSE2", xlim=c(0, 1), ylim=c(0, 0.1))


#### pc
# recall vs precision
plot_with_SD_bars(pc_all, 
                  xvar = "Precision.Mean", yvar = "Recall.Mean", 
                  xsdvar = "Precision.SD", ysdvar = "Recall.SD",
                  methods = methods,
                  colors = mycolors,
                  xlab = "Precision", ylab = "Recall", diagonal = TRUE, xlim=c(0, 1), ylim=c(0,1))

# MSE2 vs precision
plot_with_SD_bars(pc_all, 
                  xvar = "Precision.Mean", yvar = "MSE.Mean", 
                  xsdvar = "Precision.SD", ysdvar = "MSE.SD",
                  methods = methods,
                  colors = mycolors,
                  xlab = "Precision", ylab = "MSE2", xlim=c(0, 1), ylim=c(0, 0.2))

# MSE2 vs recall
plot_with_SD_bars(pc_all, 
                  xvar = "Recall.Mean", yvar = "MSE.Mean", 
                  xsdvar = "Recall.SD", ysdvar = "MSE.SD",
                  methods = methods,
                  colors = mycolors,
                  xlab = "Recall", ylab = "MSE2", xlim=c(0, 1), ylim=c(0, 0.1))

#### g2
# recall vs precision
plot_with_SD_bars(g2_all, 
                  xvar = "Precision.Mean", yvar = "Recall.Mean", 
                  xsdvar = "Precision.SD", ysdvar = "Recall.SD",
                  methods = methods,
                  colors = mycolors,
                  xlab = "Precision", ylab = "Recall", diagonal = TRUE, xlim=c(0, 1), ylim=c(0,1))

# MSE2 vs precision
plot_with_SD_bars(g2_all, 
                  xvar = "Precision.Mean", yvar = "MSE.Mean", 
                  xsdvar = "Precision.SD", ysdvar = "MSE.SD",
                  methods = methods,
                  colors = mycolors,
                  xlab = "Precision", ylab = "MSE2", xlim=c(0, 1), ylim=c(0, 0.3))

# MSE2 vs recall
plot_with_SD_bars(g2_all, 
                  xvar = "Recall.Mean", yvar = "MSE.Mean", 
                  xsdvar = "Recall.SD", ysdvar = "MSE.SD",
                  methods = methods,
                  colors = mycolors,
                  xlab = "Recall", ylab = "MSE2", xlim=c(0, 1), ylim=c(0, 0.3))
