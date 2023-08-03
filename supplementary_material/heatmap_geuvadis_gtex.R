# Load libraryies --------------------------------------------------------------

library (reshape2)
library (ggplot2)
library (egg)

# Load GEUVADIS and GTEx data --------------------------------------------------

load('~/Sync/Evan/classesUofI/BCB600/datasets/geuvadis_gtex/data_geuvadis.RData')
load('~/Sync/Evan/classesUofI/BCB600/datasets/geuvadis_gtex/data_gtex.RData')

# Correlation heatmaps ---------------------------------------------------------

# Q20 ----------------------------------

# Calculate the correlation of Q20 for geuvadis.
cor_geu_Q20 <- round(cor(na.omit(data_geuvadis$Q20)), 2)

# Melt the correlation matrix.
melted_cor_geu_Q20 <- melt(cor_geu_Q20)

# plot the correlation heatmap.
p_geu_Q20 <- ggplot(data = melted_cor_geu_Q20,
                    aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  ggtitle('Q20 GEUVADIS') +
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  scale_fill_gradient2(low = "purple", high = "orange", mid = "white",
                       midpoint = 0, limit = c(-1,1), space = "Lab") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(angle = 45,
                                   hjust = 1),
        legend.position = 'none')

# Calculate the correlation of Q20 for gtex.
cor_gte_Q20 <- round(cor(na.omit(data_gtex$Q20)), 2)

# Melt the correlation matrix.
melted_cor_gte_Q20 <- melt(cor_gte_Q20)

# plot the correlation heatmap.
p_gte_Q20 <- ggplot(data = melted_cor_gte_Q20,
                    aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  ggtitle('Q20 GTEx') +
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  scale_fill_gradient2(low = "purple", high = "orange", mid = "white",
                       midpoint = 0, limit = c(-1,1), space = "Lab") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(angle = 45,
                                   hjust = 1))

egg::ggarrange(p_geu_Q20, p_gte_Q20, ncol = 2)

# Q21 ----------------------------------

# Calculate the correlation of Q21 for geuvadis.
cor_geu_Q21 <- round(cor(na.omit(data_geuvadis$Q21)), 2)

# Melt the correlation matrix.
melted_cor_geu_Q21 <- melt(cor_geu_Q21)

# plot the correlation heatmap.
p_geu_Q21 <- ggplot(data = melted_cor_geu_Q21,
                    aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  ggtitle('Q21 GEUVADIS') +
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  scale_fill_gradient2(low = "purple", high = "orange", mid = "white",
                       midpoint = 0, limit = c(-1,1), space = "Lab") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(angle = 45,
                                   hjust = 1),
        legend.position = 'none')

# Calculate the correlation of Q21 for gtex.
cor_gte_Q21 <- round(cor(na.omit(data_gtex$Q21)), 2)

# Melt the correlation matrix.
melted_cor_gte_Q21 <- melt(cor_gte_Q21)

# plot the correlation heatmap.
p_gte_Q21 <- ggplot(data = melted_cor_gte_Q21,
                    aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  ggtitle('Q21 GTEx') +
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  scale_fill_gradient2(low = "purple", high = "orange", mid = "white",
                       midpoint = 0, limit = c(-1,1), space = "Lab") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(angle = 45,
                                   hjust = 1))

egg::ggarrange(p_geu_Q21, p_gte_Q21, ncol = 2)

# Q23 ----------------------------------

# Calculate the correlation of Q23 for geuvadis.
cor_geu_Q23 <- round(cor(na.omit(data_geuvadis$Q23)), 2)

# Melt the correlation matrix.
melted_cor_geu_Q23 <- melt(cor_geu_Q23)

# plot the correlation heatmap.
p_geu_Q23 <- ggplot(data = melted_cor_geu_Q23,
                    aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  ggtitle('Q23 GEUVADIS') +
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  scale_fill_gradient2(low = "purple", high = "orange", mid = "white",
                       midpoint = 0, limit = c(-1,1), space = "Lab") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(angle = 45,
                                   hjust = 1),
        legend.position = 'none')

# Calculate the correlation of Q23 for gtex.
cor_gte_Q23 <- round(cor(na.omit(data_gtex$Q23)), 2)

# Melt the correlation matrix.
melted_cor_gte_Q23 <- melt(cor_gte_Q23)

# plot the correlation heatmap.
p_gte_Q23 <- ggplot(data = melted_cor_gte_Q23,
                    aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  ggtitle('Q23 GTEx') +
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  scale_fill_gradient2(low = "purple", high = "orange", mid = "white",
                       midpoint = 0, limit = c(-1,1), space = "Lab") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(angle = 45,
                                   hjust = 1))

egg::ggarrange(p_geu_Q23, p_gte_Q23, ncol = 2)

# Q37 ----------------------------------

# Calculate the correlation of Q37 for geuvadis.
cor_geu_Q37 <- round(cor(na.omit(data_geuvadis$Q37)), 2)

# Melt the correlation matrix.
melted_cor_geu_Q37 <- melt(cor_geu_Q37)

# plot the correlation heatmap.
p_geu_Q37 <- ggplot(data = melted_cor_geu_Q37,
                    aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  ggtitle('Q37 GEUVADIS') +
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  scale_fill_gradient2(low = "purple", high = "orange", mid = "white",
                       midpoint = 0, limit = c(-1,1), space = "Lab") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(angle = 45,
                                   hjust = 1),
        legend.position = 'none')

# Calculate the correlation of Q37 for gtex.
cor_gte_Q37 <- round(cor(na.omit(data_gtex$Q37)), 2)

# Melt the correlation matrix.
melted_cor_gte_Q37 <- melt(cor_gte_Q37)

# plot the correlation heatmap.
p_gte_Q37 <- ggplot(data = melted_cor_gte_Q37,
                    aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  ggtitle('Q37 GTEx') +
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  scale_fill_gradient2(low = "purple", high = "orange", mid = "white",
                       midpoint = 0, limit = c(-1,1), space = "Lab") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(angle = 45,
                                   hjust = 1))

egg::ggarrange(p_geu_Q37, p_gte_Q37, ncol = 2)

# Q50 ----------------------------------

# Calculate the correlation of Q50 for geuvadis.
cor_geu_Q50 <- round(cor(na.omit(data_geuvadis$Q50)), 2)

# Melt the correlation matrix.
melted_cor_geu_Q50 <- melt(cor_geu_Q50)

# plot the correlation heatmap.
p_geu_Q50 <- ggplot(data = melted_cor_geu_Q50,
                    aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  ggtitle('Q50 GEUVADIS') +
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  scale_fill_gradient2(low = "purple", high = "orange", mid = "white",
                       midpoint = 0, limit = c(-1,1), space = "Lab") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(angle = 45,
                                   hjust = 1),
        legend.position = 'none')

# Calculate the correlation of Q50 for gtex.
cor_gte_Q50 <- round(cor(na.omit(data_gtex$Q50)), 2)

# Melt the correlation matrix.
melted_cor_gte_Q50 <- melt(cor_gte_Q50)

# plot the correlation heatmap.
p_gte_Q50 <- ggplot(data = melted_cor_gte_Q50,
                    aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  ggtitle('Q50 GTEx') +
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  scale_fill_gradient2(low = "purple", high = "orange", mid = "white",
                       midpoint = 0, limit = c(-1,1), space = "Lab") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(angle = 45,
                                   hjust = 1))

egg::ggarrange(p_geu_Q50, p_gte_Q50, ncol = 2)

# Q62 ----------------------------------

# Calculate the correlation of Q62 for geuvadis.
cor_geu_Q62 <- round(cor(na.omit(data_geuvadis$Q62)), 2)

# Melt the correlation matrix.
melted_cor_geu_Q62 <- melt(cor_geu_Q62)

# plot the correlation heatmap.
p_geu_Q62 <- ggplot(data = melted_cor_geu_Q62,
                    aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  ggtitle('Q62 GEUVADIS') +
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  scale_fill_gradient2(low = "purple", high = "orange", mid = "white",
                       midpoint = 0, limit = c(-1,1), space = "Lab") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(angle = 45,
                                   hjust = 1),
        legend.position = 'none')

# Calculate the correlation of Q62 for gtex.
cor_gte_Q62 <- round(cor(na.omit(data_gtex$Q62)), 2)

# Melt the correlation matrix.
melted_cor_gte_Q62 <- melt(cor_gte_Q62)

# plot the correlation heatmap.
p_gte_Q62 <- ggplot(data = melted_cor_gte_Q62,
                    aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  ggtitle('Q62 GTEx') +
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  scale_fill_gradient2(low = "purple", high = "orange", mid = "white",
                       midpoint = 0, limit = c(-1,1), space = "Lab") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(angle = 45,
                                   hjust = 1))

egg::ggarrange(p_geu_Q62, p_gte_Q62, ncol = 2)

# Q8 -----------------------------------

# Calculate the correlation of Q8 for geuvadis.
cor_geu_Q8 <- round(cor(na.omit(data_geuvadis$Q8)), 2)

# Melt the correlation matrix.
melted_cor_geu_Q8 <- melt(cor_geu_Q8)

# plot the correlation heatmap.
p_geu_Q8 <- ggplot(data = melted_cor_geu_Q8,
                   aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  ggtitle('Q8 GEUVADIS') +
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  scale_fill_gradient2(low = "purple", high = "orange", mid = "white",
                       midpoint = 0, limit = c(-1,1), space = "Lab") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(angle = 45,
                                   hjust = 1),
        legend.position = 'none')

# Calculate the correlation of Q8 for gtex.
cor_gte_Q8 <- round(cor(na.omit(data_gtex$Q8)), 2)

# Melt the correlation matrix.
melted_cor_gte_Q8 <- melt(cor_gte_Q8)

# plot the correlation heatmap.
p_gte_Q8 <- ggplot(data = melted_cor_gte_Q8,
                   aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  ggtitle('Q8 GTEx') +
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  scale_fill_gradient2(low = "purple", high = "orange", mid = "white",
                       midpoint = 0, limit = c(-1,1), space = "Lab") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(angle = 45,
                                   hjust = 1))

egg::ggarrange(p_geu_Q8, p_gte_Q8, ncol = 2)
