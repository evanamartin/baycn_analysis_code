# Load libraries ---------------------------------------------------------------

library (reshape2)
library (ggplot2)
library (egg)

# Load data --------------------------------------------------------------------

load("~/Sync/Evan/classesUofI/BCB600/datasets/geuvadis_gtex/data_geuvadis_pcs.RData")

# Correlation heatmaps ---------------------------------------------------------

# Q8 with PCs --------------------------

# Calculate the correlation of Q8 for geuvadis.
cor_geu_Q8 <- round(cor(na.omit(data_Q8_pc)), 2)

# Melt the correlation matrix.
melted_cor_geu_Q8 <- melt(cor_geu_Q8)

# plot the correlation heatmap.
p_geu_Q8 <- ggplot(data = melted_cor_geu_Q8,
                   aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  # ggtitle('Q8 GEUVADIS') +
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 3) +
  scale_fill_gradient2(low = "purple", high = "orange", mid = "white",
                       midpoint = 0, limit = c(-1,1), space = "Lab") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        legend.position = 'none') +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))

# Q21 with PCs -------------------------

# Calculate the correlation of Q21 for geuvadis with PCs.
cor_geu_Q21 <- round(cor(na.omit(data_Q21_pc)), 2)

# Melt the correlation matrix.
melted_cor_geu_Q21 <- melt(cor_geu_Q21)

# plot the correlation heatmap.
p_geu_Q21 <- ggplot(data = melted_cor_geu_Q21,
                    aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  # ggtitle('Q21 GEUVADIS') +
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 3) +
  scale_fill_gradient2(low = "purple", high = "orange", mid = "white",
                       midpoint = 0, limit = c(-1,1), space = "Lab") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        legend.position = 'none') +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))

# Q23 with PCs -------------------------

# Calculate the correlation of Q23 for geuvadis.
cor_geu_Q23 <- round(cor(na.omit(data_Q23_pc)), 2)

# Melt the correlation matrix.
melted_cor_geu_Q23 <- melt(cor_geu_Q23)

# plot the correlation heatmap.
p_geu_Q23 <- ggplot(data = melted_cor_geu_Q23,
                    aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  # ggtitle('Q23 GEUVADIS') +
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 3) +
  scale_fill_gradient2(low = "purple", high = "orange", mid = "white",
                       midpoint = 0, limit = c(-1,1), space = "Lab") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank()) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))

# Q37 with PCs -------------------------

# Calculate the correlation of Q37 for geuvadis.
cor_geu_Q37 <- round(cor(na.omit(data_Q37_pc)), 2)

# Melt the correlation matrix.
melted_cor_geu_Q37 <- melt(cor_geu_Q37)

# plot the correlation heatmap.
p_geu_Q37 <- ggplot(data = melted_cor_geu_Q37,
                    aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  # ggtitle('Q37 GEUVADIS') +
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 3) +
  scale_fill_gradient2(low = "purple", high = "orange", mid = "white",
                       midpoint = 0, limit = c(-1,1), space = "Lab") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        legend.position = 'none') +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))

# Q50 with PCs -------------------------

# Calculate the correlation of Q50 for geuvadis.
cor_geu_Q50 <- round(cor(na.omit(data_Q50_pc)), 2)

# Melt the correlation matrix.
melted_cor_geu_Q50 <- melt(cor_geu_Q50)

# plot the correlation heatmap.
p_geu_Q50 <- ggplot(data = melted_cor_geu_Q50,
                    aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  # ggtitle('Q50 GEUVADIS') +
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 3) +
  scale_fill_gradient2(low = "purple", high = "orange", mid = "white",
                       midpoint = 0, limit = c(-1,1), space = "Lab") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        legend.position = 'none') +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))

# Save combined plot -----------------------------------------------------------

pdf('/Users/Evatar/Desktop/cor_heatmap_pcs.pdf', width = 6, height = 13)
egg::ggarrange(p_geu_Q8,
               p_geu_Q21,
               p_geu_Q23,
               p_geu_Q37,
               p_geu_Q50, ncol = 1)
dev.off()
