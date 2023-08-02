# Load libraries ---------------------------------------------------------------

library (reshape2)
library (ggplot2)

# Load drosophila data ---------------------------------------------------------

# Read in the drosophila data.
drosophila_raw <- read.delim('/Users/Evatar/Downloads/drosophila.txt',
                             comment.char = '#')

# Remove the rows with genetic information.
drosophila_continuous <- drosophila_raw[, -c(1:5)]

# Load the discrete data from baycn.
data("drosophila")

# Correlation heatmap: discrete ------------------------------------------------

# Calculate the correlation of the discrete drosophila data.
cor_dros_dis <- round(cor(drosophila), 2)

# Melt the correlation matrix.
melted_cor_dros_dis <- melt(cor_dros_dis)

# plot the correlation heatmap.
p_dros_dis <- ggplot(data = melted_cor_dros_dis,
                     aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  ggtitle('') +
  # geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  scale_fill_gradient2(low = 'darkblue', high = 'darkorange', mid = 'white',
                       midpoint = 0, limit = c(-1, 1), space = 'Lab',
                       name = '') +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(angle = 90,
                                   hjust = 1),
        legend.position = 'right')

p_dros_dis

# ggsave(filename = 'drosophila_hm.pdf',
#        plot = p_dros_dis,
#        height = 7 ,
#        width = 7,
#        path = '/Users/Evatar/Desktop/')

# Correlation heatmap: continuous ----------------------------------------------

# Calculate the correlation of the discrete drosophila data.
cor_dros_con <- round(cor(drosophila_continuous), 2)

# Melt the correlation matrix.
melted_cor_dros_con <- melt(cor_dros_con)

# plot the correlation heatmap.
p_dros_con <- ggplot(data = melted_cor_dros_con,
                     aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  ggtitle('') +
  # geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  scale_fill_gradient2(low = 'darkblue', high = 'darkorange', mid = 'white',
                       midpoint = 0, limit = c(-1, 1), space = 'Lab',
                       name = '') +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(angle = 90,
                                   hjust = 1),
        legend.position = 'right')

p_dros_con

# ggsave(filename = 'drosophila_hm_con.pdf',
#        plot = p_dros_con,
#        height = 7 ,
#        width = 7,
#        path = '/Users/Evatar/Desktop/')
