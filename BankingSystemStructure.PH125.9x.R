##### Interlinkages in the Malaysian Banking System: Principal Component Analysis on Sector-Wide Macrofinancial Indicators #####

# Clear global environment
# rm(list=ls())

##### Loading Libraries and Dataset #####
library(missMDA)
library(FactoMineR)
library(ggfortify)
library(cluster)
library(knitr)
library(ggplot2)
library(ggthemes)
library(RColorBrewer)
library(readxl)
# To use own downloaded dataset from http://www.bnm.gov.my/index.php?ch=statistic
indicators.raw <- read_excel("/Users/musaddiqmuhtar/Documents/1 Work/EWS PCA/EWS PCA.xlsx", sheet = "Transformed Indicators")
indicators <- data.frame(indicators.raw)

indicators.raw$MonthYr <- format(as.Date(indicators.raw$Month), "%Y-%m")  # To obtain year-month format
indicators.raw$Month <- as.Date(indicators.raw$Month)  # Converting "Month" from character class to date class

raw.summary.stats <- data.frame(Minimum = apply(na.omit(indicators.raw[, c(2:10, 12:15)]), 2, min),
                                Maximum = apply(na.omit(indicators.raw[, c(2:10, 12:15)]), 2, max),
                                Mean = apply(na.omit(indicators.raw[, c(2:10, 12:15)]), 2, mean),
                                Median = apply(na.omit(indicators.raw[, c(2:10, 12:15)]), 2, median),
                                StDev = apply(na.omit(indicators.raw[, c(2:10, 12:15)]), 2, sd),
                                Count = apply(na.omit(indicators.raw[, c(2:10, 12:15)]), 2, length))

# Imputing missing values using regularised iterative PCA algorithm
indicators.imputed.adj <- imputePCA(indicators[-21, c(2:10, 12:15)],
                                    ncp = 5, scale = TRUE,
                                    method = "Regularized")

# Imputing missing values for dataset including npl.r
indicators.imputed.nplr <- imputePCA(indicators[-21, 2:15],
                                     ncp = 10, scale = TRUE,
                                     method = "Regularized")

pca.ews <- PCA(indicators.imputed.adj$completeObs)

# In-built R PCA proceduree
pca.ews.R <- prcomp(indicators.imputed.adj$completeObs, scale. = TRUE)
# ews.pca.adj$sdev^2  # Eigenvalues row vector

indicators.pca.ews <- data.frame(cbind(indicators.imputed.adj$completeObs, pca.ews.R$x[, 1:3]))
indicators.pca.ews.econyy <- data.frame(cbind(indicators.pca.ews, indicators.imputed.nplr$completeObs[, 10], indicators$econ.yy[-21]))
colnames(indicators.pca.ews.econyy)[17] = "npl.r"
colnames(indicators.pca.ews.econyy)[18] = "econ.yy"

# Summary statistics (to compare with line 23)
imputed.summary.stats <- data.frame(Minimum = apply(indicators.pca.ews[, 1:13], 2, min),
                                    Maximum = apply(indicators.pca.ews[, 1:13], 2, max),
                                    Mean = apply(indicators.pca.ews[, 1:13], 2, mean),
                                    Median = apply(indicators.pca.ews[, 1:13], 2, median),
                                    StDev = apply(indicators.pca.ews[, 1:13], 2, sd), 
                                    Count = apply(indicators.pca.ews[, 1:13], 2, length))

### Comparison of datasets that are incomplete and completed with imputed values
# Incomplete dataset
ggplot(indicators.raw, aes(Month, npl.r)) +
  scale_x_date(date_labels = "%b-%Y") +
  geom_line(color = "darkred") +
  geom_point() +
  xlab("Date") +
  ylab("NPL Ratio") +
  scale_x_date(breaks = "24 months", date_labels = "%b %y") +
  theme_light()

# Dataset completed with imputed values
ggplot(indicators.pca.ews.econyy, aes(indicators.raw$Month[-21], npl.r)) +
  scale_x_date(date_labels = "%b-%Y") +
  geom_line(color = "darkred") +
  geom_point() +
  xlab("Date") +
  ylab("NPL Ratio") +
  scale_x_date(breaks = "24 months", date_labels = "%b %y") +
  theme_light()

# Incomplete dataset pairs plot
pairs(na.omit(indicators.raw[, 5:10]), panel = panel.smooth)

# Dataset completed with imputed values pairs plot
pairs(indicators.pca.ews.econyy[, 4:9], panel = panel.smooth)

### PCA results
kable(round(pca.ews$eig, digits = 3), caption = "Table of eigenvalues")

# Screeplot
barplot(pca.ews$eig[, "eigenvalue"],
        border = TRUE, col = "darkred", ylim = c(0, 5), las = 2,
        ylab = "Eigenvalue",
        names.arg = rownames(pca.ews$eig))

# Table of correlations
kable(round(pca.ews$var$coord[, 1:2], digits = 3), caption = "Correlations between variables and PCs")

# Circle of correlations
plot(pca.ews, choix = "var")

# Contribution of each variable to each PCs
kable(round(rbind(pca.ews$var$contrib, TOTAL = colSums(pca.ews$var$contrib)), digit = 3), caption = "Contributions of variables on each PC")

### PC explanatory performance
ggplot(indicators.pca.ews.econyy, aes(PC1, PC2, PC3, color = npl.r)) +
  geom_hline(yintercept = 0, color = "gray70") +
  geom_vline(xintercept = 0, color = "gray70") +
  geom_point() +
  scale_colour_gradient(low = "rosybrown1", high = "darkred") +
  labs(title = "Early Warning System PCA plot of observations", subtitle = "with percentage of variance in parentheses") +
  xlab("PC1 (31.03%)") +  # Labelled with the correspnding percentage of variance for PC1
  ylab("PC2 (26.54%)") +  # Labelled with the correspnding percentage of variance for PC2
  labs(color = "NPL Ratio", size = "NPL Ratio") +
  theme_light()

# 3D plot with plotly
r <- plot_ly(data = combined.data.backcast, x = ~PC1, y = ~PC2, z = ~PC3,
             marker = list(color = ~cof, colorscale = c('#FFE1A1', '#683531'), showscale = TRUE)) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = "PC1 (28.27%)"),
                      yaxis = list(title = "PC2 (20.49%)"),
                      zaxis = list(title = "PC3 (11.75%)")),
         annotations = list(
           x = 1.13,
           y = 1.05,
           text = 'NPL Ratio',
           xref = 'paper',
           yref = 'paper',
           showarrow = FALSE
         ))
r

### Appendix sections
# Dataset summaries
kable(round(raw.summary.stats, digits = 3), caption = "Before transformation")
kable(round(imputed.summary.stats, digits = 3), caption = "After transformation")

# Contribution graphs
ggplot(as.data.frame(pca.ews$var$contrib), aes(x = row.names(as.data.frame(pca.ews$var$contrib)), y = Dim.1)) +
  geom_bar(position = "dodge", stat = "identity", fill = "darkred") +
  geom_text(aes(label = round(pca.ews$var$contrib[, 1], digits = 2), vjust = -0.3)) +
  labs(subtitle = "Note: Overall percentage of variance captured by PC1 is 31.03%") +
  xlab("Macrofinancial Indicators") +
  ylab("% contribution to PC1") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        panel.background = element_rect(fill = "white", colour = "gray70"),
        panel.grid.major = element_line(colour = "gray90", size = rel(0.5)),
        panel.grid.minor = element_line(colour = "gray90", size = rel(0.25)))

ggplot(as.data.frame(pca.ews$var$contrib), aes(x = row.names(as.data.frame(pca.ews$var$contrib)), y = Dim.2)) +
  geom_bar(position = "dodge", stat = "identity", fill = "darkred") +
  geom_text(aes(label = round(pca.ews$var$contrib[, 2], digits = 2), vjust = -0.3)) +
  labs(subtitle = "Note: Overall percentage of variance captured by PC2 is 26.54%") +
  xlab("Macrofinancial Indicators") +
  ylab("% contribution to PC2") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        panel.background = element_rect(fill = "white", colour = "gray70"),
        panel.grid.major = element_line(colour = "gray90", size = rel(0.5)),
        panel.grid.minor = element_line(colour = "gray90", size = rel(0.25)))

ggplot(as.data.frame(pca.ews$var$contrib), aes(x = row.names(as.data.frame(pca.ews$var$contrib)), y = Dim.3)) +
  geom_bar(position = "dodge", stat = "identity", fill = "darkred") +
  geom_text(aes(label = round(pca.ews$var$contrib[, 3], digits = 2), vjust = -0.3)) +
  labs(subtitle = "Note: Overall percentage of variance captured by PC3 is 9.66%") +
  xlab("Macrofinancial Indicators") +
  ylab("% contribution to PC3") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        panel.background = element_rect(fill = "white", colour = "gray70"),
        panel.grid.major = element_line(colour = "gray90", size = rel(0.5)),
        panel.grid.minor = element_line(colour = "gray90", size = rel(0.25)))
