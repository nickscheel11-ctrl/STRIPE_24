#================================================================================================================

#Box Plot by season tiny

#================================================================================================================

tiny <- tiny %>%
  mutate(season = case_when(between(date, as.Date("2024-06-01"), 
                                    as.Date("2024-09-01")) ~ "summer", 
                            between(date, as.Date("2024-9-01"), 
                                    as.Date("2024-10-26")) ~ "early_fall", 
                            TRUE ~ "late_fall")) 

season_box <- boxplot(thg.wet.ng.g ~ season, data = tiny, 
                      xlab = "Season",
                      ylab = "Total Hg (ng/g)", 
                      main = "Total Hg in ZM",
                      notch = FALSE, 
                      varwidth = TRUE, 
                      col = c("white") 
                      
                      #names = c("0","1", "2", "3", "4", "5") 
)

one.way <- aov(thg.wet.ng.g ~ season, data = tiny)

summary(one.way)

#test if they're normally distributed
shapiro.test(tiny$thg.wet.ng.g[tiny$season == "summer"])
shapiro.test(tiny$thg.wet.ng.g[tiny$season == "early_fall"])
shapiro.test(tiny$thg.wet.ng.g[tiny$season == "late_fall"])

#print results
# Store results in a named list
shapiro_results <- list(
  summer = shapiro.test(tiny$thg.wet.ng.g[tiny$season == "summer"]),
  early_fall = shapiro.test(tiny$thg.wet.ng.g[tiny$season == "early_fall"]),
  late_fall = shapiro.test(tiny$thg.wet.ng.g[tiny$season == "late_fall"])
)

# Print all results
lapply(shapiro_results, print)

#print parameters and p values
sapply(shapiro_results, function(x) c(statistic = x$statistic, p_value = x$p.value))


#late fall is not normally distributed so using Kruskal-Wallis

# Perform Kruskal-Wallis test
kruskal_result <- kruskal.test(thg.wet.ng.g ~ season, data = tiny)

# Display result
kruskal_result

#p-value is less than p<0.05, there is statistical significance. using Dunn's test for pairwise

# load the FSA package
library(FSA)

# Perform Dunn's Test
dunnTest(thg.wet.ng.g ~ season, data = tiny, method = "bonferroni")




#=======================================
#version with dunns results plotted
#========================================

library(ggplot2)
library(ggpubr)


# Create the ggplot boxplot
p <- ggplot(tiny, aes(x = season, y = thg.wet.ng.g)) +
  geom_boxplot(fill = "white", varwidth = TRUE) +
  labs(
    title = "Total Hg in ZM",
    x = "Season",
    y = "Total Hg (ng/g)"
  ) +
  theme_minimal()

library(FSA)
dunn_res <- dunnTest(thg.wet.ng.g ~ season, data = tiny, method = "bonferroni")


# Extract p-values and pairs
dunn_df <- dunn_res$res

# Filter only significant comparisons (e.g., p < 0.05)
sig_pairs <- dunn_df[dunn_df$P.adj < 0.05, ]

# Clean the comparisons for plotting
sig_pairs$Comparison <- gsub(" - ", " vs ", sig_pairs$Comparison)

# Split the comparisons into groups
sig_pairs <- sig_pairs %>%
  tidyr::separate(Comparison, into = c("group1", "group2"), sep = " vs ")

# Add y-position for each comparison (you can adjust this as needed)
sig_pairs$y.position <- seq(
  max(tiny$thg.wet.ng.g, na.rm = TRUE) * 1.05,
  length.out = nrow(sig_pairs),
  by = 0.05
)

# Add annotation layer
p + stat_pvalue_manual(sig_pairs, 
                       label = "P.adj", 
                       tip.length = 0.01,
                       bracket.size = 0.5,
                       step.increase = 0.05)



#==============================
#correct order
#=============================
# Reorder the 'season' factor
tiny$season <- factor(tiny$season, levels = c("summer", "early_fall", "late_fall"))

# Create the ggplot boxplot
p <- ggplot(tiny, aes(x = season, y = thg.wet.ng.g)) +
  geom_boxplot(fill = "white", varwidth = TRUE) +
  labs(
    title = "6-13mm Size Class",
    x = "Season",
    y = "Total Hg (ng/g)"
  ) +
  theme_minimal()

# Display the boxplot
print(p)

#==============================================================
#sig figs and stars
#==============================================================

# Add a new column for significance stars
sig_pairs$sig_label <- cut(
  sig_pairs$P.adj,
  breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
  labels = c("***", "**", "*", "ns")
)

# Split the comparisons into two groups
sig_pairs <- sig_pairs %>%
  tidyr::separate(Comparison, into = c("group1", "group2"), sep = " vs ")

# Set y-position for annotations
sig_pairs$y.position <- seq(
  max(tiny$thg.wet.ng.g, na.rm = TRUE) * 1.05,
  length.out = nrow(sig_pairs),
  by = 0.05
)

# Final plot with stars instead of numeric p-values
tiny_season <- p + stat_pvalue_manual(sig_pairs, 
                       label = "sig_label", 
                       tip.length = 0.01,
                       bracket.size = 0.5,
                       step.increase = 0.05)+
  geom_jitter(width = 0.15, alpha = 0.5, color = "darkblue")
  

#plot with both big and tiny
library(patchwork)

tiny_season + big_season+
  plot_annotation(title = "THg in Mussel Tissue by Season")

