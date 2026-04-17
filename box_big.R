#================================================================================================================

#Box Plot by season big

#================================================================================================================

library(ggplot2)
library(car)

big <- big %>%
  mutate(season = case_when(between(date, as.Date("2024-06-01"), 
                                    as.Date("2024-09-01")) ~ "summer", 
                            between(date, as.Date("2024-9-01"), 
                                    as.Date("2024-10-26")) ~ "early_fall", 
                            TRUE ~ "late_fall")) 

# Reorder the 'season' factor
big$season <- factor(big$season, levels = c("summer", "early_fall", "late_fall"))

season_box_b <- boxplot(thg.wet.ng.g ~ season, data = big, 
                      xlab = "Season",
                      ylab = "Total Hg (ng/g)", 
                      main = "Total Hg in ZM",
                      notch = FALSE, 
                      varwidth = TRUE, 
                      col = c("white") 
                      
                      #names = c("0","1", "2", "3", "4", "5") 
)

one.way <- aov(thg.wet.ng.g ~ season, data = big)

summary(one.way)

#test if they're normally distributed
shapiro.test(big$thg.wet.ng.g[big$season == "summer"])
shapiro.test(big$thg.wet.ng.g[big$season == "early_fall"])
shapiro.test(big$thg.wet.ng.g[big$season == "late_fall"])

# Define seasons
seasons <- c("summer", "early_fall", "late_fall")

# Run Shapiro-Wilk tests for each season
shapiro_results <- lapply(seasons, function(season) {
  test <- shapiro.test(big$thg.wet.ng.g[big$season == season])
  data.frame(
    Season = season,
    W = test$statistic,
    p_value = test$p.value
  )
})

# Combine results into a single data frame
shapiro_df <- do.call(rbind, shapiro_results)

print(shapiro_df)






tukey_result <- TukeyHSD(one.way)
print(tukey_result)


# Install if needed:
# install.packages("multcompView")

library(multcompView)

# Get Tukey letters
tukey_letters <- multcompLetters4(one.way, tukey_result)

# Extract group letters
letters_df <- as.data.frame.list(tukey_letters$season)
letters_df$season <- rownames(letters_df)

# Calculate boxplot stats for label placement
medians <- tapply(big$thg.wet.ng.g, big$season, median)

# Add letters to boxplot
text(x = 1:length(medians), y = medians + 0.1, labels = letters_df$Letters)


#=====
#ggplot2 version
#==================

# Load required libraries
library(ggplot2)
library(dplyr)
library(multcompView)

# Run ANOVA and Tukey's HSD
one.way <- aov(thg.wet.ng.g ~ season, data = big)
tukey_result <- TukeyHSD(one.way)

# Get compact letter display for groupings
tukey_letters <- multcompLetters4(one.way, tukey_result)
letters_df <- as.data.frame.list(tukey_letters$season)
letters_df$season <- rownames(letters_df)

# Merge with median values for label positioning
plot_data <- big %>%
  group_by(season) %>%
  summarise(median = median(thg.wet.ng.g, na.rm = TRUE)) %>%
  left_join(letters_df, by = "season")

# Plot
ggplot(big, aes(x = season, y = thg.wet.ng.g)) +
  geom_boxplot(width = 0.6, fill = "white", varwidth = TRUE) +
  labs(title = "Total Hg in ZM", x = "Season", y = "Total Hg (ng/g)") +
  geom_text(data = plot_data, aes(x = season, y = median + 0.1, label = Letters),
            vjust = 0, size = 5) +
  theme_minimal()


#=============================
#asterisks
#=============================

# install.packages("ggpubr")  # if you don't have it
library(ggpubr)

# Define comparisons you want to show
comparisons <- list(
  c("summer", "early_fall"),
  c("summer", "late_fall"),
  c("early_fall", "late_fall")
)

# Plot with stat_compare_means for asterisks
ggplot(big, aes(x = season, y = thg.wet.ng.g)) +
  geom_boxplot(width = 0.6, fill = "white", varwidth = TRUE) +
  stat_compare_means(comparisons = comparisons, method = "tukey", label = "p.signif") +
  labs(title = "Total Hg in ZM", x = "Season", y = "Total Hg (ng/g)") +
  theme_minimal()

#===================
#stats globe
#====================
anova_result <- aov(thg.wet.ng.g ~ season, data = big) # perform ANOVA
summary(anova_result)

tukey_result <- TukeyHSD(anova_result) # post-hoc analysis
tukey_result

# Set plot margins: c(bottom, left, top, right)
par(mar = c(5, 4, 6, 2))  # Increase top margin

# Plot with title added separately
tuk <- plot(tukey_result, xlim = c(-10, 50))
title("Tukey HSD Post-hoc Results", line = 5)

ggplot(big, aes(x = season, y = thg.wet.ng.g)) +
  geom_boxplot(width = 0.6, fill = "white", varwidth = TRUE) +
  labs(title = "Total Hg in ZM", x = "Season", y = "Total Hg (ng/g)") +
  geom_text(data = plot_data, aes(x = season, y = median + 0.1, label = Letters),
            vjust = 0, size = 5) +
  theme_minimal()


#====
#try again
#=======
# Load necessary packages
library(ggplot2)
library(multcompView)
library(dplyr)

# Run ANOVA
anova_result <- aov(thg.wet.ng.g ~ season, data = big)
summary(anova_result)

# Run Tukey HSD test
tukey_result <- TukeyHSD(anova_result)

# Extract Tukey HSD results
tukey_df <- as.data.frame(tukey_result$season)

# Convert row names (e.g., "Fall-Spring") to columns
tukey_df$Comparison <- rownames(tukey_df)

# Create a compact letter display (CLD) of significance groups
tukey_letters <- multcompLetters(tukey_result$season[,"p adj"])$Letters

# Convert to a data frame for plotting
letter_df <- data.frame(
  season = names(tukey_letters),
  Letters = tukey_letters
)

# Compute medians to help position letters above boxes
plot_data <- big %>%
  group_by(season) %>%
  summarise(median = median(thg.wet.ng.g)) %>%
  left_join(letter_df, by = "season")

# Plot
ggplot(big, aes(x = season, y = thg.wet.ng.g)) +
  geom_boxplot(width = 0.6, fill = "white", varwidth = TRUE) +
  labs(title = "Total Hg in ZM", x = "Season", y = "Total Hg (ng/g)") +
  geom_text(data = plot_data, aes(x = season, y = median + 0.1, label = Letters),
            vjust = 0, size = 5) +
  theme_minimal()

#================
#with asterisks
#================
# Load libraries
library(ggplot2)
library(ggpubr)
library(dplyr)

# ANOVA and Tukey HSD
anova_result <- aov(thg.wet.ng.g ~ season, data = big)
tukey_result <- TukeyHSD(anova_result)

# Convert Tukey HSD output to a tidy dataframe
tukey_df <- as.data.frame(tukey_result$season)
tukey_df$Comparison <- rownames(tukey_df)

# Filter significant comparisons and assign significance levels
tukey_df <- tukey_df %>%
  mutate(p_value = `p adj`,
         significance = case_when(
           p_value < 0.001 ~ "***",
           p_value < 0.01 ~ "**",
           p_value < 0.05 ~ "*",
           TRUE ~ ""
         )) %>%
  filter(significance != "")

# Split comparison into groups
tukey_df <- tukey_df %>%
  separate(Comparison, into = c("group1", "group2"), sep = "-")

# Plot with asterisks for significant comparisons
ggplot(big, aes(x = season, y = thg.wet.ng.g)) +
  geom_boxplot(width = 0.6, fill = "white", varwidth = TRUE) +
  labs(title = "Total Hg in ZM", x = "Season", y = "Total Hg (ng/g)") +
  stat_pvalue_manual(
    tukey_df,
    label = "significance",
    xmin = "group1",
    xmax = "group2",
    y.position = seq(max(big$thg.wet.ng.g) * 1.05, length.out = nrow(tukey_df), by = 0.2),
    tip.length = 0.01
  ) +
  theme_minimal()

plot(anova_result, 2) # check normality of residuals

leveneTest(thg.wet.ng.g ~ season,
           data = big)

#data violates levene Test
#using Welch's ANOVA
oneway.test(thg.wet.ng.g ~ season, data = big, var.equal = FALSE)


#instead of Tukey test, use pairwise t test with corrections
pairwise.t.test(big$thg.wet.ng.g, big$season,
                p.adjust.method = "holm",
                pool.sd = FALSE)

#with asterisks

# Run Welch's ANOVA
welch_anova <- oneway.test(thg.wet.ng.g ~ season, data = big, var.equal = FALSE)
welch_anova
summary(welch_anova)

# Pairwise comparisons with Holm adjustment
pairwise_results <- pairwise.t.test(big$thg.wet.ng.g, big$season,
                                    p.adjust.method = "holm", 
                                    pool.sd = FALSE) # Welch correction

pairwise_results
# Convert pairwise results to a data frame
pairwise_df <- as.data.frame(pairwise_results$p.value)
pairwise_df$Comparison <- rownames(pairwise_df)

# Convert p-values into asterisks for significance
pairwise_df <- pairwise_df %>%
  mutate(significance = case_when(
    p.value < 0.001 ~ "***",
    p.value < 0.01 ~ "**",
    p.value < 0.05 ~ "*",
    TRUE ~ ""
  )) %>%
  filter(significance != "") # Keep only significant comparisons

# Split the comparisons into groups
pairwise_df <- pairwise_df %>%
  separate(Comparison, into = c("group1", "group2"), sep = "-")

#after error
# Run pairwise t-tests with Holm correction
pairwise_results <- pairwise.t.test(big$thg.wet.ng.g, big$season,
                                    p.adjust.method = "holm", 
                                    pool.sd = FALSE) # Welch correction


# Convert the pairwise t-test results into a data frame
pairwise_df <- as.data.frame(pairwise_results$p.value)

# Gather the data into long format
pairwise_df <- pairwise_df %>%
  gather(key = "group2", value = "p_value", -Comparison) %>%
  filter(!is.na(p_value))  # Remove NA values (for comparisons within same group)

# Separate the Comparison column into two groups
pairwise_df <- pairwise_df %>%
  separate(Comparison, into = c("group1", "group2"), sep = "-")

# Check the structure of pairwise_df to ensure correctness
head(pairwise_df)


# Convert the pairwise t-test results into a data frame
pairwise_df <- as.data.frame(pairwise_results$p.value)

# Gather the data into long format
pairwise_df <- pairwise_df %>%
  gather(key = "group2", value = "p_value", -Comparison) %>%
  filter(!is.na(p_value))  # Remove NA values (for comparisons within same group)

# Separate the Comparison column into two groups
pairwise_df <- pairwise_df %>%
  separate(Comparison, into = c("group1", "group2"), sep = "-")

# Check the structure of pairwise_df to ensure correctness
head(pairwise_df)

#3
# Assign significance based on p-values
pairwise_df <- pairwise_df %>%
  mutate(significance = case_when(
    p_value < 0.001 ~ "***",
    p_value < 0.01 ~ "**",
    p_value < 0.05 ~ "*",
    TRUE ~ ""
  )) %>%
  filter(significance != "")  # Only keep significant comparisons

# Check the resulting pairwise dataframe
head(pairwise_df)

#4
# Get the medians for placing asterisks
plot_data <- big %>%
  group_by(season) %>%
  summarise(median = median(thg.wet.ng.g))

# Plot with asterisks for significance
ggplot(big, aes(x = season, y = thg.wet.ng.g)) +
  geom_boxplot(width = 0.6, fill = "white", varwidth = TRUE) +
  labs(title = "Total Hg in ZM", x = "Season", y = "Total Hg (ng/g)") +
  stat_pvalue_manual(
    pairwise_df,
    label = "significance",
    xmin = "group1",
    xmax = "group2",
    y.position = seq(max(big$thg.wet.ng.g) * 1.05, length.out = nrow(pairwise_df), by = 0.2),
    tip.length = 0.01
  ) +
  theme_minimal()

#after 2nd error
# Run pairwise t-tests with Holm correction
pairwise_results <- pairwise.t.test(big$thg.wet.ng.g, big$season,
                                    p.adjust.method = "holm", 
                                    pool.sd = FALSE) # Welch correction

# Extract p-values matrix from the pairwise results
pairwise_matrix <- as.data.frame(pairwise_results$p.value)

# Convert the p-value matrix into a tidy format
pairwise_df <- pairwise_matrix %>%
  rownames_to_column("group1") %>%
  gather(key = "group2", value = "p_value", -group1) %>%
  filter(!is.na(p_value)) # Remove any NA values (when group1 == group2)

# Now we have a tidy data frame with pairwise comparisons
head(pairwise_df)

# Assign significance based on p-values
pairwise_df <- pairwise_df %>%
  mutate(significance = case_when(
    p_value < 0.001 ~ "***",
    p_value < 0.01 ~ "**",
    p_value < 0.05 ~ "*",
    TRUE ~ ""
  )) %>%
  filter(significance != "")  # Only keep significant comparisons

# Check the resulting pairwise dataframe
head(pairwise_df)

# Get the medians for placing asterisks
plot_data <- big %>%
  group_by(season) %>%
  summarise(median = median(thg.wet.ng.g))

# Plot with asterisks for significance
ggplot(big, aes(x = season, y = thg.wet.ng.g)) +
  geom_boxplot(width = 0.6, fill = "white", varwidth = TRUE) +
  labs(title = "Total Hg in ZM by Season (13-20mm)", x = "Season", y = "Total Hg (ng/g)") +
  stat_pvalue_manual(
    pairwise_df,
    label = "significance",
    xmin = "group1",
    xmax = "group2",
    y.position = seq(max(big$thg.wet.ng.g) * 1.05, length.out = nrow(pairwise_df), by = 0.2),
    tip.length = 0.01
  ) +
  theme_minimal()


ggplot(big, aes(x = season, y = thg.wet.ng.g)) +
  geom_boxplot(width = 0.6, fill = "white", varwidth = TRUE, outlier.shape = NA) +  # Hide default outliers
  geom_jitter(width = 0.15, alpha = 0.5, color = "darkblue") +  # Jittered points
  labs(title = "Total Hg in ZM by Season (13-20mm)", x = "Season", y = "Total Hg (ng/g)") +
  stat_pvalue_manual(
    pairwise_df,
    label = "significance",
    xmin = "group1",
    xmax = "group2",
    y.position = seq(max(big$thg.wet.ng.g) * 1.05, length.out = nrow(pairwise_df), by = 0.2),
    tip.length = 0.01
  ) +
  theme_minimal()

big_clean <- big %>%
  filter(thg.wet.ng.g > 50)  # example threshold

#redo with cleaned data
# Run pairwise t-tests with Holm correction
pairwise_results <- pairwise.t.test(big_clean$thg.wet.ng.g, big_clean$season,
                                    p.adjust.method = "holm", 
                                    pool.sd = FALSE) # Welch correction

# Extract p-values matrix from the pairwise results
pairwise_matrix <- as.data.frame(pairwise_results$p.value)

# Convert the p-value matrix into a tidy format
pairwise_df <- pairwise_matrix %>%
  rownames_to_column("group1") %>%
  gather(key = "group2", value = "p_value", -group1) %>%
  filter(!is.na(p_value)) # Remove any NA values (when group1 == group2)

# Now we have a tidy data frame with pairwise comparisons
head(pairwise_df)

# Assign significance based on p-values
pairwise_df <- pairwise_df %>%
  mutate(significance = case_when(
    p_value < 0.001 ~ "***",
    p_value < 0.01 ~ "**",
    p_value < 0.05 ~ "*",
    TRUE ~ ""
  )) %>%
  filter(significance != "")  # Only keep significant comparisons

# Check the resulting pairwise dataframe
head(pairwise_df)

# Get the medians for placing asterisks
plot_data <- big %>%
  group_by(season) %>%
  summarise(median = median(thg.wet.ng.g))

# Plot with asterisks for significance
ggplot(big_clean, aes(x = season, y = thg.wet.ng.g)) +
  geom_boxplot(width = 0.6, fill = "white", varwidth = TRUE) +
  labs(title = "Total Hg in ZM by Season (13-20mm)", x = "Season", y = "Total Hg (ng/g)") +
  stat_pvalue_manual(
    pairwise_df,
    label = "significance",
    xmin = "group1",
    xmax = "group2",
    y.position = seq(max(big$thg.wet.ng.g) * 1.05, length.out = nrow(pairwise_df), by = 0.2),
    tip.length = 0.01
  ) +
  theme_minimal()


ggplot(big_clean, aes(x = season, y = thg.wet.ng.g)) +
  geom_boxplot(width = 0.6, fill = "white", varwidth = TRUE, outlier.shape = NA) +  # Hide default outliers
  geom_jitter(width = 0.15, alpha = 0.5, color = "darkblue") +  # Jittered points
  labs(title = "Total Hg in ZM by Season (13-20mm)", x = "Season", y = "Total Hg (ng/g)") +
  stat_pvalue_manual(
    pairwise_df,
    label = "significance",
    xmin = "group1",
    xmax = "group2",
    y.position = seq(max(big$thg.wet.ng.g) * 1.05, length.out = nrow(pairwise_df), by = 0.2),
    tip.length = 0.01
  ) +
  theme_minimal()

#fixed brackets
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)

# Filter data
big_clean <- big %>% filter(thg.wet.ng.g > 50)

# Pairwise comparisons with Holm correction (Welch t-test)
pairwise_results <- pairwise.t.test(
  big_clean$thg.wet.ng.g, big_clean$season,
  p.adjust.method = "holm",
  pool.sd = FALSE
)

# Convert matrix to tidy format
pairwise_df <- as.data.frame(pairwise_results$p.value) %>%
  rownames_to_column("group1") %>%
  pivot_longer(-group1, names_to = "group2", values_to = "p_value") %>%
  filter(!is.na(p_value)) %>%
  mutate(significance = case_when(
    p_value < 0.001 ~ "***",
    p_value < 0.01 ~ "**",
    p_value < 0.05 ~ "*",
    TRUE ~ ""
  )) %>%
  filter(significance != "")

# Assign y-position dynamically to avoid overlap
y_max <- max(big_clean$thg.wet.ng.g, na.rm = TRUE)
pairwise_df$y.position <- seq(
  from = y_max * 1.05,
  by = y_max * 0.05,
  length.out = nrow(pairwise_df)
)

# Plot
big_season <- ggplot(big_clean, aes(x = season, y = thg.wet.ng.g)) +
  geom_boxplot(width = 0.6, fill = "white", varwidth = TRUE, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.5, color = "darkblue") +
  stat_pvalue_manual(
    pairwise_df,
    label = "significance",
    xmin = "group1",
    xmax = "group2",
    y.position = "y.position",
    tip.length = 0.01
  ) +
  labs(
    title = "13-20mm Size Class",
    x = "Season",
    y = "Total Hg (ng/g)"
  ) +
  theme_minimal()
