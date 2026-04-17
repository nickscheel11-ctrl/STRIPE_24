# Add 'group' variable
plot_tiny$group <- "Tiny"
big_plot$group <- "Big"

# Create standardized data frames with matching columns
plot_tiny2 <- data.frame(
  x = plot_tiny$x,
  y = plot_tiny$y,
  fit = plot_tiny$fit_t,
  lwr = plot_tiny$lwr_t,
  upr = plot_tiny$upr_t,
  group = plot_tiny$group
)

big_plot2 <- data.frame(
  x = big_plot$x,
  y = big_plot$y,
  fit = big_plot$fit_b,
  lwr = big_plot$lwr_b,
  upr = big_plot$upr_b,
  group = big_plot$group
)

# Combine them safely
combined_plot <- rbind(plot_tiny2, big_plot2)

ggplot(combined_plot, aes(x = x, y = y, color = group, fill = group)) +
  geom_point() +
  geom_line(aes(y = fit)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2, color = NA) +
  
  labs(
    title = "Comparing Size Classes",
    x = "Day of Year",
    y = "BTHg (ng/g)",
    color = "Group",
    fill = "Group"
  ) +
  
  geom_vline(xintercept = 297, color = "red", linetype = "dashed") +
  geom_vline(xintercept = 245, color = "red", linetype = "dashed") +
  
  annotate("text", x = 270, y = 40, label = "Early Fall", vjust = -0.5) +
  annotate("text", x = 220, y = 40, label = "Summer", vjust = -0.5) +
  annotate("text", x = 320, y = 40, label = "Late Fall", vjust = -0.5) +
  
  theme_minimal()


# custom colors
ggplot(combined_plot, aes(x = x, y = y, color = group, fill = group)) +
  geom_point() +
  geom_line(aes(y = fit)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2, color = NA) +
  
  # Custom colors for lines and fills
  scale_color_manual(values = c("Tiny" = "blue", "Big" = "red")) +
  scale_fill_manual(values = c("Tiny" = "blue", "Big" = "red")) +
  
  labs(
    title = "Comparing Size Classes",
    x = "Day of Year",
    y = "BTHg (ng/g)",
    color = "Group",
    fill = "Group"
  ) +
  
  geom_vline(xintercept = 297, color = "red", linetype = "dashed") +
  geom_vline(xintercept = 245, color = "red", linetype = "dashed") +
  
  annotate("text", x = 270, y = 40, label = "Early Fall", vjust = -0.5) +
  annotate("text", x = 220, y = 40, label = "Summer", vjust = -0.5) +
  annotate("text", x = 320, y = 40, label = "Late Fall", vjust = -0.5) +
  
  theme_minimal()

#custom legend titles for size classes
ggplot(combined_plot, aes(x = x, y = y, color = group, fill = group)) +
  geom_point() +
  geom_line(aes(y = fit)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2, color = NA) +
  
  # Custom colors and legend labels
  scale_color_manual(
    name = "Size Class",
    values = c("Tiny" = "blue", "Big" = "red"),
    labels = c("Tiny" = "6–13 mm", "Big" = "13–20 mm")
  ) +
  scale_fill_manual(
    name = "Size Class",
    values = c("Tiny" = "blue", "Big" = "red"),
    labels = c("Tiny" = "6–13 mm", "Big" = "13–20 mm")
  ) +
  
  labs(
    title = "Seasonal Trends in BTHg",
    x = "Day of Year",
    y = "BTHg (ng/g)"
  ) +
  
  geom_vline(xintercept = 297, color = "red", linetype = "dashed") +
  geom_vline(xintercept = 245, color = "red", linetype = "dashed") +
  
  annotate("text", x = 270, y = 40, label = "Early Fall", vjust = -0.5) +
  annotate("text", x = 220, y = 40, label = "Summer", vjust = -0.5) +
  annotate("text", x = 320, y = 40, label = "Late Fall", vjust = -0.5) +
  
  theme_minimal()

