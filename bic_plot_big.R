# Fit polynomial models and calculate BIC
tmodel1 <- lm(thg.wet.ng.g ~ poly(daynum, 1, raw = TRUE), data = tiny)
BICt1 <- BIC(tmodel1)

tmodel2 <- lm(thg.wet.ng.g ~ poly(daynum, 2, raw = TRUE), data = tiny)
BICt2 <- BIC(tmodel2)

tmodel3 <- lm(thg.wet.ng.g ~ poly(daynum, 3, raw = TRUE), data = tiny)
BICt3 <- BIC(tmodel3)

tmodel4 <- lm(thg.wet.ng.g ~ poly(daynum, 4, raw = TRUE), data = tiny)
BICt4 <- BIC(tmodel4)

tmodel5 <- lm(thg.wet.ng.g ~ poly(daynum, 5, raw = TRUE), data = tiny)
BICt5 <- BIC(tmodel5)

tmodel6 <- lm(thg.wet.ng.g ~ poly(daynum, 6, raw = TRUE), data = tiny)
BICt6 <- BIC(tmodel6)

tmodel7 <- lm(thg.wet.ng.g ~ poly(daynum, 7, raw = TRUE), data = tiny)
BICt7 <- BIC(tmodel7)

# Collect BICs into a data frame for plotting
bic_df <- data.frame(
  Order = 1:7,
  BIC = c(BICt1, BICt2, BICt3, BICt4, BICt5, BICt6, BICt7)
)

# Plot BIC vs polynomial order
library(ggplot2)
bic_plot_t <- ggplot(bic_df, aes(x = Order, y = BIC)) +
  geom_line() +
  geom_point() +
  labs(title = "6-13 mm Size Class", x = "Polynomial Degree (k)", y = "BIC")

bic_plot_t

library(patchwork)
bic_plot_t + bic_plot_b +
  plot_annotation("Bayesian Information Criterion (BIC) for Polynomial Regressions")

