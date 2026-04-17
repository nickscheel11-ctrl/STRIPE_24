
#==============================================================================================================================
#Evaluating model for 13-20 mm
#==============================================================================================================================
library(ISLR)
library(dplyr)
library(ggplot2)

#models
bmodel1 <- lm(big$thg.wet.ng.g ~ poly(big$daynum, 1), data = big)
bmodel2 <- lm(big$thg.wet.ng.g ~ poly(big$daynum, 2), data = big)
bmodel3 <- lm(big$thg.wet.ng.g ~ poly(big$daynum, 3), data = big)
bmodel4 <- lm(big$thg.wet.ng.g ~ poly(big$daynum, 4), data = big)
bmodel5 <- lm(big$thg.wet.ng.g ~ poly(big$daynum, 5), data = big)
bmodel6 <- lm(big$thg.wet.ng.g ~ poly(big$daynum, 6), data = big)
bmodel7 <- lm(big$thg.wet.ng.g ~ poly(big$daynum, 7), data = big)

#ANOVA
print(anova(bmodel1,bmodel2,bmodel3,bmodel4,bmodel5, bmodel6, bmodel7))

#BIC
BIC_bs <- print(BIC(bmodel1,bmodel2,bmodel3,bmodel4,bmodel5, bmodel6, bmodel7))

orders_b <- c(1,2,3,4,5,6,7)
plot(BIC_bs$BIC ~ orders_b)

BIC_b4 <-BIC(bmodel4)
print(BIC_b4)

#confidence interval for best model

# Generate predictions with 95% confidence intervals
predictions <- predict(bmodel4, newdata = data.frame(x = x), 
                       interval = "confidence", level = 0.95)

head(predictions)

#plotting with CI

# Create a data frame for plotting
big_plot <- data.frame(x = big$daynum, y = big$thg.wet.ng.g, 
                        fit_b = predictions[, "fit"], 
                        lwr_b = predictions[, "lwr"], 
                        upr_b = predictions[, "upr"])

# Plot the data with the polynomial fit and confidence intervals
bigp <- ggplot(plot_data, aes(x = big$daynum, y = big$thg.wet.ng.g)) +
  geom_point(color= "red") +  # original data points
  geom_line(aes(x= big_plot$x, y = big_plot$fit_b), color = "red") +  # fitted polynomial line
  geom_ribbon(aes(ymin = lwr_b, ymax = upr_b), alpha = 0.2, fill= "red") +  # confidence interval
  labs(title = "THg (13-20mm)" )+
  xlab("Day of Year")+
  ylab("THg (ng/g)")+
  annotate(geom="text", x=270, y=110, label="k=4",
           color="black")+
  annotate("text", x = 270, y = 100, 
           label = paste("BIC = ", round(BIC_b4, 2)), 
           color = "black")+
  geom_vline(xintercept = 297, 
             color = "red", linetype = "dashed", size = 1)+
  annotate("text", x = 297, y = 70, label = "Turnover", angle = 90, vjust = -0.5)+
  
  geom_vline(xintercept = 245, 
             color = "red", linetype = "dashed", size = 1)+
  
  annotate("text", x = 245, y = 75, label = "End of Summer", angle = 90, vjust = -0.5)


bigp
# plot model
par(mfrow=c(1,1))

plot(big$thg.wet.ng.g~ big$daynum, col= "orange")+
  lines(sort(big$daynum),
        fitted(bmodel4)[order(big$daynum)],
        col = "orange")+
  text(x = 285, y = 100, paste0("k = ", 4, ", BIC = ", signif(BIC_b4, 4)))



#==============================================================================================================================
#Evaluating model for 6-13 mm
#==============================================================================================================================

#BIC
tmodel1 <- lm(tiny$thg.wet.ng.g ~ poly(tiny$daynum, degree =1, raw= T), data = tiny) #n272.3234
BICt1 <- BIC(tmodel1)

tmodel2 <- lm(tiny$thg.wet.ng.g ~ poly(tiny$daynum, 2), data = tiny)
BICt2 <- BIC(tmodel2)

tmodel3 <- lm(tiny$thg.wet.ng.g ~ poly(tiny$daynum, 3), data = tiny)
BICt3 <- BIC(tmodel3)

tmodel4 <- lm(tiny$thg.wet.ng.g ~ poly(tiny$daynum, 4), data = tiny)
BICt4 <- BIC(tmodel4)

tmodel5 <- lm(tiny$thg.wet.ng.g ~ poly(tiny$daynum, 5), data = tiny)
BICt5 <- BIC(tmodel5)

tmodel6 <- lm(tiny$thg.wet.ng.g ~ poly(tiny$daynum, 6), data = tiny)
BICt6 <- BIC(tmodel6)

tmodel7 <- lm(tiny$thg.wet.ng.g ~ poly(tiny$daynum, 7), data = tiny)
BICt5 <- BIC(tmodel5)

orders_t <- c(1,2,3,4,5,6,7)
BICts <- c(BICt1, BICt2, BICt3, BICt4, BICt5, BICt6, BICt7)

print(BICts)
plot(BICts ~ orders_t)
#ANOVA
print(anova(tmodel1,tmodel2,tmodel3,tmodel4,tmodel5, tmodel6, tmodel7))


an_t5 <- print(anova(tmodel5))


#AIC
library(AICcmodavg)

#define list of models
models <- list(tmodel1, tmodel2, tmodel3, tmodel4, tmodel5, tmodel6, tmodel7)

#specify model names
mod.names <- c('first_order', 'second_order',
               'third_order', 'fourth_order',
               'fifth_order', 'sixth_order',
               'seventh_order')

aictab(cand.set = models, modnames = mod.names)

# plot model

plot(tiny$thg.wet.ng.g~ tiny$daynum, col= "blue")+
  lines(sort(tiny$daynum),
        fitted(tmodel5)[order(tiny$daynum)],
        col = "blue")+
  text(x = 285, y = 110, paste0("k = ", 5, ", BIC = ", signif(BICt5 , 4)))+
  text(x = 285, y = 105, paste0("Pr > F =", 3.05*10^-8) )


# Create a data frame for plotting
plot_data <- data.frame(x = tiny$daynum, y = tiny$thg.wet.ng.g, 
                        fit = predictions[, "fit"], 
                        lwr = predictions[, "lwr"], 
                        upr = predictions[, "upr"])

# Plot the data with the polynomial fit and confidence intervals
tinyp <- ggplot(plot_data, aes(x = tiny$daynum, y = tiny$thg.wet.ng.g)) +
  geom_point() +  # original data points
  geom_line(aes(y = fit), color = "blue") +  # fitted polynomial line
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2) +  # confidence interval
  labs(title = "THg (6-13mm)" )+
  xlab("Day of Year")+
  ylab("THg (ng/g)")+
  annotate(geom="text", x=300, y=100, label="k=5",
           color="black")+
  annotate("text", x = 300, y = 95, 
           label = paste("BIC = ", round(BICt5, 2)), 
           color = "black")


text(x = 285, y = 100, paste0("k = ", 4, ", BIC = ", signif(BIC_t5, 4)))
tiny_p







#=============================================================================
#plot both at same time
#=============================================================================


both <- ggplot(plot_tiny, aes(x = x, y = y)) +
  geom_line(aes(y = fit_t), color = "blue") +  # fitted polynomial line
  geom_ribbon(aes(ymin = lwr_t, ymax = upr_t), alpha = 0.2, fill="blue") +  # confidence interval
  labs(title = "Comparing Size Classes") +
  
  # Add models and condfidence intervals
  geom_line(data = big_plot, aes(x = x, y = fit_b), color = "red") +
  geom_ribbon(data = big_plot, aes(x = x, ymin = lwr_b, ymax = upr_b), alpha = 0.2, fill = "red")+
  xlab("Day of Year")+
  ylab("THg (ng/g)")+
  geom_vline(xintercept = 297, 
             color = "red", linetype = "dashed", size = 1)+
  annotate("text", x = 297, y = 90, label = "Turnover", angle = 90, vjust = -0.5)+
  
  geom_vline(xintercept = 245, 
             color = "red", linetype = "dashed", size = 1)+
  
  annotate("text", x = 245, y = 90, label = "End of Summer", angle = 90, vjust = -0.5)

#trying to add points

both <- ggplot(plot_tiny, aes(x = x, y = y)) +
  geom_line(aes(y = fit_t), color = "blue") +  # fitted polynomial line
  geom_ribbon(aes(ymin = lwr_t, ymax = upr_t), alpha = 0.2, fill="blue") +  # confidence interval
  labs(title = "Comparing Size Classes") +
  
  # Add models and condfidence intervals
  geom_line(data = big_plot, aes(x = x, y = fit_b), color = "red") +
  geom_ribbon(data = big_plot, aes(x = x, ymin = lwr_b, ymax = upr_b), alpha = 0.2, fill = "red")+
  xlab("Day of Year")+
  ylab("THg (ng/g)")+
  geom_vline(xintercept = 297, 
             color = "red", linetype = "dashed", size = 1)+
  annotate("text", x = 297, y = 90, label = "Turnover", angle = 90, vjust = -0.5)+
  
  geom_vline(xintercept = 245, 
             color = "red", linetype = "dashed", size = 1)+
  
  annotate("text", x = 245, y = 90, label = "End of Summer", angle = 90, vjust = -0.5)+
  geom_point(plot_tiny, aes(plot_tiny, aes(x=x, y=y)))


both

#=============================
#par plot
#=============================

tiny_p <- ggplot(plot_tiny, aes(x = x, y = y)) +
  geom_point(color= "blue")+
  geom_line(aes(y = fit_t), color = "blue") +  # fitted polynomial line
  geom_ribbon(aes(ymin = lwr_t, ymax = upr_t), alpha = 0.2, fill= "blue") +  # confidence interval
  labs(title = "THg (6-13mm)")+
  xlab("Day of Year")+
  ylab("THg (ng/g)")+
  annotate(geom="text", x=265, y=117, label="k=5",
           color="black")+
  annotate("text", x = 270, y = 110, 
           label = paste("BIC = ", round(BICt5, 2)), 
           color = "black")+

geom_vline(xintercept = 297, 
           color = "red", linetype = "dashed", size = 1)+
  annotate("text", x = 297, y = 85, label = "Turnover", angle = 90, vjust = -0.5)+
  
  geom_vline(xintercept = 245, 
             color = "red", linetype = "dashed", size = 1)+
  
  annotate("text", x = 245, y = 90, label = "End of Summer", angle = 90, vjust = -0.5)
  

tiny_p
  
  
  

library(patchwork)

bigp/
  tiny_p/
  both


both <- ggplot(plot_tiny, aes(x = x, y = y)) + 
  geom_line(aes(y = fit_t), color = "blue") +  # fitted polynomial line
  geom_ribbon(aes(ymin = lwr_t, ymax = upr_t), alpha = 0.2, fill = "blue") +  # confidence interval
  labs(title = "Comparing Size Classes") +
  
  # Add models and confidence intervals from big_plot
  geom_line(data = big_plot, aes(x = x, y = fit_b), color = "red") +
  geom_ribbon(data = big_plot, aes(x = x, ymin = lwr_b, ymax = upr_b), alpha = 0.2, fill = "red") +
  
  xlab("Day of Year") +
  ylab("THg (ng/g)") +
  
  # Add vertical lines and annotations
  geom_vline(xintercept = 297, color = "red", linetype = "dashed", size = 1) +
  annotate("text", x = 297, y = 90, label = "Turnover", angle = 90, vjust = -0.5) +
  
  geom_vline(xintercept = 245, color = "red", linetype = "dashed", size = 1) +
  annotate("text", x = 245, y = 75, label = "End of Summer", angle = 90, vjust = -0.5) +
  
  # Add points for plot_tiny
  geom_point(color = "blue", data = plot_tiny, aes(x = x, y = y))+
  
  # add points for big_plot
  geom_point(color = "red", data = big_plot, aes(x = x, y = y))
  

both

