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


#plot
#plot(big$daynum, big$thg.wet.ng.g)+
  #lines(hp, fitted(fit), col='red', type='b')

#plot
plot(big$thg.wet.ng.g~ big$daynum)
lines(big$daynum, fitted(bmodel4), col='red', type='b')

#ggplot
fit <- bmodel4
prd <- data.frame(hp = seq(from = range(big$daynum)[1], to = range(big$daynum)[2], length.out = 28))
err <- predict(fit, newdata = prd, se.fit = TRUE)

prd$lci <- err$fit - 1.96 * err$se.fit
prd$fit <- err$fit
prd$uci <- err$fit + 1.96 * err$se.fit

ggplot(prd, aes(x = hp, y = fit)) +
  theme_bw() +
  geom_line() +
  geom_smooth(aes(ymin = lci, ymax = uci), stat = "identity") +
  geom_point(data = big, aes(x = daynum, y = thg.wet.ng.g))

# plot

par(mfrow=c(1,1))

plot(big$thg.wet.ng.g~ big$daynum, col= "orange")+
lines(sort(big$daynum),
      fitted(bmodel4)[order(big$daynum)],
      col = "orange")  

plot(tiny$thg.wet.ng.g~ tiny$daynum, col= "blue")+
  lines(sort(tiny$daynum),
        fitted(tmodel5)[order(tiny$daynum)],
        col = "blue")  

