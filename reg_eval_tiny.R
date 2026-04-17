install.packages('AICcmodavg')

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
BICt6 <-(tmodel6)

tmodel7 <- lm(tiny$thg.wet.ng.g ~ poly(tiny$daynum, 7), data = tiny)
BICt7 <- (tmodel7)

#ANOVA
print(anova(tmodel1,tmodel2,tmodel3,tmodel4,tmodel5, tmodel6, tmodel7))

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

#train set

library(caTools)
set.seed(100) # set seed for reproducibility

# define split vector
tsplit <- sample.split(tiny$thg.wet.ng.g, SplitRatio = 0.65)

# split data set
ttrain_set <- tiny[tsplit == TRUE, ]
tval_set <- tiny[tsplit == FALSE, ]


# check dimensions of the training and validation set
dim(ttrain_set)
dim(tval_set)

tmodel1 <- lm(ttrain_set$thg.wet.ng.g ~ poly(ttrain_set$daynum, 1), data = ttrain_set)
BIC(tmodel1)

tmodel2 <- lm(ttrain_set$thg.wet.ng.g ~ poly(ttrain_set$daynum, 2), data = ttrain_set)
BIC(tmodel2)

tmodel3 <- lm(ttrain_set$thg.wet.ng.g ~ poly(ttrain_set$daynum, 3), data = ttrain_set)
BIC(tmodel3)

tmodel4 <- lm(ttrain_set$thg.wet.ng.g ~ poly(ttrain_set$daynum, 4), data = ttrain_set)
BIC(tmodel4)

tmodel5 <- lm(ttrain_set$thg.wet.ng.g ~ poly(ttrain_set$daynum, 5), data = ttrain_set)
BIC(tmodel5)

tmodel6 <- lm(ttrain_set$thg.wet.ng.g ~ poly(ttrain_set$daynum, 6), data = ttrain_set)
BIC(tmodel6)

tmodel7 <- lm(ttrain_set$thg.wet.ng.g ~ poly(ttrain_set$daynum, 7), data = ttrain_set)
BIC(tmodel7)


