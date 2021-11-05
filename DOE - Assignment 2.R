library("easypackages")
paq <- c('paramtest','pwr', 'ggplot2', 'nlme', 'dplyr', 'randomizr', 'knitr', 'multcomp', 'lsmeans', 'emmeans', 'car', 'gmodels', 'powerMediation', 'HH', 'effects', 'agricolae', 'daewr', 'SuppDists', 'outliers', 'phia', 'stats', 'crossdes')
libraries(paq)

#Two-sources of variation (ts)

# Load dataset
dat1 <- read_csv("assig2_1.csv")

#Check data structure
head(dat1)
str(dat1)
table(dat1$color)
table(dat1$time)

#Descriptive
dat1 %>%
  group_by(color, time) %>%
  summarise(N = n(), Mean = mean(outcome, na.rm=TRUE))

#Models with and without interaction

#Variance
ts_fit1a <- aov(outcome ~ color + time + color:time, data = dat1)
ts_fit1aaa <- aov(outcome ~ time + color + time:color, data = dat1)
summary(ts_fit1a)
summary(ts_fit1aaa)
ts_fit1b <- aov(outcome ~ color + time, data = dat1)
summary(ts_fit1b)

anova(ts_fit1b, ts_fit1a)

#Linear regressions
ts_lm1 <- lm(outcome ~ color + time + color:time, data = dat1)
ts_lm1a <- lm(outcome ~ color + time, data = dat1)
summary(ts_lm1)
summary(ts_lm1a)




ts_lm2 <- lm(after ~ as.factor(gender) + as.factor(age), data = dat)

summary(ts_lm1)
summary(ts_lm2)


# Tukey multiple pairwise-comparisons
TukeyHSD(ts_fit1a)

#####################

# ANCOVA

dat2 <- read_csv("assig2_3.csv")
names(dat2)
table(dat2$treat)
str(dat2)

dat2$treat2 = as.factor(dat2$treat)

# testing the interaction
anova(lm(post ~ pre * treat, data = dat2))

# if not interested into looking at the effect of the covariate
anova(lm(post ~ pre + treat, data = dat2))
summary(lm(post ~ pre + treat, data = dat2))
summary(lm(post ~ pre * treat, data = dat2))

fit111 <- lm(post ~ pre + treat, data = dat2)

m1_aov <- aov(post ~ pre + treat, data = dat2)




# Tukey multiple pairwise-comparisons
TukeyHSD(m1_aov)
summary(glht(fit1,  linfct=mcp(treat="Tukey")))

#######################

# RBD
dat3 <- read_csv("assig2_2.csv")
names(dat3)
str(dat3)

dat3 %>%
  group_by(block, trt) %>%
  summarise(N = n(), Mean = mean(y, na.rm=TRUE))

# model with and without interaction
b_fit1a <- lm(y ~ block + trt + block:trt, data = dat3)
b_fit1aa <- lm(y ~ as.factor(block) + as.factor(trt) + as.factor(block):as.factor(trt), data = dat3)
anova(b_fit1a)


dat3$trt2 = as.factor(dat3$trt)
b_fit1aaa <- lm(y ~ block + trt2 + block:trt2, data = dat3)
anova(b_fit1aaa)

b_fit1b <- lm(y ~ block + trt, data = dat3)
anova(b_fit1b)

summary(glht(b_fit1aaa,  linfct = mcp(trt2="Tukey")))


library(emmeans)
fit2a.em <- emmeans(fit1a, "Allocation")
pairs(fit2a.em)
library(car)
with(my_alloc, leveneTest(scores,
                          interaction(Hair, Allocation)))