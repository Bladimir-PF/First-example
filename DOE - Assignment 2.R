#####
library("easypackages")
paq <- c('paramtest','pwr', 'ggplot2', 'nlme', 'dplyr', 'randomizr', 'knitr', 'multcomp', 'lsmeans', 'emmeans', 'car', 'gmodels', 'powerMediation', 'HH', 'effects', 'agricolae', 'daewr', 'SuppDists', 'outliers', 'phia', 'stats', 'crossdes')
libraries(paq)
#####

# Two-sources of variation
dat1 <- read_csv("assig2_1.csv")
head(dat1)
str(dat1)

#Descriptive
dat1 %>%
  group_by(color, time) %>%
  summarise(N = n(), Mean = mean(outcome, na.rm=TRUE))

#Models
m1_in = lm(outcome ~ color + time + color:time, data = dat1)
m1_ni <- lm(outcome ~ color + time, data = dat1)
anova(m1_in)
anova(m1_ni)

#Contrasts
m1_in2 = aov(outcome ~ color + time + color:time, data = dat1)
TukeyHSD(m1_in2)

#######################

# RBD
dat2 <- read_csv("assig2_2.csv")
names(dat2)
str(dat2)

#Descriptive
dat2 %>%
  group_by(block, trt) %>%
  summarise(N = n(), Mean = mean(y, na.rm=TRUE))

#Models
m2_in = lm(y ~ block + trt + block:trt, data = dat2)
m2_ni <- lm(y ~ block + trt, data = dat2)
anova(m2_in)
anova(m2_ni)

#adding factor treatment
dat2$trt2 = as.factor(dat2$trt)
m2_in2 = lm(y ~ block + trt2 + block:trt2, data = dat2)

#Contrasts
summary(glht(m2_in2,  linfct = mcp(trt2="Tukey")))

#####################

# ANCOVA
dat3 <- read_csv("assig2_3.csv")
str(dat3)
summary(dat3$post)

#Descriptive
dat3 %>%
  group_by(treat) %>%
  summarise(N = n(), Mean_pre= mean(pre, na.rm=TRUE), Mean_post = mean(post, na.rm=TRUE))

#Models
m3_in = lm(post ~ pre * treat, data = dat3)
m3_ni <- lm(post ~ pre + treat, data = dat3)
anova(m3_in)
anova(m3_ni)
