


ssT1 <- (14*((24.9−41.3)^2)) + (25*((10.9−41.3)^2)) + (19*((93.3−41.3)^2))

ssT <- (30*((13−19.5)^2)) + (27*((27.1−19.5)^2)) + (13*((18.8−19.5)^2))
sstot <- (70-1)*41.99
ssE <- sstot-ssT

ssE2 <- ((5-1)*10)+((6-1)*11.2)+((5-1)*2.5)




# if we assume that sd.t=sd.c then we can calculate Cohen's d
numerator <- ((15-1)*1.92) + ((10-1)*0.29)  # Calculate the numerator of the pooled standard deviation 
s.pooled <-sqrt(numerator/(15+10-2)) # Calculate the pooled standard deviation

##The Pooled Standard Deviation: a weighted average of each group's standard deviation.
(24.1-18.9)/s.pooled # Calculate Cohen's d

library(readr)
dat1 <- read_csv("dat1.csv")

# create a data frame
dat1 <-  data.frame(dat1)
names(dat1) <- c("Treatment", "Outcome")

# load package and output descriptives by group
library(dplyr)
df1_ex1 <-  dat1 %>% # the names of the new data frame and the data frame to be summarised
  group_by(Treatment) %>%   # the grouping variable
  dplyr::summarise(mean_T = mean(as.numeric(Outcome)),  # calculates the mean of each group
                   var_T = var(as.numeric(Outcome)), # calculates the variance of each group
                   n_T = n(),  # calculates the sample size per group
                   sum_Y =  sum(as.numeric(Outcome)), # sum of the outcome
                   sum_Y2 = sum(as.numeric(Outcome)^2), # sum of the outcome sqaure 
                   SE_T = sd(as.numeric(Outcome))/sqrt(n())) # calculates the standard error of each group
# print descriptiives
df1_ex1
# fit the model
fit_aov <- aov(Outcome ~ as.factor(Treatment), data = dat1)
# create the output
summary(fit_aov)
# Tukey multiple pairwise-comparisons
TukeyHSD(fit_aov) 
# Error Bar
library(ggplot2)
ggplot(dat1, aes(Treatment, Outcome)) +
  stat_summary(fun.data = mean_se, geom = "errorbar") +
  theme_classic()
# Boxplot
library(ggplot2)
ggplot(dat1, aes(x=as.factor(Treatment), y=Outcome)) +
  geom_boxplot(fill= "gray", color="black") +
  theme_classic() 
# assumptions
bartlett.test(Outcome ~ as.factor(Treatment), data = dat1)
car::leveneTest(Outcome ~ as.factor(Treatment), data = dat1)
# plot assumptions
par(mfrow=c(2,2))
plot(fit_aov)