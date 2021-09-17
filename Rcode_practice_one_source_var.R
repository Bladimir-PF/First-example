# One source of Variation ---------------------------------

Outcome <- c(20, 22, 18, 14, 21, 24, 28, 26, 20, 26, 20, 10, 12, 8, 11, 9)
Treat <- c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3)
Length <- rep(0:1, 8)

dat1 <-  data.frame(cbind(Treat, Outcome, Length))
names(dat1) <- c("Treatment", "Outcome", "Length")
   
str(dat1)
View(dat1)
  
# One way ANOVA----------------------------------------------------
mean(as.numeric(Outcome))
var(as.numeric(Outcome))
  
library(dplyr)
summary_ex1 <-  dat1 %>% # the names of the new data frame and the data frame to be summarized
    group_by(Treatment) %>%   # the grouping variable
    dplyr::summarise(mean_T = mean(as.numeric(Outcome)),  # calculates the mean of each group
                     sd_T = sd(as.numeric(Outcome)), # calculates the standard deviation of each group
                     n_T = n(),  # calculates the sample size per group
                     SE_T = sd(as.numeric(Outcome))/sqrt(n())) # calculates the standard error of each group
 summary_ex1 
 
  fit_aov <- aov(Outcome ~ as.factor(Treat))
  summary(fit_aov)
Treat2 <- as.factor(Treat)
dat3<- data.frame(dat1, Treat2)


apa.1way.table(iv = Treat2, dv = Outcome, data = dat3, filename ="ex1_desc_table.doc")
  
   
  #--------critical value of F test
  qf(.95, df1=2, df2= 13)  # critical value of F
  pf(33.17, df1 = 2, df2  = 13, lower.tail=FALSE) # probability F 
  
  
  # Tukey multiple pairwise-comparisons
  TukeyHSD(fit_aov) 
  plot(  TukeyHSD(fit_aov) )
  # error bar
  library(ggplot2)
  ggplot(summary_ex1, aes(Treatment, mean_T)) + 
    geom_errorbar(aes(ymin = mean_T - sd_T, ymax = mean_T + sd_T), width=0.2) +
    labs(y="Scores ", x = "Treatment") +
    theme_classic()
  
  # boxplot
  library(ggplot2)
  ggplot(dat1, aes(x=as.factor(Treatment), y=Outcome)) +
    geom_boxplot(fill= "gray", color="black") +
    theme_classic() 

  # histogram a quick and dirty way to assess normality ----------------
  ggplot(dat1, aes(Outcome)) + 
    facet_wrap(~as.factor(Treatment)) +
    geom_histogram(binwidth = 3)
	# using residuals 
	plot(fit_aov, 1)
	
 #---------- a test for normality
 
 # Extract the residuals
aov_residuals <- residuals(object = fit_aov )
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals )
#-------------------
# homogeneity of variance 
  # 1. Bartlett's test of Homogeneity of variances
bartlett.test(Outcome ~ as.factor(Treatment), data = dat1)
plot(fit_aov, 2)

# all assumptions pplots
par(mfrow=c(2,2))
plot(fit_aov)

library(car)
leveneTest(fit_aov)





  