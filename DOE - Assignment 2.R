library("easypackages")
paq <- c('paramtest','pwr', 'ggplot2', 'nlme', 'dplyr', 'randomizr', 'knitr', 'multcomp', 'lsmeans', 'emmeans', 'car', 'gmodels', 'powerMediation', 'HH', 'effects', 'agricolae', 'daewr', 'SuppDists', 'outliers', 'phia', 'stats', 'crossdes')
libraries(paq)


#4. Two sources of variation

#First load data

dat <- read.table(header=TRUE, text='
 subject gender   age before after
       1   F   old    9.5   7.1
       2   M   old   10.3  11.0
       3   M   old    7.5   5.8
       4   F   old   12.4   8.8
       5   M   old   10.2   8.6
       6   M   old   11.0   8.0
       7   M young    9.1   3.0
       8   F young    7.9   5.2
       9   F   old    6.6   3.4
      10   M young    7.7   4.0
      11   M young    9.4   5.3
      12   M   old   11.6  11.3
      13   M young    9.9   4.6
      14   F middle    8.6   6.4
      15   F young   14.3  13.5
      16   F   old    9.2   4.7
      17   M young    9.8   5.1
      18   F   old    9.9   7.3
      19   F young   13.0   9.5
      20   M young   10.2   5.4
      21   M young    9.0   3.7
      22   F middle    7.9   6.2
      23   M   old   10.1  10.0
      24   M middle    9.0   1.7
      25   M middle    8.6   2.9
      26   M middle    9.4   3.2
      27   M middle    9.7   4.7
      28   M middle    9.3   4.9
      29   F middle   10.7   9.8
      30   M   old    9.3   9.4 ')

#Check data structure

head(dat)

#Descriptive

dat %>%
  group_by(gender, age) %>%
  summarise(N = n(), Mean = mean(after, na.rm=TRUE),  SD = sd(after, na.rm=TRUE))

#Model with interaction

m1 <- lm(after ~ as.factor(gender) + as.factor(age) + as.factor(gender):as.factor(age), data = dat)
summary(m1)

#Pairs averaged over age

(pigs_m1_a <- pairs(emmeans(m1, "age")))

#Pairs averaged over gender

(pigs_m1_g <- pairs(emmeans(m1, "gender")))

plot(pigs_m1_a, comparisons = TRUE)


#Check comparisons 

coef(pigs_m1_a)

#Using a glht

m1_dun <-  multcomp::glht(m1, linfct = mcp (age ="Dunnett"),
                          alternative = "greater")
summary(m1_dun)

#SUM SQUARES

#The order of the factor matters here

fit1a <- aov(after ~ gender + age + gender:age, data = dat)
fit1b <- aov(after ~ age + gender  + age:gender, data = dat)
summary(fit1a)
summary(fit1b)

#The sum squares adds to the total

SS.I1 <- anova(lm(after ~ 1,                 data=dat),
               lm(after ~ gender,               data=dat))
SS.I2 <- anova(lm(after ~ gender,               data=dat),
               lm(after ~ gender+age,           data=dat))
SS.Ii <- anova(lm(after ~ gender+age,           data=dat),
               lm(after ~ gender+age + gender:age, data=dat))

SS.I1[2, "Sum of Sq"]

SS.I2[2, "Sum of Sq"]

SS.Ii[2, "Sum of Sq"]

#TOTAL SS

sstot <- anova(lm(after ~ 1,       data=dat),
               lm(after ~ gender:age, data=dat))

sstot[2, "Sum of Sq"]

SS.I1[2, "Sum of Sq"] + SS.I2[2, "Sum of Sq"] + SS.Ii[2, "Sum of Sq"]

#ANOVA with SS II

(fit2a <- Anova(lm(after ~ gender + age 
                   + gender:age, data = dat), type = "II"))
(fit2b <- Anova(lm(after ~ age + gender 
                   + age:gender, data = dat), type = "II"))

#TOTAL SS

sstot <- anova(lm(after ~ 1,       data=dat),
               lm(after ~ gender:age, data=dat))

sstot[2, "Sum of Sq"]

#Total Type II do not add

fit2a[1, 1] + fit2a[2,1] + fit2a[3,1] + fit2a[4,1]

#ANOVA with SS III

#Hypotheses are only tested when using effect- or orthogonal coding for categorical variables

(fit3a <- Anova(lm(after ~ gender + age 
                   + gender:age, data = dat,
                   contrasts=list(gender = contr.sum, age =contr.sum)), type = "III"))

#It does not get affected by order

(fit3b <- Anova(lm(after ~ age + gender 
                   + age:gender, data = dat,
                   contrasts=list(age = contr.sum, gender =contr.sum)), type = "III"))

#TOTAL SS

sstot <- anova(lm(after ~ 1,       data=dat),
               lm(after ~ gender:age, data=dat))

sstot[2, "Sum of Sq"]

# Total Type III do not add
fit3a[1, 1] + fit3a[2,1] + fit3a[3,1] + fit3a[4,1]

#-------------------------------------------------
## POWER
#-------------------------------------------------
# a single estimation
powerInteract2by2(n=35, tauBetaSigma=.3, alpha=0.05, nTests=1)[1]

nvals <- c(5, 10, 20, 40)
deltas <- c(0.3, 0.4, 0.5)
plot(nvals, seq(0,1, length.out=length(nvals)), xlab="n", ylab="power",
     main="Power Curve for\nTwo-way Interaction", type="n")
for (i in 1:3) {
  powvals <- sapply(nvals, 
                    function (x) powerInteract2by2(n=x,
                                                   tauBetaSigma=deltas[i], alpha=0.05, nTests=1)[1])
  lines(nvals, powvals, lwd=2, col=i)
}
abline(h=.8, lty = 2)
legend("bottomright", lwd=2, col=1:3, legend=c("0.3", "0.4", "0.5"))

----------------------------------

# Exercise 5
# contrast, linear combination, multiple comparisons

set.seed(357)

# generate some data
my_data <- data.frame( X1 = rnorm(10, 0, 1),
                       X2 = rnorm(10, 5, 1),
                       X3 = rnorm(10, 2, 2),
                       X4 = rnorm(10, 6, 1))
# lets see the data
my_data

# linear combination of means and output via t-test
t.test(apply(my_data[, c(1,3)],2,mean)-apply(my_data[, c(2,4)],2,mean))[[5]]/2

t.test(apply(my_data[, c(1,4)],2,mean)-apply(my_data[, c(2,3)],2,mean))[[5]]/2

# another alternative setting linear combinations
# this is what we do by contrasts
t.test(t(t(my_data)*c(1,-1,1,-1)))

# Or creating an object first
contr1 <- c(1,-1,1,-1)
t.test(t(t(my_data)*contr1))

# here is another alternative for it
y <- as.vector(as.matrix(my_data))
treat <- factor(rep(1:4, each = 10))
head(cbind(y,treat))
tail(cbind(y,treat))
fit1 <- lm(y ~ treat)
summary(fit1)

# adjusted pairs comparison using Tukey
pairs(emmeans(fit1, ~ treat))

# unadjusted pairs comparison
# difference between 1 and 3 and 2 and 4 
pairs(emmeans(fit1, ~ treat), adjust = "none")

# here we manipulate the design matrix
(X <- model.matrix(fit1))  # design/model matrix
t(X)%*%X  # can you identify what occurs in this matrix?
solve(t(X)%*%X) # and this one?

# how about this way?
contrasts(treat)  # first group is droped
# now defined our own 
levels(treat) # check the order of factor levels
c1 <- c(0,0,0,0)
c2 <- c(1,-1, 1,-1)
c3 <- c(0,0,0,0)
mat <- cbind(c1,c2,c3)
contrasts(treat) <- mat
fit2 <- lm(y ~ treat )
coef(fit2)
mean(y)


# Here we just change the reference group
# assume that three is what we want as a reference group now
contrasts(treat) <-'contr.treatment'(levels(treat), base = 3)
fit3 <- lm(y ~ treat )
coef(fit3)
# compare to fit1
coef(fit1)
apply(my_data,2,mean) # means of each group

# Now two-way data (recall this data is just for example)
dat <- read.table(header=TRUE, text='
 subject gender   age before after
                  1   F   old    9.5   7.1
                  2   M   old   10.3  11.0
                  3   M   old    7.5   5.8
                  4   F   old   12.4   8.8
                  5   M   old   10.2   8.6
                  6   M   old   11.0   8.0
                  7   M young    9.1   3.0
                  8   F young    7.9   5.2
                  9   F   old    6.6   3.4
                  10   M young    7.7   4.0
                  11   M young    9.4   5.3
                  12   M   old   11.6  11.3
                  13   M young    9.9   4.6
                  14   F middle    8.6   6.4
                  15   F young   14.3  13.5
                  16   F   old    9.2   4.7
                  17   M young    9.8   5.1
                  18   F   old    9.9   7.3
                  19   F young   13.0   9.5
                  20   M young   10.2   5.4
                  21   M young    9.0   3.7
                  22   F middle    7.9   6.2
                  23   M   old   10.1  10.0
                  24   M middle    9.0   1.7
                  25   M middle    8.6   2.9
                  26   M middle    9.4   3.2
                  27   M middle    9.7   4.7
                  28   M middle    9.3   4.9
                  29   F middle   10.7   9.8
                  30   M   old    9.3   9.4 ')

dat %>%
  group_by(gender, age) %>%
  summarise(N = n(), Mean = mean(after, na.rm=TRUE),  SD = sd(after, na.rm=TRUE))

# fit the model
m1 <- lm(after ~ gender + age + gender:age, data = dat)
summary(m1)
# pairs averaged over age
(pigs_m1_a <- pairs(emmeans(m1, "age")))
# pairs averaged over gender
(pigs_m1_g <- pairs(emmeans(m1, "gender")))
plot(pigs_m1_a, comparisons = TRUE)

# get the coefficients
coef(pigs_m1_a)
# get some polynomial contrast
(const_poly <- contrast(emmeans(m1, "age"), "poly"))
coef(const_poly )

# another example with another package
m1_dun <-  multcomp::glht(m1, linfct = mcp (age ="Dunnett"),
                          alternative = "greater")
summary(m1_dun)


# Exercise 6
# ANCOVA

## packages
library("HH")
library('paramtest')
library('pwr')
library('ggplot2')
library("multcomp")
library('lsmeans')
library('emmeans')
library('dplyr')
library("car")
library("gmodels")
library("effects")

# data generation
set.seed(3)
n <- 20
p <- 2
A.eff <- c(40, -15)
beta <- -0.45
sigma <- 4
B <- rnorm(n * p, 0, 15)
A <- gl(p, n, lab = paste("Group", LETTERS[1:2]))
mm <- model.matrix(~A + B)
dat <- data.frame(A = A, B = B, 
                  Y = as.numeric(c(A.eff, beta) %*% t(mm)) +
                    rnorm(n * p, 0, 4))
dat$B <- dat$B + 20
names(dat) <- c("treat", "pre", "post")

head(dat)
str(dat)
dat %>%
  group_by(treat) %>%
  summarise(N = n(), 
            Mean = mean(post, na.rm=TRUE),  
            SD = sd(post, na.rm=TRUE))

# some plots
par(mfrow=c(1,2))  
boxplot(pre ~ treat, dat)
boxplot(post ~ treat, dat)

# fitting ancova model
## HH package needed for this line below
# mod <- ancova(post ~ treat + pre, data=dat)
# pred <- predict(mod)

pred <- predict(lm(post ~ treat + pre, data = dat))
# plotting 
ggplot(data = cbind(dat, pred),
       aes(pre, post, color=treat)) + geom_point() +
  facet_grid(. ~ treat) + geom_line(aes(y=pred))

# testing the interaction
anova(lm(post ~ treat * pre, data = dat))
pred <- predict(lm(post ~ treat * pre, data = dat))


# if not interested into looking at the effect of the cov
fit1 <- lm(post ~ treat + pre, data = dat)
anova(fit1)
# otherwise, type III sums of squares
contrasts(dat$treat) <- contr.sum
fit2 <- lm(post ~ treat + pre, data = dat)
anova(fit2)

# analyses
summary(lm(post ~ treat + pre, data = dat))
confint(lm(post ~ treat + pre, data = dat))
# which are aligned with the coef of the mod above from the HH
# coef(mod)

## with interaction
# generate some data for power
possible.ns <- seq(from=100, to=1000, by=50)
powers <- rep(NA, length(possible.ns))
powers.cov <- rep(NA, length(possible.ns))        # Need a second empty vector
alpha <- 0.05
sims <- 500
for (j in 1:length(possible.ns)){
  N <- possible.ns[j]
  
  significant.experiments <- rep(NA, sims)
  significant.experiments.cov <- rep(NA, sims) # Need a second empty vector here too
  
  for (i in 1:sims){
    gender <- c(rep("F", N/2), rep("M", N/2))       # Generate "gender" covariate
    age <- sample(x=18:65, size=N, replace=TRUE)    # Generate "age" covariate
    effectofgender <- 10                            # Hypothesize the "effect" of gender on income
    effectofage <- 2                                # Hypothesize the "effect" of age on income
    
    ## Hypothesize Control Outcome as a function of gender, age, and error
    Y0 <- effectofgender*(gender=="M") + 
      effectofage*age +
      rnorm(n=N, mean=100, sd=20)
    
    ## This is all the same ##
    tau <- 5
    Y1 <- Y0 + tau
    Z.sim <- rbinom(n=N, size=1, prob=.5)
    Y.sim <- Y1*Z.sim + Y0*(1-Z.sim)
    fit.sim <- lm(Y.sim ~ Z.sim)
    
    ## This is the novel analysis -- including two covariates to increase precision ##
    fit.sim.cov <- lm(Y.sim ~ Z.sim + 
                        (gender=="M") + age)
    
    ## extract p-values and calculate significance ##
    p.value <- summary(fit.sim)$coefficients[2,4]
    p.value.cov <- summary(fit.sim.cov)$coefficients[2,4]
    significant.experiments[i] <- (p.value <= alpha)
    significant.experiments.cov[i] <- (p.value.cov <= alpha)
  }
  
  powers[j] <- mean(significant.experiments)
  powers.cov[j] <- mean(significant.experiments.cov)
}

# some plot of power
plot(possible.ns, powers, ylim=c(0,1))
points(possible.ns, powers.cov, col="red")

# Can you compute effect sizes using pooled SD and root mean squared error?
# recall the last point of the handout

summary(fit2)
sigma(fit2)

# d using sqrt MSE
coef(fit2)[2] / sigma(fit2)

# d using pool 

newdat <- dat %>%
  group_by(treat) %>%
  summarise(N = n(), 
            Mean = mean(post, na.rm=TRUE),  
            var = var(post, na.rm=TRUE)) # change to be var
newdat <- as.data.frame(newdat)

(Spool <- sqrt(((newdat[1,2]  - 1 ) * newdat[1,3] + 
                  (newdat[2,2]  - 1 ) * newdat[2,3])/
                 (newdat[1,2] + newdat[2,2] - 2)))
# so d is
coef(fit2)[2] / Spool


# RBD
library(agricolae)
#an example with two treatments and 3 levels on the blocking variable
treat <- c(1,2)
outdesign <- design.rcbd(treat, 3, seed = 12)
(rcb <- outdesign$book)
levels(rcb$block) <- c("large", "medium", "small")
head(rcb)


#an example with four treatments and 4 levels on the blocking variable
treat <- c(1,2,3,4)
outdesign <- design.rcbd(treat, 4, seed = 12)
(rcb <- outdesign$book)
levels(rcb$block) <- c("block1", "block2", "block3", "block4")
head(rcb)

# lets load the package
library(randomizr)

# Load dataset
data(HairEyeColor)
HairEyeColor <- data.frame(HairEyeColor)

# Transform so each row is a subject
# Columns describe subject's hair color, eye color, and gender
hec <- HairEyeColor[rep(1:nrow(HairEyeColor),
                        times = HairEyeColor$Freq), 1:3]

# lets see the few first and last units on the data set  
head(hec)
tail(hec)

# total sample size 
N <- nrow(hec)

# remove the rownames
rownames(hec) <- NULL

# simple random assignment 
Z1 <- simple_ra(N = N)
table(Z1)
#----------------------------------------------------
# Now lets block in Hair
Z <- block_ra(blocks = hec$Hair)
table(Z, hec$Hair)

# How about having T1 and T2 and Ctrl?
Z <- block_ra(blocks = hec$Hair, conditions = c("T1", "T2", "Ctrl"))
table(Z, hec$Hair)
head(Z)
# creates ID 
hec$ID <- paste0("ID", seq_len(nrow(hec)))

rbd_alloc <- function(x){
  set.seed(343)   # for reproducibility 
  cbind(x, Allocation =
          randomizr::block_ra(blocks = x$Hair, 
                              conditions = c("T1", "T2", "Ctrl")) )
}
my_alloc <- rbd_alloc(hec)
head(my_alloc)
table(my_alloc$Hair, my_alloc$Allocation)
# write.csv(my_alloc, "my_allocation.csv") # always save this file 


# outcome 
set.seed(123)
my_alloc$scores <-  rnorm(nrow(my_alloc), 5, 2)
head(my_alloc)

library(dplyr)
my_alloc %>%
  group_by(Allocation, Hair) %>%
  summarise(N = n(), Mean = mean(scores, na.rm=TRUE), 
            SD = sd(scores, na.rm=TRUE))




my_alloc$scores <-  rnorm(nrow(my_alloc), 5, 2)
head(my_alloc)

library(dplyr)
my_alloc %>%
  group_by(Allocation, Hair) %>%
  summarise(N = n(), Mean = mean(scores, na.rm=TRUE),  
            SD = sd(scores, na.rm=TRUE))



library(ggplot2)
# boxplot
ggplot2::ggplot(my_alloc,
                aes(x=as.factor(Allocation), 
                    y=scores, 
                    fill = as.factor(Hair))) +
  geom_boxplot() 




library(ggplot2)
# boxplot
ggplot2::ggplot(my_alloc,
                aes(x=as.factor(Hair), 
                    y=scores, 
                    fill = as.factor(Allocation))) +
  geom_boxplot() 



# model 1
fit1a <- lm(scores ~ Hair + Allocation, data = my_alloc)
anova(fit1a)

fit1b <- lm(scores ~ Allocation + Hair, data = my_alloc)
anova(fit1b)

drop1(fit1a, ~.)



library(dplyr)
my_alloc %>%
  group_by(Allocation) %>%
  summarise(N = n(), Mean = mean(scores, na.rm=TRUE),
            SD = sd(scores, na.rm=TRUE))



library(dplyr)
my_alloc %>%
  group_by(Hair) %>%
  summarise(N = n(), Mean = mean(scores, na.rm=TRUE),  
            SD = sd(scores, na.rm=TRUE))

with(my_alloc, interaction.plot(my_alloc$Allocation, 
                                my_alloc$Hair, my_alloc$scores))
with(my_alloc, interaction.plot(my_alloc$Allocation, 
                                my_alloc$Hair, my_alloc$scores, ylim = c(0,10)))



fit2a <- lm(scores ~ Hair + Allocation + 
              Hair:Allocation, data = my_alloc)
anova(fit2a)

library(multcomp)
summary(glht(fit1a,  linfct=mcp(Allocation="Tukey")))
# how about fit2 with the interaction
summary(glht(fit2a,  linfct=mcp(Allocation="Tukey"))) # it will not run


library(emmeans)
fit2a.em <- emmeans(fit2a, "Allocation")
pairs(fit2a.em)

library(car)
with(my_alloc, leveneTest(scores,
                          interaction(Hair, Allocation)))

plot(fit2a, 1)

plot(fit2a, 2)

plot(fit2a, 3)
plot(fit2a, 4)
# Power


library(daewr)
bmin <-2
bmax <- 4
alpha <- .05 
sigma2 <- 1 # mean square error
css <- 5  # sum of treatment effects squares
blocks <- seq(bmin, bmax)
nu1 <- 3 -1  # df numerator
nu2 <- (blocks -1 ) * nu1 # df denominator
nc <- (blocks *css) /sigma2
power <- Fpower(alpha, nu1, nu2, nc)
data.frame(blocks, nu1, nu2, nc, power)



#----------------- from Seohee ------------------------------------------

com_block_random_p <- function(alpha, sigma2, del, nu, b, s) {
  v1 <- nu-1
  v2 <- nu*(b*s-1)
  fvalue <- qf(1 - alpha,v1, v2)
  phi <- sqrt(b*s*del^2/ (2*nu*sigma2))
  noncn <- nu*phi^2
  power <- 1-pf(fvalue, v1, v2, noncn)
  data.frame(alpha, sigma2, del, nu, b, s, power)
}

nu<- 5
del<- 0.5
sigma2<- 0.4
alpha<- 0.05
b <- 3
s <- seq(10, 75)
com_block_random_p(alpha, sigma2, del, nu, b, s)

b <- seq(1,20)
s <- 13
com_block_random_p(alpha, sigma2, del, nu, b, s)

