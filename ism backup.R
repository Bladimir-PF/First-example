---
  title: "ISM Assignment 3"
author: "Geraldo B. Padilla F."
date: "11/19/2021"
output:
  word_document: default
html_document: default
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r, echo=FALSE, warning=FALSE, message=FALSE, results='hide'}
pack <- c("tidyverse", "mosaic", "ggformula", 'car', 'broom', 'statthink', 'emmeans')
lapply(pack, require, character.only = TRUE)
```

```{r, echo=FALSE, warning=FALSE, message=FALSE}
dat <- readr::read_csv("https://raw.githubusercontent.com/lebebr01/psqf_6243/main/data/fertility.csv")
theme_set(theme_statthinking())
attach(dat)
```

## Questions

1. Using the data descriptions above, create a research question to explore for this assignment that includes a categorical predictor. Note, for this first part, include just a single categorical predictor in your research questions.

To begin with, I would be interested in exploring the relationship between the country's gross national income per capita (`gni_class`) and the `fertility rate`, on the assumption that this index varies as a function of available income.

Explicitly, the guiding question would be the following: does the estimated gross national income per capita of the country (`gni_class`) explain the variation of the `fertility rate` among women across in different countries?

Before we begin, an additional comment. The relationship between economic resources and fertility rate can be confusing. Generally speaking, with more money people have access to better services for having and raising children (health, education and housing), so one would expect that women living in countries with high per capita incomes would tend to have more children; however, to earn money people need to work (usually hard), which means having less time available. On the other hand, the highest salaries are available in the most skilled jobs, so people have to specialize and spend time on additional training, all of which translates, again, into less time available for activities such as having and raising children. In short, the guiding question is just the tip of the iceberg.

2. Explore the research question from #1 descriptively. Summarize key similarities/differences from the descriptive analysis.

To begin with, the dependent variable fertility rate (TF) shows an asymmetric distribution to the right (Figure 1), with minimum and maximum values between 1.24 and 7.03 respectively, and a large deviation of the values at the lower right. This positive asymmetry is also reflected in the mean and median positions, with values of 2.70 and 2.20 respectively. In addition, the upper bar chart in Figure 1 reports the distribution of the fertility rate in the first and third quartiles, where 50% of the data fall between 1.66 and 3.08.

Figure 1. Density plot of Fertility rate
```{r}
gf_density(~ fertility_rate, data = dat, color = 'black', fill = 'lightblue') %>%
  gf_boxploth(.5 ~ fertility_rate, data = dat, fill = "orange", width = .035) %>%
  gf_labs(x = 'Fertility rate')
```

Table 1. Frequencies of GNI (absolute and relatives)
```{r}
dat %>%
  group_by(gni_class) %>%
  summarise(N = n()) %>%
  mutate(Perc = round((N / sum(N))*100, digits = 2))
```

The variable `gni_class` is composed of 4 categories (Table 1). The distribution of absolute frequencies is uneven, with groups between 22 (Low) and 40 countries (Upper/Middle).

Regarding the relationship between fertility rate and GNI in the sample countries, the means and standard deviations vary considerably. Fertility rate means decrease as per capita economic resources (ordinal categories) increase.

Table 2. Descriptive statistics of fertility rate by GNI groups
```{r}
dat %>%
  group_by(gni_class) %>%
  summarise(Mean_FR = mean(fertility_rate), sd_FR = sd(fertility_rate), Min_FR = min(fertility_rate), Max_FR = max(fertility_rate))
```

This difference in means is corroborated by analysis of variance (ANOVA). The between-group variation (GNI) is larger than the between-unit variation (Table 3), which produces a large F value (80.98) and allows us to reject the null hypothesis for this sample (with a p-value <2e-16).

Table 3
```{r}
fit_aov <- aov(fertility_rate ~ as.factor(gni_class), data = dat)
summary(fit_aov)
```

Visually, the differences between groups (GNI), both in medians and dispersion (quartiles), can be seen in Figure 2.

Figure 2. Fertility rate according to GNI groups
```{r}
gf_violin(gni_class ~ fertility_rate, data = dat, fill = 'lightblue', draw_quantiles = c(0.1, 0.5, 0.9)) %>%
  gf_labs(x = "Fertility rate",
          y = "GNI per capita")
```

The Low group has the highest median fertility rate, and its deviation of values is similar to that shown by the Low/Middle group. On the other hand, the Upper/Middle and Upper groups have lower medians and deviation, especially the highest income category (Upper).

3. Fit a linear regression model to explore the research question from #1. Interpret each regression coefficient estimate from the linear regression model. That is, what do the specific linear regression coefficients mean in the context of the data problem at hand?

The linear regression model (Table 4) has as dependent variable the fertility rate of the countries and as predictor variable the income per capita (gni_class). Because the independent variable is categorical (4 levels), the parameters estimated by the regression model are interpreted slightly differently from the coefficients of a conventional linear regression model (quantitative variables).

Table 4. Linear Regression Model with dummy variables
```{r}
m1 = lm(fertility_rate ~ gni_class)
summary(m1)
```

In this case, the intercept (B0) represents the mean of the dependent variable (fertility rate) for the reference group (gni_low), while the other coefficients of the model (B1, B2 and B3) express the difference in means between the reference category and the other categories of the variable. On the other hand, as the gni_class variable is a string variable, the software relies on the alphabetical order of the categories to construct the dummy variables (k-1 = 3), a fact that in our example does not cause problems, since despite being an ordinal variable, each category is individually contrasted with the reference level.

First, the R-squared is .669, which means that the gni_class variable helps explain approximately 67% of the variability in fertility rates across countries. On the other hand, the RSE of the model is .807, which means that, on average, the observed values deviate by .807 units from the values predicted by the model.  The coefficient of variation of the model is .807/2.7 = .289, which can be interpreted as the percentage error being approximately 29%. Based on these data, we can assume that the model fits the data acceptably.

On the estimated coefficients (B1, B2 and B3), the differences between the means of each group with the reference category are significant (p-values below .01). As the categories are represented by dummy variables, the model resembles a multiple regression model (as many independent variables as categories of the independent variable). That is, the slopes are independent and only consider the variation between the reference and comparison categories. Thus, the variation of one point in the Low/Middle category (i.e., when the group changes from Low to Low/Middle) is equivalent to a decrease of 1.89 units in the average fertility rate of the countries (which is equivalent to 3.01, the observed mean of this group). This is equivalent to saying 'keeping the other attributes constant'. The interpretation of B2 and B3 is similar.

4. Evaluate the linear regression assumptions for the model fitted in #3. Summarize the degree to which the model meets the statistical assumptions of linear regression. 2 pts

```{r}
resid_m1 <- augment(m1)
```

Regarding the linearity of the regression model (Figure 3), we can assume that it is broadly complied with. Between the predicted values and the residuals of the third category we observe some drawbacks for the fit of the model, however, this does not seem to be a major problem.

Figure 3. Linearity
```{r}
plot(m1, 1)
```

The normal distribution of the residuals seems to be a major problem (Figure 4). Although the distribution of residuals appears normal between -1 and 1 quantile, the deviation of the extremes is high. On the other hand, the number of observations (n = 124) is not very high, so options to improve this aspect of the model should be explored.

Figure 4
```{r}
plot(m1, 2)
```

This abnormality in the distribution of residuals can be seen in the following figure (5). The mean is close to 0, however, the data are very close to the mean and quickly deviate to the extremes.

Figure 5
```{r}
gf_density(~ .resid, data = resid_m1) %>%
  gf_labs(x = "Residuals")
```

The homogeneity of variance seems acceptable (Figure 6), although again the third category exhibits problems of fit between the predicted values and the standardized residuals. This misalignment does not significantly affect the model, since, as we saw, the standard errors were small and the hypothesis tests (p-values) were significant at a 1% significance level.

Figure 6
```{r}
plot(m1, 3)
```

Despite this judgment, when considering the results of formal tests on homogeneity of variance, both the Bartlett test and the Levene's test suggest accepting the alternative hypothesis (heteroscedasticity between groups) at the 1% significance level.

Table 5.
```{r}
bartlett.test(fertility_rate ~ as.factor(gni_class), data = dat)
car::leveneTest(fertility_rate ~ as.factor(gni_class), data = dat)
```

Finally, Cook's distance shows that there are 3 values to explore (80, 83 and 123). If this were a real investigation, the next step would be to review these values and decide whether they represent possible events or are a tabulation error. Then, one could analyze the effect of removing these values, comparing models with and without them. However, in general and given that there are no values greater than 1 (rule of thumb), we can assume that there are no problems with influential values.

Figure 7. Leverage points
```{r}
plot(m1, 4)
```

In summary, we could say that the model adequately meets the assumptions of linearity, normal distribution of residuals, homoscedasticity and influential values, although options could be explored to improve homoscedasticity and the presence of outliers (80, 83 and 123).

5. Summarize the statistical evidence to answer the research question from #1. Be as specific as possible about which tests you are exploring and whether the statistical evidence supports or does not support the null hypothesis.

So far, statistical evidence allows us to establish that the level of per capita income (gni_class) helps to explain the variability of fertility rates among countries, especially if we consider the accuracy of the regression model (R-squared and RSE). Although this analysis is not interested in predicting values, the standard errors of each estimated parameter (B1, B2 and B3) were still significant at the 1% significance level, which provides evidence in favor of the alternative hypothesis of each (the difference between the means of the categories is not equal to 0 in the population; in other words, there are significant relationships between the dependent variable and the different categories of the independent variable).

6. Based on the residual plots explored in #4, is there evidence that key predictors are missing from the model? Why or why not? Note, you can also use the overall model statistics to help justify your answer in this question.
a. If you feel there are missing predictors from the model, which predictors from the data would you hypothesize to include next? Why this predictor next?

Based on the residual plots, it is difficult to establish the lack of any variable in the model since patterns in the distribution cannot be easily identified (most common sign that something strange is going on in the relationship between predictor and predicted variable).
However, based on the remaining explained variability (33% or 34% if one considers the adjusted R-squared, which is not very different from the normal R-squared since the predictor variables were not many), one could add other variables from the database to improve the accuracy and usefulness of the model.

In particular, the variables measuring the education (`educ_female`) and `contraceptive` use of women (or their partners in the case of the second variable) could be interesting to analyze. As I said before, one of my research assumptions is that the fertility rate is affected by different factors, one of them being per capita income, in addition to education and preventive sexual measures. Both variables are expected to be negatively related to the fertility rate. In other words, the assumption of this modification to the model is that as women in countries have more years of education and use contraceptive methods, the fertility rate decreases.

7. Add any predictors you think may help increase the utility of the statistical model fitted as main effects/additive effects. Note, you are welcome to include more than one attribute in this step, but include at least one. For this assignment, let’s not consider any interactions, make sure the terms are additive. Perform model comparisons to identify the extent to which this second model performs better than the first model fitted in #3.

To facilitate the interpretation of the estimated model parameters, I will use independent variables centered on the mean.

```{r}
dat = dat %>%
  mutate(educ_fem_mc = educ_female - mean(educ_female),
         contraceptv_mc = contraceptive - mean(contraceptive))
```

By performing an ANOVA test for both models, we can see that by introducing years of education (`educ_fem_mc`) and contraceptive (`contraceptv_mc`) use among women (both centered around the mean) our estimate of the fertility rate across countries improves (F = 15.774 and p-value = 8.494e-07).

Table 6. 
```{r}
m2 = lm(fertility_rate ~ educ_fem_mc + contraceptv_mc + gni_class, data = dat)
anova(m1, m2)
```

Due to the use of predictors centered on the mean, the interpretation of the coefficients is quite straightforward. Indeed, the interpretation of the intercept (B0) is as follows: the average fertility rate among countries classified as Low per capita income is 3.83, when the average female education and contraceptive use are centered on their mean.

In turn, the estimated parameters B1 and B2 represent the variation that a deviation from the mean female education (in years) or contraceptive use (percentage) has on the fertility rate, on average and holding all other predictors constant (among them and with respect to the GNI categories). In this case, regardless of the reference category (gni_class), the effects of B1 and B2 on the fertility rate are negative and significant at the 1% significance level (again, controlling or adjusting for the other attributes). For example, a one-year increase over the mean in the average education of women produces a decrease of 0.124 units in the countries' fertility rate, on average and holding contraceptive use and gni_class constant.

Table 7.
```{r}
summary(m2)
```

The coefficients for the GNI categories are interpreted similarly to the previous model: they still represent the difference in means with respect to the reference category (Low), although now the phrase "controlling by the other predictors" or "for given values of education and contraceptive use centered on the mean" should be added. An important difference between this model and the previous one is that the relationship between the gni_class categories and fertility rate was adjusted after the inclusion of other predictors in the model, which capture part of the variability of the fertility rate and allow for a more precise analysis of the impact of the different income categories on this phenomenon.

8. Similar to #5, summarize the statistical evidence to answer the research question from the best fitting model. Be as specific about what the statistical evidence means in the context of the original data and research question from #1. a. If the model in #8 differs from that in #5, did any of the main takeaways change for this new model? Why did this occur?

Table 8. 
```{r}
gl_m1 = glance(m1)
(glm1 = select(gl_m1, r.squared:p.value))
```

Table 9. 
```{r}
gl_m2 = glance(m2)
(glm2 = select(gl_m2, r.squared:p.value))
```

To support my answer to this question, I will focus mainly on the changes in the model's adjusted R-squared and RSE, expressions of the estimation accuracy.
After adding the variables of years of education and contraceptive use, both centered on the mean, the adjusted r-squared (I will use this one because of the number of variables added, although there is not much difference with respect to the coefficient of multiple determination) rose from .661 to .728, which is equivalent to saying that the inclusion both variables help explain approximately 19.8% more of the variation in the fertility rate across countries than the previous model ([.339 - .272] / .339 = .198).
On the other hand, the RSE of the model decreased from .807 to .723, which means that the average distance between the observed and predicted values is smaller.
In the context of this exercise, this evidence means that the fertility rate between countries is closely linked to the economic, educational and health status of the inhabitants. One could argue that women with more education and access to contraceptive methods tend to have fewer children than women living in countries where these indicators are lower. Also, economic resources per capita remain a significant characteristic in trying to explain the behavior of fertility rates even after controlling for education and contraceptive use among women. 
In general terms, both models provide similar evidence. In the second model, the coefficients of GNI decreased somewhat as the variables introduced captured some of the variation in fertility rates, however, they continued to express a decline in fertility rates compared to the Low gni category.

9. Attempt to create a figure that captures/summarizes the key takeaway from the statistical results/conclusions from #8. Why did you choose this figure to highlight the statistical results/conclusions? That is, what makes the figure you chose effective in showing the statistical results?

Since the relationships in a multiple regression model are more like a plane than a straight line, it is difficult to choose a graphical representation that accurately expresses the results of the model (m2).
For this reason, I have chosen a correlation plot which, although it expresses rather interaction, demonstrates one of the main ideas I wanted to explore in this exercise. The graph shows the differences between the different GNI levels in the relationship between female education and fertility rates.

Figure 8.
```{r}
gf_point(fertility_rate ~ educ_fem_mc, data = dat, color = ~gni_class, size = 4) %>%
  gf_smooth(method = 'lm', size = 1)
```

In the graph we can see how, in general, fertility rates between countries decrease when women have more years of education, especially if we compare between those countries with low per capita income and those with the highest per capita income.
This visualization quickly provides a clear idea: the decision to have children (fertility rate) is not only a fact linked to the age or cycle of women, but also depends on the economic and educational conditions of the countries. As expected, having children is not only a biological issue, but also a matter of social factors.