library(tidyverse)
library(car)
library(knitr)
library(pwr)
library(psych)
library(rmarkdown)
library(zoom)
library(stargazer)

#a
# uncertain about the effect size
pwr.anova.test(k = 5, n = NULL, f = 0.08, sig.level = 0.05, power = 0.90)

# With larger effect size the number of customers
# required shrinks.
# 2410. n 482 * 5 groups.

#b

df <- read.csv(file = 'gotaflix-abn.csv',sep = ",")
str(df)
df$Cover <- as.factor(df$Cover)
df$Engagement <- as.numeric(df$Engagement)

psych::describeBy(df$Engagement,list(df$Cover), mat=T)

#c

lm <- lm(Engagement ~ Cover,df)

# The intercept represents the A cover. It's the reference for all the other covers.
# C = 1 -> 0.160367 + 0.017948
# contrasts(df$Cover)


#d
car::qqPlot(Engagement ~ Cover,df)
car::qqPlot(lm$residuals)

shapiro.test(df$Engagement)

# W =  1, p > a, Data seems to be normal. Use this one
# qqPlots, looks normal.

#e
# We interpret the null hypothesis of the test as being if the data have homoscedasticity. 
# Since the P-value is 0.93, it is larger than alpha and the null hypothesis can't be rejected.

plot(lm)
car::leveneTest(lm)

#f
# There is no test that can be run to verify independence.
# It's part of the design of the experiment, and should
# be handled when data is collected.

#g
df2 <- read.csv(file = 'gotaflix-abn-modified.csv',sep = ",")
df2$Cover <- as.factor(df2$Cover)
df2$Engagement <- as.numeric(df2$Engagement)

lm2 <- lm(Engagement ~ Cover,df2)
plot(df2)
car::leveneTest(lm2)

# We interpret the null hypothesis of the test as being if the data have homoscedasticity. 
# Since the P-value is very small, and much smaller than alpha, the null hypothesis can be rejected
# and we can with confidence say that the data does not have homoscedasticity


#h
summary(lm)

car::Anova(lm)
# The model is statistically significant and we reject the hypothesis that the mean is equal for all groups.

tuk <- TukeyHSD(aov(lm))
plot(tuk)
# We are confident that Cover C is better than A, B and D by looking at the plot of the Tukey test.
# However, we can't say with confidence that C is better than E.


# PART 2 Full factorial experiment

# a

# There are four experimental groups assigned to each of the combinations of treatments.
# The groups consist of the randomly assigned users.

# b

df3 <- read.csv(file = 'gotaflix-2wayANOVA.csv',sep = ",")
str(df3)
df3$Engagement <- as.numeric(df3$Engagement)
df3$Cover <- as.factor(df3$Cover)
df3$Summary <- as.factor(df3$Summary)

# Intercept for lm3 and lm4 is for factors Cover = Character and Summary = Character
lm3 <- lm(Engagement ~ Cover + Summary, df3)


lm4 <- lm(Engagement ~ Cover + Summary + Cover:Summary, df3)
lm5 <- lm(Engagement ~ Cover*Summary, df3)

# c

# The samples were taken randomly so the assumption of independence holds


# The shapiro wilks test with W=0.999 with a p value of 0.3843, therefore we cannot reject the 
# null hypothesis
shapiro.test(df3$Engagement)

# The results of the levene test tells us that we can reject the null hypothesis that the data is
# homoscedastic, however with large sample sizes the test is likely to report small p values for
# very small deviations from homoscedasticity. Instead we use qqPlot on the resiudals to inspect
# homoscedasticity. Our conclusion is that we cannot reject the hypothesis that the data is homoscedastic.
car::leveneTest(lm5)
car::qqPlot(sample(lm5$residuals,100))


# d
# The statisticly significant result is that The combination of Character for both Genre and Summary
# has better engagement than all other combinations of Genre and Summary. 

anovatwoway <- car::Anova(lm5)
tuk <- TukeyHSD(aov(lm5))
plot(tuk)

# The effect size 
pwr.anova.test(k = 4, n = 800, f = NULL, sig.level = 0.05, power = 0.90)
#render('lab2_report_group9.Rmd')





















