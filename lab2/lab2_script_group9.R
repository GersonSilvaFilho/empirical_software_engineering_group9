library(tidyverse)
library(car)
library(knitr)
library(pwr)
library(psych)
library(rmarkdown)


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
plot(lm)
car::leveneTest(lm)

#f
# There is no test that can be run to verify independence.
# It's part of the design of the experiment, and should
# be handled when data is collected.







