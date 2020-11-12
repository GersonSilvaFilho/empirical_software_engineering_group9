library(tidyverse)
library(car)
library(knitr)
library(rmarkdown)
library(psych)


d1 <- c(229,186,396,233,238,158,259,317,222,375,156,108,197,227,379,234)
# a
description <- data.frame(Mean=mean(d1),SD=sd(d1),Variance=var(d1),Median=median(d1))


mean(d1)
sd(d1)
var(d1)
median(d1)
range(d1)
# b
# sample of all features chosen at random

# c
#h0: mean(d1)<=225h, h1: mean(d1)>225h
#One tail


#d
# Yes it changes, hist50 skewed to the left, hist100 skewed to the right


dbin<-data.frame(d1)
ggplot(data = dbin, aes(x=d1))+
  geom_histogram(binwidth = 50)+
  labs(title='Histogram of a normal distribution', x='X', y='Number of occurences')

ggplot(data = dbin, aes(x=d1))+
  geom_histogram(binwidth = 75)+
  labs(title='Histogram of a normal distribution', x='X', y='Number of occurences')

ggplot(data = dbin, aes(x=d1))+
  geom_histogram(binwidth = 100)+
  labs(title='Histogram of a normal distribution', x='X', y='Number of occurences')


#e
car::qqPlot(d1)

#f
shapiro.test(d1)

# No idependence of residuals from QQ plot, curved 95% lines, skewed data from histogram
# Shapiro wilks test, comment on this later


#g
# Sample size > 30 and unknown SD of the population


t.test(d1, mu=225 ,conf.level=0.95)





## Performance
df2 <- read.csv(file = 'performance.csv',sep = ",")
kable(df2)
df2 <- pivot_longer(df2, cols=everything(), names_to="Group", values_to="Time")
kable(df2)
# a
psych::describeBy(df2$Time,df2$Group)


# b
# Group converted from char to factors, time numeric
str(df2)

df2$Group <- as.factor(df2$Group)
df2$Time <- as.numeric(df2$Time)

str(df2)

#c

lm1 <- lm(Time ~ Group,df2)

# Intercept 16.005
# b 0.01
# X optimized = 0, original = 1

#d

# p < a , since p is greater than a, the factor group is not statistically significant
# We can't assume that timeOptimized is better than timeOriginal
t.test(formula = Time~Group,
       data=df2, var.equal=T)







render('lab1_report_group9.Rmd')

