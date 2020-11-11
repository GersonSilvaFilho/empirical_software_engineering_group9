library(tidyverse)
library(car)
library(knitr)


d2 <- read.csv(file = 'performance.csv',sep = ",")
d1 <- c(229,186,396,233,238,158,259,317,222,375,156,108,197,227,379,234)
# a
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


t.test(d1, mu=225, conf.level=0.95)





