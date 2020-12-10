library(tidyverse)
library(car)
library(knitr)
library(pwr)
library(psych)
library(rmarkdown)
library(zoom)
library(stargazer)
N<-1000

amount_of_code_model <- function(N, X_lang, X_ide, X_experience) { 
  
  ref <- 100 #Intercept/reference group. This is java, intelij and junior

  #main effects
  lang_cpp <- 50
  lang_python <- -30
  
  ide_visual_studio <- -10
  
  xp_senior <- 30
  
  # second order interactions
  lang_cpp_ide_visual_studio <- 20  
  lang_python_ide_visual_studio <- -20  
  lang_python_xp_senior <- 10 
  lang_cpp_xp_senior <- 10
  ide_visual_studio_xp_senior <- 20

  
  #third order interactions
  lang_cpp_xp_senior_ide_visual_studio <- 10
  lang_python_xp_senior_ide_visual_studio <- -15 

  #input
  x_lang_cpp <- X_lang[1]
  x_lang_python <- X_lang[2]
  
  x_ide_visual_studio <- X_ide[1] 
  
  x_senior <- X_experience[1]

  response_std <- 20

  #This is the linear model that controls the response variable

  y <- ref+ lang_cpp*x_lang_cpp+lang_python*x_lang_python +
     ide_visual_studio  * x_ide_visual_studio +
     xp_senior * x_senior +
    lang_cpp_ide_visual_studio * x_lang_cpp * x_ide_visual_studio +
    lang_python_ide_visual_studio * x_lang_python *x_ide_visual_studio +
    lang_python_xp_senior * x_lang_python * x_senior +
    lang_cpp_xp_senior * x_lang_cpp * x_senior +
    ide_visual_studio_xp_senior * x_senior * x_ide_visual_studio +

    lang_cpp_xp_senior_ide_visual_studio * x_lang_cpp * x_ide_visual_studio * x_senior +
    lang_python_xp_senior_ide_visual_studio * x_lang_python * x_ide_visual_studio * x_senior

    y_out<- rnorm(N, mean=y, sd = response_std) # This generates a normal distribution 
  
  # if(likert)
  #  y_out<- findInterval(y_out,vec=c(-Inf,-2.5,-1, 1,2.5,Inf)+ref) 
  
  # this is fairly unbiased for standard deviation of 1 in the responses and 0 mean. 
  # That is why we add the value of the reference group.
  # Here the reference will have a mean of 3 in likert scale
  return(y_out)
}


set.seed(8649)

language_java <- c(0,0) 
language_cpp <- c(1,0) 
language_python <- c(0,1)

i_itelij <- c(0)
i_visual_studio <- c(1)

experience_junior <- c(0) 
experience_senior <- c(1)

g1 <- data.frame(
  Language= rep('Java', N), IDE= rep('Intelij', N), Experience=rep('Junior', N), 
  y=amount_of_code_model(N=N, X_lang =language_java , X_ide = i_itelij, X_experience = experience_junior)
)

g2 <- data.frame(
  Language= rep('C++', N), IDE= rep('Intelij', N), Experience=rep('Junior', N), 
  y=amount_of_code_model(N=N, X_lang =language_cpp , X_ide = i_itelij, X_experience = experience_junior)
)

g3 <- data.frame(
  Language= rep('Python', N), IDE= rep('Intelij', N), Experience=rep('Junior', N), 
  y=amount_of_code_model(N=N, X_lang =language_python , X_ide = i_itelij, X_experience = experience_junior)
)

g4 <- data.frame(
  Language= rep('Java', N), IDE= rep('Visual Studio', N), Experience=rep('Junior', N), 
  y=amount_of_code_model(N=N, X_lang =language_java , X_ide = i_visual_studio, X_experience = experience_junior)
)

g5 <- data.frame(
  Language= rep('C++', N), IDE= rep('Visual Studio', N), Experience=rep('Junior', N), 
  y=amount_of_code_model(N=N, X_lang =language_cpp , X_ide = i_visual_studio, X_experience = experience_junior)
)

g6 <- data.frame(
  Language= rep('Python', N), IDE= rep('Visual Studio', N), Experience=rep('Junior', N), 
  y=amount_of_code_model(N=N, X_lang =language_python , X_ide = i_visual_studio, X_experience = experience_junior)
)

g7 <- data.frame(
  Language= rep('Java', N), IDE= rep('Intelij', N), Experience=rep('Senior', N), 
  y=amount_of_code_model(N=N, X_lang =language_java , X_ide = i_itelij, X_experience = experience_senior)
)

g8 <- data.frame(
  Language= rep('C++', N), IDE= rep('Intelij', N), Experience=rep('Senior', N), 
  y=amount_of_code_model(N=N, X_lang =language_cpp , X_ide = i_itelij, X_experience = experience_senior)
)

g9 <- data.frame(
  Language= rep('Python', N), IDE= rep('Intelij', N), Experience=rep('Senior', N), 
  y=amount_of_code_model(N=N, X_lang =language_python , X_ide = i_itelij, X_experience = experience_senior)
)

g10 <- data.frame(
  Language= rep('Java', N), IDE= rep('Visual Studio', N), Experience=rep('Senior', N), 
  y=amount_of_code_model(N=N, X_lang =language_java , X_ide = i_visual_studio, X_experience = experience_senior)
)

g11 <- data.frame(
  Language= rep('C++', N), IDE= rep('Visual Studio', N), Experience=rep('Senior', N), 
  y=amount_of_code_model(N=N, X_lang =language_cpp , X_ide = i_visual_studio, X_experience = experience_senior)
)

g12 <- data.frame(
  Language= rep('Python', N), IDE= rep('Visual Studio', N), Experience=rep('Senior', N), 
  y=amount_of_code_model(N=N, X_lang =language_python , X_ide = i_visual_studio, X_experience = experience_senior)
)

n<-8
all_groups <- list(g1, g2, g3, g4, g5, g6, g7, g8, g9, g10, g11, g12)

#creating an empty data frame from an existing one (with the same column)
d<-g1[0,]
for(g in all_groups) {
  d<-rbind(d, dplyr::sample_n(g, size=n)) #appending rows at the end for every group 
}

d$Language<-as.factor(d$Language) 
d$IDE<-as.factor(d$IDE) 
d$Experience<-as.factor(d$Experience)
d <- within(d, Language <- relevel(Language,ref=2))
levels(d$Language) <- c("Java","C++", "Python")
levels(d$IDE) <- c("Intelij", "Visual Studio")
levels(d$Experience) <- c("Junior", "Senior")

m1 <- lm(y ~ Language*IDE*Experience, data=d)

summary(m1)

# 1 
# a)
# Test Assumption 1 Homoscedasticity. The result show that we cannot reject homoscedasticity.
# QQplot shows that the data is homoscedastic.
car::leveneTest(m1)
car::qqPlot(m1)
# Test Assumtion 2 Normality. The result show that we cannot reject normallity.
# QQplot on the residuals shows normallity.
shapiro.test(m1$residuals)
car::qqPlot(m1$residuals)
# Assumption 3 Independence. Based on how we collected the data we can assume independence.

# The Analysis

# The anova test shows that we can reject that Language, Experience, Language:IDE, Language:Experience and
# IDE:Experience has the same mean for the levels because of the low p values.
# Hypothesis 1: IDE alone doesn't seem to have an effect on LOC but we can see that it has a
# significant effect in combination with Language and Experience.
# Hypothesis 2: Experience seem to have a significant effect on LOC based on the result.
# Hypothesis 3: Language seem to have a significant effect on LOC bases on the result.
car::Anova(m1)

# Without drawing conclusions for all combinations presented by the Tukey test we can say that
# the highest combination for LOC is Language = C++, IDE = Visual Studio and Experience = Senior.
TukeyHSD(aov(m1))


#b
# We are comparing the model we created (y) in the function with m1.
# Model looks similar with some variations in the estimates. This could
# be explained by the standard deviation we decided upon or that m1 interpreted
# the data in another way than what we set up. The difference in the first order
# is very small compared to our true model, however the third order interactions are
# deviating quite a bit compared to our true model. Maybe two tables side by side for
# comparison.


#2
# a)
n2 <- 4
d2<-g1[0,]
for(g in all_groups) {
  d2<-rbind(d2, dplyr::sample_n(g, size=n2)) #appending rows at the end for every group 
}

d2$Language<-as.factor(d2$Language) 
d2$IDE<-as.factor(d2$IDE) 
d2$Experience<-as.factor(d2$Experience)
d2 <- within(d2, Language <- relevel(Language,ref=2))
levels(d2$Language) <- c("Java","C++", "Python")
levels(d2$IDE) <- c("Intelij", "Visual Studio")
levels(d2$Experience) <- c("Junior", "Senior")

m2 <- lm(y ~ Language*IDE*Experience, data=d2)
summary(m2)
# Test Assumption 1 Homoscedasticity. The result show that we cannot reject homoscedasticity.
# QQplot shows that the data is homoscedastic.
car::leveneTest(m2)
car::qqPlot(m2)

# Test Assumtion 2 Normality. The result show that we cannot reject normallity.
# QQplot on the residuals shows normallity.
shapiro.test(m2$residuals)
car::qqPlot(m2$residuals)
# Assumption 3 Independence. Based on how we collected the data we can assume independence.


# The Analysis

# Running the anova test on m2 (smaller sample size model) shows that we can only reject that
# the means are equal for Language and Experience.
# Hypothesis 1: IDE doesn't seem to have an effect on LOC neither alone nor as an interaction.
# Hypothesis 2: Experience seem to have a significant effect on LOC based on the result.
# Hypothesis 3: Language seem to have a significant effect on LOC bases on the result.
car::Anova(m2)


# Tukey test shows that the highest combination for LOC is Language = C++ and Experience = Senior.
TukeyHSD(aov(m2))



#b
# We are comparing the model we created (y) in the function with m2.
# Model looks similar with some variations in the estimates. This could
# be explained by the standard deviation we decided upon or that m1 interpreted
# the data in another way than what we set up. The difference in the first order
# is very small compared to our true model, however the third order interactions are
# deviating quite a bit compared to our true model. Maybe two tables side by side for
# comparison.

#c
# m1 vs m2, When we compare the two models we see some differences between them.
# These could be explained by the standard deviation and the small sample size.
# We also get more coefficients with small p values that are to be considered 
# statistically significant for m1.

# Comparing the analysis on the two models show similar result for Hypothesis 2 and 3 but
# for model two we do not see an interaction effect for IDE. This means we can't reject the null
# hypothesis for hypothesis 1 that the means are equal for the different IDEs.



