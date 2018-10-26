################################################################################
# MVS Assignment 2
################################################################################

# Links
# http://www.understandingdata.net/2017/03/22/cfa-in-lavaan/
# https://stats.stackexchange.com/questions/140909/how-do-i-interpret-lavaan-output

# Initial Setup and Dependencies -----------------------------------------------
rm(list=ls())
setwd("C:/Users/johnv/OneDrive/01_Uni/00_Master Studies/3. Semester (KU Leuven)/Multivariate Statistics/Assignment 2")
library(lavaan)
library(semPlot)
library(tidyverse)
library(lessR)

# GFI function
GFI <- function(Si, Sobs){
  if( class(Si) == "list"){
    Si <- as.matrix(as.data.frame(Si$cov))
  }
  
  nominator <- sum(diag(solve(Si) %*% Sobs - diag(ncol(Sobs))))^2
  denominator <- sum(diag(solve(Si) %*% Sobs))^2
  return(1 - (nominator / denominator))
}

# Data Loading and Preparation -------------------------------------------------
basketdf <- read.table("http://feb.kuleuven.be/martina.vandebroek/public/STATdata/basketball.txt", header=T)
basket_cov <- basketdf %>% filter(X_TYPE_ == "COV") %>% select(2:ncol(basketdf))
rownames(basket_cov) <- colnames(basket_cov)
basket_cov <- as.matrix(basket_cov)


# Task 1 -----------------------------------------------------------------------
# A ----------------------------------------------------------------------------
# Specify two-factor model
model1 <- '
# latent variables
F1 =~ X1 + X2 + X3 + X4
F2 =~ X5 + X6 + X7 + X8
'
fit1 <- lavaan::sem(model1, sample.nobs = 320, sample.cov = basket_cov, orthogonal = F)
inspect(fit1)
inspect(fit1,"est")

# Analysis of model fit
summary(fit1,fit.measures=TRUE)
GFI(Si = fitted(fit1), Sobs = basket_cov)
# GFI > 0.95 --> good
# Baseline model and specified model p-value < 5% --> bad
# Comparative Fit index 0.892 < 0.95 --> bad
# --> overall bad model fit

resid(fit1, type = "standardized")
# Largest standardized residuals > 1.96
# X4, X3: 6.089
# X7, X6: -5.744
# X8, X1: 3.842
# X3, X1: -3.381
# X2, X1: 3.282
# X8, X6: 3.243
# X6, X5: 2.924
# X8, X2: 2.795
# X7, X3: -2.563

modindices(fit1)
# mi: Modification indices (LM stat in SAS)
# epc: Expected parameter change (Parm Change in SAS)
# sepc.lv: Standardized value of epc based on variances of latent variables
# sepc.all: Standardized value of epc based on variances of latent and observed variables

# If covariance between X3 and X4 would be estimated, then reduction in (n-1)F
# of 43.828

# Plot using standardized loading estimates
semPaths(fit1, "std",edge.label.cex = 2.0, cex=1.5, curvePivot = TRUE)

# B ----------------------------------------------------------------------------

model2 <- '
# latent variables
F1 =~ X1 + X2
F2 =~ X3 + X4
F3 =~ X5 + X6 + X7 + X8
'

fit2 <- lavaan::sem(model2, sample.nobs = 320, sample.cov = basket_cov, orthogonal = F)

# Analysis of model fit
summary(fit2,fit.measures=TRUE)
GFI(Si = fitted(fit2), Sobs = basket_cov)
# GFI > 0.95 --> good
# Baseline model and specified model p-value < 5% --> bad
# Comparative Fit index 0.975 > 0.95 --> good 
# AIC: 2100.703 < 2156.084 --> model2 better
# BIC: 2172.301 < 2220.145 --> model2 better

resid(fit2, type = "standardized")
# Largest standardized residuals > 1.96
# X7, X6: -5.924
# X8, X1: 3.867
# X8, X6: 3.280
# X6, X5: 2.952
# X7, X1: -2.386
# X8, X2: 2.249
# X8, X3: 2.218

modindices(fit2)
# Maybe introduce forth factor containing X6, X7


# Plot using standardized loading estimates
semPaths(fit2, "std",edge.label.cex = 2.0, cex=1.5, curvePivot = TRUE)

# C ----------------------------------------------------------------------------
# parameterEstimates() only outputs unstandardized estimates
# In order to compare loadings, standardization has to be performed
# Use standardizedSolution()
standardizedSolution(fit2)

# Test whether loadings of X7 and X8 are the same (t-Test)
estX7X8 <- standardizedSolution(fit2)$est.std[7:8] # estimates
seX7X8 <- standardizedSolution(fit2)$se[7:8] # standard errors
# H0: X7 = X8
# H1: X7 != X8
alpha <- 0.05
((estX7X8[1] - estX7X8[2]) / seX7X8[1]) > qt(p=1-(alpha/2), df=320-19)
# ODER
((estX7X8[1] - estX7X8[2]) / seX7X8[1]) < -qt(p=1-(alpha/2), df=320-19)
# RH0 --> Loadings are significantly different from each other

# Task 2 -----------------------------------------------------------------------

advbasket_cov <- read.table("https://feb.kuleuven.be/public/u0004359/STATdata/advanced%20basketball.txt", header=T)
advbasket_cov <- advbasket_cov %>% filter(X_TYPE_ == "COV") %>% select(2:ncol(advbasket_cov))
rownames(advbasket_cov) <- colnames(advbasket_cov)
advbasket_cov <- as.matrix(advbasket_cov)

# Specify SEM
model3 <- '
Y1 ~ X2 + X1 + Y2
Y2 ~ X1 + X3
X1 ~~ X3
'
fit3 <- lavaan::sem(model3,sample.nob = 320,sample.cov = advbasket_cov, fixed.x = F)

# Analyze model fit
summary(fit3, fit.measures = T)
Si1 <- as.matrix(as.data.frame(fitted(fit3)$cov))
Si1 <- corReorder(R = Si1, vars = c(X1, X2, X3, Y1, Y2), heat.map = F)
GFI(Si = Si1, Sobs = advbasket_cov)
# GFI > 0.95 --> good
# Baseline model and specified model p-value < 5% --> bad
# Comparative Fit index 0.977 > 0.95 --> good
# All coefficients significant
# AIC: 3515.273
# BIC: 3556.725

modindices(fit3)
semPaths(fit3, "std",edge.label.cex = 2.0, cex=1.5, curvePivot = TRUE)

# Option 1
model4 <- '
Y1 ~ X2 + X1 + Y2
Y2 ~ X1 + X2 + X3
X1 ~~ X3
X2 ~~ X3
X1 ~~ X2
'
fit4 <- lavaan::sem(model4,sample.nob = 320,sample.cov = advbasket_cov, fixed.x = F)
summary(fit4, fit.measures = T)
Si2 <- as.matrix(as.data.frame(fitted(fit4)$cov))
Si2 <- corReorder(R = Si2, vars = c(X1, X2, X3, Y1, Y2), heat.map = F)
GFI(Si = Si2, Sobs = advbasket_cov)
# GFI > 0.95 --> good
# Baseline model and specified model p-value < 5% --> bad
# Comparative Fit index 0.998 > 0.95 --> good
# All coefficients significant
# AIC: 3466.969 < 3515.273 --> better

modindices(fit4)
semPaths(fit4, "std",edge.label.cex = 2.0, cex=1.5, curvePivot = TRUE)

# Option 2????


