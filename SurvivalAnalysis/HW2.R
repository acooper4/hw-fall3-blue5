# Survival Analysis 
# Homework 2 - Blue Team 5 Fall 3 
# Created by Roger Dugas on Oct. 31, 2018 

#------------------------------------------IMPORT DATA SET/ INSTALL PACKAGES----------------------------------------------

# Read in the Katrina Data Set into R
setwd('C:\\Users\\Wbr\\Desktop\\NCSU Backup 2018-10-09\\Analytics\\AA502\\Survival Analysis\\Code\\survivalcsv')
katrina <- read.csv('katrina.csv')

# Install the required packages
#install.packages('survminer')
#install.packages('muhaz')
library(survival)
library(survminer)
library(muhaz)
library(ggplot2)
library(reshape2)
library(dplyr)
library(flexsurv)

# HOMEWORK ASSIGNMENT:
# Provide a follow-up to your last report and a set of recommendations summarizing the findings from your analysis.
# For this analysis, you will only focus on one type of failure-flood, so you will use reason instead of survive as your
# status variable, treating everything that didn't fail due to flooding as censored.1 In SAS, you can specify multiple
# censoring codes by separating them with commas: hour*reason(0, 2, 3, 4). In R, use Surv(time = hour, event
#                                                                                         = reason == 1). This new report should include the following information:
#   . Create an AFT model with the following variables: backup, bridgecrane, servo, trashrack, elevation,
# slope, age. Don't worry about interactions for now.
# . Fit this model with the exponential, Weibull, log-normal, and log-logistic distributions and discuss the possible
# distribution of the data.
# . Once you have chosen a distribution, provide the coefficient estimates and standard errors from your model
# and comment on anything that you find interesting. You must interpret a few of them (at least one categorical
#                                                                                      and one continuous).
# . The Army Corps of Engineers only has enough time and money to upgrade 20 pumps. Choose an upgrade for
# 20 pumps (one upgrade per pump); you can decide this however you'd like, but explain why you've recommended
# those 20. You don't have to give one explanation for every individual pump, just state any overarching
# reasons for upgrading the particular set of pumps that you chose.

#---------------------------------------------DESCRIPTIVE STATISTICS--------------------------------------------

# Quick check of the summary of the data set
summary(katrina)
str(katrina)

# Create string variable to assign to the numberic values of "reason"  
reason3 <- factor(katrina$reason, levels = c(0, 1, 2, 3, 4), 
       labels = c("Survived", "Flood", "Motor", "Surge", 
                  "Jammed"))

# add new variable to data frame 
df_pump <- cbind(katrina, reason3)

# Calculate the mean survival time for each type of failure
mean(subset(df_pump, reason3 == "Survived")$hour)
mean(subset(df_pump, reason3 == "Flood")$hour)
mean(subset(df_pump, reason3 == "Motor")$hour)
mean(subset(df_pump, reason3 == "Surge")$hour)
mean(subset(df_pump, reason3 == "Jammed")$hour)


# calculate the median survival time for each type of failure
median(subset(df_pump, reason3 == "Survived")$hour)
median(subset(df_pump, reason3 == "Flood")$hour)
median(subset(df_pump, reason3 == "Motor")$hour)
median(subset(df_pump, reason3 == "Surge")$hour)
median(subset(df_pump, reason3 == "Jammed")$hour)



#-------------------------------------------------PLOT SURVIVAL CURVES--------------------------------------------
# Do this for:  exponential, Weibull, log-normal, and log-logistic distributions


# Create survival curves for all pumps, survfit() takes a formula just like usual
# ~ predictor + ... + predictor to fit model with set of  predictors

###############################################
# exponential Distribution
###############################################

pump_fit_exp <- flexsurvreg(Surv(time = hour, event = reason3 == "Flood") ~ backup+bridgecrane+servo+trashrack+elevation+slope+age,
                        data = katrina, dist = "exp")

pump_fit_exp

# plot cumulative hazard along with KM estimates 
# Include confidence internval of fitted distribution (ci = True)

plot(pump_fit_exp, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n", xmax = 20,
     xlab = "week", ylab = "cumulative hazard", main = "Exponential Distribution")

plot(pump_fit_exp, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
     xlab = "week", ylab = "cumulative hazard", main = "Exponential Distribution")

# Plot the survival curve for all pumps
plot(pump_fit_exp, xlab = "Time", ylab = "Survival Probability")
# Better looking plot 
ggsurvplot(pump_fit_exp, data = katrina, conf.int = TRUE, palette = "blue")

###############################################
# Weibul Distribution
###############################################

pump_fit_weibull <- flexsurvreg(Surv(time = hour, event = reason3 == "Flood") ~ backup+bridgecrane+servo+trashrack+elevation+slope+age,
                        data = katrina, dist = "weibull")

pump_fit_weibull

# plot cumulative hazard along with KM estimates 
# Include confidence internval of fitted distribution (ci = True)

plot(pump_fit_weibull, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n", xmax = 20,
     xlab = "week", ylab = "cumulative hazard", main = "Weibull Distribution")

plot(pump_fit_weibull, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
     xlab = "week", ylab = "cumulative hazard", main = "Weibull Distribution")

# Plot the survival curve for all pumps
plot(pump_fit_weibull, xlab = "Time", ylab = "Survival Probability")
# Better looking plot 
ggsurvplot(pump_fit_weibull, data = katrina, conf.int = TRUE, palette = "blue")

###############################################
#log-normal
###############################################

pump_fit_lnorm <- flexsurvreg(Surv(time = hour, event = reason3 == "Flood") ~ backup+bridgecrane+servo+trashrack+elevation+slope+age,
                        data = katrina, dist = "lnorm")

pump_fit_lnorm

# plot cumulative hazard along with KM estimates 
# Include confidence internval of fitted distribution (ci = True)

plot(pump_fit_lnorm, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n", xmax = 20,
     xlab = "week", ylab = "cumulative hazard", main = "Log-normal Distribution")

plot(pump_fit_lnorm, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
     xlab = "week", ylab = "cumulative hazard", main = "Log-normal Distribution")


# Plot the survival curve for all pumps
plot(pump_fit_lnorm, xlab = "Time", ylab = "Survival Probability")
# Better looking plot 
ggsurvplot(pump_fit_lnorm, data = katrina, conf.int = TRUE, palette = "blue")

###############################################
#log-logistic
###############################################

pump_fit_llogistic <- flexsurvreg(Surv(time = hour, event = reason3 == "Flood") ~ backup+bridgecrane+servo+trashrack+elevation+slope+age,
                              data = katrina, dist = "llogis")

pump_fit_llogistic

# plot cumulative hazard along with KM estimates 
# Include confidence internval of fitted distribution (ci = True)


plot(pump_fit_llogistic, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",xmax = 20,
     xlab = "week", ylab = "cumulative hazard", main = "Log-logistic Distribution")

plot(pump_fit_llogistic, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
     xlab = "week", ylab = "cumulative hazard", main = "Log-logistic Distribution")

# Plot the survival curve for all pumps
plot(pump_fit_llogistic, xlab = "Time", ylab = "Survival Probability")
# Better looking plot 
ggsurvplot(pump_fit_llogistic, data = katrina, conf.int = TRUE, palette = "blue")

############################################################################################
# I choose Weibull
# Best graphs
# Lowest AIC



pump_fit_weibull

