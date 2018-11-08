# Survival Analysis 
# Homework 2 - Blue Team 5 Fall 3 
# Created by Roger Dugas on Nov.7, 2018

#------------------------------------------IMPORT DATA SET/ INSTALL PACKAGES----------------------------------------------

# Read in the Katrina Data Set into R
katrina <- read.csv('/Users/rogerdugas/Desktop/MSA NC State/1.2 Fall 3/Survival Analysis/2017SA Data/survivalcsv/katrina.csv')

# Install the required packages
install.packages('survminer')
install.packages('muhaz')
library(flexsurv)
library(survival)
library(survminer)
library(muhaz)
library(ggplot2)
library(reshape2)
library(dplyr)


# HOMEWORK ASSIGNMENT:
# Provide a follow-up to your last report and a set of recommendations summarizing the findings from your analysis.
# For this analysis, you will only focus on one type of failure—flood, so you will use reason instead of survive as your
# status variable, treating everything that didn’t fail due to flooding as censored.1

# In R, use Surv(time = hour, event = reason == 1). This new report should include the following information:
#  • Create an AFT model with the following variables: backup, bridgecrane, servo, trashrack, elevation,
#    slope, age. Don’t worry about interactions for now.
#  • Fit this model with the exponential, Weibull, log-normal, and log-logistic distributions and discuss the possible
#    distribution of the data.
#  • Once you have chosen a distribution, provide the coefficient estimates and standard errors from your model
#    and comment on anything that you find interesting. You must interpret a few of them (at least one categorical and one continuous).
#  • The Army Corps of Engineers only has enough time and money to upgrade 20 pumps. Choose an upgrade for
#    20 pumps (one upgrade per pump); you can decide this however you’d like, but explain why you’ve recom-
#    mended those 20. You don’t have to give one explanation for every individual pump, just state any overarching
#    reasons for upgrading the particular set of pumps that you chose.

#---------------------------------------------VARIABLE MANUPULATION-------------------------------------------------

# How many pumps are of reason = 1, or flooding
sum(katrina$reason == 1) # 115 pumps failed due to flooding out of 770 pumps

# Quick check of the summary of the data set
summary(katrina)
str(katrina)

# Create data set with Failure-Flood as the survived variable and 
# treating everything that didn’t fail due to flooding as censored
flood <- katrina
flood$survive1 <- ifelse(katrina$reason == 1 , 1, 0)

#-----------------------------------------AFT MODEL/FITTING DISTRIBUTIONS-------------------------------------------

#######Weibull Distribution##########
# The syntax in flexsurvreg() is the same as survreg() weibull distribution
fit_wb <- flexsurvreg(Surv(hour, event = survive1 == 1) ~ backup + bridgecrane + servo + trashrack + elevation +
                        slope + age, data = flood, dist = "weibull")

# Now plot cumulative hazard along with KM estimates using plot(..., type = "cumhaz")
# we hope that the curve and CI are pretty close to the KM estimates
# the "ci" option is the CI of your fitted distribution, the "conf.int" option
# is for the CI of the KM estimate
plot(fit_wb, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
     xlab = "Time (hrs)", ylab = "Cumulative Hazard", main = "Weibull Distribution", col="blue")
axis(1, seq(0,50,10), font=1)
# Plot the survival curve for all pumps
plot(fit_wb, xlab = "Time", ylab = "Survival Probability")
# Better looking plot
ggsurvplot(fit_wb, data = flood, conf.int = TRUE, palette = "blue")

fit_wb # AIC = 1470.381

#######Exponential Distribution##########
fit_exp <- flexsurvreg(Surv(hour, event = survive1 == 1) ~ backup + bridgecrane + servo + trashrack + elevation +
                         slope + age, data = flood, dist = "exponential")
# plot shows that this distribution isn't a good fit
plot(fit_exp, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n", 
     xlab = "Time (hrs)", ylab = "Cumulative Hazard",
     main = "Exponential Distribution", col = "blue")
axis(1, seq(0,50,10), font=1)

#########Lognormal##########
fit_lnorm <- flexsurvreg(Surv(hour, event = survive1 == 1) ~ backup + bridgecrane + servo + trashrack + elevation +
                           slope + age, data = flood, dist = "lognormal")
# plot shows lack of fit in very early time periods
plot(fit_lnorm, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
     xlab = "Time (hrs)", ylab = "Cumulative Hazard", main = "Lognormal Distribution", col = "blue")
axis(1, seq(0,50,10), font=1)

fit_lnorm # AIC = 1478.353

#########Log-logistic#########
fit_llogis <- flexsurvreg(Surv(hour, event = survive1 == 1) ~ backup + bridgecrane + servo + trashrack + elevation +
                            slope + age, data = flood, dist = "llogis")
plot(fit_llogis, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
     xlab = "Time (hrs)", ylab = "Cumulative Hazard",
     main = "Log-logistic Distribution", col = "Blue")
axis(1, seq(0,50,10), font=1)

# From looking at the different distributions we beleive that the Weibull Distribution fits our data best

#-----------------------------------------WEIBULL DIST COEFFICIENTS/INTERPRETATIONs----------------------------------------

# Fit AFT model using survreg()
# Like glm(), there's a "dist" argument (default is weibull)
fit1 <- survreg(Surv(hour, event = survive1 == 1) ~ backup + bridgecrane + servo + trashrack + elevation +
                  slope + age, data = flood, dist = "weibull")
# Look at the coefficient estimates for the AFT model
summary(fit1)
exp(coef(fit1))

summary(flood)
# Categorical Variable Interpretations
# 1. For the pumps that have a backup system, the predicted failure (flooding) time was 1.28 times longer than those 
# that did not have a backup system (all else equal)
# 2. For the pumps that have a servomechanism, the predcited failure (flooding) time was 1.39 times longer than those 
# who didn't not have one (all else equal)

# Continuous Variable Interpretations
# 1. The predicted time to pump failure (flooding) is .94 times shorter for every additional 
# slope increase 
# 2. The predicted time to pump failure (flooding) is 1.05 times longer for every additional 
# elevation increase 

# We should either upgrade the backup systems or servomechanism for the 20 pumps. Maybe to 10 backups 
# and 10 servos

#----------------------------------------PREDICTIONS---------------------------------------------------

# Predicted quantiles
# Predict() returns a list with the type (called "fit") and se (called "se.fit")
survprob_75_50_25 <- predict(fit1, type = "quantile", se.fit = TRUE,
                             p = c(0.25, 0.5, 0.75))
# note that these are the quantiles for EVENT times, not survival times
# so 0.25 means this is the time where events have happened to 25% of people,
# so S(t) = 0.75 at whatever this time is
head(survprob_75_50_25$fit)

# or you can predict survival probabilities at some time of interest
# let's look at the 10-hr survival probabilities
# I'll still use psurvreg(), but my "time" for everyone is just 10 instead of
# their actual week
survprob_10hr <- 1 - psurvreg(10, mean = predict(fit1, type = "lp"), 
                              scale = fit1$scale,
                              distribution = fit1$dist)
head(survprob_10hr)

# so now for the people who didn't get financial aid, we're going to predict
# the mean time to recidivism if they HAD gotten it
# to do this, we're assuming that they'll still have the event at the same
# estimated survival probability as they did previously
floor_nofin <- floor %>%
  # first, we need to get the linear predictor from the old model
  mutate(old_lp = predict(fit1, type = "lp"),
         # add ID variable so we know which subjects they are
         ID = row_number()) %>%
  # next, we're only interested in those who had the event AND no financial aid
  dplyr::filter(reason == 1, servo == 0) %>%
  # now i'm doing two things:
  # 1. find the survival prob at the time of their event
  # 2. pretending they did have financial aid (change fin from 0 to 1)
  mutate(old_time = hour,
         surv_prob = 1 - psurvreg(old_time,
                                  mean = old_lp,
                                  scale = fit1$scale,
                                  distribution = fit1$dist),
         old_fin = fin,
         fin = old_fin + 1)

# now with that dataset, i need to find their new time
results1 <- floor_nofin %>%
  # estimate their new linear predictor value if they had financial aid
  mutate(new_lp = predict(fit1, newdata = floor_nofin, type = "lp"),
         # now, keeping the same survival probability as before, get the time
         # corresponding to the new_lp value
         new_time = qsurvreg(1 - surv_prob,
                             mean = new_lp,
                             scale = fit1$scale,
                             distribution = fit1$dist),
         # and compute the difference
         pred_time_diff = new_time - old_time) %>%
  select(ID, surv_prob, old_time, new_time, pred_time_diff)
head(results1)




