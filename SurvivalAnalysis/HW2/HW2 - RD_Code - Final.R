# Survival Analysis 
# Homework 2 - Blue Team 5 Fall 3 
# Created by Roger Dugas on Nov.7, 2018

#------------------------------------------IMPORT DATA SET/ INSTALL PACKAGES----------------------------------------------

# Read in the Katrina Data Set into R
katrina <- read.csv('/Users/rogerdugas/Desktop/MSA NC State/1.2 Fall 3/Survival Analysis/2017SA Data/survivalcsv/katrina.csv')

# Install the required packages
install.packages('survminer')
install.packages('survival')
install.packages('muhaz')
library(flexsurv)
library(survival)
library(survminer)
library(muhaz)
library(ggplot2)
library(reshape2)
library(dplyr)


##### HOMEWORK ASSIGNMENT:
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

# Std errors 
summary(fit1)$table[,2]

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

# We should either upgrade the backup systems or servomechanism for the 20 pumps. 

#------------------------------------------------PREDICTIONS---------------------------------------------------

# Look at the pumps that do not have a servomechanism, we're going to predict
# the mean time to flooding if they HAD gotten it to do this, we're assuming that they'll still have the 
# event at the same estimated survival probability as they did previously
flood_noservo <- flood %>%
  # first, we need to get the linear predictor from the old model
  mutate(old_lp = predict(fit1, type = "lp"),
         # add ID variable so we know which subjects they are
         ID = row_number()) %>%
  # next, we're only interested in those who had the event AND no servo
  dplyr::filter(reason == 1, servo == 0) %>%
  # now i'm doing two things:
  # 1. find the survival prob at the time of their event
  # 2. pretending they did have servo (change servo from 0 to 1)
  mutate(old_time = hour,
         surv_prob = 1 - psurvreg(old_time,
                                  mean = old_lp,
                                  scale = fit1$scale,
                                  distribution = fit1$dist),
         old_servo = servo,
         servo = old_servo + 1)

# Now with that dataset, we need to find their new time
results1 <- flood_noservo %>%
  # Estimate their new linear predictor value if they had financial aid
  mutate(new_lp = predict(fit1, newdata = flood_noservo, type = "lp"),
         # now, keeping the same survival probability as before, get the time
         # corresponding to the new_lp value
         new_time = qsurvreg(1 - surv_prob,
                             mean = new_lp,
                             scale = fit1$scale,
                             distribution = fit1$dist),
         # and compute the difference
         pred_time_diff = new_time - old_time) %>%
  select(ID, surv_prob, old_time, new_time, pred_time_diff)

# The results show that we have 63 pumps that failed due to flooding that did not have a servomechanism
results1

#-----------------------------------------SELECTING 20 SERVO PUMPS-------------------------------------------------
# Selection process for the 20 pumps that we beleive should be upgraded 
mean(results1$pred_time_diff) # The mean predicted time difference is 9.2 hours 
results1[order(results1$surv_prob),]
results1[order(results1$pred_time_diff),] 
df <- results1[results1$old_time >= 26, ]# select pumps that have a "old time" of >= 12 hours
df0 <- df[df$new_time <= 48, ] # Just look at pumps that hav a "new time" of less than 48 hours
df1 <- df0[df0$surv_prob >= .8, ] # filter pumps that have a survival prob of over 80% 
df2 <- df1[order(-df1$pred_time_diff),] # Order pumps by predicted time difference 
rownames(df2) <- NULL
df_final <- df2[-c(21:35),] # Select the 20 pumps

# Summary Statistics for 20 pumps the we are upgrading with a Servomechanism 
mean(df_final$surv_prob) # .88 avg survival probability 
mean(df_final$old_time) # average time of 27.5 hours before failing due to flooding
mean(df_final$new_time) # average time of 38 hours before failing after upgrading servo
mean(df_final$pred_time_diff) # a 10.5 hour difference 


