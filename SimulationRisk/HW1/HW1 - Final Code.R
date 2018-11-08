# Survival Analysis 
# Homework 1 - Blue Team 5 Fall 3 
# Created on Oct. 31, 2018 - Updated through Nov. 5th, 2018

#------------------------------------------IMPORT DATA SET/ INSTALL PACKAGES----------------------------------------------

# Read in the Katrina Data Set into R
katrina <- read.csv('/Users/rogerdugas/Desktop/MSA NC State/1.2 Fall 3/Survival Analysis/2017SA Data/survivalcsv/katrina.csv')

# Install the required packages
install.packages('survminer')
install.packages('muhaz')
library(survival)
library(survminer)
library(muhaz)
library(ggplot2)
library(reshape2)
library(dplyr)

# HOMEWORK ASSIGNMENT:
# Pump survival is denoted in the variable survive, which is an indicator for whether 
# or not the pump survived the entirety of the storm. 
# There are five potential failure conditions which take the following values in the variable reason:

#• (0) no failure (this is equivalent to survive = 1)
#• (1) flood: overflow or accumulation of water that submerges the pump station
#• (2) motor: mechanical failure
#• (3) surge: onshore gush of water typically associated with levee or structural damage
#• (4) jammed: accumulation of trash or landslide materials

#• backup pump (upgrade available): a redundant system used to protect the station from flooding when the main pump is not operating
#• bridge crane (upgrade available): allows vertical access to equipment and protecting materials
#• servo (upgrade available): servomechanism used to provide control of a desired operation through the Super-
#  visory Council and Data Acquisition (SCADA) systems
#• trashrack cleaner (upgrade available): protects hydraulic structures against the inlet of debris, vegetation, or
#  trash
#• elevation(maintenanceavailable):elevationofthepumpstation;maybealteredbyonefootviamaintenance
#• slope: ravine slope surrounding the pump station
#• age: difference between the pump’s installation date and the current date
#• H1--H48: pumping status reported by pump stations during a 48-hour emergency period (accuracy of pump
#  status not guaranteed to be error-free)

#---------------------------------------------DESCRIPTIVE STATISTICS--------------------------------------------

# Quick check of the summary of the data set
summary(katrina)
str(katrina)

# Summary statistics for each type of pump station failure. 
table(katrina$reason)
# Count of survived and failures for each reason:
# 316 - survived
# 115 - flood
# 112 - motor 
# 111 - surge
# 116 - jammed 
mean(katrina$reason == 0) # 41% survived 
mean(katrina$reason == 1) # 14.93% flood 
mean(katrina$reason == 2) # 14.54% motor
mean(katrina$reason == 3) # 14.41% surge
mean(katrina$reason == 4) # 15.06% jammed

# Create string variable to assign to the numberic values of "reason"  
reason3 <- factor(katrina$reason, levels = c(0, 1, 2, 3, 4), 
       labels = c("Survived", "Flood", "Motor", "Surge", 
                  "Jammed"))

# add new variable to data frame 
df_pump <- cbind(katrina, reason3)

# Start looking at some descriptive stats - simple histogram
ggplot(df_pump, aes(reason3)) +
  geom_bar(fill = "#0073C2FF") +
  theme_pubclean()

# Compute the frequency of each category 
df_pump_freq <- df_pump %>%
  group_by(reason3) %>%
  summarise(counts = n())

# Histogram with frequencies 
newtext <- element_text(color = "black", size = 12)
ggplot(df_pump_freq, aes(x = reason3, y = counts)) +
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = counts), vjust = -0.3) + 
  theme_pubclean()+
  labs(y = "# of Pumps", x = "") +
  theme_gray() +
  theme(axis.text = newtext) +
  theme(axis.title=element_text(size=14)) +
  ggtitle("Golf Coast Pump Stations")

# Calculate the means of each category
df_pump_mean <- df_pump_freq %>%
  arrange(desc(reason3)) %>%
  mutate(prop = round(counts*100/sum(counts), 1),
         lab.ypos = cumsum(prop) - 0.5*prop)
head(df_pump_mean, 5)

# % of pumps survived the hurricane
surv <- sum(katrina$reason == 0) # 316 pumps survived hurricane
surv/(nrow(katrina)) # 41% of pumps survived the hurricane 

# Percentage of pumps in each failure
# Create pie chart with means values for each category 
ggplot(df_pump_mean, aes(x = "", y = prop, fill = reason3)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  geom_text(aes(y = lab.ypos, label = prop), color = "white", size = 6)+
  coord_polar("y", start = 0)+
  ggpubr::fill_palette("jco")+
  theme_void()+
  ggtitle("Percentage of Pumps that Survived vs. Each Failure")+
  theme(legend.title=element_blank())+
  theme(legend.text=element_text(size=13))

# Median survival times 
summary(katrina$hour) # the median survival times is 45

#-------------------------------------------------PLOT SURVIVAL CURVES--------------------------------------------

# Create survival curves for all pumps, survfit() takes a formula just like usual
# ~ 1 to fit model with no predictors
pump_fit <- survfit(Surv(time = hour, event = survive == 0) ~ 1, data = katrina)

# Plot the survival curve for all pumps
plot(pump_fit, xlab = "Time", ylab = "Survival Probability")
# Better looking plot 
ggsurvplot(pump_fit, data = katrina, conf.int = TRUE, palette = "blue",ggtheme = theme_bw(),
           legend.title = "Survived")

# Create data frame without survived rows
df_reason <- df_pump[df_pump$survive == 0, ]
# Rename reason 3 column 
colnames(df_reason)[colnames(df_reason)=="reason3"] <- "Reason"

# Plot the survival curves by reason
pump_fit1 <- survfit(Surv(time = hour, event = survive == 0) ~ Reason, data = df_reason)

# View survival curves by reason
ggsurvplot(pump_fit1, data = df_reason, combine = TRUE,     # Combine curves
           risk.table = TRUE, tables.height = 0.35, # Adjust tables height
           tables.theme = theme_cleantable(),       # Clean risk table
           palette = "jco",
           conf.int = TRUE, 
           ggtheme = theme_bw(), 
           legend.title = ""
)

#-----------------------------------------------------Bin Failure Types---------------------------------------------
# The idea here is to create survival curves with the binned data to see how much information is lost.
# These graphs are only useful when compared to the ones generated in the prior section (2 back)

# First create new columns that bins the failure types
df_reason$combined <- with(df_reason, ifelse(is.element(df_reason$reason,  c(1,3)), "Water","Mechanical"))
df_reason$comb_reason <- with(df_reason, ifelse(is.element(df_reason$reason,  c(1,3)), 1,2))

# Plot the survival curves by combined reason
pump_fit2 <- survfit(Surv(time = hour, event = survive == 0) ~ combined, data = df_reason)

# View survival curves by combined reasons
ggsurvplot(pump_fit2, data = df_reason, combine = TRUE, 	# Combine curves
           risk.table = TRUE, tables.height = 0.35, # Adjust tables height
           tables.theme = theme_cleantable(),   	# Clean risk table
           censor = FALSE,                      	# Remove censor points
           palette = "jco",
           conf.int = TRUE, 
           ggtheme = theme_bw(), 
           legend.title = "",
           legend.labs = c("Mechanical", "Water")
)

#------------------------------------------------------LOG-RANK TEST------------------------------------------------

# use the survdiff() function, this also takes the formula & data inputs as usual, but need a new argument:
# rho = 0 is the normal log-rank test, rho = 1 is the weighted test
# remember to use the data set without the survived pumps : df_reason
survdiff(Surv(time = hour, event = survive == 0) ~ reason, rho = 0, data = df_reason)


#-----------------------------------------------------HAZARD FUNCTION---------------------------------------------

# kphaz.fit() automatically assumes 0 is censored and 1 is the event, therefore we need to make our 
# survived pump values all 0's and the reasons all 1's, essentially flip flopping the binary values.
katrina$survive1 <- ifelse(katrina$survive == 1, 0, 1) 

# Create a new variable hour2, setting censored observations
# to have hour2 = 49 so that the function doesn't plot them as if they all had the event in the last hour
katrina$hour2 <- ifelse(katrina$hour == 48 & katrina$survive1 == 0, 49, katrina$hour)

# kphaz.fit() has the same arguments as Surv()
pump_haz <- with(katrina, kphaz.fit(hour2, survive1))
# and we plot it with kphaz.plot()
kphaz.plot(pump_haz, main = "Hazard Function")

# Cumulative hazard
ggsurvplot(pump_fit, fun = "cumhaz", palette = "blue", ggtheme = theme_bw())
