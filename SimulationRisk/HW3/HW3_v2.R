# Simulatin & Risk HW 3
# Created 11/16/2018
# Initial Author - Bill Richmond

# Homework Deliverables/Goals

# You should provide a business report that includes the following:
#   
# Run a simulation for the number of wells in the project that includes whether the well is 
# producing or dry. 
# 
# Provide a histogram of the distribution of the proportion of wells that is producing 
# – you will need to know how many wells are dry and how many are wet for each simulation to calculate this.
# o Calculate the 5% VaR from this distribution.
# o Calculate the 5% CVaR from this distribution.
# o Interpret these values.
# 
# One of the hardest things to simulate is the truncated Normal distribution 
# – a Normal distribution with one or both of the tails truncated at a value. The reason this is a hard 
# distribution to simulate from is because you cannot simply just take all values outside of the 
# bounds of the truncation and set them to the truncated value. This leaves large collections of 
# observations in the tail (SEE CHARTS BELOW). You will need to correctly simulate from a 
# truncated Normal distribution to obtain the probabilities for the producing well. 
# 
# Provide your histograms for the probability of hydrocarbons and probability of reservoir (the 
#                                               two pieces that are truncated Normal)

# Packages
#install.packages('truncnorm')
#install.packages('graphics')
#install.packages('quantmod')
#install.packages('TTR')
#install.packages('ks')
#install.packages('scales')
#install.packages('fExtremes')
#install.packages('RTriangle')
#install.packages('mefa')

# Needed Libraries for Analysis #
library(truncnorm)
library(graphics)
library(quantmod)
library(TTR)
library(ks)
library(scales)
library(fExtremes)
library(ggplot2)

library(readxl)
library(dplyr)
library(RTriangle) 
library(triangle)

library(mefa)
library(scales)

# Parameters
setwd('C:\\Users\\Wbr\\Desktop\\NCSU Backup 2018-10-09\\Analytics\\AA502\\Simulation and Risk\\Homework\\HW3\\')

NumSimulations = 50000   # Number of simulation runs
set.seed =  55555       # set the seed number to allow duplication of results
VaRCutoff = .05         # The VaR cutoff set in the RFP

Hydrocarbons = rtruncnorm(NumSimulations, a=0.0, b = 1.00, mean = .99, sd = .05)
df_Hydrocarbons = data.frame(Hydrocarbons)


# Histogram of Hydrocarbons to see if truncated normal
# ggployt Histogram would be better, but this is fast


ggplot(df_Hydrocarbons, aes(x=Hydrocarbons)) + 
  geom_histogram(binwidth=.0045,color="black", fill="white") +
  geom_vline(aes(xintercept=mean(df_Hydrocarbons$Hydrocarbons)), color="red", linetype="solid", size=1) +
  #scale_x_continuous(labels = scales::comma) +
  #  scale_x_continuous(breaks=seq(0,.5,1), labels = comma)+
  labs(x = "Probability of Hydrocarbons") +
  labs(y = "Counts") +
  labs(title = "Simulated Probability of Hydrocarbons") +
  #annotate("text", label ="Mean of 0.9562182", x =17000000, y = 6000, color = "red") +
  theme(
    plot.title = element_text(color="black", size=14, face="bold",family="Arial"),
    axis.title.x = element_text(color="dark blue", size=14, face="bold",family="Arial"),
    axis.title.y = element_text(color="dark blue", size=14, face="bold",family="Arial"),
    plot.subtitle = element_text(color="black", size=12, face="bold",family="Arial") 
  )



# Truncated Normal for Resevoir
Resevoir = rtruncnorm(NumSimulations, a=0.0, b = 1.00, mean = .80, sd = .10)
df_Resevoir = data.frame(Resevoir)

qplot(df_Resevoir$Resevoir,geom="histogram")
ggplot(df_Resevoir, aes(x=Resevoir)) +
  geom_histogram(binwidth = .008)



ggplot(df_Resevoir, aes(x=Resevoir)) + 
  geom_histogram(binwidth=.0085,color="black", fill="white") +
  geom_vline(aes(xintercept=mean(df_Resevoir$Resevoir)), color="red", linetype="solid", size=1) +
  #scale_x_continuous(labels = scales::comma) +
  #  scale_x_continuous(breaks=seq(0,.5,1), labels = comma)+
  labs(x = "Probability of Resevoir") +
  labs(y = "Counts") +
  labs(title = "Simulated Probability of Resevoir") +
  #annotate("text", label ="Mean of 0.9562182", x =17000000, y = 6000, color = "red") +
  theme(
    plot.title = element_text(color="black", size=14, face="bold",family="Arial"),
    axis.title.x = element_text(color="dark blue", size=14, face="bold",family="Arial"),
    axis.title.y = element_text(color="dark blue", size=14, face="bold",family="Arial"),
    plot.subtitle = element_text(color="black", size=12, face="bold",family="Arial") 
  )



# Probability of producing well (Really of a wet well)
df_ProducingWell = df_Resevoir * df_Hydrocarbons

# Number of Wells that will be drilled
# The number is rounded, since we have to drill an integer number of wells
Number_of_Wells = round(runif(n=NumSimulations, min = 10, max = 30 ),digits = 0)
df_Number_of_Wells = data.frame(Number_of_Wells)

# Loop that will determine the number of wet wells and dry wells
# There may be a vecorized way to do this, but ...
df_Number_of_Wet_Wells <- data.frame(Wet=integer(NumSimulations), Dry = integer(NumSimulations),Proportion = double(NumSimulations) )
for (j in 1:NumSimulations){
  wet_dry = rbinom(df_Number_of_Wells$Number_of_Wells[j],1,df_ProducingWell$Resevoir[j])
  df_Number_of_Wet_Wells$Wet[j] = sum(wet_dry)
  df_Number_of_Wet_Wells$Dry[j] = df_Number_of_Wells$Number_of_Wells[j] - df_Number_of_Wet_Wells$Wet[j]
  df_Number_of_Wet_Wells$Proportion[j] = df_Number_of_Wet_Wells$Wet[j]/df_Number_of_Wells$Number_of_Wells[j]
}

# caclculate quantiles to get the VaR
quantile(df_Number_of_Wet_Wells$Proportion,probs = seq(0, .1, VaRCutoff))
# The 5% VAR (1 year?) is 52%. If I am stating this correctly, 5% of the time the proportion of wells that are wet is less than or equal to 52%

# Find the average of the lowest .05*NumSimulations values to get the CVaR.
EndPoint = VaRCutoff*NumSimulations
Minimum_Proportions = sort(df_Number_of_Wet_Wells$Proportion,decreasing=F)[1:EndPoint]
mean(Minimum_Proportions)
# CVAR = 0.4499617

# Mean and Median Proportions
mean(df_Number_of_Wet_Wells$Proportion)    #0.7590932
median(df_Number_of_Wet_Wells$Proportion)  #0.7692308


ggplot(df_Number_of_Wet_Wells, aes(x=Proportion)) + 
  geom_histogram(binwidth=.03,color="black", fill="white") +
  geom_vline(aes(xintercept=mean(df_Number_of_Wet_Wells$Proportion)), color="blue", linetype="solid", size=1) +
  geom_vline(aes(xintercept=quantile(df_Number_of_Wet_Wells$Proportion, VaRCutoff)), color="red", linetype="solid", size=1) +
  #scale_x_continuous(labels = scales::comma) +
#  scale_x_continuous(breaks=seq(0,.5,1), labels = comma)+
  labs(x = "Proportion of Wet Wells") +
  labs(y = "Counts") +
  labs(title = "Simulated Proportion of Wet Wells") +
#  annotate("text", label ="Mean of $12.04M", x =17000000, y = 6000, color = "red") +
  theme(
    plot.title = element_text(color="black", size=14, face="bold",family="TT Arial"),
    axis.title.x = element_text(color="dark blue", size=14, face="bold",family="TT Arial"),
    axis.title.y = element_text(color="dark blue", size=14, face="bold",family="TT Arial"),
    plot.subtitle = element_text(color="black", size=12, face="bold",family="TT Arial") 
  )

ggplot(df_Number_of_Wet_Wells, aes(x=Wet)) + 
  geom_bar(binwidth=.03,color="black", fill="white") +
  geom_vline(aes(xintercept=mean(df_Number_of_Wet_Wells$Wet)), color="blue", linetype="solid", size=1) +
  geom_vline(aes(xintercept=quantile(df_Number_of_Wet_Wells$Wet, VaRCutoff)), color="red", linetype="solid", size=1) +
  #scale_x_continuous(labels = scales::comma) +
  #  scale_x_continuous(breaks=seq(0,.5,1), labels = comma)+
  labs(x = "Number of Wet Wells") +
  labs(y = "Counts") +
  labs(title = "Simulated Number of Wet Wells") +
  #  annotate("text", label ="Mean of $12.04M", x =17000000, y = 6000, color = "red") +
  theme(
    plot.title = element_text(color="black", size=14, face="bold",family="TT Arial"),
    axis.title.x = element_text(color="dark blue", size=14, face="bold",family="TT Arial"),
    axis.title.y = element_text(color="dark blue", size=14, face="bold",family="TT Arial"),
    plot.subtitle = element_text(color="black", size=12, face="bold",family="TT Arial") 
  )

ggplot(df_Number_of_Wet_Wells, aes(x=Dry)) + 
  geom_histogram(binwidth=.03,color="black", fill="white") +
  geom_vline(aes(xintercept=mean(df_Number_of_Wet_Wells$Dry)), color="blue", linetype="solid", size=1) +
  geom_vline(aes(xintercept=quantile(df_Number_of_Wet_Wells$Dry, (1-VaRCutoff))), color="red", linetype="solid", size=1) +
  #scale_x_continuous(labels = scales::comma) +
  #  scale_x_continuous(breaks=seq(0,.5,1), labels = comma)+
  labs(x = "Number of Dry Wells") +
  labs(y = "Counts") +
  labs(title = "Simulated Numbr of Dry Wells") +
  #  annotate("text", label ="Mean of $12.04M", x =17000000, y = 6000, color = "red") +
  theme(
    plot.title = element_text(color="black", size=14, face="bold",family="TT Arial"),
    axis.title.x = element_text(color="dark blue", size=14, face="bold",family="TT Arial"),
    axis.title.y = element_text(color="dark blue", size=14, face="bold",family="TT Arial"),
    plot.subtitle = element_text(color="black", size=12, face="bold",family="TT Arial") 
  )
########################################## I DID NOT DO THIS #######################################################
# Don't Know if this is necessary, but...
# Do 50,000 Simulations of:
  # Simulate a number of wells to drill
  # Simulate the proportion that are dry
  # Calculate the number dry
  # For that number, simulate that many dry well costs

# To get the VaR and CVAR for dry wells
# df_Number_of_Wet_Wells Has what we want
for (i in 1:NumSimulations){
  
}



##########################################################
#######################SET-UP STEPS#######################
##########################################################


#------------------------------------------------IMPORT/CLEAN DATASET(HW1)-----------------------------------------------
# Import 2nd worksheet of data set containing drilling data from 1960 to 2007

drill <- read_excel("Analysis_Data.xlsx", 
                    sheet=2, col_names = TRUE, skip=2)
# Update column names
colnames(drill) <- c("Date", "oil_cost", "gas_cost", "drywell_cost", "oil_change", "gas_change", "drywell_change")

# Convert the return values from character to nurmeric values
drill <- drill %>% mutate(oil_change=as.numeric(oil_change)) %>%
  mutate(gas_change=as.numeric(gas_change)) %>% 
  mutate(drywell_change=as.numeric(drywell_change))

# Calculate the averages cost for oil, gas, and drywell, excluding year 2007
drill <- drill %>% 
  mutate(avg = (oil_cost+gas_cost+drywell_cost)/3)

# Remove year 2017 at the bottom of the dataset
drill_2006 <- drill[-48,]

# Subset dataset starting at year 1991 to 2006 (16 observations)
drill_final <- drill_2006[32:47,]

# Create vector of the 3 % change columns (oil, gas, drywell) from 1991 to 2006
data_vector <- c(drill_final$oil_change, drill_final$gas_change, drill_final$drywell_change)
mean(data_vector) # mean value of 0.13
sd(data_vector) # stdv value of 0.18

# Assuming normal distribution - using loops 
drilling.2019 <- rep(0,NumSimulations)
for(i in 1:NumSimulations) {
  drilling.2006 <- mean(c(drill_final$oil_cost[nrow(drill_final)],
                          drill_final$gas_cost[nrow(drill_final)],
                          drill_final$oil_cost[nrow(drill_final)]))
  change <- rnorm(n = 1, mean = mean(data_vector), sd = sd(data_vector))
  drilling.t <- drilling.2006*(1+change)
  for (j in 1:5) {
    change <- rnorm(n = 1, mean = mean(data_vector), sd = sd(data_vector))
    drilling.t <- drilling.t*(1+change)
  }
  for (j in 1:3) {
    change <- rtriangle(n = 1, a = 0.07, b = 0.22, c = 0.0917)
    drilling.t <- drilling.t*(1-change)
  }
  for (j in 1:4) {
    change <- rtriangle(n = 1, a = 0.02, b = 0.06, c = 0.05)
    drilling.t <- drilling.t*(1+change)
  }
  drilling.2019[i] <- drilling.t
}

mean(data_vector)
sd(data_vector)


#--------------------------------------------------####COSTS####-------------------------------------------------
#--------------------------------------------------SEISMIC COST--------------------------------------------
# Simulate Seismic with normal distribution N(3,.35)
# Seismic costs per section is $43,000
seismic <- 43000*rnorm(n=NumSimulations,mean=3, sd=.35)
summary(seismic)
df_seismic <- data.frame(seismic)

#-----------------------------------------------------LEASE COST-----------------------------------------------
# Simulate acres per well with normal distribution N(600,50)
# Price per acre is $960
lease <- 960*rnorm(n=NumSimulations, mean=600, sd=50)
summary(lease)
df_lease <- data.frame(lease)

#---------------------------------------------------COMPLETION COST-----------------------------------------------
# We hypothesize that his cost is Normally distributed with a mean of $390,000 and a standard deviation of $50,000.
complete <- rnorm(n=NumSimulations, mean=390000, sd=50000)
summary(complete)
df_complete <- data.frame(complete)
hist(complete, breaks = 50)


#--------------------------------------------------PROFESSIONAL OVERHEAD--------------------------------------------
# We believe the salary and benefit cost is best represented by a triangular distribution,
# with a most likely cost as $215,000, with a minimum of $172,000 and a maximum of $279,500.
# This will remain constant across the lifetime of a well. only if the well is not dry.
salary  <- rtriangle(n=NumSimulations, a=172000, b=279500, c=215000)
summary(salary)
df_salary <- data.frame(salary)


#------------------------------------------COST OF A SINGLE DRY WELL------------------------------------------
# Cost of a dry well 
# Drilling costs, Seismic, Lease, Salary just for Year 0
Drywell_cost <- 1000*drilling.2019 + seismic + lease + salary


# Create data frame 
df_dry <- data.frame(Drywell_cost)



