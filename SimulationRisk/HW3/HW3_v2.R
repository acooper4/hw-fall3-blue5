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
install.packages('truncnorm')
#install.packages('graphics')
#install.packages('quantmod')
#install.packages('TTR')
#install.packages('ks')
#install.packages('scales')
install.packages('fExtremes')

# Needed Libraries for Analysis #
library(truncnorm)
library(graphics)
library(quantmod)
library(TTR)
library(ks)
library(scales)
library(fExtremes)
library(ggplot2)

# Parameters
NumSimulations = 50000   # Number of simulation runs
set.seed =  55555       # set the seed number to allow duplication of results

Hydrocarbons = rtruncnorm(NumSimulations, a=0.0, b = 1.00, mean = .99, sd = .05)
df_Hydrocarbons = data.frame(Hydrocarbons)


# Histogram of Hydrocarbons to see if truncated normal
# ggployt Histogram would be better, but this is fast
qplot(df_Hydrocarbons$Hydrocarbons,geom="histogram")

# Truncated Normal for Resevoir
Resevoir = rtruncnorm(NumSimulations, a=0.0, b = 1.00, mean = .80, sd = .10)
df_Resevoir = data.frame(Resevoir)

qplot(df_Resevoir$Resevoir,geom="histogram")

# Probability of producing well (Really of a wet well)
df_ProducingWell = df_Resevoir * df_Hydrocarbons

# Number of Wells that will be drilled
# The number is rounded, since we have to drill an integer number of wells
Number_of_Wells = round(runif(n=NumSimulations, min = 10, max = 30 ),digits = 0)
df_Number_of_Wells = data.frame(Number_of_Wells)

# Loop that will determine the number of wet wells and dry wells
# There may be a vecorized way to do this, but ...
df_Number_of_Wet_Wells <- data.frame(Wet=integer(NumSimulations), Dry = integer(NumSimulations))
for (j in 1:NumSimulations){
  wet_dry = rbinom(df_Number_of_Wells$Number_of_Wells[j],1,df_ProducingWell$Resevoir[j])
  df_Number_of_Wet_Wells$Wet[j] = sum(wet_dry)
  df_Number_of_Wet_Wells$Dry[j] = df_Number_of_Wells$Number_of_Wells[j] - df_Number_of_Wet_Wells$Wet[j]
}


