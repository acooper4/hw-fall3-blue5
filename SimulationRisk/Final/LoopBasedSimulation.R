# Simulatin & Risk HW Project
# Created 11/24/2018 -  Blue Team 5



#--------------------------------------INSTALL PACKAGES/LOAD LIBRARIES---------------------------------
# Install required packages
# install.packages('truncnorm')
# install.packages('graphics')
# install.packages('quantmod')
# install.packages('TTR')
# install.packages('ks')
# install.packages('scales')
# install.packages('fExtremes')

# Install required libaries
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
library(ks)
library(RTriangle) 
library(triangle)

#----------------------------------------------SET-UP--------------------------------------------------

runs <- 200               # Number of simulation runs      
#set.seed(55555)              # See -- use to compare results only
VaR <- 0.05                   # Set the Value at Risk Cutoff   
taxrate = 0.046               # Severence Taxes
wacc = .10                    # Weighted Average Cost of Capital
YearsOfProduction = 15        # Number of years wells will produce. Used for NPV

# create wacc vector
discount <- rep(0,YearsOfProduction); 
for (i in 1:YearsOfProduction){
          discount[i] = (1+wacc)^i
}

# set working directory
setwd('C:\\Users\\Wbr\\Desktop\\NCSU Backup 2018-10-09\\Analytics\\AA502\\Simulation and Risk\\Homework\\Project\\')

# At this point, it would improve the simulation to parameterize all values. That has not been done yet; although I am starting
# It would also be faster to process if everthing was in a matrix rather than a dataframe. Since everything is numeric (all the same type) 
#         a df is not needed. I have not done this yet either
MinWells = 10                 # Minimum wells -- input to the Uniform distribution for number of wells
MaxWells = 30                 # Maximum wells -- input to the Uniform distribution for number of wells




#-------------------------------------------------------------------------------------#
# Create dataframes to hold intermediate data for graphing

df_wet_wells <- data.frame(Wet=integer(runs), Dry = integer(runs),Proportion = double(runs)) # Create empty dataframe
drilling.2019 <- data.frame(drill_cost=double()) # Create empty dataframe
df_seismic = data.frame(seismic=double()) # Create empty dataframe
df_lease = data.frame(lease=double()) # Create empty dataframe
df_prof_oh = data.frame(prof_oh=double()) # Create empty dataframe
df_completion = data.frame(completion=double()) # Create empty dataframe
df_oil_prices = data.frame(oil_prices=double()) # Create empty dataframe
df_Total_Npv = data.frame(total_npv=double()) # Create empty dataframe
df_Oil_Production = data.frame(oil=double()) # Create empty dataframe

################################################################################################
#This simulation is broken into sections
# Set up
# Start iterations
#         Simulating the number of wells for the iteration 
#         Some components below vary by well. Others do not. If it varies by well, USE LOOP OR OTHER PROCESS 
#                   the number of wells for that simulation run
#         For Each well simulate NPV
#          For NPV Simulate Revenue & Cost
#         For Revenue Simulate Price and Volume(Number of Barrels) and finally Net Revenue Interest
#                   Price changes over each of the 15 years but not per well
#                  Volume changes over each of the 15 years and differs per well
#                   NRI changes per well, but not over time
#         For Cost Simulate Year 0 Costs and On-going Costs
#         For Year 0 Cost simulate:
#                   Seismic Cost
#                  Lease Cost
#                   Drilling Cost
#                  Completion Cost (Wet Wells Only)
#                   Professional Overhead
#         for Operating Costs, simulate:
#                   Operating Cost which vary by year but not by well
#                  Professional Overhead
#          Taxes do not vary by year or well
# End iteration
################################################################################################

for (sim in 1:runs){
          if(sim%%100==0){print(sim)}
          nbr_wells <- sample(MinWells:MaxWells,1 )  # Number of wells to drill
          
          # prices are the same for all wels in each run
          oil_prices = prices()
          
          # Operating costs are teh same for all wells in each run
          operating_costs = operating_cost(YearsOfProduction)
          
# For each well, determine NPV or cost of dry well
          # Initialize variables
          
          drilling_cost_dry = 0
          seismic_cost_wet = 0
          lease_cost_wet = 0
          prof_oh_cost_wet = 0
          completion_cost_wet = 0
          
          drilling_cost_wet = 0
          seismic_cost_dry = 0
          lease_cost_dry = 0
          prof_oh_cost_dry = 0
          
          Year_0_wet = 0
          Year_0_dry = 0
          Total_Npv = 0
          NPV = 0
          
          drilling_cost_wet_tot = 0       
          seismic_cost_wet_tot = 0            
          lease_cost_wet_tot = 0                                               
          prof_oh_cost_wet_tot = 0          
          completion_cost_wet_tot = 0  
          
          drilling_cost_dry_tot = 0       
          seismic_cost_dry_tot = 0            
          lease_cost_dry_tot = 0                                               
          prof_oh_cost_dry_tot = 0          
          completion_cost_dry_tot = 0
          oil_volume_Tot = rep(0,YearsOfProduction)
          oil_volume = rep(0,YearsOfProduction)
          
          for (well_sim in 1:nbr_wells){
          # Determine if the well is wet or dry          
                    res = rtruncnorm(1, a=0.0, b = 1.00, mean = .80, sd = .10)         # Resevoir Risk Factor
                    hydro <- rtruncnorm(1, a=0.0, b = 1.00, mean = .99, sd = .05)      # Hydrocarbon risk Factor
                    wet_wellprb = res * hydro                                          # Probability of producing wet well
                    wet_dry = rbinom(n=1, size = 1, prob = wet_wellprb)                # Create a 0 or 1. 1's are wet wells
                    df_wet_wells$Wet[sim] = df_wet_wells$Wet[sim] + wet_dry            # Sum the number of 1's (wet wells)
           
          # If well is wet, determine NPV
          # If well is dry, determine dry  hole cost
                    
                    if(wet_dry == 1){
                              # year 0 costs
                              drilling_cost_wet = drillingcost()                # From phase 1
                              seismic_cost_wet = seismic()                      # Num sections ~N(3,.35); Cost per section = $43,000
                              lease_cost_wet = lease()                          # Num acres ~ N(600,50); Cost per acre = $960                              
                              prof_oh_cost_wet = salary()                       # ~ triangle ( 172k, 215k, 279,500)
                              completion_cost_wet = complete()                  # ~N($390,000, $50,000.)
                              Year_0_wet = drilling_cost_wet + seismic_cost_wet + lease_cost_wet + prof_oh_cost_wet + completion_cost_wet
                              
                              # Operating Income (Revenues - costs)
                              oil_volume = oil_production(YearsOfProduction)
                              gross_revenue = oil_volume * oil_prices
                              NRI <- rnorm(1, .75, .02)                                   # Varies by well, but constant across time
                              Net_revenue = gross_revenue * NRI * (1-taxrate)
                              
                              prof_oh_cost_annual = rep(prof_oh_cost_wet,YearsOfProduction)
                              operating_expenses = oil_volume * operating_costs 
                              operating_expenses = operating_expenses+ prof_oh_cost_annual
                              
                              Net_Income = Net_revenue - operating_expenses
                              Discounted_NI = Net_Income/discount
                              NPV = sum(Discounted_NI) - Year_0_wet
                              
                    }
                    else{
                              #year 0 costs
                              drilling_cost_dry = drillingcost()                # From phase 1
                              seismic_cost_dry = seismic()                      # Num sections ~N(3,.35); Cost per section = $43,000
                              lease_cost_dry = lease()                          # Num acres ~ N(600,50); Cost per acre = $960                              
                              prof_oh_cost_dry = salary()                       # ~ triangle ( 172k, 215k, 279,500)
                              Year_0_dry = drilling_cost_dry + seismic_cost_dry + lease_cost_dry + prof_oh_cost_dry 
                    }
                    
                    Total_Npv = Total_Npv+NPV-Year_0_dry
                    
                    # Accumulate for graphing
                    drilling_cost_wet_tot = drilling_cost_wet_tot+drilling_cost_wet       
                    seismic_cost_wet_tot = seismic_cost_wet_tot + seismic_cost_wet            
                    lease_cost_wet_tot = lease_cost_wet_tot + lease_cost_wet                                               
                    prof_oh_cost_wet_tot = prof_oh_cost_wet_tot + prof_oh_cost_wet           
                    completion_cost_wet_tot = completion_cost_wet_tot + completion_cost_wet  
                    
                    drilling_cost_dry_tot = drilling_cost_dry_tot+drilling_cost_dry       
                    seismic_cost_dry_tot = seismic_cost_dry_tot + seismic_cost_dry            
                    lease_cost_dry_tot = lease_cost_dry_tot + lease_cost_dry                                               
                    prof_oh_cost_dry_tot = prof_oh_cost_dry_tot + prof_oh_cost_dry           
                    oil_volume_Tot = oil_volume_Tot + oil_volume

          }
          df_wet_wells$Dry[sim] = nbr_wells - df_wet_wells$Wet[sim] # The number of dry wells = total number of wells - number of wet wells
          df_wet_wells$Proportion[sim] = df_wet_wells$Wet[sim]/nbr_wells # proportion of wet wells = number of wet wells/ total number of wells
          drilling.2019 = rbind(drilling.2019,drilling_cost_dry_tot + drilling_cost_wet_tot)
          df_seismic = rbind(df_seismic,seismic_cost_wet_tot + seismic_cost_dry_tot)
          df_lease = rbind(df_lease,lease_cost_wet_tot + lease_cost_dry_tot)
          df_prof_oh = rbind(df_prof_oh,prof_oh_cost_wet_tot + prof_oh_cost_dry_tot)
          df_completion = rbind(df_completion,completion_cost_wet_tot)
          df_oil_prices = rbind(df_oil_prices,oil_prices)
          df_Total_Npv = rbind(df_Total_Npv,Total_Npv)
          df_Oil_Production = rbind(df_Oil_Production,oil_volume_Tot)

}





##################################################################################################################
#  Below are functions to siplify the simulation
#  There are functions for:
#     Drilling cost
#     Seismic cost
#     Land cost
#     
#     Prices
#     Operating Costs (prices)
#     Oil Production
#     Standardizing and Destandardizing data
#
##################################################################################################################

# Drillling COsts
############################### Phase 1 ########################################################
#  Simulate drilling costs. THis gives the 2019 Drilling Costs.
###########################################################################

drillingcost = function (){ 
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

# Subset dataset starting at year 1991 to 2006 (16 observations)
drill_final <- drill[32:47,]

# Create vector of the 3 % change columns (oil, gas, drywell) from 1991 to 2006
drill_data_vector <- c(drill_final$oil_change, drill_final$gas_change, drill_final$drywell_change)
mean(drill_data_vector) # mean value of 0.13
sd(drill_data_vector) # stdv value of 0.18

# Assuming normal distribution that uses the mean and sd from above - using loops
# For each loop, calculate the cost using:
# The normal distributin for years 1-5 (2006-2011)
# Use the first triangle distribution for the next 3 years
# Use the second triangle distribution for the final 4 years
drilling.2019 <- 0
drilling.2006 <- mean(c(drill_final$oil_cost[nrow(drill_final)],
                        drill_final$gas_cost[nrow(drill_final)],
                        drill_final$oil_cost[nrow(drill_final)]))

          change <- rnorm(n = 1, mean = mean(drill_data_vector), sd = sd(drill_data_vector))
          drilling.t <- drilling.2006*(1+change)
          for (j in 1:5) {
                    change <- rnorm(n = 1, mean = mean(drill_data_vector), sd = sd(drill_data_vector))
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
          drilling.2019 <- drilling.t

drilling.2019 = 1000*drilling.2019                #Multiply drilling costs by 1000
return(drilling.2019)
}

#-----------------------------------------------------Seismic COST-----------------------------------------------

# Simulate Seismic with normal distribution N(3,.35)
# Seismic costs per section is $43,000
seismic <- function() {
          seismic_cost = 43000*rnorm(n=1,mean=3, sd=.35)
return(seismic_cost)
}
#-----------------------------------------------------LEASE COST-----------------------------------------------
# Simulate acres per well with normal distribution N(600,50)
# Price per acre is $960
lease <- function() {
          lease_cost = 960*rnorm(n=1, mean=600, sd=50)
          return(lease_cost)
}

#---------------------------------------------------COMPLETION COST-----------------------------------------------
# We hypothesize that his cost is Normally distributed with a mean of $390,000 and a standard deviation of $50,000.
complete <- function() {
          complete_cost = rnorm(n=1, mean=390000, sd=50000)
          return(complete_cost)
}

#--------------------------------------------------PROFESSIONAL OVERHEAD--------------------------------------------
# We believe the salary and benefit cost is best represented by a triangular distribution, with a most likely cost as $215,000, with a minimum of $172,000 and a maximum of $279,500.

salary  <- function() {
          prof_oh_cost = rtriangle(n=1, a=172000, b=279500, c=215000)
          return(prof_oh_cost)
}


# --------------------- Simulate Prices Per Year------------------------------------------# 

# Import 1st worksheet of data set containing price projections
prices = function() {
          price <- read_excel("Analysis_Data.xlsx", 
                    sheet=1, col_names = TRUE, skip=2)

          price <- price[2:16,]         # Only look at next 15 years, starting in 2020

          df_price <- matrix(nrow=1,ncol=length(price$Year)) #Create an empty matrix to contain the simulations
          for (i in 1:length(price$Year)){
                    df_price[,i]<-rtriangle(n=1,price$`Low Oil Price`[i],price$`High Oil Price`[i],price$`AEO2018 Reference`[i])
          } 

          df_price <- data.frame(df_price)        # make the matrix a dataframe
return(df_price)
}

#-----------------------------------------------OPERATING EXPENSES--------------------------------------------------
# Normally distributed with a mean of $2.25 per barrel with a standard # deviation of $0.30 per barrel. 
# The expenses would be the same for every well in a given year,but could change from year to year with the distribution above.

operating_cost = function(YearsOfProduction){ 
          df_Operating_Costs <- matrix(nrow=1,ncol=YearsOfProduction) #Create an empty matrix to contain the simulations
          for (i in 1:YearsOfProduction){
                    df_Operating_Costs[,i]= rnorm(1, 2.25, .30)
          } 
          df_Operating_Costs <- data.frame(df_Operating_Costs)        # make the matrix a dataframe

return(df_Operating_Costs)
}

# --------------------- Simulate Volume Per Well Per Year------------------------------------------#

oil_production = function(YearsOfProduction) { 
          # Initial Production rate simulates values for the IP with lognormal distribution
          IP <- rlnorm(n=1, meanlog=6, sdlog=.28)

          # Decline Rate - Uniform distribution for rate of decline
          decline <- runif(1, min=.15, max=.32)

          # Next we will make these correlated based on the following correlation matrix

          R <- matrix(data=cbind(1,.64, .64, 1), nrow=2)

          # Use Choleski Decomposition
          U <- t(chol(R))

          # Creating functions - Want variance of 1, so we standardize then we  have to destandardize
          # Use the mean and std for each from above
          standardize_IP <- function(x){
                    x.std = (x - 420)/120
                    return(x.std)
          }
          destandardize_IP <- function(x.std, x){
                   x.old = (x.std * 120) + 420
                  return(x.old)
          }
          
          standardize_Dec <- function(x){
                    x.std = (x - .235)/.049
                    return(x.std)
          }
          destandardize_Dec <- function(x.std, x){
                    x.old = (x.std * 0.049) + mean(x)
                    return(x.old)
          }

          # Combine into one matrix with the standardized values
          # Decline first, since it was uniform (least flexible distribution)
          IP_decline <- cbind(standardize_Dec(decline),standardize_IP(IP) )

          # Get corrlated distribution by using the Choleski Decomposition
          correlated_rates <- U %*% t(IP_decline)

          ##this is putting things in columns, so 2 columns of data. 
          correlated_rates <- t(correlated_rates)

          #Note That here I have reversed the order of IP and Decline.
          # This was done, becuase we originally had this order and the code below uses this order.
          # I reversed the order above because it was important for the ordering in the Choleski Decomposition
          final.correlated_rates <- cbind(destandardize_IP(correlated_rates[,1], IP), destandardize_Dec(correlated_rates[,2], decline))

          # Simulate the Oil volumne per year from 2020 to 2034
          Begin_Production = matrix(nrow=1,ncol=YearsOfProduction) # Create an empty matrix 
          End_Production = matrix(nrow=1,ncol=YearsOfProduction)   # Create an empty matrix 

          for (k in 1:YearsOfProduction){
                   Begin_Production[,k] = final.correlated_rates[,1] * (1-final.correlated_rates[,2])^(k-1)
                    End_Production[,k] = final.correlated_rates[,1] * (1-final.correlated_rates[,2])^(k)
          }

          oil_volume  = 365*((Begin_Production + End_Production)/2)
          df_oil_volume = data.frame(oil_volume)

return(df_oil_volume)
}
