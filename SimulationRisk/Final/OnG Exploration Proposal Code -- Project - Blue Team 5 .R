# Simulatin & Risk HW Project
# Created 11/19/2018 -  Blue Team 5



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
 
runs <- 10000               # Number of simulation runs      
#set.seed(55555)              # See -- use to compare results only
VaR <- 0.05                   # Set the Value at Risk Cutoff   
taxrate = 0.046               # Severence Taxes
wacc = .10                    # Weighted Average Cost of Capital
                              # set working directory
setwd('C:\\Users\\Wbr\\Desktop\\NCSU Backup 2018-10-09\\Analytics\\AA502\\Simulation and Risk\\Homework\\Project\\')

# At this point, it would improve the simulation to parameterize all values. That has not been done yet; although I am starting
# It would also be faster to process if everthing was in a matrix rather than a dataframe. Since everything is numeric (all the same type) 
#         a df is not needed. I have not done this yet either
MinWells = 10                 # Minimum wells -- input to the Uniform distribution for number of wells
MaxWells = 30                 # Maximum wells -- input to the Uniform distribution for number of wells

YearsOfProduction = 15        # Number of years wells will produce. Used for NPV
iterations = runs*MaxWells

################################################################################################
#This simulation is broken into sections
# Set up
# Simulating the number of wells. 
# Some components below vary by well. Others do not. If it varies by well, USE LOOP OR OTHER PROCESS 
#         the number of wells for that simulation run
# For Each well simulate NPV
# For NPV Simulate Revenue & Cost
# For Revenue Simulate Price and Volume(Number of Barrels) and finally Net Revenue Interest
#         Price changes over each of the 15 years but not per well
#         Volume changes over each of the 15 years and differs per well
#         NRI changes per well, but not over time
# For Cost Simulate Year 0 Costs and On-going Costs
# For Year 0 Cost simulate:
#         Seismic Cost
#         Lease Cost
#         Drilling Cost
#         Completion Cost (Wet Wells Only)
#         Professional Overhead
# for Operating Costs, simulate:
#         Operating Cost which vary by year but not by well
#         Professional Overhead
#         Taxes do not vary by year or well

################################################################################################


################################################################################################
#          SIMULATE NUMBER OF WELLS
################################################################################################

#--------------------------------------DRY HOLE RISK FACTORS------------------------------------------

wells_drywet <- sample(MinWells:MaxWells,runs, replace = TRUE )  # Number of wells to drill
wells_df = data.frame(wells_drywet)

# For loop that will determine the number of wet wells and dry wells 
# For each run, loop through the number of wells
df_wet_wells <- data.frame(Wet=integer(runs), Dry = integer(runs),Proportion = double(runs)) # Create empty dataframe
for (j in 1:runs){
          for (k in 1:wells_df[j,1]){
                    res = rtruncnorm(1, a=0.0, b = 1.00, mean = .80, sd = .10)         # Resevoir Risk Factor
                    hydro <- rtruncnorm(1, a=0.0, b = 1.00, mean = .99, sd = .05)      # Hydrocarbon risk Factor
                    wet_wellprb = res * hydro                                       # Probability of producing wet well
                    wet_dry = rbinom(n=1, size = 1, prob = wet_wellprb)     # Create a 0 or 1. 1's are wet wells
                    df_wet_wells$Wet[j] = df_wet_wells$Wet[j] + wet_dry         # Sum the number of 1's (wet wells)
          }
  df_wet_wells$Dry[j] = wells_df$wells_drywet[j] - df_wet_wells$Wet[j] # The number of dry wells = total number of wells - number of wet wells
  df_wet_wells$Proportion[j] = df_wet_wells$Wet[j]/wells_df$wells_drywet[j] # proportion of wet wells = number of wet wells/ total number of wells
}

#------------------------------------------For Each Run Determine the NPV -------------------------------------

################################################################################################
#                                SIMULATE REVENUE
################################################################################################

# --------------------- Simulate Prices Per Year------------------------------------------# 

# Import 1st worksheet of data set containing price projections
price <- read_excel("Analysis_Data.xlsx", 
                    sheet=1, col_names = TRUE, skip=2)

price <- price[2:16,]         # Only look at next 15 years, starting in 2020

df_price <- matrix(nrow=runs,ncol=length(price$Year)) #Create an empty matrix to contain the simulations
for (i in 1:length(price$Year)){
          df_price[,i]<-rtriangle(n=runs,price$`Low Oil Price`[i],price$`High Oil Price`[i],price$`AEO2018 Reference`[i])
} 

df_price <- data.frame(df_price)        # make the matrix a dataframe

# --------------------- Simulate Volume Per Well Per Year------------------------------------------#

# The number of simulations is runs times Maxwells. This will be used in calculating the total revenue
# Initial Production rate simulates values for the IP with lognormal distribution
IP <- rlnorm(n=runs*MaxWells, meanlog=6, sdlog=.28)

# Decline Rate - Uniform distribution for rate of decline
decline <- runif(runs*MaxWells, min=.15, max=.32)

# Next we will make these correlated based on the following correlation matrix

R <- matrix(data=cbind(1,.64, .64, 1), nrow=2)

# Use Choleski Decomposition
U <- t(chol(R))

# Creating functions - Want variance of 1, so we standardize then we  have to destandardize
standardize <- function(x){
          x.std = (x - mean(x))/sd(x)
          return(x.std)
}
destandardize <- function(x.std, x){
          x.old = (x.std * sd(x)) + mean(x)
          return(x.old)
}

# Combine into one matrix with the standardized values
# Decline first, since it was uniform (least flexible distribution)
IP_decline <- cbind(standardize(decline),standardize(IP) )

# Get corrlated distribution by using the Choleski Decomposition
correlated_rates <- U %*% t(IP_decline)

##this is putting things in columns, so 2 columns of data. 
correlated_rates <- t(correlated_rates)

#Note That here I have reversed the order of IP and Decline.
# This was done, becuase we originally had this order and the code below uses this order.
# I reversed the order above because it was important for the ordering in the Choleski Decomposition
final.correlated_rates <- cbind(destandardize(correlated_rates[,1], IP), destandardize(correlated_rates[,2], decline))

# Simulate the Oil volumne per year from 2020 to 2034
Begin_Production = matrix(nrow=runs*MaxWells,ncol=YearsOfProduction) # Create an empty matrix 
End_Production = matrix(nrow=runs*MaxWells,ncol=YearsOfProduction)   # Create an empty matrix 

for (k in 1:YearsOfProduction){
          Begin_Production[,k] = final.correlated_rates[,1] * (1-final.correlated_rates[,2])^(k-1)
          End_Production[,k] = final.correlated_rates[,1] * (1-final.correlated_rates[,2])^(k)
}

oil_volume  = 365*((Begin_Production + End_Production)/2)
df_oil_volume = data.frame(oil_volume)


# --------------------- Simulate NRI ------------------------------------------#

#This is constant across time, but varies per well. Since there are up to 30 wells, create 1 for each possible well

df_NRI <- rnorm(runs*MaxWells, .75, .02) 


# The ordering here may seem strange, but it is due to what varies by time vs by well.
# NRI * Quantity ach year will give an adjusted quantity each year. That will be summed across wells and multiplied by price
df_NRI_Adjusted_Oil_Volume = data.frame(df_NRI*oil_volume)

# --------------------- Gross Revenue = Price * Volume ------------------------------------------#

############ This is done Below as part of summing across wells for each run


############################### Phase 1 ########################################################
#  Simulate drilling costs. THis gives the 2019 Drilling Costs.
###########################################################################
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
drilling.2019 <- rep(0,runs*MaxWells)
drilling.2006 <- mean(c(drill_final$oil_cost[nrow(drill_final)],
                        drill_final$gas_cost[nrow(drill_final)],
                        drill_final$oil_cost[nrow(drill_final)]))
for(i in 1:iterations) {

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
  drilling.2019[i] <- drilling.t
}
drilling.2019 = data.frame(drilling.2019)
drilling.2019 = 1000*drilling.2019                #Multiply drilling costs by 1000

#--------------------------------------------------####COSTS####-------------------------------------------------
#--------------------------------------------------SEISMIC COST--------------------------------------------
# Simulate Seismic with normal distribution N(3,.35)
# Seismic costs per section is $43,000
seismic <- 43000*rnorm(n=runs*MaxWells,mean=3, sd=.35)
df_seismic <- data.frame(seismic)

#-----------------------------------------------------LEASE COST-----------------------------------------------
# Simulate acres per well with normal distribution N(600,50)
# Price per acre is $960
lease <- 960*rnorm(n=runs*MaxWells, mean=600, sd=50)
df_lease <- data.frame(lease)

#---------------------------------------------------COMPLETION COST-----------------------------------------------
# We hypothesize that his cost is Normally distributed with a mean of $390,000 and a standard deviation of $50,000.
complete <- rnorm(n=runs*MaxWells, mean=390000, sd=50000)
df_complete <- data.frame(complete)

#--------------------------------------------------PROFESSIONAL OVERHEAD--------------------------------------------
# We believe the salary and benefit cost is best represented by a triangular distribution, with a most likely cost as $215,000, with a minimum of $172,000 and a maximum of $279,500.

salary  <- rtriangle(n=runs*MaxWells, a=172000, b=279500, c=215000)
df_salary <- data.frame(salary)

#-----------------------------------------------OPERATING EXPENSES--------------------------------------------------
# Normally distributed with a mean of $2.25 per barrel with a standard # deviation of $0.30 per barrel. 
# The expenses would be the same for every well in a given year,but could change from year to year with the distribution above.

df_Operating_Costs <- matrix(nrow=runs,ncol=YearsOfProduction) #Create an empty matrix to contain the simulations
for (i in 1:YearsOfProduction){
          df_Operating_Costs[,i]= rnorm(runs, 2.25, .30)
} 
df_Operating_Costs <- data.frame(df_Operating_Costs)        # make the matrix a dataframe



##################################################################################################################
#                                    Sum Across Wells
#  The simulations above that vary by well created 1 simulation per potential well per simulation run
#  Each simulation run will have a known number of wet wells and of dry wells.
#  The revenues and costs for each run will be calculated based on the number of wet and dry wells
#  This will be done by adding togethe the appropriate number of rows.
##################################################################################################################
# The two lines below kill the parameterization based on number of years.

#Create empty dataframes to hold the simulation data
Annual_Oil_Vol = data.frame(Year1Vol=double(),Year2Vol=double(),Year3Vol=double(),Year4Vol=double(),Year5Vol=double(),
                            Year6Vol=double(),Year7Vol=double(),Year8Vol=double(),Year9Vol=double(),Year10Vol=double(),
                            Year11Vol=double(),Year12Vol=double(),Year13Vol=double(),Year14Vol=double(),Year15Vol=double())

Annual_NRI_Oil_Vol = data.frame(Year1Vol=double(),Year2Vol=double(),Year3Vol=double(),Year4Vol=double(),Year5Vol=double(),
                            Year6Vol=double(),Year7Vol=double(),Year8Vol=double(),Year9Vol=double(),Year10Vol=double(),
                            Year11Vol=double(),Year12Vol=double(),Year13Vol=double(),Year14Vol=double(),Year15Vol=double())

Total_Seismic = data.frame(Seismic_Cost = double() )
Total_Lease = data.frame(Lease_Cost = double() )
Total_Completion = data.frame(Completion_Cost = double() )
Total_Overhead_Wet = data.frame(Overhead_Cost = double() )
Total_Overhead_Dry = data.frame(Overhead_Cost = double() )
Total_Drilling = data.frame(Drilling_Cost = double() )

for (run in 1:runs){                              # for each simulation run
          if (df_wet_wells$Wet[run] == 1){
          Annual_NRI_Oil_Vol = rbind(Annual_NRI_Oil_Vol,sum(df_NRI_Adjusted_Oil_Volume[(run-1)*MaxWells+1:df_wet_wells$Wet[run],]))  # Add up df_NRI_Adjusted_Oil_Volume
          Annual_Oil_Vol = rbind(Annual_Oil_Vol,sum(df_Oil_Volume[(run-1)*MaxWells+1:df_wet_wells$Wet[run],]))  # Add up df_NRI_Adjusted_Oil_Volume
          
                   }
          else{
          Annual_NRI_Oil_Vol = rbind(Annual_NRI_Oil_Vol,colSums(df_NRI_Adjusted_Oil_Volume[(run-1)*MaxWells+1:df_wet_wells$Wet[run],]))  # Add up df_NRI_Adjusted_Oil_Volume
          Annual_Oil_Vol = rbind(Annual_Oil_Vol,colSums(df_oil_volume[(run-1)*MaxWells+1:df_wet_wells$Wet[run],]))  # Add up df_NRI_Adjusted_Oil_Volume
          }
          
          Total_Completion = rbind(Total_Completion,sum(df_complete[(run-1)*MaxWells+1:df_wet_wells$Wet[run],]))

         Total_Seismic = rbind(Total_Seismic,sum(df_seismic[(run-1)*MaxWells+1:(df_wet_wells$Wet[run]+df_wet_wells$Dry[run]),]))
         Total_Lease = rbind(Total_Lease,sum(df_lease[(run-1)*MaxWells+1:(df_wet_wells$Wet[run]+df_wet_wells$Dry[run]),]))
         Total_Overhead_Wet = rbind(Total_Overhead_Wet,sum(df_salary[(run-1)*MaxWells+1:df_wet_wells$Wet[run],]))
         Total_Overhead_Dry = rbind(Total_Overhead_Dry,sum(df_salary[(run-1)*MaxWells+df_wet_wells$Wet[run]+1:df_wet_wells$Dry[run],]))
         
         Total_Drilling = rbind(Total_Drilling,sum(drilling.2019[(run-1)*MaxWells+1:(df_wet_wells$Wet[run]+df_wet_wells$Dry[run]),]))
}

# Calculate the Cost Totals for Producing Years
df_Total_OH_Wet = data.frame(rep(Total_Overhead_Wet,YearsOfProduction))   # replicate the overhead cost to each year
Total_Operating_Costs = Annual_Oil_Vol*df_Operating_Costs                 # Multiply Operating Cost per barrel times the number of barrels
Total_Operating_Costs = Total_Operating_Costs+   df_Total_OH_Wet          #and add the OH costs

# Calculate the Cost Totals For Year 0 

Year_0_Costs = Total_Seismic+Total_Lease+Total_Drilling+Total_Completion+Total_Overhead_Dry+Total_Overhead_Wet

# Calculate Total Revenue
Total_Revenue = Annual_NRI_Oil_Vol*df_price*(1-taxrate)
 # create wacc vector
discount <- rep(0,YearsOfProduction); 
for (i in 1:YearsOfProduction){
          discount[i] = (1+wacc)^i
}

Net_Revenue = data.frame(Total_Revenue - Total_Operating_Costs)
Discounted_CashFlow = Net_Revenue/discount
NPV = rowSums(Discounted_CashFlow) - Year_0_Costs


hist(Total_Revenue$X5571.94865096617)
# for Operating Costs, simulate:
#         Operating Cost which vary by year but not by well
#         Professional Overhead
#         Taxes do not vary by year or well

write.csv(df_Operating_Costs, file = "df_Operating_Costs.csv")
write.csv(Annual_Oil_Vol, file = "Annual_Oil_Vol.csv")
write.csv(Total_Operating_Costs, file = "Total_Operating_Costs.csv")
write.csv(df_Total_OH_Wet, file = "df_Total_OH_Wet.csv")
write.csv(Total_Overhead_Dry, file = "Total_Overhead_Dry.csv")
write.csv(Total_Completion, file = "Total_Completion.csv")
write.csv(Total_Drilling, file = "Total_Drilling.csv")
write.csv(Total_Lease, file = "Total_Lease.csv")
write.csv(Total_Seismic, file = "Total_Seismic.csv")
write.csv(df_price, file = "df_price.csv")
write.csv(Annual_NRI_Oil_Vol, file = "Annual_NRI_Oil_Vol.csv")
write.csv(Total_Revenue, file = "Total_Revenue.csv")
write.csv(Net_Revenue, file = "Net_Revenue.csv")
write.csv(Discounted_CashFlow, file = "Discounted_CashFlow.csv")
write.csv(NPV, file = "NPV.csv")
write.csv(Year_0_Costs, file = "Year_0_Costs.csv")


############## multiply the revenue distribution per year by the barrels sold distribution 
# per year and the NRI distribution per year this will give us the revenue per year
#idea is to multiply the elements of each year for each column in the 3 matrices together
#df_price-op_cost, df_NRI, oil_volume
revenue_risk <- matrix(nrow=runs,ncol=15)
for(i in 1:15){
  revenue_risk[,i] <- (df_price[,i] - op_cost[,i])*df_NRI[,i]*oil_volume[,i]
}

##View the means to make sure they are decreasing over years and to validate code (they do decrease)
means <-c()
for(i in 1:15){
  means[i] <- mean(revenue_risk[,i])
}

rev_after_tax <- (1-.046)*revenue_risk
PV_rev <- rev_after_tax

#------------------------------------------COST OF A SINGLE DRY WELL------------------------------------------
# Cost of a dry well 
# Drilling costs, Seismic, Lease, Salary just for Year 0
Drywell_cost <- 1000*drilling.2019 + seismic + lease + salary


# Create data frame 
df_dry <- data.frame(Drywell_cost)


#-------------------------------------------NPV OF SINGLE WET WELL-------------------------------------------
# Calculate the Net Present Value for the simulated wells
# First need to calculate the Net Future Revenue 
for(i in 1:15) {
  PV_rev[,i] <- PV_rev[,i]*(1+wacc)^(-(i-1))
}

# Use rowSums to add the rows together to get the total revenue for the 15 years
# Calculate the initial cost
initial_cost <- 1000*drilling.2019 + seismic + lease + 15*salary + complete

# Sum 15 discounted years to calculate the Present Value 
Total_PV_rev <-  rowSums(PV_rev)

# Make the costs a cash outflow (negative)
initial_cost <- initial_cost *-1

# Calculates NPV 
NPV = initial_cost + Total_PV_rev

hist(NPV)

# Create data frame of NPV
NPV_df <- data.frame(NPV)
summary(NPV)

# Create histogram of simulated NPV of a single wet well
ggplot(NPV_df, aes(x=NPV)) + 
  geom_histogram(binwidth=450000,color="black", fill="white") +
  geom_vline(aes(xintercept=mean(NPV_df$NPV)), color="red", linetype="solid", size=1) +
  #scale_x_continuous(labels = scales::comma) +
  scale_x_continuous(breaks=seq(-3000000,33000000,5000000), labels = comma)+
  labs(x = "NPV per Wet Well $") +
  labs(y = "Counts") +
  labs(title = "Simulated Net Present Value (NPV) per Wet Well") +
  annotate("text", label ="Mean of $12.04M", x =17000000, y = 6000, color = "red") +
  theme(
    plot.title = element_text(color="black", size=14, face="bold",family="Arial"),
    axis.title.x = element_text(color="dark blue", size=14, face="bold",family="Arial"),
    axis.title.y = element_text(color="dark blue", size=14, face="bold",family="Arial"),
    plot.subtitle = element_text(color="black", size=12, face="bold",family="Arial") 
  )

# Some Statistics 
mean(NPV) # Mean NPV of $12.04M
median(NPV)# Median NPV of $11.8M
max(NPV) # Max NPV of $32M
min(NPV) # Min NPV of $-2.94

# Inter quartile range for NPV
table <- c(quantile(NPV, .05),
           quantile(NPV, .25),
           quantile(NPV, .75),
           quantile(NPV, .50),
           quantile(NPV, .90),
           quantile(NPV, .95),
           quantile(NPV, .10))

table_df <- data.frame(table)

