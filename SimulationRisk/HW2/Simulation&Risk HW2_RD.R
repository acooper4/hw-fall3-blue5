# Simulation & Risk HW2
# Blue Team 5 Fall 3
# Created on 11/12/2018 - Blue Team 5

####### HW2 ASSIGNEMENT #########
# Compagnie Pétrolière et Gazière, INC. (hereafter the “Company”), acting by and through its department of 
# Price Analysis is seeking proposals for analytics services. The scope of services includes the following:
# Simulate the following two distributions using the information provided in the project RFP under Phase 2
# (also listed below):
#   o Cost of a single dry well.
#   o Net Present Value of a single wet well.
# Use your results from the previous RFP to simulate drilling costs.

#------------------------------------------------IMPORT LIBRARIES/PACKAGES-----------------------------------------
install.packages('RTriangle')
library(readxl)
library(dplyr)
library(graphics)
library(ks)
library(RTriangle) 
library(triangle)
library(ggplot2)
# Download mefa so the rep(df, n) function
install.packages('mefa')
library(mefa)
library(scales)

##########################################################
#######################SET-UP STEPS#######################
##########################################################
# 1. Please select the amount of simulations to use:   ###
runs <- 100000                                         ###
# 2. The first section of this script is from the HW1, ###
# the second section is HW2                            ###
# 3. Please select the seed                            ###
set.seed(55555)                                        ###
# 4. Please set the WACC percentage for discouting the ###
# FNR values for the 15 years:                         ###
wacc <- .1                                             ###
# Now you can run the remainder of the Code. Enjoy :)  ###
##########################################################
##########################################################
##########################################################

#------------------------------------------------IMPORT/CLEAN DATASET(HW1)-----------------------------------------------
# Import 2nd worksheet of data set containing drilling data from 1960 to 2007
drill <- read_excel("/Users/rogerdugas/Desktop/MSA NC State/1.2 Fall 3/Simulation & Risk/HW1/Analysis_Data.xlsx", 
                    sheet=2, col_names = TRUE, skip=2)

# Quick look at the structure of our data, and some simple manipulation
str(drill) # We have 48 observations and 7 variables 
summary(drill) # Summary statistics
head(drill) # Take a look at the first 10 observations, we can see that the Return values are character
colnames(drill) # Need to update column names so that the code looks less busy 

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

# Check normallity of data qith QQ plot 
qqnorm(data_vector)
qqline(data_vector)

# Convert to data frame for qqplot
data_vector1 <- as.data.frame(data_vector)

# QQ plot in ggplot
#p <- ggplot(data_vector1, aes(sample = data_vector))
#p + stat_qq() + stat_qq_line() + ggtitle("Normal QQ Plot") +
#  xlab("Theoretical Quantiles") + ylab("Sample Quantiles") +
#  theme(
#    plot.title = element_text(color="black", size=14, face="bold.italic",family="Times New Roman"),
#    axis.title.x = element_text(color="#993333", size=14, face="bold",family="Times New Roman"),
#    axis.title.y = element_text(color="#993333", size=14, face="bold",family="Times New Roman")
#  )

# Assuming normal distribution - using loops 
drilling.2019 <- rep(0,runs)
for(i in 1:runs) {
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



hist(drilling.2019, breaks = 50, 
  main = "Estimated Distribution of 2019 Drilling Costs - Normal",
  xlab = "Drilling Costs ($ in Thousands)")
abline(v = drilling.2006, col="red", lwd =2)
mtext(paste("2006 Drilling Costs", 
            dollar(drilling.2006), sep = " = "), at=drilling.2006,col="red")

#--------------------------------------------------####COSTS####-------------------------------------------------
#--------------------------------------------------SEISMIC COST--------------------------------------------
# Simulate Seismic with normal distribution N(3,.35)
# Seismic costs per section is $43,000
seismic <- 43000*rnorm(n=runs,mean=3, sd=.35)
summary(seismic)
df_seismic <- data.frame(seismic)

# Create Histogram of simulated seismic costs for year 0 using ggplot - Normal
ggplot(df_seismic, aes(x=seismic)) + 
  geom_histogram(binwidth=2500,color="black", fill="white") +
  geom_vline(aes(xintercept=mean(df_seismic$seismic)), color="red", linetype="solid", size=1) +
  #scale_x_continuous(labels = scales::comma) +
  scale_x_continuous(breaks=seq(0,200000,25000), labels = comma)+
  labs(x = "Seismic Cost per Well $") +
  labs(y = "Counts") +
  labs(title = "Simulation of Seismic Costs per Well") +
  annotate("text", label ="Mean of $128,952", x = 160000, y = 6000, color = "red") +
  theme(
    plot.title = element_text(color="black", size=14, face="bold",family="Arial"),
    axis.title.x = element_text(color="dark blue", size=14, face="bold",family="Arial"),
    axis.title.y = element_text(color="dark blue", size=14, face="bold",family="Arial"),
    plot.subtitle = element_text(color="black", size=12, face="bold",family="Arial") 
)

#-----------------------------------------------------LEASE COST-----------------------------------------------
# Simulate acres per well with normal distribution N(600,50)
# Price per acre is $960
lease <- 960*rnorm(n=runs, mean=600, sd=50)
summary(lease)
df_lease <- data.frame(lease)

# Create Histogram of simulated lease costs for year 0 using ggplot - Normal
ggplot(df_lease, aes(x=lease)) + 
  geom_histogram(binwidth=7000,color="black", fill="white") +
  geom_vline(aes(xintercept=mean(df_lease$lease)), color="red", linetype="solid", size=1) +
  #scale_x_continuous(labels = scales::comma) +
  scale_x_continuous(breaks=seq(300000,800000,100000), labels = comma)+
  labs(x = "Lease Cost per Well $") +
  labs(y = "Counts") +
  labs(title = "Simulation of Lease Costs per Well") +
  annotate("text", label ="Mean of $576,134", x =650000, y = 6000, color = "red") +
  theme(
    plot.title = element_text(color="black", size=14, face="bold",family="Arial"),
    axis.title.x = element_text(color="dark blue", size=14, face="bold",family="Arial"),
    axis.title.y = element_text(color="dark blue", size=14, face="bold",family="Arial"),
    plot.subtitle = element_text(color="black", size=12, face="bold",family="Arial") 
  )

#---------------------------------------------------COMPLETION COST-----------------------------------------------
# We hypothesize that his cost is Normally distributed with a mean of $390,000 and a standard deviation of $50,000.
complete <- rnorm(n=runs, mean=390000, sd=50000)
summary(complete)
df_complete <- data.frame(complete)
hist(complete, breaks = 50)

# Create Histogram of simulated completion costs using ggplot
ggplot(df_complete, aes(x=complete)) + 
  geom_histogram(binwidth=6000,color="black", fill="white") +
  geom_vline(aes(xintercept=mean(df_complete$complete)), color="red", linetype="solid", size=1) +
  #scale_x_continuous(labels = scales::comma) +
  scale_x_continuous(breaks=seq(120000,700000,100000), labels = comma)+
  labs(x = "Completion Cost per Well $") +
  labs(y = "Counts") +
  labs(title = "Simulation of Completion Costs per Well") +
  annotate("text", label ="Mean of $389,873", x =475000, y = 6000, color = "red") +
  theme(
    plot.title = element_text(color="black", size=14, face="bold",family="Arial"),
    axis.title.x = element_text(color="dark blue", size=14, face="bold",family="Arial"),
    axis.title.y = element_text(color="dark blue", size=14, face="bold",family="Arial"),
    plot.subtitle = element_text(color="black", size=12, face="bold",family="Arial") 
  )

#--------------------------------------------------PROFESSIONAL OVERHEAD--------------------------------------------
# We believe the salary and benefit cost is best represented by a triangular distribution,
# with a most likely cost as $215,000, with a minimum of $172,000 and a maximum of $279,500.
# This will remain constant across the lifetime of a well. only if the well is not dry.
salary  <- rtriangle(n=runs, a=172000, b=279500, c=215000)
summary(salary)
df_salary <- data.frame(salary)
hist(salary, breaks = 50)

# Create Histogram of simulated professional overhead using ggplot
ggplot(df_salary, aes(x=salary)) + 
  geom_histogram(binwidth=3000,color="black", fill="white") +
  geom_vline(aes(xintercept=mean(df_salary$salary)), color="red", linetype="solid", size=1) +
  #scale_x_continuous(labels = scales::comma) +
  scale_x_continuous(breaks=seq(100000,350000,25000), labels = comma)+
  labs(x = "Professional Overhead Cost per Well $") +
  labs(y = "Counts") +
  labs(title = "Simulation of Professional Overhead Costs per Well") +
  annotate("text", label ="Mean of $222,252", x =250000, y = 5000, color = "red") +
  theme(
    plot.title = element_text(color="black", size=14, face="bold",family="Arial"),
    axis.title.x = element_text(color="dark blue", size=14, face="bold",family="Arial"),
    axis.title.y = element_text(color="dark blue", size=14, face="bold",family="Arial"),
    plot.subtitle = element_text(color="black", size=12, face="bold",family="Arial") 
  )

#----------------------------------------------PRODUCTION RISK--------------------------------------------------
# The IP’s follow a Lognormal distribution with a mean of 420 BOPD and a standard deviation of 120 BOPD.
# The underlying Normal distribution for this Lognormal has a mean of 6 and standard deviation of 0.28.
# At year 1
# Initial Production rate simulates values for the IP with lognormal distribution
IP <- rlnorm(n=runs, meanlog=6, sdlog=.28)

# Decline Rate - Uniform distribution for rate of decline
decline <- runif(runs, min=.15, max=.32)

# Next we will make these correlated
# Then, since the IP is only for the first year I can do a IP[i] in the 
# first iteration and a decline[i] throughout since assumed not to change over years
# Then I should make a loop that calculates the volume per year
# Then multiply this volume per year by the revenue risk calculations
# corelation = .64

# Here is the correlation matrix
R <- matrix(data=cbind(1,.64, .64, 1), nrow=2)

# This is the important function: decomposition of R
# Little t() takes the transpose of the matrix. lets you multiply easier
U <- t(chol(R))

# Creating functions - originally variance was 1, but we standardize here then we will have to destandardize
standardize <- function(x){
  x.std = (x - mean(x))/sd(x)
  return(x.std)
}
destandardize <- function(x.std, x){
  x.old = (x.std * sd(x)) + mean(x)
  return(x.old)
}

# Combine into one matrix with the standardized values
IP_decline <- cbind(standardize(IP), standardize(decline))

##now you have the U again. and matrix multiplication is the %*%. 
##now you have that U and then matrix multipliication by the transpose of the data to get correlation.
##Now it is 2Xn and correlated. so we will transpose it later to make them into columns again. 
##this is why the chol() is transposed I think, you are multiplying the  by U so the columns get correlated
correlated_rates <- U %*% t(IP_decline)

##this is putting things in columns, so 2 columns of data. 
correlated_rates <- t(correlated_rates)

##now we destandardize to make it into the original data again.   
##now we have data that looks like above but is now correlated. 
final.correlated_rates <- cbind(destandardize(correlated_rates[,1], IP), destandardize(correlated_rates[,2], decline))

#individual vectors if needed
IP_correlated <- destandardize(correlated_rates[,1], IP)
decline_correlated <- destandardize(correlated_rates[,2], decline)

###Attempt to calculate the Oil volumne per year
###should need it for 15 years from 2019 because that's when 
###you need the net revenue for
PR <- c()
PR1<-c()
PR2<-c()
##Creates a loop to make a vector for each year
for(i in 1:runs){
  #the first year is the the initial amount
  PR1[i] <- IP_correlated[i]
  Rate_YE <- (1-decline_correlated[i])*IP_correlated[i]
  #the second year is first - decline*inital amount
  PR2[i]<-Rate_YE
  #loops for years 3-16 #not sure if 16 is right??
  for(j in 3:16){
    Rate_YE <- (1-decline_correlated[i])*Rate_YE
    ker <- paste("PR", j, sep="")
    assign(ker, Rate_YE)
  }
  PR[i] <- Rate_YE
}

##anyway, this is a datafram with each column corresponding to Oil Volume for a year
##i have PR16 because it is the rate at the end of year 15 is my thinking
Production_df <- data.frame(PR1,PR2,PR3,PR4,PR5,PR6,PR7,PR8,PR9,PR10,PR11,PR12,PR13,PR14,PR15, PR16)

##now get Oil Volume = 365*(rate_year_begin + rate_year_end)/2
#create matrix
oil_volume <- matrix(nrow=runs,ncol=15)
#do calculation for volume per year to get the barrels per year
for(i in 1:15){
  oil_volume[,i] <- 365*(Production_df[,i]+Production_df[,i+1])/2
}
oil_volume <- data.frame(oil_volume)
###
#now oil_volume is a distribution of the amount of BARRELS harvested per year. Multiply this by the revenue per barrel to get revenue per year!
###

#-----------------------------------------------OPERATING EXPENSES--------------------------------------------------
# A reasonable West Texas cost would be Normally distributed with a mean of $2.25 per barrel with a standard
# deviation of $0.30 per barrel. The expenses would be the same for every well in a given year,
# but could change from year to year with the distribution above.
##May put this in above to subtract from revenue per barrel

#create a vector 1 for each year
y_15 <- rnorm(15, 2.25, .30)
#data frame transposed to make 15 columns
y_15 <- data.frame(t(y_15))
#replicate each year n times
op_cost <- rep(y_15, runs)

#-----------------------------------------------REVENUE RISK-----------------------------------------------------
# Our model represents a typical West Texas scenario with an
# assumed NRI distributed Normally with a mean of 75% and a standard deviation of 2%. This
# calculation is done per well for the entire life of the well.
####NRI IS CONSTANT OVER YEARS!!!###
df_NRI <- matrix(nrow=runs,ncol=15) 
for(i in 1:15){ 
  df_NRI[,i] <- rnorm(runs, .75, .02)
}
df_NRI <- data.frame(df_NRI)

# Import 1st worksheet of data set containing price projections
price <- read_excel("/Users/rogerdugas/Desktop/MSA NC State/1.2 Fall 3/Simulation & Risk/HW1/Analysis_Data.xlsx", 
                    sheet=1, col_names = TRUE, skip=2)

# Only look at next 15 years
price <- price[1:15,]

#Create an empty matrix to contain the simulations
df_price <- matrix(nrow=runs,ncol=length(price$Year)) 
for (i in 1:length(price$Year)){
  df_price[,i]<-rtriangle(n=runs,price$`Low Oil Price`[i],price$`High Oil Price`[i],price$`AEO2018 Reference`[i])
} 

#Fill the matrix
df_price <- data.frame(df_price) #append the matrix with the row sums

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

hist(Drywell_cost, breaks=100)

# Create data frame 
df_dry <- data.frame(Drywell_cost)
summary(Drywell_cost)

# Create histogram of simulated cost of a single dry well
ggplot(df_dry, aes(x=Drywell_cost)) + 
  geom_histogram(binwidth=200000,color="black", fill="white") +
  geom_vline(aes(xintercept=mean(df_dry$Drywell_cost)), color="red", linetype="solid", size=1) +
  #scale_x_continuous(labels = scales::comma) +
  scale_x_continuous(breaks=seq(1000000,17000000,4000000), labels = comma)+
  labs(x = "Cost per Dry Well $") +
  labs(y = "Counts") +
  labs(title = "Simulated Cost of per Dry Well") +
  annotate("text", label ="Mean of $4.46M", x =6500000, y = 6000, color = "red") +
  theme(
    plot.title = element_text(color="black", size=14, face="bold",family="Arial"),
    axis.title.x = element_text(color="dark blue", size=14, face="bold",family="Arial"),
    axis.title.y = element_text(color="dark blue", size=14, face="bold",family="Arial"),
    plot.subtitle = element_text(color="black", size=12, face="bold",family="Arial") 
  )

# Some statistics 
mean(Drywell_cost) # Average cost of a drywell will be ~4.44M
median(Drywell_cost) # Median cost of a drywell will be ~4.24M
max(Drywell_cost) # Max of $15.34M
min(Drywell_cost) # Min of $1.16M

# Inter quartile range for NPV
table_dry <- c(quantile(Drywell_cost, .05),
               quantile(Drywell_cost, .25),
               quantile(Drywell_cost, .75),
               quantile(Drywell_cost, .50),
               quantile(Drywell_cost, .90),
               quantile(Drywell_cost, .95),
               quantile(Drywell_cost, .99))

table_df_dry <- data.frame(table_dry)

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

