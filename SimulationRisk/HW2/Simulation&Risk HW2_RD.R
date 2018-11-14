# Simulation & Risk HW2
# Blue Team 5 Fall 3
# Created on 11/12/2018 - Roger Dugas

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

#-------------------------------------------------IMPORT/CLEAN DATASET-----------------------------------------------
# Import 2nd worksheet of data set containing drilling data from 1960 to 2007
drill <- read_excel("/Users/rogerdugas/Desktop/MSA NC State/1.2 Fall 3/Simulation & Risk/HW1/Analysis_Data.xlsx", 
                    sheet=2, col_names = TRUE, skip=2)

# Quick look at the structure of our data, and some simple manipulation
str(drill) # We have 48 observations and 7 variables 
summary(drill) # SUmmary statistics
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

str(data_vector)
data_vector1 <- as.data.frame(data_vector)

# QQ plot in ggplot
p <- ggplot(data_vector1, aes(sample = data_vector))
p + stat_qq() + stat_qq_line() + ggtitle("Normal QQ Plot") +
  xlab("Theoretical Quantiles") + ylab("Sample Quantiles") +
  theme(
    plot.title = element_text(color="black", size=14, face="bold.italic",family="Times New Roman"),
    axis.title.x = element_text(color="#993333", size=14, face="bold",family="Times New Roman"),
    axis.title.y = element_text(color="#993333", size=14, face="bold",family="Times New Roman")
  )

#--------------------------------------------------####EXPENSES####-------------------------------------------------
#-----------------------------------------SEISMIC AND LEASE COSTS YEAR 0--------------------------------------------
set.seed(55043)
# Simulate Seismic with normal distribution N(3,.35)
# Seismic costs per section is $43,000
seismic <-c()
per_acre <- c()
P0 <- 43000;
A0 <- 960;
for(i in 1:10000){
  Pt <- P0*(rnorm(n=1, mean=3, sd=.35))
  seismic[i]<- Pt
  At <- A0*(rnorm(n=1, mean=600, sd=50))
  per_acre[i]<- At
}

seismic <- P0*(rnorm(n=10000, mean=3, sd=.35))



summary(seismic)
df_seismic <- data.frame(seismic)

# Create Histogram of simulated seismic costs for year 0 using ggplot - Normal
ggplot(df_seismic, aes(x=seismic)) + 
  geom_histogram(binwidth=2500,color="black", fill="white") +
  geom_vline(aes(xintercept=mean(df_seismic$seismic)), color="red", linetype="dashed", size=1) +
  scale_x_continuous(labels = scales::comma) +
  labs(x = "Seismic Cost per Well $") +
  labs(y = "Counts") +
  labs(title = "Simulation of Seismic Costs per Well for Year 0") +
  theme(
    plot.title = element_text(color="black", size=14, face="bold",family="Times New Roman"),
    axis.title.x = element_text(color="#993333", size=14, face="bold",family="Times New Roman"),
    axis.title.y = element_text(color="#993333", size=14, face="bold",family="Times New Roman"),
    plot.subtitle = element_text(color="black", size=12, face="bold",family="Times New Roman")
)

# Simulate acres per well with normal distribution N(600,50)
# Price per acre is $960
A0 <- 960; 
per_acre <- c()
for(i in 1:10000){
  At <- A0*(rnorm(n=1, mean=600, sd=50))
  per_acre[i]<- At
}
summary(per_acre)
df_per_acre <- data.frame(per_acre)

seismic + per_acre

# Create Histogram of simulated lease costs for year 0 using ggplot - Normal
ggplot(df_per_acre, aes(x=per_acre)) + 
  geom_histogram(binwidth=5000,color="black", fill="white") +
  geom_vline(aes(xintercept=mean(df_per_acre$per_acre)), color="red", linetype="dashed", size=1) +
  scale_x_continuous(labels = scales::comma) +
  labs(x = "Lease Cost per Well $") +
  labs(y = "Counts") +
  labs(title = "Simulation of Lease Cost per Well for Year 0") +
  theme(
    plot.title = element_text(color="black", size=14, face="bold",family="Times New Roman"),
    axis.title.x = element_text(color="#993333", size=14, face="bold",family="Times New Roman"),
    axis.title.y = element_text(color="#993333", size=14, face="bold",family="Times New Roman"),
    plot.subtitle = element_text(color="black", size=12, face="bold",family="Times New Roman")
)

#---------------------------------------------------COMPLETION COSTS-----------------------------------------------
# We hypothesize that his cost is Normally distributed with a mean of $390,000 and a standard deviation of $50,000.
completion <- c()
for(i in 1:10000){
  C0 <- 1; 
  Ct <- C0*(rnorm(n=1, mean=390000, sd=50000))
  completion[i]<- Ct
}
summary(completion)
df_completion <- data.frame(completion)
hist(completion, breaks = 50)

# Create Histogram of simulated completion costs for year 0 using ggplot
ggplot(df_completion, aes(x=completion)) + 
  geom_histogram(binwidth=5000,color="black", fill="white") +
  geom_vline(aes(xintercept=mean(df_completion$completion)), color="red", linetype="dashed", size=1) +
  scale_x_continuous(labels = scales::comma) +
  labs(x = "Completion Costs per Well $") +
  labs(y = "Counts") +
  labs(title = "Simulation of Completion Cost per Well") +
  theme(
    plot.title = element_text(color="black", size=14, face="bold",family="Times New Roman"),
    axis.title.x = element_text(color="#993333", size=14, face="bold",family="Times New Roman"),
    axis.title.y = element_text(color="#993333", size=14, face="bold",family="Times New Roman"),
    plot.subtitle = element_text(color="black", size=12, face="bold",family="Times New Roman")
  )
#--------------------------------------------------PROFESSIONAL OVERHEAD--------------------------------------------
# We believe the salary and benefit cost is best represented by a triangular distribution,
# with a most likely cost as $215,000, with a minimum of $172,000 and a maximum of $279,500.
# This will remain constant across the lifetime of a well. only if the well is not dry.
salary <- c()
for(i in 1:10000){
    S0 <- 1; 
    St <- S0*(rtriangle(1, a=172000, b=279500, c=215000))
  salary[i] <- St
}
summary(salary)
df_salary <- data.frame(salary)
hist(salary, breaks = 50)

# Create Histogram of simulated professional overhead using ggplot
ggplot(df_salary, aes(x=salary)) + 
  geom_histogram(binwidth=5000,color="black", fill="white") +
  geom_vline(aes(xintercept=mean(df_salary$salary)), color="red", linetype="dashed", size=1) +
  scale_x_continuous(labels = scales::comma) +
  labs(x = "Professional Overhead Costs per Well $") +
  labs(y = "Counts") +
  labs(title = "Simulation of Professional Overhead Cost per Well") +
  theme(
    plot.title = element_text(color="black", size=14, face="bold",family="Times New Roman"),
    axis.title.x = element_text(color="#993333", size=14, face="bold",family="Times New Roman"),
    axis.title.y = element_text(color="#993333", size=14, face="bold",family="Times New Roman"),
    plot.subtitle = element_text(color="black", size=12, face="bold",family="Times New Roman")
  )

#----------------------------------------------PRODUCTION RISK--------------------------------------------------
# The IP’s follow a Lognormal distribution with a mean of 420 BOPD and a standard deviation of 120 BOPD.
# The underlying Normal distribution for this Lognormal has a mean of 6 and standard deviation of 0.28.
# At year 1
IP <- c()
for(i in 1:10000){
  I0 <- 1; 
  It <- I0*(rnorm(n=1, mean=6, sd=.28))
  IP[i]<- It
}
summary(IP)
df_IP <- data.frame(IP)
hist(IP, breaks = 50)

# Decline rate 
decline <- c()
for(i in 1:10000){
  D0 <- 1; 
  Dt <- D0*(runif(n=1, min = .15 , max = .32 ))
  decline[i]<- Dt
}
summary(decline)
df_decline <- data.frame(decline)
hist(decline, breaks = 50)


#----------------------------------------CREATE FINAL EXPENSE DATA TABLE------------------------------------------

expense <- cbind(df_seismic, df_per_acre, df_completion, df_salary)




# NPV <-  -intitial costs + FNRevenue*(1+WACC)^-1 + FNRevenue*(1+WACC)^-2 + .... all the way to 15 years 

# Calculate the initial costs 
#totalcost_2019 <- numeric()
#for(i in 1:10000) {
#  seismic <- 43000*rnorm(n=1,mean=3, sd=.35)
#  lease <- 960*rnorm(n=1, mean=600, sd=50)
#  salary  <- rtriangle(n=1, a=172000, b=279500, c=215000)
#  totalcost <- seismic + lease + salary
#  totalcost_2019[i] <- totalcost
#}

#-------------------------------------------------YEAR 0 INITIAL COSTS------------------------------------------
# Need to bring in drilling costs from HW 1
# P0 0 initial value (2006)
set.seed(55043)
P0 <- 2279.8
drillcost <-c()
#Normal distribution + triangle Simulation
for(i in 1:100000){
  P0 <- 2279.8; 
  Pt <- P0*(1+rnorm(n=1, mean=.1314913, sd=.1784372))
  for (j in 1:5) {
    Pt <- Pt*(1+rnorm(n=1, mean=.1314913, sd=.1784372))^j
  } 
  for(k in 1:3) {
    Pt <- Pt*(1-rtriangle(1, a=.07, b=.22, c = .0917))
  } 
  for(l in 1:3){
    Pt <- Pt*(1+rtriangle(1, a=.02, b=.06, c =.05))
  }
  drillcost[i]<- Pt
}

# Quick Histogram of drill cost distribution
hist(drillcost, breaks=600, xlim=c(0,150000))
abline(v = 2279.8, col="red", lwd=2)

# Create a vector with other initial costs (seismic, lease, complete, salary/overhead)
seismic <- 43000*rnorm(n=100000,mean=3, sd=.35)
lease <- 960*rnorm(n=100000, mean=600, sd=50)
complete <- rnorm(n=100000, mean=390000, sd=50000)
salary  <- rtriangle(n=100000, a=172000, b=279500, c=215000)

# Combine all costs (including drill cost from HW1) into one vector
initialcost <- seismic + lease + complete + 15*salary + drillcost
# See it as a data frame 
initialcost_df <- data.frame(initialcost)

# Quick histogram showing distribution of initital costs
hist(initialcost, breaks = 50)

mean(initialcost)
















#--------------------------------------------CODING WORK------------------------------------------------------
# Round values so that they are whole numbers, in order to do so we need to convert to data.frame 
drillcost_df <- as.data.frame(drillcost)
drillcost_df <- round(drillcost_df$drillcost)
drillcost_df <- as.data.frame(drillcost_df)

# Create Histogram of simulated costs for 2019 using ggplot - Normal
ggplot(drillcost_df, aes(x=drillcost)) + 
  geom_histogram(binwidth=2500,color="black", fill="white") +
  geom_vline(aes(xintercept=2279.8), color="red", linetype="dashed", size=.6) +
  scale_x_continuous(labels = scales::comma) +
  labs(x = "Average Drilling Cost $") +
  labs(y = "Counts") +
  labs(title = "Simulation of Drilling Costs for 2019") +
  theme(
    plot.title = element_text(color="black", size=14, face="bold",family="Times New Roman"),
    axis.title.x = element_text(color="#993333", size=14, face="bold",family="Times New Roman"),
    axis.title.y = element_text(color="#993333", size=14, face="bold",family="Times New Roman"),
    plot.subtitle = element_text(color="black", size=12, face="bold",family="Times New Roman")
  )







#-----------------------------------------------REVENUE RISK-----------------------------------------------------
# Import 1st worksheet of data set containing price projections
price <- read_excel("/Users/rogerdugas/Desktop/MSA NC State/1.2 Fall 3/Simulation & Risk/HW1/Analysis_Data.xlsx", 
                    sheet=1, col_names = TRUE, skip=2)

# Only look at next 15 years
price <- price[1:15,]

#Set number of runs
runs <-10000 

#Create an empty matrix to contain the simulations
df_price <- matrix(nrow=runs,ncol=length(price$Year)) 
for (i in 1:length(price$Year)){
  df_price[,i]<-rtriangle(n=runs,price$`Low Oil Price`[i],price$`High Oil Price`[i],price$`AEO2018 Reference`[i])
  df_price[,i] <- df_price[,i]*(1+.1)^-(i-1)
} 
#Fill the matrix
df_price <- data.frame(df_price) #append the matrix with the row sums

Revenueyear0 <- rowSums(df_price)
df_price$X2 * (1+.1)^-1








# Our model represents a typical West Texas scenario with an
# assumed NRI distributed Normally with a mean of 75% and a standard deviation of 2%. This
# calculation is done per well for the entire life of the well.
NRI <- c()
for(i in 1:10000){
  N0 <- 1; 
  Nt <- N0*(rnorm(n=1, mean=.75, sd=.2))
  NRI[i]<- Nt
}
summary(NRI)
df_NRI <- data.frame(NRI)
hist(NRI, breaks = 30)

#-----------------------------------------------OPERATING EXPENSES--------------------------------------------------
# A reasonable West Texas cost would be Normally distributed with a mean of $2.25 per barrel with a standard
# deviation of $0.30 per barrel. The expenses would be the same for every well in a given year,
# but could change from year to year with the distribution above.
exp <- c()
for(i in 1:10000){
  E0 <- 1; 
  Et <- E0*(rnorm(n=1, mean=2.25, sd=.3))
  exp[i]<- Et
}
summary(exp)
df_exp <- data.frame(exp)
hist(exp, breaks = 50)

# Severance Taxes. State taxes levied on produced oil and gas are assumed to be a constant value
# of 4.6% of revenue. Taxes are applied after the NRI.


# The operating expenses are subtracted from the gross sales to arrive at net sales.




#------------------------------------------COST OF A SINGLE DRY WELL------------------------------------------
# Cost of a dry well 
# Drilling costs, Seismic, Lease, Salary just for Year 0
Drywell_cost <- drillcost + seismic + lease + salary

hist(Drywell_cost, breaks=100, xlim = c(700000,1500000))
mean(Drywell_cost) # Average cost of a drywell will be $947,139

#-------------------------------------------NPV OF SINGLE WET WELL-------------------------------------------

