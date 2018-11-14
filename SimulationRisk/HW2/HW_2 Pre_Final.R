library(readxl)
library(dplyr)
library(graphics)
library(ks)
library(RTriangle) 
library(triangle)
#reads in excel file 2007 and before
historical <- read_excel("E:/Fall/Simulation and Risk/HW1/Analysis_Data.xlsx", sheet=2, col_names = TRUE, skip=2)


#changes the column names to make the dataset more readable
colnames(historical) <- c("year", "oil", "gas", "drywell", "oil_return", "gas_return", "drywell_return")

#averages cost (only 2006 matters)
historical <- historical %>% mutate(avg = (oil+gas+drywell)/3)  

#takes out 2007
historical_06 <- historical[-48,]

#makes the return columns numeric
historical_06 <- historical_06 %>% mutate(oil_return=as.numeric(oil_return)) %>%
    mutate(gas_return=as.numeric(gas_return)) %>% mutate(drywell_return=as.numeric(drywell_return))
    

#takes out everything before 1991 and leaves us with 3 rows of returns (48 observations)
historical_new <- historical_06[32:47,]


#creates a vector of the 3 return columns. this is the 48 observations in the HW hint. 3 columns*16 observations
sim_stats <- c(historical_new$oil_return, historical_new$gas_return, historical_new$drywell_return)


##will get the average (or expected) return and SD over these 48 observations
mean(sim_stats) #.1314913
sd(sim_stats) #.1784372




#The original data of returns is fairly normal as seen by QQPlot
qqnorm(sim_stats); qqline(sim_stats)
hist(sim_stats)

#begin simulation of the return
#set seed
set.seed(55043)
#initial value, P0, is that of 2006
P0 <- 2279.8





####create a loop
#pass Pt through the loop from 2006 to 2007 to 2008 to 2009 to 2010 to 2011 to 2012
#pass Pt to the first triangle distribution
#pass Pt to the second triangle distribution.
#now you have 2019

set.seed(55043)
#initialize P_2019 before the loop
P_2019 <-c()
#Normal distribution + triangle Simulation
for(i in 1:10000){
    #initialize the cost
    P0 <- 2279.8; 
    Pt <- P0*(1+rnorm(n=1, mean=.1314913, sd=.1784372))
    #loop through 2007-2012 with normal
    for (j in 1:5) {
       Pt <- Pt*(1+rnorm(n=1, mean=.1314913, sd=.1784372))^j
    }
    #rtriangle() is a random triangle function. #2013-2015
    for(k in 1:3) {
        Pt <- Pt*(1-rtriangle(1, a=.07, b=.22, c = .0917))
    }
    #2016-2019
    for(l in 1:3){
        Pt <- Pt*(1+rtriangle(1, a=.02, b=.06, c =.05))
    }
    
    #assigns the value of Pt (investment returns in 2019) to the i'th element of P_2019
    #P_2019 is our simulation of the possible events in the year 2019
    P_2019[i]<- Pt
}

Pt
P0
P_2019

P_2019 <- data.frame(P_2019, 2279.8)





#minimum value
P_2019[which.min(P_2019)]

#maximum value
P_2019[which.max(P_2019)]

#inter quartile range
quantile(P_2019, .25)
quantile(P_2019, .75)
quantile(P_2019, .50)
quantile(P_2019, .90)
quantile(P_2019, .95)
quantile(P_2019, .99)



mean(P_2019)
mean(P_2019)/P0
median(P_2019)

##tells the bandwidth to use for a distribution of returns
density(sim_stats, bw="SJ-ste") #bandwidth = .07935
K_2019 <-c()
#Kernel Simulation
for(i in 1:10000){
    #initialize the cost
    P0 <- 2279.8; #K_2019
    Kt <- P0*(1+ rkde(fhat=kde(sim_stats, h=.07935), n=1))
    for (j in 1:5) {
       #2006-2012 kernel estimate
        Kt <- P0*(1+ rkde(fhat=kde(sim_stats, h=.07935), n=1))^j
    }
    #2013-2015 Triangle distribution
    for(k in 1:3) {
        Kt <- Kt*(1-rtriangle(1, a=.07, b=.22, c = .0917))
    }
    #2016-2019 Triangle Distribution
    for(l in 1:3){
        Kt <- Kt*(1+rtriangle(1, a=.02, b=.06, c =.05))
    }
    
    
    K_2019[i] <- Kt
}

K_2019



#minimum value
K_2019[which.min(K_2019)]

#maximum value
K_2019[which.max(K_2019)]

quantile(K_2019, .25)
quantile(K_2019, .75)
quantile(K_2019, .50)
quantile(K_2019, .90)
quantile(K_2019, .95)
quantile(K_2019, .99)



mean(K_2019)
mean(K_2019)/P0
median(K_2019)



#############################################################################
############################ HW 2 Code Begin ################################
#############################################################################


#--------------------------------------------------####COSTS####-------------------------------------------------
#--------------------------------------------------SEISMIC COSTS--------------------------------------------
set.seed(55043)
# Simulate Seismic with normal distribution N(3,.35)
# Seismic costs per section is $43,000
seismic <- 43000*rnorm(n=100000,mean=3, sd=.35)
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
#-----------------------------------------------------LEASE COST-----------------------------------------------
# Simulate acres per well with normal distribution N(600,50)
# Price per acre is $960
lease <- 960*rnorm(n=100000, mean=600, sd=50)
summary(lease)
df_lease <- data.frame(lease)

# Create Histogram of simulated lease costs for year 0 using ggplot - Normal
ggplot(df_lease, aes(x=lease)) + 
    geom_histogram(binwidth=5000,color="black", fill="white") +
    geom_vline(aes(xintercept=mean(df_lease$lease)), color="red", linetype="dashed", size=1) +
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
complete <- rnorm(n=100000, mean=390000, sd=50000)
summary(complete)
df_complete <- data.frame(complete)
hist(complete, breaks = 50)

# Create Histogram of simulated completion costs for year 0 using ggplot
ggplot(df_complete, aes(x=complete)) + 
    geom_histogram(binwidth=5000,color="black", fill="white") +
    geom_vline(aes(xintercept=mean(df_complete$complete)), color="red", linetype="dashed", size=1) +
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
salary  <- rtriangle(n=100000, a=172000, b=279500, c=215000)
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



#####Production Risk

##IP should be a distribution for the first year
##then take this IP and it will be the first RATE_Year_Begin
##then the RATE_Year_End will be the RATE_Year_Begin of the next year

#simulates values for the IP with lognormal distribution
IP <- rlnorm(n=10000, meanlog=6, sdlog=.28) #not suree why, but looks like you should use normal numbers

#unform distribution for rate of decline
decline <- runif(10000, min=.15, max=.32)

#put these together as two independent columns
IP_decline <- data.frame(IP,decline)

##Next I will make these correlated
###Then, since the IP is only for the first year I can do a IP[i] in the 
###first iteration and a decline[i] throughout since assumed not to change over years
####Then I should make a loop that calculates the volume per year
#####Then multiply this volume per year by the revenue risk calculations

##corelation = .64##############################################
####################               ###################
                #####################

##here is the correlation matrix
R <- matrix(data=cbind(1,.64, .64, 1), nrow=2)
R
##this is the important function: decomposition of R
##little t() takes the transpose of the matrix. lets you multiply easier
U <- t(chol(R))

##creating functions. originally variance was 1, but we standardize here.
standardize <- function(x){
    x.std = (x - mean(x))/sd(x)
    return(x.std)
}

##this just does the opposite and destandardizes
##this helps at the end. 
destandardize <- function(x.std, x){
    x.old = (x.std * sd(x)) + mean(x)
    return(x.old)
}

##combine into one matrix with the standardized values
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
for(i in 1:10000){
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
View(Production_df)

##now get Oil Volume = 365*(rate_year_begin + rate_year_end)/2
#create matrix
oil_volume <- matrix(nrow=10000,ncol=15)
#do calculation for volume per year to get the barrels per year
for(i in 1:15){
    oil_volume[,i] <- 365*(Production_df[,i]+Production_df[,i+1])/2
}
oil_volume <- data.frame(oil_volume)
View(oil_volume)
class(oil_volume)
###
#now oil_volume is a distribution of the amount of BARRELS harvested per year. Multiply this by the revenue per barrel to get revenue per year!
###


####################################################################
#################Operating Expenses#################################
####################################################################
##May put this in above to subtract from revenue per barrel

#download mefa so the rep(df, n) function
library(mefa)
#create a vector 1 for each year
y_15 <- rnorm(15, 2.25, .30)
#data frame transposed to make 15 columns
y_15 <- data.frame(t(y_15))
#replicate each year n times
op_cost <- rep(y_15, 10000)
View(y_15)
View(op_cost)

###############################################################################################################
#########################################Revenue Risk##########################################################
###############################################################################################################

##NRI
####NRI IS CONSTANT OVER YEARS!!!###
#Set number of runs
runs <-10000 
df_NRI <- matrix(nrow=runs,ncol=15) 
for(i in 1:15){ 
    df_NRI[,i] <- rnorm(runs, .75, .02)
}
df_NRI <- data.frame(df_NRI)
View(df_NRI)   

# Only look at next 15 years
price <- price[1:15,]
View(price)


#Create an empty matrix to contain the simulations
df_price <- matrix(nrow=runs,ncol=length(price$Year)) 
##This seems to give the revenue per barrel, we should multiply by production cost
for (i in 1:length(price$Year)){
    df_price[,i]<-rtriangle(n=runs,price$`Low Oil Price`[i],price$`High Oil Price`[i],price$`AEO2018 Reference`[i])
    df_price[,i] <- df_price[,i]#*(1+.1)^-(i-1) #do we need anything after [,i]?
        ## my thinking is that we just do a triangle distribution for each individual year and that takes care of it?
                
} 

df_price <- data.frame(df_price)


############## multiply the revenue distribution per year by the barrels sold distribution per year and the NRI distribution per year############
#this will give us the revenue per year
#idea is to multiply the elements of each year for each column in the 3 matrices together
#df_price-op_cost, df_NRI, oil_volume
revenue_risk <- matrix(nrow=10000,ncol=15)
for(i in 1:15){
    revenue_risk[,i] <- (df_price[,i] - op_cost[,i])*df_NRI[,i]*oil_volume[,i]
}

####NRI IS CONSTANT OVER YEARS!!!###


##View the means to make sure they are decreasing over years and to validate code (they do decrease)
means <-c()
for(i in 1:15){
    means[i] <- mean(revenue_risk[,i])
}
means
View(revenue_risk)


rev_after_tax <- (1-.046)*revenue_risk
View(rev_after_tax)
PV_rev <- rev_after_tax

########################################################################
#######################NET PRESENT VALUE CALCULATION####################
########################################################################
wacc<- .1

#gives the Future Net Revenue
for(i in 1:15) {
    PV_rev[,i] <- PV_rev[,i]*(1+wacc)^-(i-1)
}
#use rowSums to add the rows together to get the total revenue for the 15 years
Total_PV_rev <-  rowSums(PV_rev) - initial_cost
View(Total_PV_rev)

#Initial Costs/ Costs not yet accounted for (mostly from Year 0)
initial_costs <- seismic + lease + complete + salary
View(initial_costs)

#FNR - Final Net Revenue
FNR <- Total_PV_rev - initial_costs
View(FNR)
mean(FNR)
hist(FNR)

################################################################################
############################ JUNK CODE I THINK #################################
################################################################################


##reads in the data for projections from 2019 to 2050
price <- read_excel("E:/Fall/Simulation and Risk/HW1/Analysis_Data.xlsx", sheet=1, col_names = TRUE, skip=2)
#add returns from the max value
Projection$max_change <- (Projection$`High Oil Price`- lag(Projection$`High Oil Price`,1))/(lag(Projection$`High Oil Price`,1))
#add returns from the minimumm value
Projection$min_change <- (Projection$`Low Oil Price` - lag(Projection$`Low Oil Price`,1))/(lag(Projection$`Low Oil Price`,1))
#take out 2019 so that returns don't have missing values
projection_new <- Projection[2:32,]

#Here I take the mean of the min and max in 2019 to get the starting value.
##I don't think this is correct because he wants us to simulate 2019 also
###we should ask labarr where to get data from 2018 for oil prices
mean(c(Projection$`High Oil Price`[1],Projection$`Low Oil Price`[1])) #57.74486
  

##Makes a vector of the highest and lowest oil changes
proj_mm <- c(projection_new$min_change,projection_new$max_change)
##Takes the min, max, and mean of these oil prices for a triangle distribution
min(proj_mm) #-0.02787407
max(proj_mm) #0.2271513
mean(proj_mm)# 0.0259336

rev_proj <-c()
for(i in 1:10000) {
    P0 <- 57.74486
    Pt <- P0*(1+rtriangle(n=1,a=-0.02787407, b=0.2271513, c=0.0259336)) #I think we get quantity sold by multiplying
                                                                        #production risk simulation values
    for(j in 1:14){
        Pt <- Pt*(1+rtriangle(n=1,a=-0.02787407, b=0.2271513, c=0.0259336))
    }
    rev_proj[i]=Pt
}
hist(rev_proj, breaks=500)