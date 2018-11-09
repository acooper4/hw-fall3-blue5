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



#



#



#


#




