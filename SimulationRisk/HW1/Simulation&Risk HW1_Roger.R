# Simulation & Risk HW1
# Blue Team 5 Fall 3
# Created on 11/7/2018 - Roger Dugas

####### HW ASSIGNEMENT #########
# Simulate possible future values of 2019 drilling costs.
#   o Currently, only previous information is available for 1960 – 2007 due to changes in reporting regulations.
#   o Since the industry has changed tremendously over those decades, only the information from 1990 – 2006 
#     will be useful for this analysis. 2007 was an outlier and should be ignored.
#   o Instead of looking at the distribution of actual costs, the Company’s analysts
#     recommend simulating possible annual changes in costs to get to 2019. They have
#     calculated arithmetic changes in the data set already, but they are open to other options if you explain why you chose them.
#   o Instead of focusing on costs for oil, gas, and dry wells individually, the Company’s
#     analysts recommend to treat them all equally and assume an average cost applies to
#     them all. (HINT: You should have 48 observations. Arithmetic changes from 1991 – 2006.)
#   o A recent report has come out from the U.S. Energy Information Association detailing
#     changes in costs from 2006 to 2019 with the details here:
#   From 2006 to 2012 changes were relatively consistent in their distribution. This distribution is discussed below.
#   From 2012 to 2015 costs tended to decrease on average by 9.17% per year with a maximum of 22% and minimum of 7%.
#   From 2015 to 2018 costs tended to increase on average by 5% per year with a maximum of 6% and minimum of 2%.
#   2019 is forecasted to follow the same increase distribution as from 2015 to 2018.
#   o Previously the Price Analysis group has worked under the assumption that these arithmetic changes from one year to the next from 2006 to 2012 follow a Normal
#     distribution. Use QQ-plots or formal tests to see if you agree.
#   o Build a kernel density estimate of the distribution of arithmetic changes from 2006 to 2012, using the 48 observations described above.
#   Simulate possible future values of 2019 drilling costs under both the assumption of Normality as
#    well as under the kernel density estimate you created (HINT: Run two simulations). Make a
#    recommendation for which one you feel the company should use.

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

#--------------------------------------------------SIMULATION-------------------------------------------------------
# Set the Seed
set.seed(55043)

# P0 0 initial value (2006)
P0 <- 2279.8

set.seed(55043)
P_2019 <-c()
#Normal distribution + triangle Simulation
for(i in 1:10000){
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
  P_2019[i]<- Pt
}

###### Zach's histogram #######
hist(P_2019, breaks=300, xlim=c(0,150000))
abline(v = 2279.8, col="red", lwd=2)

# Round values so that they are whole numbers, in order to do so we need to convert to data.frame 
P_2019_df <- as.data.frame(P_2019)
P_2019_df <- round(P_2019_df$P_2019)
P_2019_df <- as.data.frame(P_2019_df)

# Create Histogram of simulated costs for 2019 using ggplot - Normal
ggplot(P_2019_df, aes(x=P_2019)) + 
  geom_histogram(binwidth=2500,color="black", fill="white") +
  geom_vline(aes(xintercept=2279.8), color="red", linetype="dashed", size=.6) +
  geom_vline(aes(xintercept=10650), color="blue", linetype="dashed", size=.6) +
  geom_vline(aes(xintercept=127844), color="green", linetype="dashed", size=.6) +
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


# Bandwidth to use for a distribution of returns
density(data_vector, bw="SJ-ste") #bandwidth = .07935
K_2019 <-c()
#Kernel Simulation
for(i in 1:10000){
  P0 <- 2279.8;
  Kt <- P0*(1+ rkde(fhat=kde(data_vector, h=.07935), n=1))
  for (j in 1:5) {
    Kt <- P0*(1+ rkde(fhat=kde(data_vector, h=.07935), n=1))^j
  }
  for(k in 1:3) {
    Kt <- Kt*(1-rtriangle(1, a=.07, b=.22, c = .0917))
  }
  for(l in 1:3){
    Kt <- Kt*(1+rtriangle(1, a=.02, b=.06, c =.05))
  }
  K_2019[i] <- Kt
}


##### ZACHs Histogram ######
hist(K_2019, breaks=100)
# , xlim=c(0,150000))
abline(v = 2279.8, col="red", lwd=2)

# Round values so that they are whole numbers, in order to do so we need to convert to data.frame 
K_2019_df <- as.data.frame(K_2019)
K_2019_df <- round(K_2019_df$K_2019)
K_2019_df <- as.data.frame(K_2019_df)

# Create Histogram of simulated costs for 2019 using ggplot - Kernel
ggplot(K_2019_df, aes(x=K_2019)) + 
  geom_histogram(binwidth=500,color="black", fill="white") +
  geom_vline(aes(xintercept=2279.8), color="red", linetype="dashed", size=.6) +
  geom_vline(aes(xintercept=3146), color="blue", linetype="dashed", size=.6) +
  geom_vline(aes(xintercept=18072), color="green", linetype="dashed", size=.6) +
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