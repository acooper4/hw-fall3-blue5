############################### Surival Analysis HW 3 Part 3 #############################

# Read in katrina.csv
katrina <- read.csv("/Users/anniecooper/Documents/MSA_2019/Fall 3/Survival Analysis/HW1/katrina.csv", header = TRUE)

# create an ID variable
katrina$ID <- 1:nrow(katrina)

####----------- Helper function to check when pump has been run 12 consecutive hours --------####

check_h12 <- function(array){
  j <- 1
  # 38 because 37-48 is the last 12 hour window
  while (j < 38) {
    # check if the sum of any 12h interval = 12
    if (sum(array[j:(j+11)])==12) {
      # if true, assign interval number. add 1 hour to lag the data (must run for 12 hours prior)
      return(j+12)
      # if false, assign 0 (never ran for 12 hours straight)
    } else if (j == 37) {
      return(0)
    }
    j <- j+1
  }
}

####----------------------------- Test with subset of data --------------------------------####

# Test with 100 rows
#katrina2 <- katrina[483:582, ]
katrina2 <- katrina

# Remove observations with nulls
katrina2 <- katrina2 %>% na.omit(katrina2)

# Determine if pump has been running for 12 consecutive hours
# Initialize empty vector t_h12 (hour after which pump has been running for 12 consecutive hours)
t_h12<- c()

# Loop through each row of katrina2 to get t_h12
for (i in 1:nrow(katrina2)){
  # Create an array of h1:h48 for each row i
  h_array <- array(katrina2[i,c(9:56)])
  # Run helper function to check consecutive runs
  t_h12 <- c(t_h12, check_h12(h_array))
}

# Bind t_h12 column to katrina
katrina2 <- cbind(katrina2, t_h12)


####---------------- Create new dataframe katrina_long to transform data to long format -----------------####

# Initialize empty matrix katrina_long
katrina_long <- data.frame(matrix(ncol = 14, nrow = 0))

# Name columns
colnames(katrina_long) <- c("ID","start","stop","h12","survive","reason","backup","bridgecrane","servo","gear","trashrack","elevation","slope","age")

# Loop through each row of katrina2 to write to katrina_long
for (i in 1:nrow(katrina2)){
  id <-katrina2[i,61]
  t_h12 <-katrina2[i,62]
  t_event <- katrina2[i,58]
  survive <-katrina2[i,57]
  reason <-katrina2[i,59]
  backup <-katrina2[i,1]
  age <-katrina2[i,2]
  bridgecrane <-katrina2[i,3]
  servo <-katrina2[i,4]
  gear <-katrina2[i,5]
  trashrack <-katrina2[i,6]
  slope <-katrina2[i,7]
  elevation <-katrina2[i,8]
  
  if (t_h12 == 0 && survive==1) {
    # if no h12 and no event
    newrow <-data.frame(
      id,
      0,
      48,
      0,
      1,
      reason,
      backup,
      bridgecrane,
      servo,
      gear,
      trashrack,
      slope,
      elevation,
      age
    )
    names(newrow) <- colnames(katrina_long)
    katrina_long <- rbind(katrina_long, newrow)
    
  } else if (t_h12 != 0 && survive==1) {
    # if h12 and no event
    newrow <-data.frame(
      id,
      0,
      (t_h12-1),
      0,
      1,
      reason,
      backup,
      bridgecrane,
      servo,
      gear,
      trashrack,
      slope,
      elevation,
      age
    )
    names(newrow) <- colnames(katrina_long)
    katrina_long <- rbind(katrina_long, newrow)
    newrow <-data.frame(
      id,
      (t_h12-1),
      48,
      1,
      1,
      reason,
      backup,
      bridgecrane,
      servo,
      gear,
      trashrack,
      slope,
      elevation,
      age
    )
    names(newrow) <- colnames(katrina_long)
    katrina_long <- rbind(katrina_long, newrow)
    
  } else if (t_h12 == 0 || t_event < t_h12) {
    # if no h12 or event time before h12
    newrow <-data.frame(
      id,
      0,
      (t_event-1),
      0,
      1,
      0,
      backup,
      bridgecrane,
      servo,
      gear,
      trashrack,
      slope,
      elevation,
      age
    )
    names(newrow) <- colnames(katrina_long)
    katrina_long <- rbind(katrina_long, newrow)
    newrow <-data.frame(
      id,
      (t_event-1),
      t_event,
      0,
      0,
      reason,
      backup,
      bridgecrane,
      servo,
      gear,
      trashrack,
      slope,
      elevation,
      age
    )
    names(newrow) <- colnames(katrina_long)
    katrina_long <- rbind(katrina_long, newrow)
  } else if (t_event == t_h12) {
    # if event time equals the 12 hr time
    newrow <-data.frame(
      id,
      0,
      (t_event-1),
      0,
      1,
      0,
      backup,
      bridgecrane,
      servo,
      gear,
      trashrack,
      slope,
      elevation,
      age
    )
    names(newrow) <- colnames(katrina_long)
    katrina_long <- rbind(katrina_long, newrow)
    newrow <-data.frame(
      id,
      (t_event-1),
      t_event,
      1,
      0,
      reason,
      backup,
      bridgecrane,
      servo,
      gear,
      trashrack,
      slope,
      elevation,
      age
    )
    names(newrow) <- colnames(katrina_long)
    katrina_long <- rbind(katrina_long, newrow)
    
  } else if (t_event > t_h12) {
    #if event time greater than h12
    newrow <-data.frame(
      id,
      0,
      (t_h12-1),
      0,
      1,
      0,
      backup,
      bridgecrane,
      servo,
      gear,
      trashrack,
      slope,
      elevation,
      age
    )
    names(newrow) <- colnames(katrina_long)
    katrina_long <- rbind(katrina_long, newrow)
    newrow <-data.frame(
      id,
      (t_h12-1),
      (t_event-1),
      1,
      1,
      0,
      backup,
      bridgecrane,
      servo,
      gear,
      trashrack,
      slope,
      elevation,
      age
    )
    names(newrow) <- colnames(katrina_long)
    katrina_long <- rbind(katrina_long, newrow)
    newrow <-data.frame(
      id,
      (t_event-1),
      t_event,
      1,
      0,
      reason,
      backup,
      bridgecrane,
      servo,
      gear,
      trashrack,
      slope,
      elevation,
      age
    )
    names(newrow) <- colnames(katrina_long)
    katrina_long <- rbind(katrina_long, newrow)
    
  }

}

# Remove row 139 because t_h12 was 49 (lagged)
katrina_long <- katrina_long[-139,]

####------------------------------- Cox model with the lagged data ---------------------------------####

fit_lag <- coxph(Surv(start, stop, event = reason %in% c(2, 3)) ~ backup + bridgecrane + servo + 
                   trashrack + elevation + slope + age + h12, data = katrina_long)
summary(fit_lag)
# R^2 is incorrect because of the long format

# concordance should work correctly
concordance(fit_lag)

###-----------------------------------COXPH() RSQ Function---------------------------------####

rsq_coxph <- function(obj, id = NULL){
  
  # get identifier of which rows of ID to pick off
  rownum <- as.numeric(names(residuals(obj)))
  nsubj <- length(rownum)
  if(!is.null(id)){
    # number of unique subjects
    nsubj <- length(unique(id[rownum]))
  }
  # df <- length(coef(obj))
  lr <- abs(2*diff(obj$loglik))
  rsq <- 1 - exp(-lr/nsubj)
  max_rsq <- 1 - exp(2*obj$loglik[1]/nsubj)
  scaled_rsq <- rsq/max_rsq
  rsqd <- c(rsq, max_rsq, scaled_rsq)
  names(rsqd) <- c("R2", "MaxR2", "ScaledR2")
  rsqd
}

# Calculate R^2 using ID (correct number of observations)
rsq_coxph(fit_lag, id = katrina_long$ID) # voila! same as summary(fit)$rsq


####-------------------------------Survival Plot----------------------------------####

ggsurvplot(survfit(fit_lag, katrina_long, id=katrina_long$ID), data = katrina,
           #palette = c("black", "purple"), legend.labs = c("no", "yes"),
           legend.title = "Motor Failure", break.y.by = 0.1,
           xlab = "Hour", ylab = "Survival Probability",
           conf.int = FALSE)

