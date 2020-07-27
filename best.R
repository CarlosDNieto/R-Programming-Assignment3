## 2.- Finding the best hospital in a state
library(stringr)

## Function that takes in the 2 character abbreviated state name and 
## an outcome name to return the hospital name that hast the lowers
## 30-days mortality rate in that state and outcome.
best <- function(state, outcome){
  library(stringr)
  
  ## Read the outcome data
  data <- read.csv(file.path("data","outcome-of-care-measures.csv"),
                   colClasses = "character")
  
  ## Check that state and outcome are valid
  stateValid <- match(state, unique(data$State))
  if(is.na(stateValid)){
    msg <- paste("Error in best('",state,"', '",outcome,"') : invalid state", sep = "")
    stop(msg)
  }
  
  outcomeValid <- match(outcome, c("heart attack", "heart failure", "pneumonia"))
  if(is.na(outcomeValid)){
    msg <- paste("Error in best('",state,"', '",outcome,"') : invalid outcome", sep = "")
    stop(msg)
  }
  
  ## Process the data (filter na's and by 'State')
  outcome <- gsub(" ", ".", str_to_title(outcome), fixed = TRUE)
  colname <- paste("Hospital.30.Day.Death..Mortality..Rates.from.", outcome, sep = "")
  colnames <- c("Hospital.Name", "State", colname)
  data <- data[, colnames]
  data <- as.data.frame(split(data, data$State)[state])
  names(data) <- c("Hospital.Name", "State", "Death.Rate")
  data[, "Death.Rate"] <- as.numeric(data[, "Death.Rate"])
  data <- data[complete.cases(data), ]
  
  ## Return the hospital name in that state with lowest 30-day death rate
  data[order(data$Death.Rate, data$Hospital.Name), ]$Hospital.Name[1]
  


}

best("TX", "heart attack")
best("TX", "heart failure")
best("MD", "heart attack")
best("MD", "pneumonia")
best("BB", "heart attack")
best("NY", "hert attack")
