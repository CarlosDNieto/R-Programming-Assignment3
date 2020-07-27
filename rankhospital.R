# 3.- Ranking hospitals by outcome in a state

rankhospital <- function(data, state, outcome, num = "best"){
  library(stringr)
  
  ## Read the outcome data
  # data <- read.csv(file.path("data","outcome-of-care-measures.csv"),
  #                  colClasses = "character")
  
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
  data <- data[order(data$Death.Rate, data$Hospital.Name), ]
  
  ## Return the hospital name in that state with lowest 30-day death rate
  if(tolower(num) == "best"){
    return(data$Hospital.Name[1])
  }
  else if(tolower(num) == "worst"){
    return(tail(data,1)$Hospital.Name)
  }
  else{
    return(data$Hospital.Name[num])
  }
}

rankhospital("TX", "heart failure", 4)
rankhospital("MD", "heart attack", "worst")
rankhospital("MN", "heart attack", 5000)
