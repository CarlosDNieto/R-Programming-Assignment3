## 4.- Ranking hospitals in all states

rankall <- function(outcome, num = "best"){
  ## Read the data
  data <- read.csv(file.path("data","outcome-of-care-measures.csv"),
                   colClasses = "character")
  data <- data[order(data$State),]
  states <- unique(data$State)
  hospitals <- character(length = length(states))
  for(i in 1:length(states)){
    hospital <- rankhospital(data, states[i], outcome, num)
    hospitals[i] <- hospital
  }
  df <- data.frame(hospitals, states)
  #df <- df[order(df$states), ]
  df
  
}

# head(rankall("heart attack", 20), 10)
# tail(rankall("pneumonia", "worst"), 3)
# tail(rankall("heart failure"), 10)
# 
# rankhospital(data, "WI", "pneumonia", "worst")
# rankhospital(data, "WV", "pneumonia", "worst")
# rankhospital(data, "WY", "pneumonia", "worst")
best("SC", "heart attack")
best("NY", "pneumonia")
best("AK", "pneumonia")

rankhospital(data, "NC", "heart attack", "worst")
rankhospital(data, "WA", "heart attack", 7)
rankhospital(data, "TX", "pneumonia", 10)
rankhospital(data, "NY", "heart attack", 7)
r <- rankall("heart attack", 4)
as.character(subset(r, states == "HI")$hospitals)
r <- rankall("pneumonia", "worst")
as.character(subset(r, states == "NJ")$hospitals)
r <- rankall("heart failure", 10)
as.character(subset(r, states == "NV")$hospitals)
