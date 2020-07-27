outcome <- read.csv("data/outcome-of-care-measures.csv",
                    colClasses = "character")
head(outcome)
ncol(outcome) ## 46 columns
nrow(outcome) ## 4706 rows
names(outcome)
summary(outcome)

## 1.- Simple histogram of 30- day death ratese from heart attack
## (column 11 in the outcome dataset)
outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11])