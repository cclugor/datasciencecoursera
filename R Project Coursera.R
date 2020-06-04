## week 4 - Coursera R Project
## Cindy Lugo

setwd("C:/Users/lugor/Downloads/rprog_data_ProgAssignment3-data")

outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)
ncol(outcome)

### hist of 30-day death rates from heart attack ####
outcome[, 11] <- as.numeric(outcome[, 11])
## You may get a warning about NAs being introduced; that is okay
hist(outcome[, 11])


#### Finding the best hospital in a state ####

best <- function(state, outcome) {
  ## Read outcome data
  x<-read.csv("outcome-of-care-measures.csv")
  ## Check that state and outcome are valid
  if(all(state %in% x$State)==F){
    stop("invalid state")
  } else if (all(outcome %in% c("heart attack","pneumonia","heart failure"))==F){
    stop("invalid outcome")
  } else if (all(outcome %in% c("heart attack","pneumonia","heart failure"))==T&all(state %in% x$State)==T){
    ## Return hospital name in that state with lowest 30-day death
    if(outcome=="heart attack"){y<-x$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack
    } else if(outcome=="heart failure"){y<-x$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure
    }  else if(outcome=="pneumonia"){y<-Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia}
y    
$ State
    $ Hospital.Name
    x$`heart attack`<-x$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack
    x$`heart failure`<-x$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure
    x$`pneumonia`<-x$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia
    x$outcome
        ## rate
  }
      
  }
  
  
outcome<-"heart failure"
state<-"T"



head(x,1)
str(x)


# 
# Write a function called best that take two arguments: the 2-character abbreviated name of a state and an
# outcome name. The function reads the outcome-of-care-measures.csv file and returns a character vector
# with the name of the hospital that has the best (i.e. lowest) 30-day mortality for the specified outcome
# in that state. The hospital name is the name provided in the Hospital.Name variable. The outcomes can
# be one of “heart attack”, “heart failure”, or “pneumonia”. Hospitals that do not have data on a particular
# outcome should be excluded from the set of hospitals when deciding the rankings.
# Handling ties. If there is a tie for the best hospital for a given outcome, then the hospital names should
# be sorted in alphabetical order and the first hospital in that set should be chosen (i.e. if hospitals “b”, “c”,
# and “f” are tied for best, then hospital “b” should be returned).
# The function should use the following template.
# best <- function(state, outcome) {
#   ## Read outcome data
#   ## Check that state and outcome are valid
#   ## Return hospital name in that state with lowest 30-day death
#   ## rate
# }
# The function should check the validity of its arguments. If an invalid state value is passed to best, the
# function should throw an error via the stop function with the exact message “invalid state”. If an invalid
# outcome value is passed to best, the function should throw an error via the stop function with the exact
# message “invalid outcome”.
