rankhospital <- function(state, disease, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  options(warn=-1)
  #Convert the character data into numeric of the data you need!
  data[,11] <- as.numeric(data[,11])
  data[,17] <- as.numeric(data[,17])
  data[,23] <- as.numeric(data[,23])
  data <- na.omit(data)
  ## Check that state and outcome are valid
  statelist <- unique(data[,7])
  validdisease <- c("heart attack", "heart failure", "pneumonia")
  if(!(state %in% statelist)) {
    stop("invalid state")
  }
  else if(!(disease %in% validdisease)) {
    stop("invalid outcome")
  }
  
  #filter the state data && take only the subset that you want
  statedata <- subset(data, State == state)
  
  ## Return hospital name in that state with the given rank 30-day death rate
  if(disease == validdisease[1]) {
    statedata <- statedata[order(statedata[,11], statedata[,2], na.last = TRUE),2]
  }
  
  else if(disease == validdisease[2]) {
    statedata <- statedata[order(statedata[,17], statedata[,2], na.last = TRUE),2]
  }

  else if(disease == validdisease[3]) {
    statedata <- statedata[order(statedata[,23],statedata[,2], na.last = TRUE),2]
  }
  statedata <- na.omit(statedata)
  num <- ifelse(num == "best", 1, ifelse(num == "worst", length(statedata), as.numeric(num)))
  statedata[num]
}