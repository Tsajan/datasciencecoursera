best <- function(state, disease) {
  #read the data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  options(warn=-1)
  #Check that state and outcome are valid
  statelist <- unique(data[,7])
  validdisease <- c("heart attack", "heart failure", "pneumonia")
  
  if(!(state %in% statelist)) {
    stop("invalid state")
  }
  else if(!(disease %in% validdisease)) {
    stop("invalid outcome")
  }
  
  #filter data according to state
  statedata <- data[data$State == state,]
  
  ## Return hospital name in that state with lowest 30-day death rate
  if(disease == validdisease[1]) {
    statedata[,11] <- as.numeric(statedata[,11])
    ansrow <- statedata[statedata[,11] == min(statedata[,11], na.rm=TRUE),]
    hospitalname <- ansrow[2]
  }

  else if(disease == validdisease[2]) {
    statedata[,17] <- as.numeric(statedata[,17])
    ansrow <- statedata[statedata[,17] == min(statedata[,17], na.rm=TRUE),]
    hospitalname <- ansrow[2]
  }

  else if(disease == validdisease[3]) {
    statedata[,23] <- as.numeric(statedata[,23])
    ansrow <- statedata[statedata[,23] == min(statedata[,23], na.rm = TRUE),]
    hospitalname <- ansrow[2]
  }
  hospitalname <- hospitalname[!is.na(hospitalname),]
  hospitalname
}