rankall <- function(outcome, num = "best") {
  
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
  
  ## Check that state and outcome are valid
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  if(!(outcome %in% outcomes)) {
    stop("invalid outcome")
  }

  statelist <- unique(data$State)
  index = ifelse(outcome=="heart attack",11,ifelse(outcome=="heart failure",17,23))
  data[,index] <- as.numeric(data[,index])
  names(data)[index] <- "Deaths"
  data <- na.omit(data)
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  
  splitbystates <- split(data, data$State)
  mydata <- lapply(splitbystates, function(x, num) {
    x <- x[order(x$Deaths, x$Hospital.name),]
    num <- ifelse(num == "best", 1, ifelse(num == "worst", length(x), as.numeric(num)))
    return (x$Hospital.Name[num])
  }, num)
  return ( data.frame(hospital=unlist(mydata), state=names(mydata)) )
}