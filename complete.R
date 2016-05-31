complete <- function(directory, id=1:332) {
  file_lst <- dir(directory)
  mydata <- data.frame(id=numeric(), nobs=numeric())
  dataset <- data.frame(id=numeric(), nobs=numeric())
  for(itm in file_lst) {
    itmid <- as.numeric(unlist(strsplit(itm,split=".",fixed=TRUE))[1])
    count <- 1L
    if(itmid %in% id) {
      temp_dataset <- read.csv(paste(directory,itm,sep="/"), header=TRUE)
      cccount <- nrow(temp_dataset[complete.cases(temp_dataset),])
      mydata[count,] <- data.frame(id=itmid, nobs=cccount)
      dataset <- rbind(dataset, mydata)
      rm(temp_dataset)
      count <- count + 1
    }
    else {
      next()
    }
  }
  dataset
}