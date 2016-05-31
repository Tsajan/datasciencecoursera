pollutantmean <- function(directory, pollutant, id=1:332) {
  file_lst <- dir(directory)
  dataset <- c()
  for(itm in file_lst) {
    itmid <- as.numeric(unlist(strsplit(itm,split=".",fixed=TRUE))[1])
    if(itmid %in% id) {
      mylist <- id
      temp_dataset <- read.csv(paste(directory,itm,sep="/"), header=TRUE)
      dataset <- rbind(temp_dataset, dataset)
      rm(temp_dataset)
    }
    else {
      next()
    }
  }
  selectcols <- dataset[,pollutant]
  vals <- selectcols[!is.na(selectcols)]
  meanval <- mean(vals)
  meanval
}