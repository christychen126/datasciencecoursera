corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  ## NOTE: Do not round the result!
  
  
  # set working directory
  if(grep("specdata", directory)==1){
    directory <- ("./specdata/")
  }
  corr_values <- c()
  
  # intialize a data frame to store complete cases
  complete_cases <- data.frame("id"=numeric(), "nobs"=numeric())
  
  # find all files in the specdata folder
  id = 1:332
  all_files <- sprintf("%03d", id)
  
  # append the curent directory
  file_paths <- paste(directory, all_files, ".csv", sep="")
  # print(length(file_paths))
  
  for(i in seq_along(file_paths)) { 
    current_file <- read.csv(file_paths[i], header=T, sep=",")
    # print(dim(current_file))
    # print(head(current_file))
    na_removed <- current_file[complete.cases(current_file),]
    dims <- dim(na_removed)
    
    if(dims[1]>threshold){
      sulfate <- na_removed[,"sulfate"]
      nitrate <- na_removed[,"nitrate"]
      corr_values <- c(corr_values, cor(nitrate, sulfate))
    }
  }
  
  #print(corr_values)
  #print(length(corr_values))
  
  #if(length(corr_values)==0){
  #  corr_values <- 0
  #}
    
  return(corr_values)
}