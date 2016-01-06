complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  # set working directory
  if(grep("specdata", directory)==1){
    directory <- ("./specdata/")
  }
  
  # intialize a data frame to store complete cases
  complete_cases <- data.frame("id"=numeric(), "nobs"=numeric())
  
  # find all files in the specdata folder 
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
    complete_cases[nrow(complete_cases)+1,] <- c(id[], dims[1])
  }
  
  return(complete_cases)
}