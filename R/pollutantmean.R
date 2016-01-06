##########################################################################################################################
## 'directory' is a character vector of length 1 indicating
## the location of the CSV files

## 'pollutant' is a character vector of length 1 indicating
## the name of the pollutant for which we will calculate the
## mean; either "sulfate" or "nitrate".

## 'id' is an integer vector indicating the monitor ID numbers
## to be used

## Return the mean of the pollutant across all monitors list
## in the 'id' vector (ignoring NA values)
## NOTE: Do not round the result!

# commands to run the program
# source("pollutantmean.R")
# pollutantmean("c:/Users/thula/Documents/UW MBA Course Work/Dropbox/AnuDocuments/DataScience/datasciencecoursera/R/dataset/specdata/", "sulfate", 1:10)
##########################################################################################################################

# first set working directory in the console
# setwd("c:/Users/thula/Documents/UW MBA Course Work/Dropbox/AnuDocuments/DataScience/datasciencecoursera/R/")

pollutantmean <- function(directory, pollutant, id = 1:332){
  
  # set working directory
  if(grep("specdata", directory)==1){
    directory <- ("./specdata/")
  }

  # initialize a vector to hold the pollutant data 
  mean_vector <- c() 
  
  # find all files in the specdata folder 
  # all_files <- as.character( list.files(directory) )
  all_files <- sprintf("%03d", id)
  
  # append the curent directory
  file_paths <- paste(directory, all_files, ".csv", sep="")
  # print(file_paths)
  
  for(i in seq_along(file_paths)) { 
    # print(file_paths[i])
    current_file <- read.csv(file_paths[i], header=T, sep=",") 
    # head(current_file) 
    # print(pollutant)
    na_removed <- current_file[!is.na(current_file[, pollutant]), pollutant] 
    mean_vector <- c(mean_vector, na_removed) 
  }
  
  result <- mean(mean_vector) 
  return(result)
}