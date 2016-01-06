rankhospital <- function(state, outcome, num = "best") { ## Read outcome data
  ## Read outcome data
  my_data<-read.csv("rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv", na.strings="Not Available", stringsAsFactors=FALSE )
  
  ## Check that state and outcome are valid   
  if(!(outcome== "heart attack"|| outcome=="heart failure"|| outcome=="pneumonia" )){
    stop("invalid outcome")
  }
  all_states <- unique(unlist(my_data[,7]))
  if(!(is.element(state, all_states))){
    stop("invalid state")
  }
  
  ## Return hospital name in that state with the given rank ## 30-day death rate
  outcome_index <- c("heart attack"=11, "heart failure"=17, "pneumonia"=23)
  hospitals <- my_data[, c(2, 7, outcome_index[[outcome]])]
  names(hospitals) <- c("hospital", "state", "outcome")
  hospitals <- hospitals[complete.cases(hospitals),] # removing rows with NA's
  temp1 <- split(hospitals, hospitals$state)
  hospitals <- data.frame(temp1[state])
  names(hospitals) <- c("hospital", "state", "outcome")
  # print(class(hospitals))
  # print(dim(hospitals))
  # print(hospitals)
  
  # handling num
  if(num=="best"){
    num = 1
  }else if(num=="worst"){
    num = nrow(hospitals)
  }
  
  # sort according to alphabtical order of hospitals
  hospitals <- hospitals[order(hospitals$hospital),]
  # print(hospitals)
  
  # sort according to decreasing value of outcome
  hospitals <- hospitals[order(hospitals$outcome, decreasing=FALSE),]
  #print(hospitals)
  # print(hospitals[1,1])
  
  return(hospitals[num,1])
}
