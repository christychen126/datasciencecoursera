best <- function(state, outcome) { 
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
  
  ## Return hospital name in that state with lowest 30-day death ## rate
  outcome_index <- c("heart attack"=11, "heart failure"=17, "pneumonia"=23)
  hospitals <- my_data[, c(2, 7, outcome_index[[outcome]])]
  names(hospitals) <- c("hospital", "state", "outcome")
  hospitals <- hospitals[complete.cases(hospitals),]
  
  temp1 <- split(hospitals, hospitals$state)
  hospitals <- data.frame(temp1[state])
  names(hospitals) <- c("hospital", "state", "outcome")
  # print(class(hospitals))
  # print(dim(hospitals))
  # print(hospitals)
  
  # sort according to alphabtical order of hospitals
  hospitals <- hospitals[order(hospitals$hospital),]
  # print(hospitals)
  
  # sort according to decreasing value of outcome
  hospitals <- hospitals[order(hospitals$outcome, decreasing=FALSE),]
  #print(hospitals)
  # print(hospitals[1,1])
  
  return(hospitals[1,1])
}
