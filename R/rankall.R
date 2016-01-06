rankall <- function(outcome, num = "best") { ## Read outcome data
  ## Read outcome data
  my_data<-read.csv("rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv", na.strings="Not Available", stringsAsFactors=FALSE )
  
  ## Check that state and outcome are valid   
  if(!(outcome== "heart attack"|| outcome=="heart failure"|| outcome=="pneumonia" )){
    stop("invalid outcome")
  }
  
  ## Return hospital name in that state with the given rank ## 30-day death rate
  all_states <- unique(unlist(my_data[,7]))
  # print(all_states)
  all_states <- all_states[order(unlist(all_states))]
  # print(all_states)
  outcome_index <- c("heart attack"=11, "heart failure"=17, "pneumonia"=23)
  hospitals <- my_data[, c(2, 7, outcome_index[[outcome]])]
  names(hospitals) <- c("hospital", "state", "outcome")
  hospitals <- hospitals[complete.cases(hospitals),] # removing rows with NA's
  temp1 <- split(hospitals, hospitals$state)
  
  ## For each state, find the hospital of the given rank
  n <- length(all_states)
  # df <- data.frame(hospital=character(), state=character())
  lis <- list()
  
  for(i in 1:n){
    hospitals <- data.frame(temp1[all_states[i]])
    names(hospitals) <- c("hospital", "state", "outcome")
    
    # sort according to alphabtical order of hospitals
    hospitals <- hospitals[order(hospitals$hospital),]
    # print(hospitals)
    
    # handling num
    if(num=="best"){
      #print("in if")
      index = 1
      # sort according to increasing value of outcome
      
      
    }else if(num=="worst"){
      #print("in else")
      index = nrow(hospitals)
      # sort according to decreasing value of outcome
      
      
    }
    #print(index)
    #print(nrow(hospitals))
    
    hospitals <- hospitals[order(hospitals$outcome, decreasing=FALSE),]
    
    # print(num)
    #if(i>=n-3){
      #print(head(hospitals,3))
    #  print(i)
    #  print(hospitals[num,1])
    #  print(hospitals[num,2])
    #  print("----")
    #}
   
    lis[[i]] <- c(hospitals[index,1], all_states[i])
   
  }
  
  ## Return a data frame with the hospital names and the ## (abbreviated) state name
  df <- data.frame(matrix(unlist(lis), nrow=n, byrow=T),stringsAsFactors=FALSE)
  names(df) <- c("hospital", "state")
  
  return(df)
}