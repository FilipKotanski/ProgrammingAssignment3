
# a function called rankhospital that takes three arguments: 
# the 2-character abbreviated name of a state (state), an outcome (outcome), 
# and the ranking of a hospital in that state for that outcome (num).
# The function reads the outcome-of-care-measures.csv ???le and returns a character vector
# with the name of the hospital that has the ranking speci???ed by the num argument.
# For example, the call rankhospital("MD", "heart failure", 5)
# would return a character vector containing the name of the hospital with the 5th lowest 30-day death rate
# for heart failure. The num argument can take values "best", "worst", or an integer indicating the ranking 
# (smaller numbers are better). If the number given by num is larger than the number of hospitals in that state,
# then the function should return NA. Hospitals that do not have data on a particular outcome
# should be excluded from the set of hospitals when deciding the rankings.
# Handling ties. It may occur that multiple hospitals have the same 30-day mortality rate for
# a given cause of death. In those cases ties should be broken by using the hospital name. 

rankhospital<-function(state,outcome,num='best'){
  
  ## Reading  data  from "outcome-of-care-measures.csv"
  ## colClasses is an argument of read.csv function, which lets us specify class
  ## for each variable/column of the data which speeds up importing of files
  data<-read.csv(file.path(getwd(),"outcome-of-care-measures.csv"),colClasses = 'character') 
  
  ## Checking if state and outcome are valid
  
  if(!(state %in% data$State))
    stop('invalid state')
  
  if(!(outcome %in% c('heart attack','heart failure','pneumonia')))
    stop('invalid outcome')
  
  ##filtering/extracting hospitals from the given state 
  
  hospitals_in_the_state<-data[data$State==state,]
  
  ##If the number given by num is larger than the number of hospitals in that state, then the function returns NA
  
  if(num>nrow(hospitals_in_the_state)&num!='worst'&num!='best') return (NA)
  
  ## there are multiple columns in the file, each of which provides information on particular data
  ## these three conditions check which outcome/column of the file we should focus on
  
  if(outcome=='heart attack') index<-11
  
  if(outcome=='heart failure') index<-17
  
  if(outcome=='pneumonia') index<-23
  
  ##filtering out/removing hospitals with data, which is unavailable 
  
  hospitals_in_the_state_without_NAs<-hospitals_in_the_state[hospitals_in_the_state[,index]!='Not Available',]
  
  ##converting index column from character to numeric so that we can apply order function
  ##we're looking for hospitals with the num ranking of mortality rates in the given state!   
  
  hospitals_in_the_state_without_NAs[,index]<-as.numeric(hospitals_in_the_state_without_NAs[,index])
  
  ##ordering hospitals in the state by index column (outcome/mortality rate) and then sorting them by their names alphabetically
  ##order function returns numeric vector giving/stating positions(1,2,3...) of each of records of data frame in the sorted data frame
  
  ordered_hospitals_in_the_state<-hospitals_in_the_state_without_NAs[order(hospitals_in_the_state_without_NAs[,index],hospitals_in_the_state_without_NAs[,2],na.last = NA),2]
  
  ##handling special cases when num is a string
  
  if(num=='best') num<-1
  if(num=='worst') num<-length(ordered_hospitals_in_the_state)
  
  return (ordered_hospitals_in_the_state[num])
}