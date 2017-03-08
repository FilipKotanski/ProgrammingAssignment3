
# a function called best that take two arguments: 
# the 2-character abbreviated name of a state and an outcome name.
# The function reads the outcome-of-care-measures.csv ???le and returns a character vector
# with the name of the hospital that has the best (i.e. lowest) 30-day mortality for 
# the speci???ed outcome in that state. The hospital name is the name provided in the Hospital.
# Name variable. The outcomes can be one of "heart attack", "heart failure", or "pneumonia".
# Hospitals that do not have data on a particular outcome should be excluded from the set of hospitals when deciding the rankings.
# Handling ties. If there is a tie for the best hospital for a given outcome,
# then the hospital names should be sorted in alphabetical order and 
# the ???rst hospital in that set should be chosen 
# (i.e. if hospitals "b", "c", and "f" are tied for best, then hospital "b" should be returned).




best<-function(state,outcome) {
  ## Reading  data  from "outcome-of-care-measures.csv"
  ## colClasses is an argument of read.csv function, which lets us specify class
  ## for each variable/column of the data which speeds up importing of files
 data<-read.csv(file.path(getwd(),"outcome-of-care-measures.csv"),colClasses = 'character')
 
 ## Checking if state and outcome are valid
 
  
 if(!(state %in% data$State))
  stop('invalid state')
 
 if(!(outcome %in% c('heart attack','heart failure','pneumonia')))
   stop('invalid outcome')
 
 ## there are multiple columns in the file, each of which provides information on particular data
 ## these three conditions check which outcome we should focus on
 
 if(outcome=='heart attack') index<-11
 
 if(outcome=='heart failure') index<-17
 
 if(outcome=='pneumonia') index<-23
 
 ##filtering/extracting hospitals from the given state 
   
   hospitals_in_the_state<-data[data$State==state,]
   
 ##filtering out/removing hospitals with data, which is unavailable 
   
   hospitals_in_the_state<-hospitals_in_the_state[hospitals_in_the_state[,index]!='Not Available',]
   
 ##converting index column from character to numeric so that we can apply min function
 ##we're looking for hospitals with the lowest mortality rates!   
   
   hospitals_in_the_state[,index]<-as.numeric(hospitals_in_the_state[,index])
   
 ## finding name(s)(..,2]!) of hospitals with the lowest mortality rate    
 ## which returns vector of logical values with TRUE value when logical expression is TRUE and FALSE otherwise
     
   the_best_hospitals<-hospitals_in_the_state[which(hospitals_in_the_state[,index]==min(hospitals_in_the_state[,index],na.rm = TRUE)),2]
  
 ##sorting names of hospitals alphabetically
      
   the_best_hospitals_sorted<-sort(the_best_hospitals,decreasing = FALSE,na.last=NA)
  
 ##returning the name of the hospital   
    
   return (the_best_hospitals_sorted[1])
 

 
 
}

