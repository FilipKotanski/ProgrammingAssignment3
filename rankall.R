# 
# a function called rankall that takes two arguments: an outcome name (outcome) and a hospital ranking (num).
# The function reads the outcome-of-care-measures.csv ???le and returns a 2-column data frame 
# containing the hospital in each state that has the ranking speci???ed in num.
# For example the function call rankall("heart attack", "best") would return a data frame containing 
# the names of the hospitals that are the best in their respective states for 30-day heart attack 
# death rates. The function should return a value for every state (some may be NA).
# The ???rst column in the data frame is named hospital, which contains the hospital name,
# and the second column is named state, which contains the 2-character abbreviation for
# the state name. Hospitals that do not have data on a particular outcome should be excluded
# from the set of hospitals when deciding the rankings.
# Handling ties. The rankall function should handle ties in the 30-day mortality rates in
# the same way that the rankhospital function handles ties

rankall<-function(outcome,num='best'){

  ## Reading  data  from "outcome-of-care-measures.csv"
  ## colClasses is an argument of read.csv function, which lets us specify class
  ## for each variable/column of the data which speeds up importing of files  
  
  data <- read.csv(file.path(getwd(),"outcome-of-care-measures.csv"), colClasses = "character") 

  ##checking if outcome is valid

  if(!(outcome %in% c('heart attack','heart failure','pneumonia')))
    stop('invalid outcome')
  
  ## there are multiple columns in the file, each of which provides information on particular data
  ## these three conditions check which outcome/column of the file we should focus on
  

  if(outcome=='heart attack') index<-11

  if(outcome=='heart failure') index<-17

  if(outcome=='pneumonia') index<-23

  ##filtering out/removing hospitals with data, which is unavailable 

  data_without_NAs<-data[data[,index]!='Not Available',]
  
  ##converting index column from character to numeric so that we can apply order function
  ##we're looking for hospitals with the num ranking of mortality rates in all state!

  data_without_NAs[,index]<-as.numeric(data_without_NAs[,index])
  
  ##sorting data frame/hospitals by three columns: state, index column(mortality rate) and the name of hospital
  ## in three different cases when num is best,worst and numeric
  
if(num=='best'){
  
num<-1                                                                                                  ##selecting/subsetting columns name and state only      
ordered_data<-data_without_NAs[order(data_without_NAs[,7],data_without_NAs[,index],data_without_NAs[,2]),c(2,7)]
}
else if(num=='worst'){
  
num<-1                                                  ##(-) sorting in reverse order
ordered_data<-data_without_NAs[order(data_without_NAs[,7],-data_without_NAs[,index],data_without_NAs[,2]),c(2,7)]
}
else{
  
ordered_data<-data_without_NAs[order(data_without_NAs[,7],data_without_NAs[,index],data_without_NAs[,2]),c(2,7)]
}

  ##The by( ) function applys a function to each level of a factor or factors.
  ##In our case by takes ordered data frame and applies anonymous function(x) x[num,]
  ##to group/subset of all hospitals from every state to subset/extract num-ranked hospital from every state
  ##The by function returns a list, where each list item represents the results for a particular subset of the data.
  ## rbind joins all elements of the list by rows and returns list of 54 elements
  ##we don't want to have list we want to have data.frame and that's why we use do.call function
  ##We can combine all the elements of the list into data.frame without storing them separately or passing them individually to rbind /rbind(list[[1]],list[[2]])
  solution<-do.call(rbind,by(ordered_data,ordered_data$State,function(x) x[num,]))
  
  ##replacing ocassional NAs in state abbreviation column
  
  solution[,2]<-by(ordered_data,ordered_data$State,function(x)x[1,2])
  
  ##returning requested solution
  
  solution
}