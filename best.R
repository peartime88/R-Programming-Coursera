#INSTALL DPLYR FOR DATASET MANIPULATION
install.packages("dplyr")
library(dplyr)


best <- function(state, outcome) {
  #READ IN DATA FILE
  outcomes <- read.csv("C:\\Users\\cheny47\\Documents\\rprog_data_ProgAssignment3-data (2)\\outcome-of-care-measures.csv", colClasses = "character")
  
  #SUBSET OUT THE COLUMNS NEEDED AND RENAME FOR SIMPLICITY
  subOutcomes <- outcomes[c(1:4706), c(2,7,11,17,23)]
  names(subOutcomes) <- c("Hospital", "State", "Heart.Attack", "Heart.Failure", "Pneumonia")
  
  #CHECK IF VALID STATE
  if(state %in% subOutcomes[,2]) {
    subOutcomes <- subset(subOutcomes, State==state) #subset needed columns for indicated state
  } else {
    message("invalid state")
  }
  
  #CHECK IF VALID OUTCOME
  if(outcome == "Heart.Attack") {
    Data <- subset(subOutcomes, Heart.Attack!="Not Available") #Remove "Not Available"s from dataset
    selectedColumns <- as.numeric(Data[, 3]) #cast Heart Attack data for that state from char to numeric
    selectedRows <- which(selectedColumns == min(selectedColumns)) #pick out the rows where the Heart Attack data is minimum
    selectedHospital <- Data[selectedRows, 1] #pick out the Hospital names where the Heart Attack data in minimum
    sortedHospital <- sort(selectedHospital) #sort the hospital names alphabetically
    return(sortedHospital[1]) #return the first one alphabetically if there is a tie for best
  }
  
  if(outcome == "Heart.Failure") {
    Data <- subset(subOutcomes, Heart.Failure!="Not Available")
    selectedColumns <- as.numeric(Data[, 4])
    selectedRows <- which(selectedColumns == min(selectedColumns))
    selectedHospital <- Data[selectedRows, 1]
    sortedHospital <- sort(selectedHospital)
    return(sortedHospital[1])
  }
  
  if(outcome == "Pneumonia") {
    Data <- subset(subOutcomes, Pneumonia!="Not Available")
    selectedColumns <- as.numeric(Data[, 5])
    selectedRows <- which(selectedColumns == min(selectedColumns))
    selectedHospital <- Data[selectedRows, 1]
    sortedHospital <- sort(selectedHospital)
    return(sortedHospital[1])
  }
}  
