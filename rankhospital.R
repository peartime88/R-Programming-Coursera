rankhospital <- function(state, outcome, num="best") {
  #READ IN DATA FILE
  outcomes <- read.csv("C:\\Users\\cheny47\\Documents\\rprog_data_ProgAssignment3-data (2)\\outcome-of-care-measures.csv", colClasses = "character")
  
  #SUBSET OUT THE COLUMNS NEEDED AND RENAME FOR SIMPLICITY
  subOutcomes <- outcomes[c(1:4706), c(2,7,11,17,23)]
  names(subOutcomes) <- c("Hospital", "State", "Heart.Attack", "Heart.Failure", "Pneumonia")
  
  #SUBSET OUT OUTCOME and HOSPITAL DATA OF INTEREST
  colRef = subOutcomes[,outcome] #Subset cols with outcome of interest
  data <- subOutcomes[subOutcomes$State == state & colRef != "Not Available", c("Hospital", outcome)] #ROWS: State of interest and where outcome of interest is available
                                                                                                      #COLS: Hospital and outcome cols
  sorted = data[order(as.numeric(data[,outcome]), data$Hospital),] #Cast new outcome data as numeric, then order it
                                                                  #ROWS: ordered outcome data
                                                                  #COLS: Hospital col
  #CHECK CONDITIONS AND RETURN OUTPUT
  if(num=="best")
    rank=1
  else if(num=="worst")
    rank=nrow(sorted)
  else
    rank=as.numeric(num)
  
  return(sorted[rank,"Hospital"])
}

