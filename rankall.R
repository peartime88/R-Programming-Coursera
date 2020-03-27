
rankall <- function(outcome, num="best") {
  #READ IN DATA FILE
  outcomes <- read.csv("C:\\Users\\cheny47\\Documents\\rprog_data_ProgAssignment3-data (2)\\outcome-of-care-measures.csv", colClasses = "character")
  
  #SUBSET OUT THE COLUMNS NEEDED AND RENAME FOR SIMPLICITY
  subOutcomes <- outcomes[c(1:4706), c(2,7,11,17,23)] 
  names(subOutcomes) <- c("Hospital", "State", "Heart.Attack", "Heart.Failure", "Pneumonia") #rename to something simplier
  
  #SUBSET OUT OUTCOME and HOSPITAL DATA OF INTEREST
  colRef = subOutcomes[,outcome] #Subset cols with outcome of interest
  data <- subOutcomes[colRef != "Not Available", c("Hospital", "State", outcome)] #ROWS: State of interest and where outcome of interest is available
                                                                                  #COLS: Hospital, State, and outcome cols
  sorted = data[order(data[,"State"], as.numeric(data[,outcome]), data[,"Hospital"]), c("Hospital", "State")] #Order by state first, then outcome
                                                                               #ROWS: state ordered, outcome ordered
                                                                               #COLS: Hosp and State cols
  #CHECK CONDITIONS AND OBTAIN DATA OF INTEREST
  if (num == "best") {
    num=1
    finalData = tapply(sorted$Hospital, sorted$State, function(rankList) {return(rankList[num])}) 
  } else if (num == "worst") {
    finalData = tapply(sorted$Hospital, sorted$State, function(rankList) {num=length(rankList); return(rankList[num])})
    #apply a function(rankList) to sorted$Hospital according to 54 levels in sorted$State
    #for each of 54 states, return the ranked hospital for that state according to specified num in argument
    #54 lists made, indices = rank bc alreaded ranked in "sorted"
    #tapply returns an array
  } else {
    num = as.numeric(num)
    finalData = tapply(sorted$Hospital, sorted$State, function(rankList) {return(rankList[num])})
  }

  df = data.frame(finalData, State=names(finalData)) #change array finalData into Data Frame
  names(df)[names(df) == "finalData"] <- "Hospital"
  return(df)
  #States on the left side are string indices, data frames have numbers on side as numbers are indices, but yours was an array
}
