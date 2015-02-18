rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  getColId <- function(name) {
    if (name == "heart attack") return(11)
    if (name == "heart failure") return(17)    
    if (name == "pneumonia") return(23)
    stop ("invalid outcome")
  }
  outcome_colid <- getColId(outcome)
  data <- read.csv("outcome-of-care-measures.csv")
  state_colid <- 7
  hospital_name_colid <- 2
  filter <- data[state_colid] == state & data[outcome_colid] != "Not Available"
  data <- data[filter, c(hospital_name_colid, outcome_colid)]
  
  if (nrow(data) == 0) return(NA)
  
  colnames(data)[2] <- "Rate"
  
  data[,2] <- sapply(data[,2], as.character)  
  data[,2] <- sapply(data[,2], as.numeric)
  data[,1] <- sapply(data[,1], as.character)
  data <- data[order(data$Rate, data$Hospital.Name),]
  if (num == "best") return (data[1,1])
  if (num == "worst") return (data[nrow(data),1])
  if (num > nrow(data)) return (NA)
  data[num,1]
}
