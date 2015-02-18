best <- function(state, outcome) {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate
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
  if (nrow(data) == 0) stop ("invalid state")
  data[,2] <- sapply(data[,2], as.character)  
  data[,2] <- sapply(data[,2], as.numeric)
  data[,1] <- sapply(data[,1], as.character)
  lowest_rate <- min(data[2])
  best_hospitals <- data[data[2] == lowest_rate,]
  best_hospitals <- best_hospitals[order(1),]
  best_hospital <- best_hospitals[[1,1]]
  best_hospital
}