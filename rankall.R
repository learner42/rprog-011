rankall <- function(outcome, num = "best") {
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
  filter <- data[outcome_colid] != "Not Available"
  
  data <- data[filter, c(hospital_name_colid, state_colid, outcome_colid)]
  
  colnames(data)[1] <- "hospital"
  colnames(data)[2] <- "state" 
  colnames(data)[3] <- "rate" 

  if (nrow(data) > 0) {  
    data[, "rate"] <- sapply(data[,"rate"], as.character)  
    data[, "rate"] <- sapply(data[,"rate"], as.numeric)
    data[, "hospital"] <- sapply(data[,"hospital"], as.character)
    data[, "state"] <- sapply(data[,"state"], as.factor)
  }
  s <- split(data, data$state)
  raw_res <- lapply(s, function (x) {
    x <- x[order(x$rate, x$hospital),]
    return_rows <- c(1,2)
    if (num == "best") return (x[1,1])
    if (num == "worst") return (x[nrow(x),1])
    if (num > nrow(data)) return (NA)
    return (x[num, 1])
  })  
  data.frame(cbind(hospital = raw_res, state = names(raw_res)))
}
