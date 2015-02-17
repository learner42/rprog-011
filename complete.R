complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  df <- data.frame( id = integer(0), nobs = integer(0))
  for (i in id) {
    fn <-file.path(directory, sprintf("%03d.csv", i))
    data <- read.csv(fn)
    na <- is.na(data$Date) | is.na(data$sulfate) | is.na(data$nitrate) | is.na(data$ID)
    cnt = length(na) - sum(na)
    df <- rbind(df, data.frame(id = i, nobs = cnt))
  }
  df
}