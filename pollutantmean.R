pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  count <- 0
  total <- 0
  for ( i in id ){
    fn <-file.path(directory, sprintf("%03d.csv", i))
    data <- read.csv(fn)
    col <- data[pollutant]
    col <- col[!is.na(col)]
    count <- count + length(col)
    total <- total + sum(col)
  }
  total/count
}