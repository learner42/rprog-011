corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  res <- vector()
  comp <- complete(directory)
  selected <- comp[comp$nobs > threshold,1]
  for (i in selected) {
    fn <-file.path(directory, sprintf("%03d.csv", i))
    data <- read.csv(fn)
    res <- rbind(res, cor(data$nitrat, data$sulfate, use = "na.or.complete"))
  }
  res
}