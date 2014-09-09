
pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  readings = numeric()
  for (idx in id) {
    filename <- sprintf("%s/%03d.csv", directory, idx)  
    data <- read.csv(filename)
    readings <- c(readings, data[[pollutant]])
  }
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)  
  round(mean(readings, na.rm =  TRUE), digits = 3)
}

