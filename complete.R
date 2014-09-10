

complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files

    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used

    ## Initialize and empty data frame with required output fields

    tid   <- character(length(id))
    tnobs <- numeric(length(id))

    j <- 1
    for (idx in id) {
        filename <- sprintf("%s/%03d.csv", directory, idx)
        data <- read.csv(filename)

        ## Find all the complete cases in data
        complete_cases <- complete.cases(data)

        ## prepare the output vectors
        tid[j] <- idx
        tnobs[j] <- sum(complete_cases)

        ## j gives us the index in the output array as the idx can be any value
        j <- j + 1
    }

    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases

    df <- data.frame(id= tid, nobs = tnobs, stringsAsFactors = FALSE)
    df
}