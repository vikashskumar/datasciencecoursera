
get_corr_for_id <- function(directory, idx) {
    filename <- sprintf("%s/%03d.csv", directory, as.integer(idx))
    data <- read.csv(filename)

    ## Use only complete cases for finding correlation
    cor(data[["sulfate"]], data[["nitrate"]], use = "complete.obs")
}


corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files

    ## Find all the complete cases in data
    complete_table <- complete(directory)
    n <- nrow(complete_table)

    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0

    outputvec <- numeric()
    for (x in 1:n) {
        ## Get correlation if we have any complete cases in the file and
        ## those cases are above threshold
        if (complete_table[x, "nobs"] >= threshold[1] & complete_table[x, "nobs"] > 0) {
            outputvec <- c(outputvec, get_corr_for_id(directory, complete_table[x, "id"]))
        }
    }

    ## Return a numeric vector of correlations
    outputvec
}

