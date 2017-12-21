

corr <- function(directory, threshold = 0) {
  if (threshold < 0) stop("Threshold value must be greater than or equal to 0.")

  # Get targeted data sets
  completeStats = complete(directory)
  completeStats = completeStats[completeStats$nobs >= threshold,]

  print(paste("File count:", nrow(completeStats)))
  if (nrow(completeStats) > 0) {
    print("Returning results")
    sapply(completeStats$id, function(n) {
            t <- read.csv(sprintf("specdata/%03d.csv", n))
            cor(t$sulfate, t$nitrate, use="complete.obs")
    })
  }
  else {
    print("Returning empty vector")
    vector(mode="numeric")
  }
}

