

complete <- function(directory, id = 1:332) {
  # Validate the function parameters
  if (length(directory) != 1) stop("A single directory must be specified for the test data.")
  if (!dir.exists(directory)) stop(paste("The specified test data directory", directory, "does not exist."))
  
  idLength <- length(id)
  if (idLength < 1 
      || idLength > 332 
      || any(!(id %in% 1:332))
      || anyDuplicated(id) > 0)
    stop("The id vector may only contain values between 1 and 332 inclusive with no repeats.")


  # Read a single file's data
  readFileData <- function (id) {
    fileData <- read.csv(sprintf("%03d.csv", id))
    fileData[complete.cases(fileData),]
  }


  # Change to the data directory to prevent excessive string concatenations
  originalDirectory = getwd()
  setwd(directory)

  # Get and process data
  rawData <- matrix(nrow=length(id), ncol=2, dimnames=list(NULL, c("id","nobs")))
  for (lcv in 1:length(id)) rawData[lcv,] <- c(id[lcv], nrow(readFileData(id[lcv])))

  # Return to the original directory
  setwd(originalDirectory)


  # Report complete cases
  as.data.frame(rawData)
}

