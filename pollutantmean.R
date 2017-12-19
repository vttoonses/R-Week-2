

pollutantmean <- function(directory, pollutant, id = 1:332) {
  # Validate the function parameters
  if (length(directory) != 1) stop("A single directory must be specified for the test data.")
  if (!dir.exists(directory)) stop(paste("The specified test data directory", directory, "does not exist."))
  if (length(pollutant) != 1) stop("A single pollutant type of \"sulfate\" or \"nitrate\" must be specified.")
  if (!(pollutant %in% c("sulfate", "nitrate"))) stop(paste("Unknown pollutant:", pollutant, ". Please specify either \"sulfate\" or \"nitrate\"."))
  
  idLength <- length(id)
  if (idLength < 1 
      || idLength > 332 
      || any(!(id %in% 1:332))
      || anyDuplicated(id) > 0)
    stop("The id vector may only contain values between 1 and 332 inclusive with no repeats.")


  # Get the data

  # Read a single file's data
  readFileData <- function (id) {
    fileData <- read.csv(sprintf("%03d.csv", id))
    targetData <- fileData[pollutant]
    targetData[!(is.na(targetData))]
  }

  # Change to the data directory to prevent excessive string concatenations
  originalDirectory = getwd()
  setwd(directory)

  testData <- unlist(sapply(id, readFileData))

  # Return to the original directory
  setwd(originalDirectory)

  # Return the mean of all that data
  mean(testData)
}

