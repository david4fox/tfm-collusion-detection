## Input
numberOfBids <- 20
numberOfContracts <- 1000
numberOfBreaks <- 50

## Main function
funtionSampleRD <- function(numberOfBids, numberOfContracts, numberOfBreaks){
  listOfSamples <- list()
  sampleRD <- data.frame()
  for(i in c(1:numberOfContracts)){
    listOfSamples[i] <- list(sort(sample(50:100, numberOfBids, replace=TRUE)))
    sampleRD[i,1] <- (listOfSamples[[i]][2]-listOfSamples[[i]][1])/sd(listOfSamples[[i]][2:numberOfBids])
  }
         
  sampleRD <- sort(sampleRD[,1], decreasing = TRUE)
  
  hist(sampleRD, main = "Distribution", breaks = numberOfBreaks, border = "orange", col = "yellow")
  return(sampleRD)
}

## Function
funtionSampleRD(numberOfBids, numberOfContracts, numberOfBreaks)
