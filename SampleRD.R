## Input
numberOfBidsMin <- 20
numberOfBidsMax <- 25
numberOfContracts <- 10000
numberOfBreaks <- 1000

## Main function
funtionSampleRD <- function(numberOfBidsMin, numberOfBidsMax, numberOfContracts, numberOfBreaks){
  listOfSamples <- list()
  sampleRD <- data.frame()
  for(i in c(1:numberOfContracts)){
    aux <- sample(numberOfBidsMin:numberOfBidsMax,1)
    listOfSamples[i] <- list(sort(sample(c(sample(10:70,1):sample(80:150,1)), aux, replace=TRUE)))
    sampleRD[i,1] <- (listOfSamples[[i]][2]-listOfSamples[[i]][1])/sd(listOfSamples[[i]][2:aux])
  }
         
  sampleRD <- sort(sampleRD[,1], decreasing = TRUE)
  
  hist(sampleRD, main = "Distribution", breaks = numberOfBreaks, border = "orange", col = "yellow", xlim = c(0,1))
}

## Function
funtionSampleRD(numberOfBidsMin, numberOfBidsMax, numberOfContracts, numberOfBreaks)



