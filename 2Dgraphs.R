# Libraries
library("plotly")

#Function
graphic2Dfuntion <- function(FirstFirm, SecondFirm, BidData){
  a <- which(BidData == FirstFirm, arr.ind = TRUE)
  b <- which(BidData == SecondFirm, arr.ind = TRUE)
  communContracts <- c()
  for(i in 1:nrow(b)){
    if(length(which(a[,1]==b[i,1]))>0){
      communContracts <- append(communContracts,b[[i,1]])
    }
  }
  communContracts <- sort(communContracts, decreasing = FALSE)
  comparisonTable <- data.frame("Contracts" = communContracts, "BidFirm1" = 0,"BidFirm2" = 0, "MaxBid" = 0,"MinBid" = 0, "NormBidFirm1" = 0,"NormBidFirm2" = 0)
  count <-0
  for(i in communContracts){
    count <- count +1
    Bids <- c()
    for(j in seq(from=3, to=ncol(BidData), by=2)){
      if(!is.na(BidData[i,j])){
        Bids <- append(Bids,as.numeric(BidData[i,j]))
      }
    }
    comparisonTable[count,2] <- BidData[[i,which(BidData[i,]==FirstFirm)+1]]
    comparisonTable[count,3] <- BidData[[i,which(BidData[i,]==SecondFirm)+1]]
    comparisonTable[count,4] <- max(Bids)
    comparisonTable[count,5] <- min(Bids)
    comparisonTable[count,6] <- (comparisonTable[count,2]-comparisonTable[count,5])/(comparisonTable[count,4]-comparisonTable[count,5])
    comparisonTable[count,7] <- (comparisonTable[count,3]-comparisonTable[count,5])/(comparisonTable[count,4]-comparisonTable[count,5])
  }
  colnames(comparisonTable) <- c("Contracts", FirstFirm, SecondFirm, "MaxBid", "MinBid",paste0(FirstFirm,"_Nomalized"),paste0(SecondFirm,"_Nomalized"))
  graph <- plot_ly(data = comparisonTable, x = comparisonTable[,6], y = comparisonTable[,7], type = "pointcloud")
  graph <<- graph
  #return(graph)
}

## Graph
FirstFirm <- "Firm1"
SecondFirm <- "Firm8"

graphic2Dfuntion(FirstFirm, SecondFirm, Bid_data)
