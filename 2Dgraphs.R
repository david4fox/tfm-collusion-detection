# Libraries
library("plotly")
library("dplyr")

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
    if((comparisonTable[count,6]>=0.5&comparisonTable[count,7]==0)|(comparisonTable[count,7]>=0.5&comparisonTable[count,6]==0)|(comparisonTable[count,6]>=0.5&comparisonTable[count,7]>=0.5)){
      comparisonTable[count,8] <- "Non Competitive"
    }else{
      comparisonTable[count,8] <- "Competitive"
    }
    if(comparisonTable[count,8]=="Competitive"){
      comparisonTable[count,9] <- comparisonTable[count,7]
      comparisonTable[count,10] <- NA
    }else{
      comparisonTable[count,9] <- NA
      comparisonTable[count,10] <- comparisonTable[count,7]
    }
    
  }
  colnames(comparisonTable) <- c("Contracts", FirstFirm, SecondFirm, "MaxBid", "MinBid",paste0(FirstFirm,"_Nomalized"),paste0(SecondFirm,"_Nomalized"),"CompetitiveZones","Competitive","NonCompetitive")
  
  graph <- plot_ly(data = comparisonTable, x = comparisonTable[,6], y = comparisonTable[,9], type = "scatter",mode = "markers" , name="Competitive",
                   marker = list(color = 'rgb(100, 150, 255)',line = list(color = 'rgb(0, 0, 255)', width = 0.5))) %>%
    layout(title = "Normalized Graph", titlefont = list(size = 30,color = 'rgb(47, 47, 147)'),
           yaxis = list(linecolor = toRGB("black"),color = toRGB("black"),title = FirstFirm, range = c(-0.05,1.05), titlefont = list(size = 18,color = 'rgb(0, 0, 0)'), tickfont = list(size = 12,color = 'rgb(0, 0, 0)')),
           xaxis = list(linecolor = toRGB("black"),color = toRGB("black"),title = SecondFirm, titlefont = list(size = 18,color = 'rgb(0, 0, 0)'), tickfont = list(size = 12,color = 'rgb(0, 0, 0)')),
           margin = list(t = 75, b = 50,r = 10,l = 80), plot_bgcolor = 'rgb(255, 255, 255)', paper_bgcolor  = 'rgb(186, 186, 186)', barmode = 'group', bargap = 10,
           legend = list(x = 0.18, y = 1.08,orientation = 'h')) %>%
    add_trace(y= comparisonTable[,10],type = "scatter",mode = "markers", name="Non competitive",marker = list(color = 'rgb(255,50,50)',line = list(color = 'rgb(255,10,10)', width = 0.5)))
  
  comparisonTable<<-comparisonTable
  graph <<- graph
  return(graph)
}

## Graph
FirstFirm <- "Firm1"
SecondFirm <- "Firm4"
#BidData <- Bid_data
graphic2Dfuntion(FirstFirm, SecondFirm, Bid_data)


