## Libraries
library("shiny")
library("shinyjs")
library("shinythemes")
library("shinyWidgets")
library("shinydashboard")
library("xlsx")
library("plotly")
#library("dplyr")


## Inicialization
tableMarkers <- 0

## Aplication definition
header <- dashboardHeader(
  title = "Data input"
)
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("File input", tabName = "menu1", icon = icon("file-upload")),
    menuItem("DataTable", tabName = "menu2", icon = icon("table")),
    menuItem("2D graph", tabName = "menu3", icon = icon("chart-scatter")),
    menuItem("Final table", tabName = "menu4", icon = icon("chart-scatter"))
  )
)
body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = "menu1",
      fluidRow(
        column(3, fileInput("xlsxFile", label = h3("Upload a xlsx file"), multiple = FALSE,placeholder = "No file selected",buttonLabel = "Browse...",width = "100%",accept = c(".xlsx"))),
        column(4, h6("")),
        column(2, actionButton("summitXLSX", label = "Summit Excel", icon = icon("table"), style="color: white; background-color: #000F89; border-color: #0011B7; padding:10; margin:0; position:rigth"))
      ),
      fluidRow(
        column(3, fileInput("csvFile", label = h3("Upload a csv file"), multiple = FALSE, placeholder = "No file selected",buttonLabel = "Browse...",width = "100%",accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))),
        column(4, radioButtons("sep", h3("Separator"), choices = c(Comma = ",", Semicolon = ";", Tab = "\t"), selected = ";", inline = TRUE)),
        column(2, actionButton("summitCSV", label = "Summit Csv", icon = icon("table"), style="color: white; background-color: #000F89; border-color: #0011B7; padding:10; margin:0; position:rigth"))
      ),
      tableOutput("tabla1"),
      tableOutput("tabla2")
    ),
    tabItem(
      tabName = "menu2",
      h2("Data table"),
      dataTableOutput("markersTable")
    ),
    tabItem(
      tabName = "menu3",
      fluidRow(
        column(10,
          box(fluidRow(
            column(4, radioButtons("firmOne", label = h4("Select one firm:"),
                                   choices = list("Firm1" = "Firm1", "Firm2" = "Firm2", "Firm3" = "Firm3", "Firm4" = "Firm4", "Firm5" = "Firm5", "Firm6" = "Firm6", "Firm7" = "Firm7", "Firm8" = "Firm8", "Firm9" = "Firm9", "Firm10" = "Firm10"), 
                                   selected = "Firm1")),
            column(4, radioButtons("firmTwo", label = h4("Select another different firm:"),
                                   choices = list("Firm1" = "Firm1", "Firm2" = "Firm2", "Firm3" = "Firm3", "Firm4" = "Firm4", "Firm5" = "Firm5", "Firm6" = "Firm6", "Firm7" = "Firm7", "Firm8" = "Firm8", "Firm9" = "Firm9", "Firm10" = "Firm10"), 
                                   selected = "Firm2"))
          ),
          width = "100%",
          height = "350px",
          background = "light-blue")
        ),
        column(2, actionButton("show", label = "Show Graph", icon = icon("table"), style="color: white; background-color: #000F89; border-color: #0011B7; padding:10; margin:0; position:rigth")
        )
      ),
      fluidRow(
        column(10, plotlyOutput("Graph2D"))
      )
    ),
    tabItem(
      tabName = "menu4",
      actionButton("DoIt", label = "Show the final table", icon = icon("table"), style="color: white; background-color: #000F89; border-color: #0011B7; padding:10; margin:0; position:rigth"),
      dataTableOutput("markersTableFinal")
    )
  )
)

ui <- dashboardPage(header, sidebar, body)

server <- function(input, output) {
  
  graphic2Dfuntion <- function(FirstFirm, SecondFirm){
    
    BidData <- read.csv("E:/Storage/Github/TFM/tableCSV.csv", header = TRUE)
    
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
    
    graphic <- plot_ly(data = comparisonTable, x = comparisonTable[,6], y = comparisonTable[,9], type = "scatter",mode = "markers" , name="Competitive",
                       marker = list(color = 'rgb(100, 150, 255)',line = list(color = 'rgb(0, 0, 255)', width = 0.5))) %>%
      layout(title = "Normalized Graph", titlefont = list(size = 30,color = 'rgb(47, 47, 147)'),
             yaxis = list(linecolor = toRGB("black"),color = toRGB("black"),title = FirstFirm, range = c(-0.05,1.05), titlefont = list(size = 18,color = 'rgb(0, 0, 0)'), tickfont = list(size = 12,color = 'rgb(0, 0, 0)')),
             xaxis = list(linecolor = toRGB("black"),color = toRGB("black"),title = SecondFirm, titlefont = list(size = 18,color = 'rgb(0, 0, 0)'), tickfont = list(size = 12,color = 'rgb(0, 0, 0)')),
             margin = list(t = 75, b = 50,r = 10,l = 80), plot_bgcolor = 'rgb(255, 255, 255)', paper_bgcolor  = 'rgb(186, 186, 186)', barmode = 'group', bargap = 10,
             legend = list(x = 0.18, y = 1.08,orientation = 'h')) %>%
      add_trace(y= comparisonTable[,10],type = "scatter",mode = "markers", name="Non competitive",marker = list(color = 'rgb(255,50,50)',line = list(color = 'rgb(255,10,10)', width = 0.5)))
    
    results <- list(graphic)
    return(graphic)
  }
  
  funtionSample <- function(numberOfBids, numberOfContracts, lowerPrice, higherPrice){
    listOfSamples <- list()
    sampleRD <- data.frame()
    sampleCV <- data.frame()
    for(i in c(1:numberOfContracts)){
      listOfSamples[i] <- list(sort(sample(lowerPrice:higherPrice, numberOfBids, replace=TRUE)))
      if(sd(listOfSamples[[i]][2:numberOfBids]) == 0){
        sampleRD[i,1] <- 100
        sampleCV[i,1] <- sd(listOfSamples[[i]])/mean(listOfSamples[[i]])
      }else{
        sampleRD[i,1] <- (listOfSamples[[i]][2]-listOfSamples[[i]][1])/sd(listOfSamples[[i]][2:numberOfBids])
        sampleCV[i,1] <- sd(listOfSamples[[i]])/mean(listOfSamples[[i]])
      }
    }
    sampleRD_frame <- data.frame("RD" = sampleRD[with(sampleRD, order(sampleRD[,1])), ])
    sampleRD_frame$Probability <- NA
    for(i in c(1:nrow(sampleRD_frame))){
      sampleRD_frame[i,2] <- sum(sampleRD_frame[1:i,1])/sum(sampleRD_frame[,1])
    }
    sampleCV_frame <- data.frame("CV" = sampleCV[with(sampleCV, order(sampleCV[,1])), ])
    sampleCV_frame$Probability <- NA
    for(i in c(1:nrow(sampleCV_frame))){
      sampleCV_frame[i,2] <- sum(sampleCV_frame[1:i,1])/sum(sampleCV_frame[,1])
    }
    
    listOfSamples <<- listOfSamples
    sampleRD_frame <<- sampleRD_frame
    sampleCV_frame <<- sampleCV_frame
    sampleRD <<- sort(sampleRD[,1], decreasing = TRUE)
    sampleCV <<- sort(sampleCV[,1], decreasing = TRUE)
  }
  
  #https://shiny.rstudio.com/gallery/file-upload.html
  output$tabla1 <- renderTable({
    
    req(input$xlsxFile)
    
    tryCatch({
      df <- read.xlsx(input$xlsxFile$datapath,
                     header = TRUE,
                     sheetIndex = 1,
                     stringsAsFactors=FALSE)
    },
    error = function(e) {
      # return a safeError if a parsing error occurs
      stop(safeError(e))
    })
    
    write.csv(df,"E:/Storage/Github/TFM/tableCSV.csv", row.names=FALSE)
    
    df <- data.frame(df)
    df[is.na(df)]=""
    
    df[,1] <- as.integer(df[,1])

    return(df)
  })
  
  output$tabla2 <- renderTable({
    
    req(input$csvFile)
    
    tryCatch({
      df <- read.csv(input$csvFile$datapath,
                     header = TRUE,
                     sep = input$sep)
    },
    error = function(e) {
      # return a safeError if a parsing error occurs
      stop(safeError(e))
    })
    
    write.csv(df,"E:/Storage/Github/TFM/tableCSV.csv", row.names=FALSE)
    
    df <- data.frame(df)
    df[is.na(df)]=""
    
    return(df)
  })
  
  observeEvent(input$summitCSV|input$summitXLSX ,{
    
    req(input$xlsxFile)
    
    tablaCSV <- read.csv("E:/Storage/Github/TFM/tableCSV.csv", header = TRUE)
    
    tablaCSV[tablaCSV==""]=NA
    
    tableMarkers <- data.frame("Contracts" = tablaCSV$Contract)
    tableMarkers$CV <- 0
    tableMarkers$RD <- 0
    for(i in 1:nrow(tablaCSV)){
      Bids <- c()
      for(j in seq(from=3, to=ncol(tablaCSV), by=2)){
        if(!is.na(tablaCSV[i,j])){
          Bids <- append(Bids,as.numeric(tablaCSV[i,j]))
        }
      }
      tableMarkers$CV[i] <- sd(Bids)/mean(Bids)
      Bids <- sort(Bids,decreasing = FALSE)
      tableMarkers$RD[i] <- (Bids[2]-Bids[1])/sd(Bids[2:length(Bids)])
    }
    
    tableMarkers$Suspicius <- ""
    tableMarkers$Suspicius[tableMarkers$RD>1&tableMarkers$CV<=0.06]="Suspicius"
    tableMarkers <<- tableMarkers
    
    write.csv(tableMarkers,"./tableMarkers.csv", row.names = FALSE)
    
    output$markersTable <- renderDataTable({tableMarkers})
  })
  
  observeEvent(input$show,{
    output$Graph2D <<- renderPlotly({
      graphic2Dfuntion(input$firmOne, input$firmTwo)
    })
  })
  
  
  observeEvent(input$DoIt,{
    
    tableMarkers <- read.csv("E:/Storage/Github/TFM/tableMarkers.csv", header = TRUE)
    tableMarkers$RD_probability <- NA
    tableMarkers$CV_probability <- NA
    tableCSV <- read.csv("E:/Storage/Github/TFM/tableCSV.csv", header = TRUE)
    tableBids <- data.frame(tableCSV[,1])
    for(i in seq(3,ncol(tableCSV),2)){
      tableBids <- cbind(tableBids, tableCSV[,i])
    }
    tableBids <- tableBids[,-1]
    for(i in 1:nrow(tableBids)){
      funtionSample(length(which(is.na(tableBids[i,])==FALSE)), 1000, min(tableBids[i,], na.rm = TRUE), max(tableBids[i,], na.rm = TRUE))
      tableMarkers$RD_probability[i] <- sampleRD_frame$Probability[max(which(tableMarkers$RD[i]>sampleRD_frame$RD))]
      tableMarkers$CV_probability[i] <- sampleCV_frame$Probability[max(which(tableMarkers$CV[i]>sampleCV_frame$CV))]
    }
    output$markersTableFinal <- renderDataTable({tableMarkers})
    
  })
  

}
shinyApp(ui, server, options = list(launch.browser=TRUE))



















