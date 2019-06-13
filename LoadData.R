# ## Libraries
# ipak <- function(pkg){
#   new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
#   if (length(new.pkg))
#     install.packages(new.pkg, dependencies = TRUE, repos='http://cran.us.r-project.org')
#   sapply(pkg, require, character.only = TRUE)
# }
# 
# packages <- c("shiny","shinyjs","shinythemes","shinyWidgets","shinydashboard","xlsx","plotly","igraph","network","sna","ndtv","dplyr","visNetwork","rsconnect")
# 
# ipak(packages)
library("shiny")
library("shinyjs")
library("shinythemes")
library("shinyWidgets")
library("shinydashboard")
library("igraph")
library("network")
library("sna")
library("ndtv")
library("xlsx")
library("plotly")
library("dplyr")
library("visNetwork")
library("rsconnect")

## Inicialization
defaultValues <- 1
tableMarkers <- 0
requirement1 <- 0
requirement2 <- 0
FinalTable <- 0
columnNumber <- 2
listOfFirms <- list("No file uploaded")
tableDefault <- data.frame(Contract=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26),
                           Date=c("2018-01-07","2018-01-14","2018-01-21","2018-01-28","2018-02-04","2018-02-11","2018-02-18","2018-02-25","2018-03-04","2018-03-11","2018-03-18","2018-03-25","2018-04-01","2018-04-08","2018-04-15","2018-04-22","2018-04-29","2018-05-06","2018-05-13","2018-05-20","2018-05-27","2018-06-03","2018-06-10","2018-06-17","2018-06-24","2018-07-01"),
                           Firm=c("Empresa 1","Empresa 6","Empresa 1","Empresa 6","Empresa 1","Empresa 6","Empresa 1","Empresa 6","Empresa 1","Empresa 6","Empresa 1","Empresa 6","Empresa 1","Empresa 6","Empresa 1","Empresa 6","Empresa 1","Empresa 6","Empresa 1","Empresa 6","Empresa 1","Empresa 6","Empresa 1","Empresa 6","Empresa 1","Empresa 6"),
                           Bid=c(19000,24000,26200,29000,25770,18200,29400,27070,25680,19200,25450,26590,26000,29920,18750,25350,27800,19100,25670,26050,17800,24200,28680,24680,18800,28380),
                           Firm=c("Empresa 2","Empresa 7","Empresa 2","Empresa 7","Empresa 2","Empresa 7","Empresa 2","Empresa 7","Empresa 2","Empresa 7","Empresa 2","Empresa 7","Empresa 2","Empresa 7","Empresa 2","Empresa 7","Empresa 2","Empresa 7","Empresa 2","Empresa 7","Empresa 2","Empresa 7","Empresa 2","Empresa 7","Empresa 2","Empresa 7"),
                           Bid=c(24720,19200,25660,26340,18000,24330,27850,17800,24140,27920,19000,27950,24480,27120,29110,28450,29260,27200,18500,29300,25090,19000,24880,29630,29350,19000),
                           Firm=c("Empresa 3","Empresa 8","Empresa 3","Empresa 8","Empresa 3","Empresa 8","Empresa 3","Empresa 8","Empresa 3","Empresa 8","Empresa 3","Empresa 8","Empresa 3","Empresa 8","Empresa 3","Empresa 8","Empresa 3","Empresa 8","Empresa 3","Empresa 8","Empresa 3","Empresa 8","Empresa 3","Empresa 8","Empresa 3","Empresa 8"),
                           Bid=c(26940,24640,18900,26720,26540,29710,18500,28040,27980,29680,28560,19000,28660,19500,24610,27610,24160,27250,28880,28800,25930,24870,19300,20000,24120,27610),
                           Firm=c("Empresa 4","Empresa 9","Empresa 4","Empresa 9","Empresa 4","Empresa 9","Empresa 4","Empresa 9","Empresa 4","Empresa 9","Empresa 4","Empresa 9","Empresa 4","Empresa 9","Empresa 4","Empresa 9","Empresa 4","Empresa 9","Empresa 4","Empresa 9","Empresa 4","Empresa 9","Empresa 4","Empresa 9","Empresa 4","Empresa 9"),
                           Bid=c(26520,26210,25340,19200,24480,29060,25370,29590,18500,25280,29420,25350,18500,28930,25760,18800,18600,29850,25450,18900,28540,26250,24430,28730,25170,29370),
                           Firm=c(NA,"Empresa 5",NA,NA,NA,NA,NA,NA,"Empresa 10",NA,NA,NA,NA,NA,NA,"Empresa 5",NA,NA,NA,NA,NA,"Empresa 10",NA,NA,"Empresa 6","Empresa 5"),
                           Bid=c(NA,24000,NA,NA,NA,NA,NA,NA,26000,NA,NA,NA,NA,NA,NA,23750,NA,NA,NA,NA,NA,25000,NA,NA,24130,23500),
                           Firm=c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,"Empresa 7","Empresa 10"),
                           Bid=c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,19000,24000),
                           Firm=c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,"Empresa 8",NA),
                           Bid=c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,26980,NA),
                           Firm=c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,"Empresa 9",NA),
                           Bid=c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,29210,NA),
                           stringsAsFactors = FALSE)

## Aplication definition
header <- dashboardHeader(
  title = "Bid Rigging Screening"
)
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("File input", tabName = "menu1", icon = icon("file-upload")),
    menuItem("DataTable", tabName = "menu2", icon = icon("table")),
    menuItem("2D graph", tabName = "menu3", icon = icon("bar-chart-o")),
    menuItem("Variance Screning", tabName = "menu4", icon = icon("angle-double-right")),
    menuItem("Final table", tabName = "menu5", icon = icon("list-alt")),
    menuItem("Group Bidding", tabName = "menu6", icon = icon("grip-horizontal")),
    menuItem("Community network", tabName = "menu7", icon = icon("project-diagram")),
    menuItem("Final table", tabName = "menu8", icon = icon("bullseye"))
  )
)
body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = "menu1",
      fluidRow(
        column(6,
               box(
                 h3("Select the starting data set"),
                 hr(),
                 selectInput("defaultData", label = "", choices = list("Default data set" = 1, "Upload a file" = 0), selected = 1),
                 width = "100%",
                 height = "100%",
                 background = "blue"
               )
        ),
        column(4,
               actionButton("summitDefault", label = "Summit default", icon = icon("thumbs-up"), style="color: white; background-color: #000F89; border-color: #0011B7; padding:10; margin:0; position:rigth")
        )
      ),
      fluidRow(
        column(8,
               fluidRow(
                 column(3, fileInput("xlsxFile", label = h3("Upload a xlsx file"), multiple = FALSE,placeholder = "No file selected",buttonLabel = "Browse...",width = "100%",accept = c(".xlsx"))),
                 column(4, h6("")),
                 column(2, actionButton("summitXLSX", label = "Summit Excel", icon = icon("thumbs-up"), style="color: white; background-color: #000F89; border-color: #0011B7; padding:10; margin:0; position:rigth"))
               )
        )
      ),
      fluidRow(
        column(8,
               fluidRow(
                 column(3, fileInput("csvFile", label = h3("Upload a csv file"), multiple = FALSE, placeholder = "No file selected",buttonLabel = "Browse...",width = "100%",accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))),
                 column(4, radioButtons("sep", h3("Separator"), choices = c(Comma = ",", Semicolon = ";", Tab = "\t"), selected = ";", inline = TRUE)),
                 column(2, actionButton("summitCSV", label = "Summit Csv", icon = icon("thumbs-up"), style="color: white; background-color: #000F89; border-color: #0011B7; padding:10; margin:0; position:rigth"))
               )
        )
      ),
      numericInput("numberColumn", label = h3("Introduce the number of aditional columns with information relative to a contract"), value = 2, width = "100%", min = 1, max = 100,step = 1),
      tableOutput("tabla1"),
      tableOutput("tabla2")
    ),
    tabItem(
      tabName = "menu2",
      fluidRow(
        column(2, h2("Data table")),
        column(2, numericInput("RDlimit", label = "", value = 1, width = "100%", min = 0, max = 100,step = 0.01)),
        column(2, numericInput("CVlimit", label = "", value = 0.06, width = "100%", min = 0, max = 1,step = 0.001)),
        column(4,h6("")),
        column(2, actionButton("ShowSuspicious", label = "Show Suspicious", icon = icon("thumbs-up"), style="color: white; background-color: #000F89; border-color: #0011B7; padding:10; margin:0; position:rigth"))
      ),
      dataTableOutput("markersTable")
    ),
    tabItem(
      tabName = "menu3",
      fluidRow(
        column(10,
               box(fluidRow(
                 column(4, radioButtons("firmOne", label = h4("Select one firm:"),
                                        choices = list("You have to upload a file and summit"), 
                                        selected = "You have to upload a file and summit")),
                 column(4, radioButtons("firmTwo", label = h4("Select another different firm:"),
                                        choices = list("You have to upload a file and summit"), 
                                        selected = "You have to upload a file and summit"))
               ),
               width = "100%",
               height = "350px",
               background = "light-blue")
        ),
        column(2, actionButton("show", label = "Show Graph", icon = icon("thumbs-up"), style="color: white; background-color: #000F89; border-color: #0011B7; padding:10; margin:0; position:rigth")
        )
      ),
      fluidRow(
        column(8,
               plotlyOutput("Graph2D")
        )
      )
    ),
    tabItem(
      tabName = "menu4",
      h3("Coeficient of Variation over time"),
      plotlyOutput("GraphCV")
    ),
    tabItem(
      tabName = "menu5",
      h3("The time it takes to process this table is high"),
      actionButton("DoIt", label = "Show the final table", icon = icon("thumbs-up"), style="color: white; background-color: #000F89; border-color: #0011B7; padding:10; margin:0; position:rigth"),
      dataTableOutput("markersTableFinal")
    ),
    tabItem(
      tabName = "menu6",
      fluidRow(
        column(6,
               fluidRow(
                 column(8,
                        box(
                          checkboxGroupButtons("groupBidding", label = h4("Select the firms that you want to analize:"),individual = TRUE, checkIcon = icon("thumbs-up"),
                                               choices = list("You have to upload a file and summit"), direction = "vertical", status = "warning"),
                          actionButton("ShowContracts", label = "Show contracts", icon = icon("thumbs-up"), style="color: white; background-color: #000F89; border-color: #0011B7; padding:10; margin:0; position:rigth"),
                          width = "100%",
                          height = "100%",
                          background = "aqua")
                 )
               )
        ),
        column(6,
               h3("The contracts in which the selected firms have bid together are the following :"),
               br(),
               tableOutput("HolaQueTal")
        )
      )
      
    ),
    tabItem(
      tabName = "menu7",
      fluidRow(
        column(12,
               h1("Firm Network"),
               visNetworkOutput("network")
        )
      )
    ),
    tabItem(
      tabName = "menu8",
      fluidRow(
        column(12,
               h1("Final table"),
               dataTableOutput("tablaFinal")
        )
      )
    )
  )
)

ui <- dashboardPage(header, sidebar, body)

server <- function(input, output, session) {
  
  graphic2Dfuntion <- function(FirstFirm, SecondFirm, columnNumber){
    
    BidData <- read.csv("./tableCSV.csv", header = TRUE)
    BidData <- BidData[,seq(-1*(columnNumber),-2,1)]
    
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
                       marker = list(color = 'rgb(100, 150, 255)', size = 15,line = list(color = 'rgb(0, 0, 255)', width = 1))) %>%
      layout(title = "", titlefont = list(size = 1,color = 'rgb(47, 47, 147)'), autosize = TRUE, width = "350px", height = "350px",
             yaxis = list(color = toRGB("black"),title = FirstFirm, range = c(-0.05,1.05), dtick = 0.5, gridcolor = toRGB("black"), titlefont = list(size = 18,color = 'rgb(255, 255, 255)'), tickfont = list(size = 12,color = 'rgb(0, 0, 0)')),
             xaxis = list(color = toRGB("black"), title = SecondFirm, range = c(-0.05,1.05), dtick = 0.5, gridcolor = toRGB("black"), titlefont = list(size = 18,color = 'rgb(255, 255, 255)'), tickfont = list(size = 12,color = 'rgb(0, 0, 0)')),
             margin = list(t = 50, b = 50,r = 50,l = 50, autoexpand = FALSE), plot_bgcolor = toRGB("white"), paper_bgcolor  = 'rgb(60, 141, 188)', barmode = 'group', bargap = 10,
             showlegend = FALSE) %>%
      add_trace(y= comparisonTable[,10],type = "scatter",mode = "markers", name="Non competitive",marker = list(color = 'rgb(255,100,100)', size = 15,line = list(color = 'rgb(150,50,50)', width = 1)))
    
    results <- list(graphic)
    return(graphic)
  }
  
  funtionSample <- function(numberOfBids, numberOfContracts, lowerPrice, higherPrice){
    listOfSamples <- list()
    sampleRD <- data.frame()
    sampleCV <- data.frame()
    for(i in c(1:numberOfContracts)){
      listOfSamples[i] <- list(sort(sample(lowerPrice:higherPrice, numberOfBids, replace=TRUE)))
      if(listOfSamples[[i]][1] > lowerPrice){
        listOfSamples[[i]][1] <- lowerPrice
      }
      if(listOfSamples[[i]][length(listOfSamples[[i]])] < higherPrice){
        listOfSamples[[i]][length(listOfSamples[[i]])] <- higherPrice
      }
      if(sd(listOfSamples[[i]][2:numberOfBids]) == 0){
        sampleRD[i,1] <- 100
        sampleCV[i,1] <- sd(listOfSamples[[i]])/mean(listOfSamples[[i]])
      }else{
        sampleRD[i,1] <- (listOfSamples[[i]][2]-listOfSamples[[i]][1])/sd(listOfSamples[[i]][2:numberOfBids])
        sampleCV[i,1] <- sd(listOfSamples[[i]])/mean(listOfSamples[[i]])
      }
    }
    sampleRD_frame <- data.frame("RD" = sampleRD[with(sampleRD, order(sampleRD[,1])), ])
    sampleRD_frame$Frecuency <- 1
    sampleRD_frame$Probability <- NA
    for(i in c(1:nrow(sampleRD_frame))){
      sampleRD_frame$Probability[i] <- sum(sampleRD_frame$Frecuency[1:i])/numberOfContracts
    }
    sampleCV_frame <- data.frame("CV" = sampleCV[with(sampleCV, order(sampleCV[,1])), ])
    sampleCV_frame$Frecuency <- 1
    sampleCV_frame$Probability <- NA
    for(i in c(1:nrow(sampleCV_frame))){
      sampleCV_frame$Probability[i] <- sum(sampleCV_frame$Frecuency[1:i])/numberOfContracts
    }
    
    listOfSamples <<- listOfSamples
    sampleRD_frame <<- sampleRD_frame
    sampleCV_frame <<- sampleCV_frame
    sampleRD <<- sort(sampleRD[,1], decreasing = TRUE)
    sampleCV <<- sort(sampleCV[,1], decreasing = TRUE)
  }
  
  functionFirmList <- function(columnNumber){
    
    tableCSV <- read.csv("./tableCSV.csv", stringsAsFactors = FALSE)
    
    for(i in seq(1+columnNumber,ncol(tableCSV),2)){
      if(i == 1+columnNumber){
        tmp <- data.frame(tableCSV[,i][!is.na(tableCSV[,i])], stringsAsFactors = FALSE)
        tmp1 <- data.frame(tableCSV[,i], stringsAsFactors = FALSE)
      }else{
        tmp <- rbind(tmp,data.frame(tableCSV[,i][!is.na(tableCSV[,i])], stringsAsFactors = FALSE))
        tmp1 <- cbind(tmp1,data.frame(tableCSV[,i], stringsAsFactors = FALSE))
      }
    }
    allFirms <- data.frame(unique(tmp), stringsAsFactors = FALSE)
    allFirms <- data.frame(allFirms[which(allFirms[,1]!=""),], stringsAsFactors = FALSE)
    colnames(allFirms) <- "Firms"
    for(i in seq(1+columnNumber,ncol(tableCSV),2)){
      if(i == 1+columnNumber){
        tmp <- tableCSV[,i]
      }else{
        tmp <- cbind(tmp, tableCSV[,i])
      }
    }
    tableFirms <- data.frame("Contract" = tableCSV$Contract)
    for(i in 1:nrow(allFirms)){
      tmp1 <- data.frame("Contract" = which(tmp == allFirms[i,1], arr.ind = TRUE)[,1])
      tmp1[,2] <- which(tmp == allFirms[i,1], arr.ind = TRUE)[,1]
      tableFirms <- merge(x = tableFirms, y = tmp1, by = "Contract",all.x = TRUE)
    }
    colnames(tableFirms) <-c("Contract", allFirms[,1])
    tableFirms[,2:ncol(tableFirms)][is.na(tableFirms[,2:ncol(tableFirms)])==FALSE] = "ok"
    tableFirms[is.na(tableFirms)==TRUE] = 0
    FinalTable <- data.frame("Firms" = allFirms[1], stringsAsFactors = FALSE)
    FinalTable <<- FinalTable
    tableFirms <<- tableFirms
    allFirms <<- allFirms
    requirement2 <<- "Hello"
  }
  
  functionCommunContracts <- function(selectedFirmsName,allFirms,tableFirms){
    selectedFirmsPosition <- c()
    for(i in selectedFirmsName){
      selectedFirmsPosition <- cbind(selectedFirmsPosition, which(allFirms == i) + 1)
    }
    
    
    
    tmp <- data.frame() 
    for(i in selectedFirmsPosition){
      
      tmp <- rbind(tmp, data.frame(which(tableFirms[,i] == "ok")))
      
    }
    colnames(tmp) <- c("Firms")
    tmp$frec <- 1
    tmp1 <- tmp
    tmp2 <- group_by(tmp1, by = Firms) %>%
      summarise(frec = sum(frec))
    
    communContracts <<- tmp2[which(tmp2$frec == length(selectedFirmsPosition)),1]
  }
  
  networkFunction <- function(allFirms,tableFirms){
    
    tableCSV <- read.csv("./tableCSV.csv", stringsAsFactors = FALSE)
    
    nodes <- data.frame(id = 1:nrow(allFirms),
                        label = allFirms$Firms,
                        group = c("Group A"),
                        value = 0.5,
                        shape = c("circle"),
                        title = paste0("<p><b>", allFirms$Firms,"</b><br>Node !</p>"),
                        shadow = FALSE,
                        stringsAsFactors = FALSE)
    
    links <- data.frame("from" = 0, "to" = 0, "weight" = 0, stringsAsFactors = FALSE)
    for(firmFrom in 2:nrow(nodes)){
      for(firmTo in (firmFrom+1):ncol(tableFirms)){
        if(length(which(tableFirms[,firmFrom] == "ok" & tableFirms[,firmTo] == "ok"))>0){
          links <- rbind(links, c(firmFrom-1, firmTo-1, length(which(tableFirms[,firmFrom] == "ok" & tableFirms[,firmTo] == "ok"))))
        }
      }
    }
    links <- links[-1,]
    
    edges <- data.frame(from = links$from,
                        to = links$to,
                        length = 200,
                        width = links$weight/3,
                        dashes = FALSE,
                        smooth = FALSE,
                        shadow = FALSE)
    
    net <- graph_from_data_frame(d=links, vertices=nodes, directed=T)
    cfg <- cluster_fast_greedy(as.undirected(net))
    for (i in 1:length(cfg$membership)) {
      nodes$group[i] <- paste0("Group ",cfg$membership[i])
    }
    
    firmNetwork <- visNetwork(nodes, edges, height = "500px", width = "100%") %>% 
      visInteraction(dragNodes = TRUE, 
                     dragView = FALSE, 
                     zoomView = FALSE) %>%
      visLayout(randomSeed = 42) %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      visConfigure(enabled = TRUE)
    return(firmNetwork)
  }
  
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
    
    write.csv(df,"./tableCSV.csv", row.names=FALSE)
    
    df <- data.frame(df)
    df[is.na(df)]=""
    
    df[,1] <- as.integer(df[,1])
    requirement1 <<- "Hello"
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
    
    write.csv(df,"./tableCSV.csv", row.names=FALSE)
    
    df <- data.frame(df)
    df[is.na(df)]=""
    requirement1 <<- "Hello"
    return(df)
  })
  
  observeEvent(input$defaultData, {
    defaultValues <- input$defaultData
  })
  
  observeEvent(input$summitDefault, {
    write.csv(tableDefault,"./tableCSV.csv", row.names=FALSE)
    requirement1 <<- "Hello"
  })
  
  observeEvent((input$summitCSV|input$summitXLSX|input$summitDefault)&input$ShowSuspicious ,{
    
    if(requirement1=="Hello"){
      
      columnNumber <<- input$numberColumn
      tableCSV <- read.csv("./tableCSV.csv", header = TRUE)
      
      
      tableCSV[tableCSV==""]=NA
      
      tableMarkers <- data.frame("Contracts" = tableCSV$Contract)
      tableMarkers$CV <- 0
      tableMarkers$RD <- 0
      tableMarkers$Suspicious <- ""
      tableMarkers$Winner <- 0
      for(i in 1:nrow(tableCSV)){
        Bids <- c()
        for(j in seq(from=columnNumber+2, to=ncol(tableCSV), by=2)){
          if(!is.na(tableCSV[i,j])){
            Bids <- append(Bids,as.numeric(tableCSV[i,j]))
          }
        }
        tableMarkers$CV[i] <- sd(Bids)/mean(Bids)
        tableMarkers$Winner[i] <- as.character(tableCSV[i,which(min(Bids)==Bids)[1]*2-1+columnNumber])
        Bids <- sort(Bids,decreasing = FALSE)
        tableMarkers$RD[i] <- (Bids[2]-Bids[1])/sd(Bids[2:length(Bids)])
      }
      
      tableMarkers$Suspicious[tableMarkers$RD>input$RDlimit&tableMarkers$CV<=input$CVlimit]="Suspicious"
      tableMarkers <<- tableMarkers
      
      write.csv(tableMarkers,"./tableMarkers.csv", row.names = FALSE)
      
      tmp <- data.frame("CV"=tableMarkers$CV)
      tmp$Date <- tableCSV$Date
      
      CVgraph <<- plot_ly(data = tmp, x =~Date, y =~CV, type = "scatter",mode = "markers" , name="Competitive", marker = list(color = 'rgb(100, 150, 255)',line = list(color = 'rgb(0, 0, 255)', width = 0.5))) %>%
        layout(title = "Coeficient of Variation over time", titlefont = list(size = 20,color = 'rgb(47, 47, 147)'),
               yaxis = list(linecolor = toRGB("black"),color = toRGB("black"),title = "CV", titlefont = list(size = 18,color = 'rgb(0, 0, 0)'), tickfont = list(size = 12,color = 'rgb(0, 0, 0)')),
               xaxis = list(linecolor = toRGB("black"),color = toRGB("black"),title = "Date", titlefont = list(size = 18,color = 'rgb(0, 0, 0)'), tickfont = list(size = 12,color = 'rgb(0, 0, 0)')),
               margin = list(t = 75, b = 50,r = 10,l = 80), plot_bgcolor = 'rgb(255, 255, 255)', paper_bgcolor  = 'rgb(186, 186, 186)', barmode = 'group', bargap = 10)
      
      output$GraphCV <<- renderPlotly({
        CVgraph
      })
      
      output$markersTable <- renderDataTable({tableMarkers})
    }
  })
  
  observeEvent(input$show,{
    output$Graph2D <<- renderPlotly({
      graphic2Dfuntion(input$firmOne, input$firmTwo, columnNumber)
    })
  })
  
  observeEvent(input$DoIt,{
    
    tableMarkers <- read.csv("./tableMarkers.csv", header = TRUE)
    tableMarkers$CV_probability <- NA
    tableMarkers$RD_probability <- NA
    tableMarkers$statisticalMarker <- NA
    tableCSV <- read.csv("./tableCSV.csv", header = TRUE)
    tableBids <- data.frame(tableCSV[,1])
    for(i in seq(2+columnNumber,ncol(tableCSV),2)){
      tableBids <- cbind(tableBids, tableCSV[,i])
    }
    tableBids <- tableBids[,-1]
    for(i in 1:nrow(tableBids)){
      funtionSample(length(which(is.na(tableBids[i,])==FALSE)), 1000, min(tableBids[i,], na.rm = TRUE), max(tableBids[i,], na.rm = TRUE))
      tableMarkers$RD_probability[i] <- 1 - sampleRD_frame$Probability[max(which(tableMarkers$RD[i]>sampleRD_frame$RD))]
      tableMarkers$CV_probability[i] <- sampleCV_frame$Probability[max(which(tableMarkers$CV[i]>sampleCV_frame$CV))]
      if(tableMarkers$RD_probability[i] <= 0.05 & tableMarkers$CV_probability[i] <= 0.05){
        tableMarkers$statisticalMarker[i] <- "Suspicious"
      }
    }
    output$markersTableFinal <- renderDataTable({tableMarkers})
    
  })
  
  observeEvent(input$summitCSV|input$summitXLSX|input$summitDefault,{
    if(requirement1=="Hello"){
      functionFirmList(columnNumber)
      
      x <- input$groupBidding
      # Can also set the label and select items
      updateCheckboxGroupButtons(session, "groupBidding",
                                 label = "Select the firms that you want to analize:",
                                 choices = allFirms[,1],
                                 selected = "No Firm selected"
      )
      
      x <- input$firmOne
      # Can also set the label and select items
      updateRadioButtons(session, "firmOne",
                         label = "Select the firms that you want to compare:",
                         choices = allFirms[,1],
                         selected = "No Firm selected"
      )
      x <- input$firmTwo
      # Can also set the label and select items
      updateRadioButtons(session, "firmTwo",
                         label = "Select the firms that you want to compare:",
                         choices = allFirms[,1],
                         selected = "No Firm selected"
      )
      
      output$network <- renderVisNetwork({
        networkFunction(allFirms,tableFirms)
      })
    }
  })
  
  observeEvent(input$ShowContracts,{
    
    selectedFirmsName<-input$groupBidding
    functionCommunContracts(selectedFirmsName,allFirms,tableFirms)
    output$HolaQueTal <- renderTable(communContracts, colnames = FALSE)
    
  })
  
  observeEvent(input$DoIt,{
    if(requirement2=="Hello"){
      FinalTable$Contracts <- 0
      for(i in c(allFirms[[1]])){
        FinalTable$Contracts[which(FinalTable[1]==i)] <- length(which(tableMarkers[5]==i))
      }
      output$tablaFinal <- renderDataTable({FinalTable})
    }
  })
  
}
shinyApp(ui, server, options = list(launch.browser=TRUE))











