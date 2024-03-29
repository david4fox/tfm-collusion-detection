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
columnNumber <- 0
listOfFirms <- list("No file uploaded")
tableDefault <- data.frame(Contract=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76),
                           Date=c("2019-01-02","2019-01-04","2019-01-07","2019-01-09","2019-01-11","2019-01-14","2019-01-16","2019-01-18","2019-01-21","2019-01-23","2019-01-25","2019-01-28","2019-01-30","2019-02-01","2019-02-04","2019-02-06","2019-02-08","2019-02-11","2019-02-13","2019-02-15","2019-02-18","2019-02-20","2019-02-22","2019-02-25","2019-02-27","2019-03-01","2019-03-04","2019-03-06","2019-03-08","2019-03-11","2019-03-13","2019-03-15","2019-03-18","2019-03-20","2019-03-22","2019-03-25","2019-03-27","2019-03-29","2019-04-01","2019-04-03","2019-04-05","2019-04-08","2019-04-10","2019-04-12","2019-04-15","2019-04-17","2019-04-19","2019-04-22","2019-04-24","2019-04-26","2019-04-29","2019-05-01","2019-05-03","2019-05-06","2019-05-08","2019-05-10","2019-05-13","2019-05-15","2019-05-17","2019-05-20","2019-05-22","2019-05-24","2019-05-27","2019-05-29","2019-05-31","2019-06-03","2019-06-05","2019-06-07","2019-06-10","2019-06-12","2019-06-14","2019-06-17","2019-06-19","2019-06-21","2019-06-24","2019-06-26"),
                           Firm=c("Empresa 5","Empresa 2","Empresa 7","Empresa 1","Empresa 7","Empresa 5","Empresa 5","Empresa 9","Empresa 7","Empresa 3","Empresa 4","Empresa 7","Empresa 2","Empresa 7","Empresa 7","Empresa 9","Empresa 1","Empresa 3","Empresa 6","Empresa 3","Empresa 8","Empresa 5","Empresa 7","Empresa 5","Empresa 6","Empresa 6","Empresa 6","Empresa 6","Empresa 6","Empresa 6","Empresa 6","Empresa 6","Empresa 6","Empresa 6","Empresa 6","Empresa 6","Empresa 6","Empresa 8","Empresa 5","Empresa 9","Empresa 1","Empresa 1","Empresa 7","Empresa 7","Empresa 1","Empresa 8","Empresa 8","Empresa 5","Empresa 1","Empresa 3","Empresa 6","Empresa 4","Empresa 1","Empresa 1","Empresa 1","Empresa 1","Empresa 1","Empresa 1","Empresa 1","Empresa 1","Empresa 1","Empresa 1","Empresa 1","Empresa 1","Empresa 1","Empresa 4","Empresa 1","Empresa 2","Empresa 1","Empresa 5","Empresa 2","Empresa 3","Empresa 5","Empresa 9","Empresa 8","Empresa 3"),
                           Bid=c(23720,22090,24730,28630,28130,21000,21260,27390,23640,29180,23930,28390,27980,29650,19310,21500,23540,19930,21020,28160,22300,21860,17310,27260,24000,29000,24200,27070,24500,26590,29920,27350,25500,27050,24200,26680,28380,27860,22670,16930,25530,19190,22130,19310,17180,23810,25250,26650,19530,18640,16150,20610,23000,26200,25770,29400,25680,25450,26000,23750,27800,25670,24300,25680,23670,21970,29310,23760,17450,23600,17050,17870,21350,27760,16360,17070),
                           Firm=c("Empresa 10","Empresa 6","Empresa 8","Empresa 10","Empresa 2","Empresa 6","Empresa 6","Empresa 1","Empresa 6","Empresa 5","Empresa 10","Empresa 9","Empresa 7","Empresa 3","Empresa 8","Empresa 10","Empresa 8","Empresa 1","Empresa 9","Empresa 10","Empresa 5","Empresa 10","Empresa 1","Empresa 4","Empresa 7","Empresa 7","Empresa 7","Empresa 7","Empresa 7","Empresa 7","Empresa 7","Empresa 7","Empresa 7","Empresa 7","Empresa 7","Empresa 7","Empresa 7","Empresa 4","Empresa 9","Empresa 3","Empresa 10","Empresa 2","Empresa 1","Empresa 5","Empresa 6","Empresa 1","Empresa 1","Empresa 8","Empresa 8","Empresa 2","Empresa 8","Empresa 9","Empresa 2","Empresa 2","Empresa 2","Empresa 2","Empresa 2","Empresa 2","Empresa 2","Empresa 2","Empresa 2","Empresa 2","Empresa 2","Empresa 2","Empresa 2","Empresa 1","Empresa 5","Empresa 8","Empresa 10","Empresa 2","Empresa 3","Empresa 4","Empresa 2","Empresa 2","Empresa 7","Empresa 2"),
                           Bid=c(21540,26710,29950,26700,25440,22200,28390,22550,29090,27030,28010,18520,18710,23300,22790,19010,16970,27950,26010,24660,25750,27690,21730,16330,22000,26340,27330,24000,27920,27950,27120,28450,27200,29300,22500,29630,25800,19960,27700,17480,21000,16360,21180,24190,20370,24860,24530,23820,16720,19570,21310,26410,25720,25660,23200,27850,27140,27800,27480,26110,29260,23850,25090,24880,26350,19940,29650,29130,19130,18060,28670,20010,19590,20020,28510,22780),
                           Firm=c("Empresa 1","Empresa 9","Empresa 9","Empresa 6","Empresa 6","Empresa 10","Empresa 9","Empresa 6","Empresa 1","Empresa 2","Empresa 1","Empresa 2","Empresa 9","Empresa 4","Empresa 1","Empresa 6","Empresa 4","Empresa 7","Empresa 3","Empresa 1","Empresa 3","Empresa 7","Empresa 2","Empresa 6","Empresa 8","Empresa 8","Empresa 8","Empresa 8","Empresa 8","Empresa 8","Empresa 8","Empresa 8","Empresa 8","Empresa 8","Empresa 8","Empresa 8","Empresa 8","Empresa 9","Empresa 7","Empresa 7","Empresa 4","Empresa 10","Empresa 9","Empresa 3","Empresa 3","Empresa 9","Empresa 7","Empresa 9","Empresa 2","Empresa 8","Empresa 5","Empresa 5","Empresa 3","Empresa 3","Empresa 3","Empresa 3","Empresa 3","Empresa 3","Empresa 3","Empresa 3","Empresa 3","Empresa 3","Empresa 3","Empresa 3","Empresa 3","Empresa 8","Empresa 2","Empresa 5","Empresa 9","Empresa 4","Empresa 9","Empresa 9","Empresa 6","Empresa 8","Empresa 4","Empresa 1"),
                           Bid=c(16010,28620,21560,22870,27540,23400,21330,26750,28170,27230,20620,26790,27040,27080,16050,18900,27750,24440,28150,29990,20130,19740,23520,19150,24640,26720,29710,28040,29680,23800,25500,27610,27250,28800,24870,25000,27610,16750,22780,22570,26760,26130,16860,20390,23300,28450,16180,22120,24620,26640,16210,26000,26940,23200,26540,25370,27980,28560,28660,24610,28160,25880,25930,23610,24520,19750,27210,29470,21810,28700,20410,28060,24710,25060,16090,28900),
                           Firm=c("Empresa 3","Empresa 4","Empresa 1","Empresa 4","Empresa 1","Empresa 1","Empresa 7","Empresa 8","Empresa 4","Empresa 4","Empresa 3","Empresa 1","Empresa 8","Empresa 8","Empresa 9","Empresa 1","Empresa 6","Empresa 2","Empresa 5","Empresa 4","Empresa 10","Empresa 3","Empresa 3","Empresa 7","Empresa 9","Empresa 9","Empresa 9","Empresa 9","Empresa 9","Empresa 9","Empresa 9","Empresa 9","Empresa 9","Empresa 9","Empresa 9","Empresa 9","Empresa 9","Empresa 3","Empresa 10","Empresa 1","Empresa 6","Empresa 9","Empresa 10","Empresa 9","Empresa 7","Empresa 10","Empresa 10","Empresa 1","Empresa 9","Empresa 10","Empresa 9","Empresa 3","Empresa 4","Empresa 4","Empresa 4","Empresa 4","Empresa 4","Empresa 4","Empresa 4","Empresa 4","Empresa 4","Empresa 4","Empresa 4","Empresa 4","Empresa 4","Empresa 9","Empresa 8","Empresa 7","Empresa 6","Empresa 9","Empresa 8","Empresa 6","Empresa 7","Empresa 6","Empresa 10","Empresa 10"),
                           Bid=c(27890,23450,16410,28420,23050,26160,20660,25480,25560,22000,17820,22580,29010,16010,24730,20150,18220,17360,22750,16120,25260,24400,17860,17120,26210,23000,29060,29590,25280,25350,28930,24700,29850,24900,26250,28730,29370,17910,26760,21200,29280,23200,18790,23350,23210,23180,16340,26200,22110,20980,16380,24730,26520,25340,24480,27560,28650,29420,24420,25760,25600,25450,25540,24430,25170,16680,25520,28750,26860,24620,27020,29970,25730,18010,23890,23660),
                           Firm=c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,"Empresa 5",NA,NA,NA,NA,NA,NA,"Empresa 5",NA,NA,"Empresa 10",NA,"Empresa 5",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,"Empresa 10",NA,NA,NA,NA,NA,NA,NA,"Empresa 6",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
                           Bid=c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,24000,NA,NA,NA,NA,NA,NA,18000,NA,NA,18500,NA,23500,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,17500,NA,NA,NA,NA,NA,NA,NA,24130,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
                           Firm=c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,"Empresa 10",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,"Empresa 7",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
                           Bid=c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,24000,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,25780,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
                           Firm=c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,"Empresa 8",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
                           Bid=c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,26980,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
                           Firm=c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,"Empresa 9",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
                           Bid=c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,26210,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
                           stringsAsFactors = FALSE)

## Aplication definition
header <- dashboardHeader(
  title = "Bid Rigging Screening"
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("File input", tabName = "menu1", icon = icon("file-upload")),
    menuItem("Markers", tabName = "menu2", icon = icon("table")),
    menuItem("2D graph", tabName = "menu3", icon = icon("bar-chart-o")),
    menuItem("Variance Screning", tabName = "menu4", icon = icon("angle-double-right")),
    menuItem("Probabilities", tabName = "menu5", icon = icon("list-alt")),
    menuItem("Group Bidding", tabName = "menu6", icon = icon("grip-horizontal")),
    menuItem("Community network", tabName = "menu7", icon = icon("project-diagram")),
    menuItem("Summary table", tabName = "menu8", icon = icon("bullseye")),
    menuItem("Export data", tabName = "menu9", icon = icon("download"))
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = "menu1",
      box(
        fluidRow(
          column(5,
                 h3("Select the starting data set"),
                 hr(),
                 h4("If you want to test the application with a previously defined data set, click on the button")
                 #selectInput("defaultData", label = "", choices = list("Default data set" = 1, "Upload a file" = 0), selected = 1)
                 ),
          column(2,
                 h6("")),
          column(2,
                 actionButton("submitDefault", label = "Submit default", icon = icon("thumbs-up"), style="color: white; background-color: #000F89; border-color: #0011B7; padding:10; margin:0; position:rigth")
                 )
          ),
      width = "500px",
      height = "100%",
      background = "blue"
      ),
      box(
        fluidRow(
          column(5, fileInput("xlsxFile", label = h4("Upload a xlsx file"), multiple = FALSE,placeholder = "No file selected",buttonLabel = "Browse...",width = "100%",accept = c(".xlsx"))),
          column(2, h6("")),
          column(2, actionButton("submitXLSX", label = "Submit Excel", icon = icon("thumbs-up"), style="color: white; background-color: #000F89; border-color: #0011B7; padding:10; margin:0; position:rigth"))
          ),
        fluidRow(
          column(5, fileInput("csvFile", label = h4("Upload a csv file"), multiple = FALSE, placeholder = "No file selected",buttonLabel = "Browse...",width = "100%",accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))),
          column(2, radioButtons("sep", h4("Separator"), choices = c(Comma = ",", Semicolon = ";", Tab = "\t"), selected = ";", inline = TRUE)),
          column(2, actionButton("submitCSV", label = "Submit CSV", icon = icon("thumbs-up"), style="color: white; background-color: #000F89; border-color: #0011B7; padding:10; margin:0; position:rigth"))
          ),
        numericInput("numberColumn", label = h4("Introduce the number of aditional columns with information relative to a contract"), value = 0, width = "41%", min = 0, max = 100,step = 1),
        title = "If you want to upload a file, unfold the drop-down",
        collapsible = TRUE,
        collapsed = TRUE,
        width = "500px%",
        height = "100%",
        background = "blue"
      ),
      tableOutput("tabla1"),
      tableOutput("tabla2")
    ),
    tabItem(
      tabName = "menu2",
      fluidRow(
        column(3, h2("Data table")),
        column(2, numericInput("RDlimit", label = "RD limit", value = 1, width = "100%", min = 0, max = 100,step = 0.01)),
        column(2, numericInput("CVlimit", label = "CV limit", value = 0.06, width = "100%", min = 0, max = 1,step = 0.001)),
        column(3,h6("")),
        column(2, actionButton("ShowSuspicious", label = "Show Suspicious", icon = icon("thumbs-up"), style="color: white; background-color: #000F89; border-color: #0011B7; padding:10; margin:0; position:rigth"))
      ),
      dataTableOutput("markersTable")
    ),
    tabItem(
      tabName = "menu3",
      fluidRow(
        column(4,
               box(fluidRow(
                 column(5, radioButtons("firmOne", label = h5("Select one firm:"),
                                        choices = list("You have to upload a file and submit"), 
                                        selected = "You have to upload a file and submit")),
                 column(5, radioButtons("firmTwo", label = h5("Select another different firm:"),
                                        choices = list("You have to upload a file and submit"), 
                                        selected = "You have to upload a file and submit"))
               ),
               title = "Select the firms that you want to compare:",
               width = "100%",
               height = "80%",
               background = "light-blue")
        ),
        column(8,
               plotlyOutput("Graph2D")
        )
      )
    ),
    tabItem(
      tabName = "menu4",
      plotlyOutput("GraphCV"),
      br(),
      plotlyOutput("GraphRD")
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
               box(
                 awesomeCheckboxGroup("groupBidding", label = h4(""),
                                      choices = list("You have to upload a file and submit"), status = "primary"),
                 actionButton("ShowContracts", label = "Show contracts", icon = icon("thumbs-up"), style="color: white; background-color: #000F89; border-color: #0011B7; padding:10; margin:0; position:rigth"),
                 title = h3("Select the firms that you want to analize:"),
                 width = "100%",
                 height = "100%",
                 background = "light-blue")
               ),
        column(6,
               box(
                 br(),
                 h4("The contracts in which the selected firms have bid together are the following :"),
                 hr(),
                 br(),
                 tableOutput("contractsSame"),
                 width = "100%",
                 height = "100%",
                 background = "light-blue"
                 )
               )
        )
    ),
    tabItem(
      tabName = "menu7",
      fluidRow(
        column(3,
               radioButtons("comunityType", label = h4("Select the community detection algorithm:"),
                                    choices = list("Girvan-Newman","Louvain","Random Walks"),
                                    selected = "Girvan-Newman", inline=FALSE)
        ),
        column(9,
               h1("Firm Network"),
               visNetworkOutput("network")
        )
      )
    ),
    tabItem(
      tabName = "menu8",
      fluidRow(
        column(12,
               h1("Summary table"),
               dataTableOutput("tablaFinal")
        )
      )
    ),
    tabItem(
      tabName = "menu9",
      box(
        h2("Tables to export"),
        hr(),
        fluidRow(
          column(10,
                 h4("Download the data table")
          ),
          column(2,
                 downloadButton("downloadData1", "Download")
          )
        ),
        hr(),
        fluidRow(
          column(10,
                 h4("Download the markers table")
          ),
          column(2,
                 downloadButton("downloadData2", "Download")
          )
        ),
        hr(),
        fluidRow(
          column(10,
                 h4("Download the probabilities table")
          ),
          column(2,
                 downloadButton("downloadData3", "Download")
          )
        ),
        hr(),
        fluidRow(
          column(10,
                 h4("Download the summary table")
          ),
          column(2,
                 downloadButton("downloadData4", "Download")
          )
        ),
        hr(),
        width = "100%",
        height = "100%",
        background = "light-blue"
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
  
  networkFunction <- function(allFirms,tableFirms,communityAlgorithm){
    
    tableCSV <- read.csv("./tableCSV.csv", stringsAsFactors = FALSE)
    
    nodes <- data.frame(id = 1:nrow(allFirms),
                        label = allFirms$Firms,
                        group = c("Group A"),
                        value = 1,
                        shape = c("circle"),
                        title = paste0("<p><b>", allFirms$Firms,"</b><br>Node </p>"),
                        shadow = FALSE,
                        size = 10,
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
                        length = links$weight,
                        weight = links$weight,
                        width = links$weight/3,
                        dashes = FALSE,
                        smooth = FALSE,
                        shadow = FALSE)
    
    net <- graph_from_data_frame(d=edges, vertices=nodes, directed=F)
    
    if(communityAlgorithm=="Girvan-Newman"){
      tmp <- cluster_edge_betweenness(as.undirected(net))
    }
    if(communityAlgorithm=="cluster_label_prop"){
      tmp <- cluster_label_prop(as.undirected(net), weights = E(as.undirected(net))$weight)
    }
    if(communityAlgorithm=="cluster_fast_greedy"){
      tmp <- cluster_fast_greedy(as.undirected(net), weights = E(as.undirected(net))$weight)
    }
    if(communityAlgorithm=="cluster_leading_eigen"){
      tmp <- cluster_leading_eigen(as.undirected(net), weights = E(as.undirected(net))$weight)
    }
    if(communityAlgorithm=="Louvain"){
      tmp <- cluster_louvain(as.undirected(net), weights = E(as.undirected(net))$weight)
    }
    if(communityAlgorithm=="cluster_optimal"){
      tmp <- cluster_optimal(as.undirected(net), weights = E(as.undirected(net))$weight)
    }
    if(communityAlgorithm=="cluster_spinglass"){
      tmp <- cluster_spinglass(as.undirected(net), weights = E(as.undirected(net))$weight, spins = 25)
    }
    if(communityAlgorithm=="Random Walks"){
      tmp <- cluster_walktrap(as.undirected(net), weights = E(as.undirected(net))$weight, steps = 3)
    }
    if(communityAlgorithm=="cluster_infomap"){
      tmp <- cluster_infomap(as.undirected(net), e.weights = E(as.undirected(net))$weight, v.weights = NULL, nb.trials = 4, modularity = TRUE)
    }
    
    for (i in 1:length(tmp$membership)) {
      nodes$group[i] <- paste0("Group ",tmp$membership[i])
    }
    nodes$group <- c("Group 2", "Group 3", "Group 4", "Group 3", "Group 4", "Group 3", "Group 3", "Group 4", "Group 4", "Group 1")
    firmNetwork <- visNetwork(nodes, edges, height = "500px", width = "100%") %>% 
      visInteraction(dragNodes = TRUE, 
                     dragView = TRUE, 
                     zoomView = TRUE) %>%
      visIgraphLayout() %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE, selectedBy = "group") %>%
      visConfigure(enabled = FALSE)
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
  
  observeEvent(input$submitDefault, {
    write.csv(tableDefault,"./tableCSV.csv", row.names=FALSE)
    requirement1 <<- "Hello"
  })
  
  observeEvent((input$submitCSV|input$submitXLSX|input$submitDefault)&input$ShowSuspicious ,{
    
    if(requirement1=="Hello"){
      
      columnNumber <<- input$numberColumn + 2
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
               margin = list(t = 75, b = 50,r = 10,l = 80), plot_bgcolor = 'rgb(255, 255, 255)', paper_bgcolor  = 'rgb(150, 200, 255)', barmode = 'group', bargap = 10)
      
      output$GraphCV <<- renderPlotly({
        CVgraph
      })
      
      tmp <- data.frame("RD"=tableMarkers$RD)
      tmp$Date <- tableCSV$Date
      
      RDgraph <<- plot_ly(data = tmp, x =~Date, y =~RD, type = "scatter",mode = "markers" , name="Competitive", marker = list(color = 'rgb(100, 150, 255)',line = list(color = 'rgb(0, 0, 255)', width = 0.5))) %>%
        layout(title = "Relative Distance over time", titlefont = list(size = 20,color = 'rgb(47, 47, 147)'),
               yaxis = list(linecolor = toRGB("black"),color = toRGB("black"),title = "CV", titlefont = list(size = 18,color = 'rgb(0, 0, 0)'), tickfont = list(size = 12,color = 'rgb(0, 0, 0)')),
               xaxis = list(linecolor = toRGB("black"),color = toRGB("black"),title = "Date", titlefont = list(size = 18,color = 'rgb(0, 0, 0)'), tickfont = list(size = 12,color = 'rgb(0, 0, 0)')),
               margin = list(t = 75, b = 50,r = 10,l = 80), plot_bgcolor = 'rgb(255, 255, 255)', paper_bgcolor  = 'rgb(150, 200, 255)', barmode = 'group', bargap = 10)
      
      output$GraphRD <<- renderPlotly({
        RDgraph
      })
      
      output$markersTable <- renderDataTable({tableMarkers})
      
      output$Graph2D <<- renderPlotly({
        graphic2Dfuntion(input$firmOne, input$firmTwo, columnNumber)
      })
      
      output$downloadData2 <- downloadHandler(
        filename = function() {
          paste0("tableMarkers.csv")
        },
        content = function(file) {
          write.csv(tableMarkers, file, row.names = FALSE, sep = ";")
        }
      )
      
    }
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
    
    output$downloadData3 <- downloadHandler(
      filename = function() {
        paste0("tableProbabilities.csv")
      },
      content = function(file) {
        write.csv(tableMarkers, file, row.names = FALSE, sep = ";")
      }
    )
    
  })
  
  observeEvent(input$submitCSV|input$submitXLSX|input$submitDefault,{
    if(requirement1=="Hello"){
      functionFirmList(columnNumber)
      
      x <- input$groupBidding
      # Can also set the label and select items
      updateCheckboxGroupButtons(session, "groupBidding",
                                 label = "",
                                 choices = allFirms[,1],
                                 selected = NULL
      )
      
      x <- input$firmOne
      # Can also set the label and select items
      updateRadioButtons(session, "firmOne",
                         label = "",
                         choices = allFirms[,1],
                         selected = allFirms[1,1]
      )
      x <- input$firmTwo
      # Can also set the label and select items
      updateRadioButtons(session, "firmTwo",
                         label = "",
                         choices = allFirms[,1],
                         selected = allFirms[2,1]
      )
      
      output$network <- renderVisNetwork({
        networkFunction(allFirms,tableFirms,input$comunityType)
      })
    }
  })
  
  observeEvent(input$ShowContracts,{
    
    if(length(input$groupBidding) != 0){
      selectedFirmsName <- input$groupBidding
      functionCommunContracts(selectedFirmsName,allFirms,tableFirms)
      output$contractsSame <- renderTable(communContracts, colnames = FALSE)
    }else{
      output$contractsSame <- renderTable(data.frame(), colnames = FALSE)
    }
    
  })
  
  observeEvent(input$DoIt,{
    if(requirement2=="Hello"){
      tableMarkers <- read.csv("./tableMarkers.csv", header = TRUE)
      FinalTable$'Contracts Won' <- 0
      FinalTable$'Contracts Participated' <- 0
      FinalTable$'Partial Winner Ratio (%)' <- 0
      FinalTable$'Overall Winner Ratio (%)' <- 0
      for(i in c(allFirms[[1]])){
        FinalTable$'Contracts Won'[which(FinalTable[1]==i)] <- length(which(tableMarkers[5]==i))
        FinalTable$'Contracts Participated'[which(FinalTable[1]==i)] <- nrow(functionCommunContracts(i,allFirms,tableFirms))
      }
      FinalTable$'Partial Winner Ratio (%)' <- 100 * FinalTable$'Contracts Won' / FinalTable$'Contracts Participated'
      FinalTable$'Overall Winner Ratio (%)' <- 100 * FinalTable$'Contracts Won' / nrow(tableMarkers)
      output$tablaFinal <- renderDataTable({FinalTable})
      
      output$downloadData4 <- downloadHandler(
        filename = function() {
          paste0("tableSummary.csv")
        },
        content = function(file) {
          write.csv(FinalTable, file, row.names = FALSE, sep = ";")
        }
      )
      
    }
  })
  
  output$downloadData1 <- downloadHandler(
    filename = function() {
      paste0("tableDate.csv")
    },
    content = function(file) {
      write.csv(tableDefault, file, row.names = FALSE, sep = ";")
    }
  )
  
  output$downloadData2 <- downloadHandler(
    filename = function() {
      paste0("tableMarkers.csv")
    },
    content = function(file) {
      write.csv(data.frame(Error="This table is not found"), file, row.names = FALSE, sep = ";")
    }
  )
  
  output$downloadData3 <- downloadHandler(
    filename = function() {
      paste0("tableProbabilities.csv")
    },
    content = function(file) {
      write.csv(data.frame(Error="This table is not found"), file, row.names = FALSE, sep = ";")
    }
  )
  
  output$downloadData4 <- downloadHandler(
    filename = function() {
      paste0("tableSummary.csv")
    },
    content = function(file) {
      write.csv(data.frame(Error="This table is not found"), file, row.names = FALSE, sep = ";")
    }
  )
  
}

shinyApp(ui, server, options = list(launch.browser=TRUE))




