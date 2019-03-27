## Libraries
library("shiny")
library("shinyjs")
library("shinythemes")
library("shinyWidgets")
library("shinydashboard")
library("xlsx")

## Inicialization


## Aplication definition
header <- dashboardHeader(
  title = "Data input"
)
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("File input", tabName = "menu1", icon = icon("dashboard")),
    menuItem("DataTable", tabName = "menu2", icon = icon("table"))
  )
)
body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = "menu1",
      fileInput("xlsxFile", label = h3("Upload a xlsx file"), multiple = FALSE,placeholder = "No file selected",buttonLabel = "Browse...",width = "250px",accept = c(".xlsx")),
      fileInput("csvFile", label = h3("Upload a csv file"), multiple = FALSE, placeholder = "No file selected",buttonLabel = "Browse...",width = "250px",accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      tableOutput("tabla")
    ),
    tabItem(
      tabName = "menu2",
      h2("Data table"),
      dataTableOutput("markersTable")
    )
  )
)

ui <- dashboardPage(header, sidebar, body)

server <- function(input, output) {
  
  #https://shiny.rstudio.com/gallery/file-upload.html
  output$tabla <- renderTable({
    
    req(input$csvFile)
    
    tryCatch({
      df <- read.csv(input$csvFile$datapath,
                     header = TRUE,
                     sep = ";")
    },
    error = function(e) {
      # return a safeError if a parsing error occurs
      stop(safeError(e))
    })
    
    df <- data.frame(df)
    df[is.na(df)]=""
    return(df)
  })
  
  output$markersTable <- renderDataTable({
    
    req(input$csvFile)
    
    tryCatch({
      tablaCSV <- read.csv(input$csvFile$datapath,
                     header = TRUE,
                     sep = ";")
    },
    error = function(e) {
      # return a safeError if a parsing error occurs
      stop(safeError(e))
    })
    
    tablaCSV[tablaCSV==""]=NA
    
    tableMarkers <- data.frame("Contracts" = tablaCSV[,1])
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
    
    tableMarkers <<- tableMarkers
    return(tableMarkers)
  })

}
shinyApp(ui, server, options = list(launch.browser=TRUE))

