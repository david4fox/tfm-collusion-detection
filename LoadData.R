# Libraries
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
    menuItem("File input", tabName = "menu1", icon = icon("dashboard"))
  )
)
body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = "menu1",
      fileInput("xlsxFile", label = h3("Upload a xlsx file"), multiple = FALSE,placeholder = "No file selected",buttonLabel = "Browse...",width = "250px",accept = c(".xlsx")),
      fileInput("csvFile", label = h3("Upload a csv file"), multiple = FALSE, placeholder = "No file selected",buttonLabel = "Browse...",width = "250px",accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      tableOutput("tabla")
    )
  )
)

ui <- dashboardPage(header, sidebar, body)

server <- function(input, output) {
  
  #https://shiny.rstudio.com/gallery/file-upload.html
  output$tabla <- renderTable({
    
    req(input$csvFile)
    
    tryCatch(
      {
        df <- read.csv(input$csvFile$datapath,
                       header = TRUE,
                       sep = ";")
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    tablaCSV <- data.frame(df)
    tablaCSV[tablaCSV==""]=NA
    tablaCSV <<- tablaCSV
    return(df)
  })

}

shinyApp(ui, server, options = list(launch.browser=TRUE))

