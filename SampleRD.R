# Libraries
library(shiny)
library(shinyjs)
library(shinythemes)
library(shinyWidgets)
library(shinydashboard)


header <- dashboardHeader(
  title = "Cuadro de mando",
  titleWidth = 350
)
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Distribution", tabName = "menu1", icon = icon("dashboard")),
    sliderInput(inputId = "numberOfContracts", label = "Number of contracts :", min = 0, max = 10000, value = 1000, width = "100%"),
    sliderInput(inputId = "numberOfBreaks", label = "Number of breaks :", min = 10, max = 10000, value = 1000, width = "100%"),
    sliderInput(inputId = "numberOfBidsMin", label = "Minimun number of bids :", min = 5, max = 15, value = 10, width = "100%"),
    sliderInput(inputId = "numberOfBidsMax", label = "Maximun number of bids :", min = 15, max = 30, value = 20, width = "100%"),
    sliderInput(inputId = "xLimt", label = "Maximun limit of the x axys :", min = 1, max = 100, value = 50, width = "100%")
  )
)
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "menu1",
            h2("Distribution"),
            verbatimTextOutput("hola"),
            plotOutput("histograma")
    )
  )
)

ui <- dashboardPage(header, sidebar, body)

server <- function(input, output) {
  output$menuitem <- renderMenu({
    menuItem("Menu item", icon = icon("calendar"))
  })
  
  ## Main function
  funtionSampleRD <- function(numberOfBidsMin, numberOfBidsMax, numberOfContracts, numberOfBreaks,xLimt){
    listOfSamples <- list()
    sampleRD <- data.frame()
    for(i in c(1:numberOfContracts)){
      aux1 <- sample(numberOfBidsMin:numberOfBidsMax,1)
      aux2 <- sample(10:70,1)
      listOfSamples[i] <- list(sort(c(aux2,sample(c(sample(aux2+1:70+1,1):sample(80:150,1)), aux1, replace=TRUE))))
      sampleRD[i,1] <- (listOfSamples[[i]][2]-listOfSamples[[i]][1])/sd(listOfSamples[[i]][2:aux1])
    }
    sampleRD <- sort(sampleRD[,1], decreasing = TRUE)
    
    output$histograma <- renderPlot({hist(sampleRD, main = "Distribution", breaks = numberOfBreaks, border = "orange", col = "yellow", xlim = c(0,xLimt))})
  }

  observeEvent(input$numberOfContracts|input$numberOfBreaks|input$numberOfBidsMax|input$numberOfBidsMin|input$xLimt,{
    funtionSampleRD(input$numberOfBidsMin, input$numberOfBidsMax, input$numberOfContracts, input$numberOfBreaks,input$xLimt)
  })
}

shinyApp(ui, server, options = list(launch.browser=TRUE))