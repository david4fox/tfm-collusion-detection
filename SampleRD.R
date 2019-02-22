# Libraries
library(shiny)
library(shinyjs)
library(shinythemes)
library(shinyWidgets)
library(shinydashboard)


header <- dashboardHeader(
  title = "SampleRD",
  titleWidth = 230,
  tags$li(a(href = 'https://www.ubu.es/',
            img(src = "https://www.ubu.es/sites/default/files/portal_page/images/escudo_color_2l_dcha_7.jpg", title = "Universidad de Burgos", height = "30px"),
            style = "padding-top:10px; padding-bottom:10px;"),
          class = "dropdown"),
  tags$li(a(href = 'http://www.google.com/',
            icon("power-off"),
            title = "Back to Apps Home"),
          class = "dropdown")
)
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Distribution", tabName = "menu1", icon = icon("dashboard")),
    sliderInput(inputId = "numberOfContracts", label = "Number of contracts :", min = 1, max = 10000, value = 1000, width = "100%"),
    sliderInput(inputId = "numberOfBreaks", label = "Number of breaks :", min = 10, max = 10000, value = 1000, width = "100%"),
    sliderInput(inputId = "numberOfBids", label = "Number of bids :", min = 5, max = 30, value = c(10,20), width = "100%"),
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
      aux1 <- sample(input$numberOfBids[1]:input$numberOfBids[2],1)
      aux2 <- sample(10:70,1)
      listOfSamples[i] <- list(sort(c(aux2,sample(c(sample(aux2+1:70+1,1):sample(80:150,1)), aux1, replace=TRUE))))
      sampleRD[i,1] <- (listOfSamples[[i]][2]-listOfSamples[[i]][1])/sd(listOfSamples[[i]][2:aux1])
    }
    sampleRD <- sort(sampleRD[,1], decreasing = TRUE)
    
    output$histograma <- renderPlot({hist(sampleRD, main = "Distribution", breaks = numberOfBreaks, border = "orange", col = "yellow", xlab = "RD", xlim = c(0,xLimt))})
  }
  observeEvent(input$numberOfContracts|input$numberOfBreaks|input$numberOfBids[2]|input$numberOfBids[1]|input$xLimt,{
    funtionSampleRD(input$numberOfBids[1], input$numberOfBids[2], input$numberOfContracts, input$numberOfBreaks,input$xLimt)
  })
}

shinyApp(ui, server, options = list(launch.browser=TRUE))


