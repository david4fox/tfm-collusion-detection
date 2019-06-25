## Libraries
library("shiny")
library("shinyjs")
library("shinythemes")
library("shinyWidgets")
library("shinydashboard")
library("igraph")
library("network")
library("sna")
library("ndtv")

## Inicialization
rangeOfBidsDefault <- c(5,12)
numberOfContractsDefault <- 10000
xLimtDefault <- 5
yLimtDefault <- 100
insertRDDefault <- 1
insertProbabilityDefault <- 0.95
numberOfBinsRDDefault <- 500
numberOfBinsCVDefault <- 100
rangeOfPricesDefault <- c(100000,600000)

## Aplication definition
header <- dashboardHeader(
  title = "Markers",
  titleWidth = 230,
  tags$li(a(href = 'https://www.ubu.es/',
            img(src = "https://www.ubu.es/sites/default/files/portal_page/images/escudo_color_1l_dcha.jpg", title = "Universidad de Burgos", height = "50px"),
            style = "padding-top:10px; padding-bottom:10px;"),
          class = "dropdown"),
  tags$li(a(href = 'http://www.google.com/',
            icon("power-off"),
            title = "Back to Apps Home"),
          class = "dropdown")
)
sidebar <- dashboardSidebar(
  sidebarMenu(
    sliderInput(inputId = "numberOfContracts", label = "Number of contracts :", min = 1, max = 10000, value = numberOfContractsDefault, width = "100%", round = TRUE),
    sliderInput(inputId = "rangeOfBids", label = "Number of bids :", min = 5, max = 100, value = rangeOfBidsDefault, width = "100%",ticks = TRUE),
    sliderInput(inputId = "rangeOfPrices", label = "Range of prices :", min = 50, max = 1000000, value = rangeOfPricesDefault, width = "100%",ticks = TRUE),
    menuItem("Relative Distance", tabName = "menu1", icon = icon("dashboard")),
    menuItem("Coeficient of Variation", tabName = "menu2", icon = icon("table")),
    menuItem("Data table", tabName = "menu3", icon = icon("table")),
    menuItem("Plots", tabName = "menu4", icon = icon("bar-chart-o"))
  )
)
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "menu1",
            fluidRow(
              column(4, h2("Relative distance distribution")),
              column(6, h2("")),
              column(2, actionButton("calculate", align="center",label=list(strong("Compute  "), icon("calculator")), style="color: white; background-color: #000F89; border-color: #0011B7; padding:10; margin:0; position:rigth"))
            ),
            fluidRow(
              column(8, plotOutput("histogramaRD")),
              column(4, box(title = list(icon("chart-area"),"Statistics"), width = "100%",status = "primary", background = "navy", solidHeader = TRUE,
                            h5("Mean:"),textOutput("media"), br(),
                            h5("Median :"),textOutput("mediana"), br(),
                            h5("Deviation :"),textOutput("desviacion"),br(),
                            h5("Something more"),br()
                        )
              )
            ),
            fluidRow(
              column(4, sliderInput(inputId = "xLimt", label = "Maximun limit of the x axys :", min = 0.1, max = 10, value = xLimtDefault, width = "100%")),
              column(4, sliderInput(inputId = "yLimt", label = "Maximun limit of the y axys :", min = 1, max = 1000, value = yLimtDefault, width = "100%")),
              column(4, sliderInput(inputId = "numberOfBinsRD", label = "Number of bins :", min = 10, max = 10000, value = numberOfBinsRDDefault, width = "100%"))
            ),
            fluidRow(
              column(4, numericInput("insertRD", label = h3("Introduce a relative distance (RD)"), value = insertRDDefault, width = "100%", min = 0, max = 100,step = 0.1)),
              column(4, h3("Probability related to the probability inserted:"), verbatimTextOutput("pBaja"))
            ),
            fluidRow(
              column(4, numericInput("insertProbability", label = h3("Introduce a probability"), value = insertProbabilityDefault, width = "100%", min = 0, max = 1,step = 0.01)),
              column(4, h3("Relative distance related to the relative distance inserted:"), verbatimTextOutput("estimatedRD"))
            )
    ),
    tabItem(
      tabName = "menu2",
      h2("Coeficient of variation distribution"),
      fluidRow(
        column(8, plotOutput("histogramaCV")),
        column(4, box(title = list(icon("chart-area"),"Statistics"), width = "100%",status = "primary", background = "navy", solidHeader = TRUE,
                      h5("Mean:"),textOutput("mediaCV"), br(),
                      h5("Median :"),textOutput("medianaCV"), br(),
                      h5("Deviation :"),textOutput("desviacionCV"),br()
                  )
        )
      ),
      fluidRow(
        column(8,h5("Something more")),
        column(4, sliderInput(inputId = "numberOfBinsCV", label = "Number of bins :", min = 10, max = 1000, value = numberOfBinsCVDefault, width = "100%"))
      )
    ),
    tabItem(
      tabName = "menu3",
      h2("Data table"),
      dataTableOutput("tablaDatos")
    ),
    tabItem(
      tabName = "menu4",
      h2("Graphs"),
      plotOutput("plotRD"),
      plotOutput("plotCV")
    )
  )
)

ui <- dashboardPage(header, sidebar, body)

server <- function(input, output) {
  output$menuitem <- renderMenu({
    menuItem("Menu item", icon = icon("calendar"))
  })
  
  ## Main function
  funtionSample <- function(rangeOfBids, numberOfContracts, lowerPrice, higherPrice){
    listOfSamples <- list()
    sampleRD <- data.frame()
    sampleCV <- data.frame()
    for(i in c(1:numberOfContracts)){
      if(rangeOfBids[1]==rangeOfBids[2]){
        numberOfBids <- rangeOfBids[1]
      }else{
        numberOfBids <- sample(rangeOfBids[1]:rangeOfBids[2], 1, replace=TRUE)
      }
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
  
  ## Histogram function RD
  functionHistogramRD <- function(sampleRD_frame,numberOfBinsRD, xLimt, yLimt,insertRD, insertProbability){
    calculoRD <<- sampleRD_frame[max(which(sampleRD_frame[,3] <= insertProbability)),1]
    output$histogramaRD <- renderPlot({hist(sampleRD_frame$RD, main = "Distribution", breaks = numberOfBinsRD, border = "#0050DC", col = "#166BFF", xlab = "RD", xlim = c(0,xLimt), ylim = c(0,yLimt))
      abline(v = insertRD, col = "red")
      abline(v = calculoRD, col = "orange")
    })
  }
  
  ## Histogram function CV
  functionHistogramCV <- function(sampleCV_frame, numberOfBinsCV){
    output$histogramaCV <- renderPlot({hist(sampleCV_frame$CV, main = "Distribution", breaks = numberOfBinsCV, border = "#0050DC", col = "#166BFF", xlab = "CV")
    })
  }
  
  ## Statictics function
  functionStatictics <- function(){
    
    calculoP05 <- sampleRD_frame[max(which(sampleRD_frame[,3]<=0.95)),1]
    calculoMedia <- mean(sampleRD_frame[,1])
    calculoMediana <- sampleRD_frame[ceiling(nrow(sampleRD_frame)/2),1]
    calculoDesviacion <- sd(sampleRD_frame[,1])
    calculoTabla <- cbind(sampleRD_frame[,c(1,3)],sampleCV_frame[,c(1,3)])
    colnames(calculoTabla) <- c("RD Valor","RD probability","CV Valor","CV probability")
    
    calculoMediaCV <- mean(sampleCV_frame[,1])
    calculoMedianaCV <- sampleCV_frame[ceiling(nrow(sampleCV_frame)/2),1]
    calculoDesviacionCV <- sd(sampleCV_frame[,1])
    
    
    output$media <<- renderText({paste0(format(x = calculoMedia,digits = 4))})
    output$mediana <<- renderText({paste0(format(x = calculoMediana,digits = 4))})
    output$desviacion <<- renderText({paste0(format(x = calculoDesviacion,digits = 4))})
    output$p05 <<- renderText({paste0(format(x = calculoP05,digits = 4))})
    output$tablaDatos <<- renderDataTable({calculoTabla})
    output$plotRD <<- renderPlot({plot(sampleRD_frame$RD, main = "RD", ylab = "RD", xlab = "Contracts", col = "blue", cex = 1, pch = 20)})
    output$plotCV <<- renderPlot({plot(sampleCV_frame$CV, main = "CV", ylab = "CV", xlab = "Contracts", col = "blue", cex = 1, pch = 20)})
    
    output$mediaCV <<- renderText({paste0(format(x = calculoMediaCV,digits = 4))})
    output$medianaCV <<- renderText({paste0(format(x = calculoMedianaCV,digits = 4))})
    output$desviacionCV <<- renderText({paste0(format(x = calculoDesviacionCV,digits = 4))})
    
  }
  
  ## First values
  funtionSample(rangeOfBidsDefault, numberOfContractsDefault, rangeOfPricesDefault[1], rangeOfPricesDefault[2])
  functionStatictics()
  functionHistogramRD(sampleRD_frame, numberOfBinsRDDefault, xLimtDefault, yLimtDefault, insertRDDefault, insertProbabilityDefault)
  functionHistogramCV(sampleCV_frame,numberOfBinsCVDefault)
  
  observeEvent(input$insertRD,{
    calculoProbabilidadBaja <- (sampleRD_frame[max(which(sampleRD_frame[,1] <= input$insertRD)),3]+sampleRD_frame[min(which(sampleRD_frame[,1] >= input$insertRD)),3])/2
    calculoProbabilidadAlta <- 1-calculoProbabilidadBaja
    output$pAlta <<- renderText({paste0(format(x = calculoProbabilidadAlta,digits = 4))})
    output$pBaja <<- renderText({paste0(format(x = calculoProbabilidadBaja,digits = 4))})
    functionHistogramRD(sampleRD_frame, input$numberOfBinsRD, input$xLimt, input$yLimt, input$insertRD, input$insertProbability)
  })
  
  observeEvent(input$insertProbability,{
    calculoRD <- sampleRD_frame[max(which(sampleRD_frame[,3] <= input$insertProbability)),1]
    output$estimatedRD <<- renderText({paste0(format(x = calculoRD,digits = 2))})
    functionHistogramRD(sampleRD_frame, input$numberOfBinsRD, input$xLimt, input$yLimt, input$insertRD, input$insertProbability)
  })
  
  observeEvent(input$numberOfBinsRD|input$xLimt|input$yLimt,{
    functionHistogramRD(sampleRD_frame, input$numberOfBinsRD, input$xLimt, input$yLimt, input$insertRD, input$insertProbability)
  })
  
  observeEvent(input$numberOfBinsCV,{
    functionHistogramCV(sampleCV_frame, input$numberOfBinsCV)
  })
  
  observeEvent(input$calculate,{
    funtionSample(input$rangeOfBids, input$numberOfContracts, input$rangeOfPrices[1], input$rangeOfPrices[2])
    functionHistogramRD(sampleRD_frame, input$numberOfBinsRD, input$xLimt, input$yLimt, input$insertRD, input$insertProbability)
    functionHistogramCV(sampleCV_frame, input$numberOfBinsCV)
    functionStatictics()
    calculoRD <- sampleRD_frame[max(which(sampleRD_frame[,3] <= input$insertProbability)),1]
    output$estimatedRD <<- renderText({paste0(format(x = calculoRD,digits = 2))})
    calculoProbabilidadBaja <- (sampleRD_frame[max(which(sampleRD_frame[,1] <= input$insertRD)),3]+sampleRD_frame[min(which(sampleRD_frame[,1] >= input$insertRD)),3])/2
    calculoProbabilidadAlta <- 1-calculoProbabilidadBaja
    output$pAlta <<- renderText({paste0(format(x = calculoProbabilidadAlta,digits = 4))})
    output$pBaja <<- renderText({paste0(format(x = calculoProbabilidadBaja,digits = 4))})
  })
}

shinyApp(ui, server, options = list(launch.browser=TRUE))

# plot(sampleRD_frame$RD)
# plot(sampleCV_frame$CV)