# Libraries
library("shiny")
library("shinyjs")
library("shinythemes")
library("shinyWidgets")
library("shinydashboard")
library("igraph")
library("network")
library("sna")
library("ndtv")

header <- dashboardHeader(
  title = "Markers",
  titleWidth = 230,
  tags$li(a(href = 'https://www.ubu.es/',
            img(src = "./escudo_COLOR_2L_DCHA.png", title = "Universidad de Burgos", height = "30px"),
            style = "padding-top:10px; padding-bottom:10px;"),
          class = "dropdown"),
  tags$li(a(href = 'http://www.google.com/',
            icon("power-off"),
            title = "Back to Apps Home"),
          class = "dropdown")
)
sidebar <- dashboardSidebar(
  sidebarMenu(
    sliderInput(inputId = "numberOfContracts", label = "Number of contracts :", min = 1, max = 10000, value = 500, width = "100%", round = TRUE),
    sliderInput(inputId = "rangeOfBids", label = "Number of bids :", min = 5, max = 30, value = c(10,20), width = "100%",ticks = TRUE),
    menuItem("Relative Distance", tabName = "menu1", icon = icon("dashboard")),
    menuItem("Coeficient of Variation", tabName = "menu2", icon = icon("table")),
    menuItem("Data table", tabName = "menu3", icon = icon("table"))
  )
)
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "menu1",
            fluidRow(
              column(4, h2("Relative distance distribution")),
              column(7, h2("")),
              column(1, actionButton("calculate", align="center",label=list(strong("Compute  "), icon("calculator")), style="color: white; background-color: #000F89; border-color: #0011B7; padding:10; margin:0; position:rigth"))
              ),
            fluidRow(
              column(9, plotOutput("histogramaRD")),
              column(3, box(title = list(icon("chart-area"),"Statistics"), width = "100%",status = "primary", background = "navy", solidHeader = TRUE,
                            h5("Mean:"),textOutput("media")
                            #"Mean :",textOutput("media"), br(),"Median :",textOutput("mediana"), br(),"Deviation :",textOutput("desviacion"),br(),"Probability 1:",textOutput("pBaja"),br(),"Probability 2:",textOutput("pAlta"),br(),"RD (p = 0.05)",textOutput("p05")
                        )
              )
            ),
            fluidRow(
              column(9, sliderInput(inputId = "xLimt", label = "Maximun limit of the x axys :", min = 1, max = 100, value = 10, width = "100%")),
              column(3, sliderInput(inputId = "numberOfBins", label = "Number of bins :", min = 10, max = 10000, value = 1000, width = "100%"))
            ),
            fluidRow(
              column(4, numericInput("insertRD", label = h3("Introduce a relative distance (RD)"), value = 1, width = "100%", min = 0, max = 100,step = 0.1)),
              column(4, h3("Probability related to the probability inserted:"), verbatimTextOutput("pBaja"))
            ),
            fluidRow(
              column(4, numericInput("insertProbability", label = h3("Introduce a probability"), value = 0.5, width = "100%", min = 0, max = 1,step = 0.01)),
              column(4, h3("Relative distance related to the relative distance inserted:"), verbatimTextOutput("estimatedRD"))
            )
    ),
    tabItem(
      tabName = "menu2",
      h2("Coeficient of Variation"),
      plotOutput("histogramaCV")
    ),
    tabItem(
      tabName = "menu3",
      h2("Data table"),
      dataTableOutput("tabla")
    )
  )
)

ui <- dashboardPage(header, sidebar, body)

server <- function(input, output) {
  output$menuitem <- renderMenu({
    menuItem("Menu item", icon = icon("calendar"))
  })

  ## Main function
  funtionSampleRD <- function(rangeOfBids, numberOfContracts){
    listOfSamples <- list()
    sampleRD <- data.frame()
    sampleCV <- data.frame()
    lowerPrice <- 60
    higherPrice <- 100
    for(i in c(1:numberOfContracts)){
      if(rangeOfBids[1]==rangeOfBids[2]){
        numberOfBids <- rangeOfBids[1]
      }else{
        numberOfBids <- sample(rangeOfBids[1]:rangeOfBids[2],1)
      }
      listOfSamples[i] <- list(sort(sample(lowerPrice:higherPrice, numberOfBids, replace=TRUE)))
      sampleRD[i,1] <- (listOfSamples[[i]][2]-listOfSamples[[i]][1])/sd(listOfSamples[[i]][2:numberOfBids])
      sampleCV[i,1] <- sd(listOfSamples[[i]])/mean(listOfSamples[[i]])
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
  
  ## Histogram function RD
  functionHistogramRD <- function(sampleRD,numberOfBins,xLimt,insertRD, estimatedRD){
    output$histogramaRD <- renderPlot({hist(sampleRD, main = "Distribution", breaks = numberOfBins, border = "#0050DC", col = "#166BFF", xlab = "RD", xlim = c(0,xLimt))
      abline(v = insertRD, col = "red")
      abline(v = calculoRD, col = "orange")
    })
  }
  
  ## Histogram function CV
  functionHistogramCV <- function(sampleCV){
    output$histogramaCV <- renderPlot({hist(sampleCV, main = "Distribution", breaks = 1000, border = "#0050DC", col = "#166BFF", xlab = "CV")
    })
  }
  
  ## Statictics function
  functionStatictics <- function(sampleRD_frame){
    
    calculoP05 <- sampleRD_frame[max(which(sampleRD_frame[,2]<=0.95)),1]
    calculoMedia <- mean(sampleRD_frame[,1])
    calculoMediana <- sampleRD_frame[ceiling(nrow(sampleRD_frame)/2),1]
    calculoDesviacion <- sd(sampleRD_frame[,1])

    output$media <<- renderText({paste0(format(x = calculoMedia,digits = 4))})
    output$mediana <<- renderText({paste0(format(x = calculoMediana,digits = 4))})
    output$desviacion <<- renderText({paste0(format(x = calculoDesviacion,digits = 4))})
    output$p05 <<- renderText({paste0(format(x = calculoP05,digits = 4))})
    output$tabla <<- renderDataTable(sampleRD_frame)
  }
  
  observeEvent(input$insertRD,{
    calculoProbabilidadBaja <- (sampleRD_frame[max(which(sampleRD_frame[,1] <= input$insertRD)),2]+sampleRD_frame[min(which(sampleRD_frame[,1] >= input$insertRD)),2])/2
    calculoProbabilidadAlta <- 1-calculoProbabilidadBaja
    output$pAlta <<- renderText({paste0(format(x = calculoProbabilidadAlta,digits = 4))})
    output$pBaja <<- renderText({paste0(format(x = calculoProbabilidadBaja,digits = 4))})
    functionHistogramRD(sampleRD, input$numberOfBins, input$xLimt, input$insertRD, output$estimatedRD)
  })
  
  observeEvent(input$insertProbability,{
    calculoRD <<- sampleRD_frame[max(which(sampleRD_frame[,2] <= input$insertProbability)),1]
    output$estimatedRD <<- renderText({paste0(format(x = calculoRD,digits = 2))})
    functionHistogramRD(sampleRD, input$numberOfBins, input$xLimt, input$insertRD, output$estimatedRD)
  })

  observeEvent(input$numberOfBins|input$xLimt,{
    functionHistogramRD(sampleRD, input$numberOfBins, input$xLimt, input$insertRD, output$estimatedRD)
  })
  
  observeEvent(input$calculate,{
    funtionSampleRD(input$rangeOfBids, input$numberOfContracts)
    functionHistogramRD(sampleRD, input$numberOfBins, input$xLimt, input$insertRD, output$estimatedRD)
    functionStatictics(sampleRD_frame)
  })
  
  functionHistogramCV(sampleCV)
}

shinyApp(ui, server, options = list(launch.browser=TRUE))


