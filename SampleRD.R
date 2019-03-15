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
  title = "SampleRD",
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
    menuItem("Relative Distance", tabName = "menu1", icon = icon("dashboard")),
    sliderInput(inputId = "numberOfContracts", label = "Number of contracts :", min = 1, max = 10000, value = 1000, width = "100%", round = TRUE),
    sliderInput(inputId = "numberOfBids", label = "Number of bids :", min = 5, max = 30, value = c(10,20), width = "100%",ticks = TRUE),
    menuItem("Coeficient of Variation", tabName = "menu2", icon = icon("table")),
    menuItem("Data table", tabName = "menu3", icon = icon("table"))
  )
)
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "menu1",
            fluidRow(
              column(4,h2("Relative distance distribution")),
              column(7,h2("")),
              column(1,actionButton("calculate", align="center",label=list(strong("Compute  "), icon("calculator")), style="color: white; background-color: #000F89; border-color: #0011B7; padding:10; margin:0; position:rigth"))
              ),
            fluidRow(
              column(9, plotOutput("histograma")),
              column(3,box(title = list(icon("chart-area"),"Statistics"), width = "100%",status = "primary", background = "navy", solidHeader = TRUE,
                           "Mean :",textOutput("media"), br(),"Median :",textOutput("mediana"), br(),"Deviation :",textOutput("desviacion"),br(),"Probability 1:",textOutput("pBaja"),br(),"Probability 2:",textOutput("pAlta"),br(),"RD (p = 0.05)",textOutput("p05")
              ))
            ),
            fluidRow(
              column(9,sliderInput(inputId = "xLimt", label = "Maximun limit of the x axys :", min = 1, max = 100, value = 50, width = "100%")),
              column(3,sliderInput(inputId = "numberOfBins", label = "Number of bins :", min = 10, max = 10000, value = 1000, width = "100%"))
            ),
            fluidRow(
              column(4,numericInput("RD", label = h3("Introduce a relative distance (RD)"), value = 1, width = "100%", min = 0, max = 100,step = 0.1)),
              column(4,h2("dfsdsdd"))
            ),
            fluidRow(
              column(4,numericInput("Probability", label = h3("Introduce a probability"), value = 0.5, width = "100%", min = 0, max = 1,step = 0.01)),
              column(4,h2("edddd"))
            )
    ),
    tabItem(
      tabName = "menu2",
      h2("Coeficient of Variation")
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
  funtionSampleRD <- function(numberOfBidsMin, numberOfBidsMax, numberOfContracts){
    listOfSamples <- list()
    sampleRD <- data.frame()
    for(i in c(1:numberOfContracts)){
      if(input$numberOfBids[1]==input$numberOfBids[2]){
        aux1 <- input$numberOfBids[1]
      }else{
        aux1 <- sample(input$numberOfBids[1]:input$numberOfBids[2],1)
      }
      aux2 <- sample(10:70,1)
      listOfSamples[i] <- list(sort(c(aux2,sample(c(sample(aux2:70+1,1):sample(80:150,1)), aux1, replace=TRUE))))
      sampleRD[i,1] <- (listOfSamples[[i]][2]-listOfSamples[[i]][1])/sd(listOfSamples[[i]][2:aux1])
    }
    sampleRD_frame <<- data.frame("RD" = sampleRD[with(sampleRD, order(sampleRD[,1])), ])
    sampleRD <<- sort(sampleRD[,1], decreasing = TRUE)
  }
  
  functionHistogram <- function(sampleRD,numberOfBins,xLimt){
    output$histograma <- renderPlot({hist(sampleRD, main = "Distribution", breaks = numberOfBins, border = "#0050DC", col = "#166BFF", xlab = "RD", xlim = c(0,xLimt))
                                      abline(v = input$RD, col = "red")
                                     })
  }
  
  functionStatictics <- function(sampleRD_frame){
    
    sampleRD_frame$Probability <- NA
    for(i in c(1:nrow(sampleRD_frame))){
      sampleRD_frame[i,2] <- sum(sampleRD_frame[1:i,1])/sum(sampleRD_frame[,1])
    }
    sampleRD_frame <<- sampleRD_frame
    calculoP05 <- sampleRD_frame[max(which(sampleRD_frame[,2]<=0.95)),1]
    
    calculoMedia <- mean(sampleRD_frame[,1])
    calculoMediana <- sampleRD_frame[ceiling(nrow(sampleRD_frame)/2),1]
    calculoDesviacion <- sd(sampleRD_frame[,1])
    calculoProbabilidadBaja <- (sampleRD_frame[max(which(sampleRD_frame[,1] <= input$RD)),2]+sampleRD_frame[min(which(sampleRD_frame[,1] >= input$RD)),2])/2
    calculoProbabilidadAlta <- 1-calculoProbabilidadBaja
    
    output$media <<- renderText({paste0(format(x = calculoMedia,digits = 4))})
    output$mediana <<- renderText({paste0(format(x = calculoMediana,digits = 4))})
    output$desviacion <<- renderText({paste0(format(x = calculoDesviacion,digits = 4))})
    output$pAlta <<- renderText({paste0(format(x = calculoProbabilidadAlta,digits = 4))})
    output$pBaja <<- renderText({paste0(format(x = calculoProbabilidadBaja,digits = 4))})
    output$p05 <<- renderText({paste0(format(x = calculoP05,digits = 4))})
    
    output$tabla <<- renderDataTable(sampleRD_frame)
  }
  
  observeEvent(input$RD,{
    functionStatictics(sampleRD_frame)
  })

  observeEvent(input$numberOfBins|input$xLimt,{
    functionHistogram(sampleRD, input$numberOfBins, input$xLimt)
  })
  observeEvent(input$calculate,{
    funtionSampleRD(input$numberOfBids[1], input$numberOfBids[2], input$numberOfContracts)
    functionHistogram(sampleRD, input$numberOfBins, input$xLimt)
    functionStatictics(sampleRD_frame)
  })
}

shinyApp(ui, server, options = list(launch.browser=TRUE))


