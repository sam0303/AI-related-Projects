library(shiny)
library(ggplot2)
library(vioplot)

ui <- fluidPage(
  titlePanel("File Input"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file","Upload the file"),
      tags$hr(),
      checkboxInput(inputId = 'header', label = 'Header', value = TRUE),
      br(),
      uiOutput("vx"),
      br(),
      uiOutput("bins"),
      br(), 
      uiOutput("color"),
      ),
    mainPanel(
      uiOutput("tb")
     
    )
    
  )
)

server <- function(input,output){
  data <- reactive({
    file1 <- input$file
    if(is.null(file1)){return()}
    read.table(file=file1$datapath, sep=",", header = input$header)
    
  })
  
  output$filedf <- renderTable({
    if(is.null(data())){return ()}
    input$file
  })
  
  output$sum <- renderPrint({
    if(is.null(data())){return ()}
    summary(data())

  })

  output$table <- renderTable({
    if(is.null(data())){return ()}
    data()
  })

  output$str <- renderPrint({
    if(is.null(data())){return ()}
    str(data())

  })

  output$vx <- renderUI({
    if(!input$header){return ()}
    selectInput("variablex", "1. Select the variable from the dataset", choices=names(data()))
  })
  output$bins <- renderUI({
    if(!input$header){return ()}
    sliderInput("bins", "2. Select the number of BINs for histogram", min=5, max = 25, value=15)
  })
  
  output$color <- renderUI({
    if(!input$header){return ()}
    radioButtons("color", "3. Select the colour of histogram", choices=c("Green", "Red", "Yellow"), selected ="Green")
  })
  
  output$p <- renderPlot({

    hist(data()[,input$variablex],breaks=seq(0, max(data()[,input$variablex]), l=input$bins+1), col=input$color, main="Histogram of dataset", xlab=input$variablex)
  })
  
  output$b <- renderPlot(
    {if(!input$header){return ()}
    x1 <- data()$High[data()$Close==4]
    x2 <- data()$Low[data()$Close==6]
    x3 <- data()$Open[data()$Close==8]
    vioplot(x1, x2, x3, names=c("4 cyl", "6 cyl", "8 cyl"),
            col="gold")
   
  })
  
  output$tb <- renderUI({
  tabsetPanel(tabPanel("Plot", plotOutput("p")), tabPanel("Violin Plot", plotOutput("b")),tabPanel("Data", tableOutput("table")), tabPanel("Structure",verbatimTextOutput("str")) , tabPanel("Summary", verbatimTextOutput("sum")))
  })
}

shinyApp(ui, server)