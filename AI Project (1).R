#Yasser Nazir - 22331
#Program Description: Child's Height Prediction using Parents Height
#AI Lab

library(shiny)

#Defines UI for application
ui <- navbarPage(title = "AI Project - Child's Height Prediction",
                 
                #TAB IMPORT RAW DATA
                tabPanel("Raw Data",
                            sidebarPanel(
                              h4("Upload Data in CSV File"),
                              fileInput("csvfile", "Choose CSV File",
                                        accept = c("text/csv", 
                                          "text/comma-separated-values,text/plain",
                                          ".csv"))),
                         
                              mainPanel(
                                tableOutput("modelData"))),
                
                #TAB TRAIN
                tabPanel("Train",
                            sidebarPanel(
                              sliderInput("trainingDataSize", "Size of Training Data:",value= 500, min = 500, max = 898),
                              sliderInput("hiddenLayers", "Hidden Layers:", min = 1, max = 20, value= 1  ),
                              
                              actionButton("trainModel", "Train Model")),
                         
                            mainPanel(
                              plotOutput("plotModelGraph"))),
                 
                 #TAB TEST DATA
                 tabPanel("Test",
                            sidebarPanel(
                              actionButton("testModel", "Test Model") ),
                          
                            mainPanel(
                            plotOutput("ModelTestGraph"),
                            tableOutput("modelTestData"))),
                 
                 
                  #TAB PREDICT
                  tabPanel("Predict",sidebarPanel(fluidRow(
                              column(6,numericInput("mothersheightv","Mother's Height",value = 62)),
                              column(6,numericInput("fathersheightv","Father's Height",value = 70))),
                            
                            actionButton("predict", "Predict Height")),
                          
                          mainPanel(
                            h2("Predicted value"),
                            textOutput("predicted")
                            )))


# Define server logic required to draw a histogram
server <- function(input, output,session) {
    library(neuralnet)
  
   observeEvent(input$trainModel,
                                {
        trainingDataSize = input$trainingDataSize
        print("Training data size")
        print(trainingDataSize)
        
        hidenLayers = input$hiddenLayers
        print("Hidden  Layers")
        print(hidenLayers)
        
        height_data <<- modelData
        
        print("Summary of Height Average")
        print(summary(height_data$childsheight))
        height_data_norm <<- as.data.frame(lapply(height_data, normalize))
        height_train <- height_data_norm[1:trainingDataSize, ]
        height_test <<- height_data_norm[trainingDataSize:898, ]
        
        print("nomal test data")
        print(height_data)
        print("Normalized test data")
        print(height_data_norm)
        
        output$modelTestData <- renderTable({
          height_test})
        
        height_model <<- neuralnet(childsheight ~ fathersheight + mothersheight, linear.output = F, data = height_train, hidden = hidenLayers)
        
        plot(height_model)
        dev.copy(png,'modelGraph.png')
        dev.off()
        
        
        output$plotModelGraph <- renderImage({
          # When input$n is 1, filename is ./images/image1.jpeg
          filename <- "modelGraph.png"
          
          # Return a list containing the filename
          list(src = filename)
        }, deleteFile = TRUE)})
   
   
   observeEvent(input$testModel,
                {
                print("Testing model")
                
                model_results <- compute(height_model, height_test[1:3])
                print("Model Results")
                
                predicted_childsheight <- model_results$net.result
                predicted_childsheight <- model_results$net.result
                
                cor(predicted_childsheight, height_test$childsheight)
                print(height_test)
                
                x = height_test$fathersheight
                print(x)
                z = height_test$mothersheight
                print(z)
                y1 = height_test$childsheight
                print(y1)
                y2 = predicted_childsheight
                print(y2)
                datafr = data.frame(x, z, y1, y2)
                print(datafr)
                # melt the data to a long format
                
                plot(predicted_childsheight, type = "o", col="blue", )
                lines(height_test$childsheight, type ="o", col="red" )
                dev.copy(png,'ModelTestGraph.png')
                dev.off()
                output$ModelTestGraph <- renderImage({
                  # When input$n is 1, filename is ./images/image1.jpeg
                  filename <- "ModelTestGraph.png"
                  
                  # Return a list containing the filename
                  list(src = filename)
                }, deleteFile = TRUE)})
   
       
   observeEvent(input$predict,
                {
                  print("Predicting height")
                  
                  test <- matrix(c(input$fathersheightv,input$mothersheightv,input$childsheightv),ncol=3,byrow=TRUE);
                  colnames(test) <- c("fathersheight","mothersheight","childsheight");
                  print("Input values")
                  print(test)
                  inputs <<- data.frame(test)
                  print("Input values data frame")
                  print(inputs)
                  
                  inputs_norm <- as.data.frame(lapply(inputs, normalize))
           
                  print("Input normalize")
                  print(inputs_norm)
                  results <- predict(height_model, inputs_norm)
                  
                  #predicted_s <- results$net.result
                  print("Predicted value")
                  results.den <- as.data.frame(lapply(results, denormalize))
                  print(results)
                  print(results.den)
                  output$predicted <- renderText({
                  paste("childs Height Normalised =", results, "\n Childs Height") 
                  paste("Childs Height =", results.den)
                  })})
    
updateData <- eventReactive(
      input$csvfile,
      {
        print("Updating model training Data")
        inFile <- input$csvfile
        
        if (is.null(inFile))
          modelData
        else{
          modelData <<- read.csv(inFile$datapath)
          print("Finished Updating model training Data")
          modelData}}) 
    
    output$modelData <- renderTable({
      updateData()})
    
    normalize <- function(x) {
        return((x - min(height_data, na.rm = TRUE)) / (max(height_data, na.rm = TRUE) - min(height_data, na.rm = TRUE)))}
    denormalize <- function(normalized_value){
      return  (normalized_value * (max(height_data) - min(height_data)) +min(height_data) )}}

#Run
shinyApp(ui = ui, server = server)