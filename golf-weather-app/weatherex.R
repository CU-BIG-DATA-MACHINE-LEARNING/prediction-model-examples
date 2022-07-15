library(shiny)
library(shinythemes)
library(data.table)
library(RCurl)
library(randomForest)
# Read data
weather <- read.csv("C:/Users/terra/projects/ColumbiaUniversity/predict/prediction-model-examples/golf-weather-app/weather.csv" )

# Build model
weather$play = factor(weather$play)

model <- randomForest(play ~ ., data = weather, ntree = 500, mtry = 4, importance = TRUE)
#ntree
#Number of trees to grow. This should not be set to too small a number, to ensure that every input row gets predicted at least a few times.
#mtry
#Number of variables randomly sampled as candidates at each split. 
#Note that the default values are different for classification (sqrt(p) where p is number of variables in x) and regression (p/3)

#testpred<-predict(model, newdf)

# Save model to RDS file
# saveRDS(model, "model.rds")

# Read in the RF model
#model <- readRDS("model.rds")

####################################
# User interface                   #
####################################

ui <- fluidPage(theme = shinytheme("united"),
                
                # Page header
                headerPanel('Play Golf?'),
                
                # Input values
                sidebarPanel(
                  HTML("<h3>Input parameters</h3>"),
                  
                  selectInput("outlook", label = "Outlook:", 
                              choices = list("Sunny" = "sunny", "Overcast" = "overcast", "Rainy" = "rainy"), 
                              selected = "Rainy"),
                  sliderInput("temperature", "Temperature:",
                              min = 64, max = 86,
                              value = 70),
                  sliderInput("humidity", "Humidity:",
                              min = 65, max = 96,
                              value = 90),
                  selectInput("windy", label = "Windy:", 
                              choices = list("Yes" = "TRUE", "No" = "FALSE"), 
                              selected = 1),
                  
                  actionButton("submitbutton", "Submit", class = "btn btn-primary")
                ),
                
                mainPanel(
                  tags$label(h3('Status/Output')), # Status/Output Text Box
                  verbatimTextOutput('contents'),
                  tableOutput('tabledata') # Prediction results table
                  
                )
)

####################################
# Server                           #
####################################

server <- function(input, output, session) {
  val <- eventReactive(
    
    input$enter, {
      
      x<-data.frame(
        outlook=input$outlook,
        temperature=input$temperature,
        humidity=input$humidity,
        windy=input$windy,
        stringsAsFactors = F)
       }
    
  )
  # Input Data
  datasetInput <- reactive({  
    
    x<-data.frame(
      outlook=input$outlook,
      temperature=input$temperature,
      humidity=input$humidity,
      windy=input$windy,
      stringsAsFactors = F)
    
    
    
    
  })
  
  # Status/Output Text Box
  output$contents <- renderPrint({
    if (input$submitbutton>0) { 
      isolate("Calculation complete.") 
    } else {
      return("Server is ready for calculation.")
    }
  })
  
  # Prediction results table
  output$tabledata <- renderTable({
   
   if (input$submitbutton>0) { 
     isolate(data.frame(Prediction=predict(model,datasetInput()), round(predict(model,datasetInput(),type="prob"), 3)))
   } 
  })
  
}

####################################
# Create the shiny app             #
####################################
shinyApp(ui = ui, server = server)