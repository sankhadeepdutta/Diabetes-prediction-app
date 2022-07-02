library(shiny)
library(shinyalert)
library(shinyWidgets)
library(randomForest)
library(caret)

ui <- fluidPage(
  useShinyalert(force = T),
  setBackgroundImage(src = "bg_pic.jpg"),
  h2(
    "ENSURA-HEALTH: A diabetes prediction application",
    align = "center",
    style = "color:white"
  ),
  hr(),
  fluidRow(column(
    offset = 4,
    width = 4,
    wellPanel(
      numericInput(
        inputId = "pregnancies",
        label = "Enter the number of times of pregnancies you have had",
        value = 0,
        min = 0
      ),
      numericInput(
        inputId = "glucose",
        label = "Plasma glucose concentration a 2 hours in an oral glucose tolerance test",
        value = 0,
        min = 0
      ),
      numericInput(
        inputId = "bp",
        label = "Enter the Diastolic blood pressure (mm Hg)",
        value = 0,
        min = 0
      ),
      numericInput(
        inputId = "insulin",
        label = "2-Hour serum insulin (mu U/ml)",
        value = 0,
        min = 0
      ),
      numericInput(
        inputId = "bmi",
        label = "Body mass index (weight in kg/(height in m)^2)",
        value = 0,
        min = 0
      ),
      numericInput(
        inputId = "age",
        label = "Age (years)",
        value = 0,
        min = 0
      ),
      
      actionButton(
        inputId = "check",
        label = "Check",
        icon = icon("clipboard-list"),
        class = "btn btn-primary"
      )
    )
  ))
)

server <- function(input, output) {
  
  model = readRDS("rf_model.rds")
  
  observeEvent(input$check, {
    test_data <- list(
      "Pregnancies" = input$pregnancies,
      "Glucose" = input$glucose,
      "BloodPressure" = input$bp,
      "Insulin" = input$insulin,
      "BMI" = input$bmi,
      "Age" = input$age
    )
    
    prediction = predict(model, test_data)
    
    if (test_data[2] == 0 ||
        test_data[3] == 0 ||
        test_data[4] == 0 ||
        test_data[5] == 0 ||
        test_data[6] == 0) {
      shinyalert("Information",
                 "Invalid input",
                 type = "info")
    }
    else if(prediction == 0) {
      shinyalert("Yay!", "You look perfectly alright!", type = "success")
    }
    else{
      shinyalert(
        "Alert!",
        "It seems that you are probably diabetic. Please consult a physician ASAP!",
        type = "error"
      )
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
