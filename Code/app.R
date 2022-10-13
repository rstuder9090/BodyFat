library(shiny)
library(readr)
library(ggplot2)

BodyFat<- read_csv("BodyFat2.csv")

ui<- fluidPage(
  
  titlePanel("Body Fat Percentage Prediction"),
  
  sidebarLayout(
    
    sidebarPanel(width =6,
                 helpText("Enter your information to the best of your ability:"),
                 sliderInput(inputId = "age",
                             label = "Age",
                             min = 20,
                             max = 90,
                             value = 45,
                             step = 1,
                             width = '100%'),
                 numericInput(inputId = "height",
                              label = "Height (cm)",
                              min = 90,
                              max = 250,
                              value = 180,
                              step = 0.25),
                 numericInput(inputId = "weight",
                              label = "Weight (kg)",
                              min = 30,
                              max = 230,
                              value = 85,
                              step = 0.25),
                 submitButton(text="Calculate my Body Fat %")
    ),
    
    mainPanel(width =6,
              h3("Your predicted Body Fat Percentage:"),
              verbatimTextOutput("fin"),
              h5("The red line in the graph below indicates where your body fat percentage falls with respect to the rest of the participants in this dataset."),
              plotOutput("hist"),
              p(),
              p("Note: this predicted value is based on only the 3 given criteria.", align="center"),
              p(span("THIS IS NOT TO BE USED AS A MEDICALLY ACCURATE MEASUREMENT OF HEALTH/FITNESS",style = "color:red"), align = "center")
              
    )
  )
)


model<- lm(BODYFAT~HEIGHT_CM + WEIGHT_KG + AGE, data=BodyFat)
B0<- summary(model)$coefficient[1,1]
B1<- summary(model)$coefficient[2,1]
B2<- summary(model)$coefficient[3,1]
B3<- summary(model)$coefficient[4,1]


server<- function(input, output) {
  
    fin<- reactive({B0 + (B1*input$height) + (B2*input$weight) + (B3*input$age)
  })
  output$hist <- renderPlot({
    ggplot(BodyFat,aes(BodyFat$BODYFAT))+
      geom_histogram(bins=40, color = "black",fill = "#337AB7")+
      xlab("Body Fat Percentage")+
      ylab("Count")+
      geom_vline(xintercept= fin(), color = "red", linetype="longdash")
  })
  output$fin <- renderText({fin()})
}


shinyApp(ui, server)
