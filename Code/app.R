library(shiny)
library(readr)
library(ggplot2)

BodyFat<- read.csv("BodyFat2.csv", header=T)
pca.lm<- lm(BODYFAT~AGE + ADIPOSITY + NECK, data=BodyFat)
summary(pca.lm)

ui<- fluidPage(
  
  titlePanel("Body Fat Percentage Prediction"),
  
  sidebarLayout(
    
    sidebarPanel(width =6,
                 helpText("Enter your information to the best of your ability:"),
                 sliderInput(inputId = "age",
                             label = "Age",
                             min = 20,
                             max = 90,
                             value =45,
                             step = 1,
                             width = '100%'),
                 numericInput(inputId = "height",
                              label = "Height (cm)",
                              min = 90,
                              max = 250,
                              value = 0,
                              step = 0.25),
                 numericInput(inputId = "weight",
                              label = "Weight (kg)",
                              min = 30,
                              max = 230,
                              value = 0,
                              step = 0.25),
                 numericInput(inputId = "neck",
                              label = "Neck Circumference (cm)",
                              min = 20,
                              max = 60,
                              value = 0),
                 helpText("Neck circumference should be measured just below the Adam's Apple while looking straight forward."),
                 submitButton(text="Calculate my Body Fat %")
    ),
    
    mainPanel(width =6,
              h3("Your predicted Body Fat Percentage:"),
              verbatimTextOutput("fin"),
              h5("The red line in the graph below indicates where your body fat percentage falls with respect to the rest of the participants in this dataset."),
              plotOutput("hist"),
              p(),
              p("Note: this predicted value is based on only the 4 given criteria.", align="center"),
              p(span("THIS IS NOT TO BE USED AS A MEDICALLY ACCURATE MEASUREMENT OF HEALTH/FITNESS",style = "color:red"), align = "center")
    )
  )
)


B0<- summary(pca.lm)$coefficient[1,1]
B1<- summary(pca.lm)$coefficient[2,1]
B2<- summary(pca.lm)$coefficient[3,1]
B3<- summary(pca.lm)$coefficient[4,1]


server<- function(input, output) {
  
  fin<- reactive({B0 + (B1*input$age) + (B2*(input$weight/((input$height * 0.01)^2))) + (B3*input$neck)
  })
  output$hist <- renderPlot({
    ggplot(BodyFat,aes(BODYFAT))+
      geom_histogram(bins=40, color = "black",fill = "#9FBEF0")+
      xlab("Body Fat Percentage")+
      ylab("Count")+
      geom_vline(xintercept= fin(), color = "red", linetype="longdash", size = 1.25)
  })
  output$fin <- renderText({paste(round(fin(),2), "%")})
}


shinyApp(ui, server)
