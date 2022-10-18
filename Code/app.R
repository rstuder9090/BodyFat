library(shiny)
library(readr)
library(ggplot2)
library(dplyr)

BodyFat2<- read.csv("BodyFat2.csv", header=T)
BodyFatsub<- BodyFat2 %>% filter(IDNO != 39)
BodyFatsub2<- subset(BodyFatsub, select = -c(IDNO, DENSITY, WEIGHT, HEIGHT))
modelsub<- lm(BODYFAT~ ABDOMEN + WRIST + WEIGHT_KG, data=BodyFatsub2)

ui<- fluidPage(
  
  titlePanel("Body Fat Percentage Prediction"),
  
  sidebarLayout(
    
    sidebarPanel(width = 6,
                 helpText("Enter your information to the best of your ability:"),
                 numericInput(inputId = "ab",
                             label = "Abdomen Circumference (cm)",
                             min = 40,
                             max = 302,
                             value = 92,
                             step = 0.25),
                 helpText("Abdomen circumference should be measured at belly buttom level while looking standing straight."),
                 numericInput(inputId = "wrist",
                              label = "Wrist Circumference (cm)",
                              min = 5,
                              max = 40,
                              value = 18,
                              step = 0.25),
                 helpText("Wrist circumference should be measured where your hand and forearm join."),
                 numericInput(inputId = "weight",
                              label = "Weight (kg)",
                              min = 22,
                              max = 453,
                              value = 81,
                              step = 0.25),
                 helpText("Please enter weight in kilograms"),
                 submitButton(text="Calculate my Body Fat %")
    ),
    
    mainPanel(width =6,
              h3("Your predicted Body Fat Percentage:"),
              verbatimTextOutput("fin"),
              span(htmlOutput("note"),style = "color:red"),
              h5("The red line in the graph below indicates where your body fat percentage falls with respect to the rest of the participants in this dataset."),
              plotOutput("hist"),
              p(),
              p("Note: this predicted value is based on only the 3 given criteria.", align="center"),
              p(span("THIS IS NOT TO BE USED AS A MEDICALLY ACCURATE MEASUREMENT OF HEALTH/FITNESS",style = "color:red"), align = "center"),
              p(),
              p(),
              p(em("For help contact the app author: Rachel Studer (rlstuder@wisc.edu)"), align = "center")
    )
  )
)


B0<- summary(modelsub)$coefficient[1,1]
B1<- summary(modelsub)$coefficient[2,1]
B2<- summary(modelsub)$coefficient[3,1]
B3<- summary(modelsub)$coefficient[4,1]


server<- function(input, output) {
  
  fin<- reactive({
    if ((B0 + (B1*input$ab) + (B2*input$wrist) + (B3*input$weight)) >= 4 & 
        (B0 + (B1*input$ab) + (B2*input$wrist) + (B3*input$weight)) <= 50){
      B0 + (B1*input$ab) + (B2*input$wrist) + (B3*input$weight)
    }else{
      0
    }
  })
  
  output$hist <- renderPlot({
    ggplot(BodyFatsub2,aes(BODYFAT))+
      geom_histogram(bins=40, color = "black",fill = "#9FBEF0")+
      xlab("Body Fat Percentage")+
      ylab("Count")+
      geom_vline(xintercept= fin(), color = "red", linetype="longdash", size = 1.25)
  })
  
  output$fin <- renderText({paste(round(fin(),2), "%")})
  
  a_input<-reactive({
    if(input$ab > 302){
      max
    }else if(input$ab < 40){
      min
    }else{
      input$ab
    }
  })
  wr_input<-reactive({
    if(input$wrist > 40){
      max
    }else if(input$wrist < 5){
      min
    }else{
      input$wrist
    }
  })
  we_input<-reactive({
    if(input$weight > 453){
      max
    }else if(input$weight < 22){
      min
    }else{
      input$weight
    }
  })
  
  textnote<-reactive({
    if(input$ab < 40){
      paste("WARNING: Your Abdomen input is not within range")
    }else if (input$ab >302){
      paste("WARNING: Your Abdomen input is not within range")
    }else if (input$wrist < 5){
      paste("WARNING: Your Wrist input is not within range")
    }else if (input$wrist > 40) {
      paste("WARNING: Your Wrist input is not within range")
    }else if (input$weight < 22){
      paste("WARNING: Your Weight input is not within range")
    }else if (input$weight > 453){
      paste("WARNING: Your Weight input is not within range")
    }else{
     paste("")
    }
  })
  
  output$note<-renderText({HTML(textnote())}) 
}


shinyApp(ui, server)
