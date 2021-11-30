library(shiny)

ui <- fluidPage(
  sliderInput(inputId = "num", 
              label = "Choose a number", 
              value = 25, min = 1, max = 100),
  textInput(inputId = "title", 
            label = "Write a title",
            value = "Histogram of Random Normal Values"),
  navlistPanel(              
    tabPanel(title = "Normal data",
             plotOutput("hist"),
             actionButton("renorm", "Resample"),
             verbatimTextOutput("stats")
             )
  )
)


server <- function(input, output) {
  rv <- reactiveValues(
    norm = rnorm(25)
  )
  
  observeEvent(input$renorm, { rv$norm <- rnorm(input$num) })
  observeEvent(input$num, { rv$norm <- rnorm(input$num) })
  
  output$hist <- renderPlot({
    hist(rv$norm, breaks = 3, main = input$title)
  })
  
  output$stats <- renderPrint({
    summary(rv$norm)
  })
}

shinyApp(ui = ui, server = server)