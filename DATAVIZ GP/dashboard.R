library(shiny)
library(ggplot2)
library(dplyr)

ui <- navbarPage("Dashboard",
                 tabPanel("Input",
                          fileInput("data", 
                                    h3("Input Dataset"))
                 ),
                 navbarMenu("Descriptive",
                            
                            tabPanel("Barplot",
                                     sidebarLayout(
                                       sidebarPanel(
                                     
                                     h2("Generate Random Normal Histogram"),
                                     numericInput("num", h5("Select vector length"),
                                                  value = 100),
                                     numericInput("mean", h5("Select vector mean"),
                                                  value = 5),
                                     numericInput("sd", h5("Select vector standard deviation"),
                                                  value = 2),
                                     numericInput("numbin", h5("Select number of bins"),
                                                  value = 10),
                                     actionButton("update", "Submit")),
                                     mainPanel(plotOutput("hist"))
                                     )
                                     
                            ),
                            tabPanel("Scatterplot")),
                 tabPanel("Maps",
                          sliderInput(inputId = "bins",
                                      label = "Number of bins:",
                                      min = 1,
                                      max = 50,
                                      value = 30),
                          ),
                 tabPanel("Clustering")
)


# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  histupdate <- observeEvent(input$update, {
    rand <- rnorm(input$num, input$mean, input$sd)
    output$hist <- renderPlot({ggplot()+
        geom_histogram(mapping=aes(x=rand), bins=input$numbin)+
        theme_bw()})
  })
}

  


shinyApp(ui = ui, server = server)

