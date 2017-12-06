
library(datasets)

# Use a fluid Bootstrap layout
ui=fluidPage(

  # Give the page a title
  titlePanel("Telephones by region"),

  # Generate a row with a sidebar
  sidebarLayout(

    # Define the sidebar with one input
    sidebarPanel(
      selectInput("region", "Region:",
                  choices=colnames(WorldPhones)),
      hr(),
      helpText("Data from AT&T (1961) The World's Telephones.")
    ),

    # Create a spot for the barplot
    mainPanel(
      plotOutput("phonePlot")
    )

  )
)

# Define a server for the Shiny app
server= function(input, output) {

    # Fill in the spot we created for a plot
    output$phonePlot <- renderPlot({

      # Render a barplot
      barplot(WorldPhones[,input$region]*1000,
              main=input$region,
              ylab="Number of Telephones",
              xlab="Year")
    })
  }

# Define server logic required to draw the integration plot
server <- function(input, output) {

  a <- eventReactive(input$button, {
    mc_int(x_range = c(input$low, input$up),
           fun = input$fun, B = input$B)
  })

  output$distPlot <- renderPlot({
    plot(a())
  })

}

# Run the application
shinyApp(ui = ui, server = server)
