
ui=fluidPage(
  titlePanel("Basic DataTable"),

  # Create a new Row in the UI for selectInputs
  mainPanel(

    fluidRow(

      column(4,
             selectInput("topicinterest",
                         "Topic Interest:",
                         c("All", "Topic", "Year", "Category")
             )
      ),

      column(4,
             selectInput("topic",
                         "Topic:",
                         c("All",
                           unique(as.character(data$Topic))))
      ),

      column(4,
             selectInput("locat",
                         "Location:",
                         c("All",
                           unique(as.character(data$Location))))
      ),

      column(4,
             selectInput("year",
                         "Year:",
                         c("All",
                           unique(as.character(data$Year))))
      ),

      column(4,
             selectInput("cat",
                         "Category:",
                         c("All", "Gender", "Race"))
      ),
      column(4,
             selectInput("gender",
                         "Gender:",
                         c("All", "Male", "Female"))
      ),




      # Create a new row for the table.
      fluidRow(
        DT::dataTableOutput("table")
      ),

      h4("Plot"),
      verbatimTextOutput("Plot"),

      plotOutput("oralPlot")

    )
  )
)



server=function(input, output) {


  # Filter data based on selections
  output$table <- DT::renderDataTable(DT::datatable({

    if (input$topicinterest != "All") {

      if (input$topic != "All") {
        data <- data[data$Topic == input$topic,]
      }
      if (input$locat != "All") {
        data <- data[data$Location == input$locat,]
      }
      if (input$year != "All") {
        data <- data[data$Year == input$year,]
      }
      if (input$cat != "All") {

        if (input$cat == "Gender") {

          if(input$gender == "Male") {
            data <- data[data$Category == "Male",]
          }

          else if(input$gender == "Female") {
            data <- data[data$Category == "Female",]
          }
        }
      }
      data
    }
  }
  ))


  # Render a barplot

  output$oralPlot <- renderPlot({

    if (input$topicinterest != "All") {

      if (input$topicinterest == "Topic") {

        data <- data[,4]

      }else if(input$topicinterest == "Year") {

        data <- data[,2]

      }else if(input$topicinterest == "Category") {

        data <- data[,7]

      }

      count= table(data)

      # Render a barplot
      barplot(count,
              main=input$topicinterest,
              horiz = TRUE,
              ylab= "Topic Indicator",
              xlab="Frequency",
            xlim = c( 0 , 300))
     }
  })

}

shinyApp(ui=ui, server=server)
