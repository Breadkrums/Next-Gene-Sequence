library(dplyr)
library(ggplot2)
library(reactable)
library(reactablefmtr)
library(tidyverse)
library(ShortRead)
library(shiny)

# Define the UI
ui <- fluidPage(
  titlePanel("Next-Gene Sequencing"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose a FASTQ file"),
      selectInput("x_var", "Select X Variable", ""),
      selectInput("y_var", "Select Y Variable", ""),
      selectInput("fill_var", "Select Fill Variable", ""),
      selectInput("chart_type", "Select Chart Type",
                  choices = c("Heatmap", "Volcano Plot")),
      actionButton("plot_button", "Plot")
    ),
    
    mainPanel(
      plotOutput("plot")
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  
  data <- reactive({
    req(input$file)
    inFile <- input$file
    validate(
      need(tolower(sub('.*[.]', '', inFile$name)) == "fastq", "Please upload a FASTQ file.")
    )
    
    # Read FASTQ file
    fastq_data <- readFastq(inFile$datapath)
    
    # Convert to tibble
    fastq_df <- as.data.frame(fastq_data)
    
    # Update selectInput choices dynamically based on columns in the dataframe
    updateSelectInput(session, "x_var", choices = names(fastq_df))
    updateSelectInput(session, "y_var", choices = names(fastq_df))
    updateSelectInput(session, "fill_var", choices = names(fastq_df))
    
    return(fastq_df)
  })
  
  output$plot <- renderPlot({
    req(data())
    
    chart_type <- input$chart_type
    x_var <- input$x_var
    y_var <- input$y_var
    fill_var <- input$fill_var
    
    if (chart_type == "Heatmap") {
      ggplot(data(), aes_string(x = x_var, y = y_var, fill = fill_var)) +
        geom_tile() +
        labs(title = "Heatmap") +
        theme_minimal()
    } else if (chart_type == "Volcano Plot") {
      ggplot(data(), aes_string(x = x_var, y = y_var, color = fill_var)) +
        geom_point() +
        labs(title = "Volcano Plot") +
        theme_minimal()
    }
  })
}

# Run the application
shinyApp(ui, server)