source("global.R", local = TRUE)

# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel(""),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      selectizeInput(inputId = 'stateInput',
                     choices = sort(unique(df$state)),
                     label = 'States',
                     multiple = T,
                     options = list(maxItems = 5,
                                    placeholder = 'Select a name'),
                     selected = 'California'),
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Data file ----
      plotlyOutput(outputId = 'plot')
      
    )
    
  )
)

# Define server logic to read selected file ----
server <- function(input, output) {
  
  output$plot <- renderPlotly({
    
    filtered_data <- df %>% 
      filter(state %in% input$stateInput)
    
    ggplot_plot <- ggplot(data = filtered_data,
                          mapping = aes(x = year,
                                        y = total_incidents,
                                        colour = state))+
      geom_line()+
      geom_point(aes(size = injured), alpha = 0.8)+
      labs(title = 'Total incidents of gun violence (2015-2018)',
           x = 'Year',
           y = 'Number of Incidents',
           colour = 'States')+
      scale_y_continuous(labels=scales::comma)+
      theme_bw()
    
    
    ggplotly(ggplot_plot)
    
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)