source("global.R", local = TRUE)
# Prepare the data
df <- excel_data %>% 
  group_by(city_or_county, year = floor_date(incident_date, unit = "year")) %>%
  mutate(year = year(year)) %>% 
  summarise(total_incidents = n(),
            killed = sum(n_killed),
            injured = sum(n_injured))

# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel(""),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      selectInput(inputId = "yearInput",
                   label = "Year",
                   choices = sort(c(unique(df$year), "All")),
                   selected = 2015),
      checkboxInput(inputId = "showLabel",
                    label = "Show Labels",
                    value = FALSE),
      sliderInput(inputId = "numOfItemsInput",
                  label = "Number of Cities",
                  min = 5,
                  max = 30,
                  value = 5),
      selectInput(inputId = 'sortBy',
                  choices = c('Deaths','Injuries','Incidents'),
                  label = 'Sort by',
                  selected = 'Deaths'),
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
    yearInput <- input$yearInput
    showLabel <- input$showLabel
    sortBy <- input$sortBy
    numItems <- input$numOfItemsInput
    
    if (yearInput == 'All') {
      title <- paste0('Top ', numItems,' Gun-related incidents in the US sorted by ', sortBy , '(2015-2018)')
      filtered_data <- df %>% 
        group_by(city_or_county) %>% 
        summarise(total_incidents = sum(total_incidents),
                  killed = sum(killed),
                  injured = sum(injured)) %>% 
        arrange(desc(total_incidents),
                desc(killed),
                desc(injured))
    } else {
      title <- paste0('Top ', numItems,' Gun-related incidents in the US sorted by ', sortBy , ' in ', yearInput)
      filtered_data <- df %>% 
        filter(year == yearInput) %>%
        group_by(city_or_county) %>% 
        summarise(total_incidents = sum(total_incidents),
                  killed = sum(killed),
                  injured = sum(injured)) %>% 
        arrange(desc(total_incidents),
                desc(killed),
                desc(injured))
    }
    
    filtered_data <- filtered_data %>% 
      slice_head(n = numItems)
# 
#     if(sortBy == "Deaths") {
#       
#       filtered_data <- filtered_data %>% 
#         arrange(desc(killed)) %>% 
#         slice_head(n = numItems)
#       
#     } else if (sortBy == "Incidents") {
#       
#       filtered_data <- filtered_data %>% 
#         arrange(desc(total_incidents)) %>% 
#         slice_head(n = numItems)
#       
#     } else if (sortBy == "Injuries") {
#       
#       filtered_data <- filtered_data %>% 
#         arrange(desc(injured)) %>% 
#         slice_head(n = numItems)
#       
#     }
#     
    
    plot <- ggplot(data = filtered_data,
           mapping = aes(x = total_incidents,
                         y = injured))+
      geom_point(mapping = aes(size = killed, colour = city_or_county),
                 alpha = 0.5)+
      scale_size(range = c(2,18))+
      labs(title = title,
        x = 'Total Incidents',
           y = 'Injured')+
      guides(colour = FALSE, alpha = FALSE, size = FALSE)+
      scale_y_continuous(labels=scales::comma)+
      scale_x_continuous(labels=scales::comma)+
      theme_bw()
    
    if(showLabel) {
      plot <- plot + geom_text(mapping = aes(label = city_or_county),
                hjust = 0.5,
                vjust = -1)
    }
    ggplotly(plot)
    

    
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)