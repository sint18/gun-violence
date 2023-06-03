source("global.R", local = TRUE)
# A list for different categories
category <- c('Domestic Violence', 'Mass Murder/Shooting', 'School Shooting', 'Suicide', 'Home Invasion', 'Drug/Gang Involvement', 'Accident', 'Armed Robbery', 'Non-Shooting Incident', 'Shot - Wounded/Injured', 'Shot - Dead')

# Extract and assign different categories
categorised_data <- excel_data %>%
  select(incident_id, incident_date, incident_characteristics) %>% 
  mutate(incident_characteristics = ifelse(grepl('Domestic Violence', incident_characteristics), category[1], incident_characteristics)) %>% 
  mutate(incident_characteristics = ifelse(grepl('Mass Murder|Mass Shooting', incident_characteristics), category[2], incident_characteristics)) %>% 
  mutate(incident_characteristics = ifelse(grepl('School Shooting', incident_characteristics), category[3], incident_characteristics)) %>% 
  mutate(incident_characteristics = ifelse(grepl('Suicide', incident_characteristics), category[4], incident_characteristics)) %>% 
  mutate(incident_characteristics = ifelse(grepl('Home Invasion', incident_characteristics), category[5], incident_characteristics)) %>% 
  mutate(incident_characteristics = ifelse(grepl('Drug|Gang', incident_characteristics), category[6], incident_characteristics)) %>%
  mutate(incident_characteristics = ifelse(grepl('Accidental', incident_characteristics), category[7], incident_characteristics)) %>% 
  mutate(incident_characteristics = ifelse(grepl('Armed robbery', incident_characteristics), category[8], incident_characteristics)) %>% 
  mutate(incident_characteristics = ifelse(grepl('Non-Shooting', incident_characteristics), category[9], incident_characteristics)) %>% 
  mutate(incident_characteristics = ifelse(grepl('Wounded/Injured', incident_characteristics), category[10], incident_characteristics)) %>% 
  mutate(incident_characteristics = ifelse(grepl('Shot - Dead', incident_characteristics), category[11], incident_characteristics))
# Inconsistent rows are assigned as 'Others'
other_incidents <- categorised_data %>% 
  filter(!incident_characteristics %in% category) %>% 
  mutate(incident_characteristics = 'Others')

# Filter only categorised data
categorised_data <- categorised_data %>% 
  filter(incident_characteristics %in% category)

# Combine rows and summarise
final_df <- categorised_data %>%   
  bind_rows(other_incidents) %>% 
  rename(year = incident_date) %>% 
  mutate(year = year(year)) %>%
  group_by(year, incident_characteristics) %>% 
  summarise(total = n())

# percent_df <- final_df %>% 
#   mutate(perc = count / sum(count)) %>%
#   arrange(perc) %>%
#   mutate(perc = scales::percent(perc))

ui <- fluidPage(
  
  # App Title
  titlePanel(""),
  
  sidebarLayout(
    sidebarPanel(
      #Input
      selectizeInput(inputId = 'incidentCharInput',
                     choices = unique(final_df$incident_characteristics),
                     label = 'Types of Incidents',
                     multiple = T,
                     options = list(maxItems = 5,
                                    minItems = 1,
                                    placeholder = 'Select a name'),
                     selected = 'Armed Robbery')
    ),
    
    #Main Panel for displaying output
    mainPanel(
      #plot output
      plotlyOutput(outputId = 'plot')
      
    )
  )
  
)

server <- function(input, output){
  
  # reactive renderplot
  output$plot <- renderPlotly({
    
    filtered_data <- final_df %>% 
      filter(incident_characteristics %in% input$incidentCharInput)
    
    ggplot_plot <- ggplot(data = filtered_data,
           mapping = aes(x = year,
                         y = total,
                         colour = incident_characteristics))+
      geom_line()+
      geom_point()+
      labs(title = 'Types of incidents across the US (2015-2018)',
           x = 'Year',
           y = 'Number of Incidents',
           colour = 'Types of Incidents')+
      scale_y_continuous(labels=scales::comma)+
      theme_bw()
    
    ggplotly(ggplot_plot)
    
    })
}

shinyApp(ui, server)

