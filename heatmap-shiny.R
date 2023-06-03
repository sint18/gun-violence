#Type of incidents
source("global.R", local = TRUE)

# get map data
map_base <- map_data('state')

# A list for different categories
category <- c('Domestic Violence', 'Mass Murder/Shooting', 'School Shooting', 'Suicide', 'Home Invasion', 'Drug/Gang Involvement', 'Accident', 'Armed Robbery', 'Non-Shooting Incident', 'Shot - Wounded/Injured', 'Shot - Dead', 'All')

# Extract and assign different categories
categorised_data <- excel_data %>%
  select(incident_id, incident_date, state, incident_characteristics) %>% 
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
  group_by(state, year, incident_characteristics) %>% 
  summarise(total = n()) %>% 
  mutate(state = tolower(state))




# year, incident type



# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel(""),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      selectInput(inputId = 'incidentType',
                     choices = sort(category),
                     label = 'Type of Incident',
                     selected = ''),
      radioButtons(inputId = "incidentYear",
                   label = 'Year',
                   choices = sort(c(unique(final_df$year), 'All')),
                   selected = 'All'),
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Data file ----
      plotOutput(outputId = 'plot')
      
    )
    
  )
)

# Define server logic to read selected file ----
server <- function(input, output) {
  
  output$plot <- renderPlot({
    
    target_year <- input$incidentYear
    target_incident <- input$incidentType
    realTitle <- ''
    
    if (target_year == 'All') {
      if(target_incident == 'All') {
        
        realTitle <- 'Heatmap of gun-related incidents in US (2015-2018)'
        
        filtered_df <- final_df %>% 
          group_by(state) %>% 
          summarise(total = sum(total))
      } else {
        
        realTitle <- paste0('Heatmap of gun-related ', target_incident, ' incidents in US (2015-2018)')
        
        filtered_df <- final_df %>% 
          filter(incident_characteristics == target_incident) %>% 
          group_by(state) %>% 
          summarise(total = sum(total))
      }

    } else {
      if(target_incident == 'All') {
        
        realTitle <- paste0('Heatmap of gun-related incidents in US (', target_year ,')')
        
        filtered_df <- final_df %>% 
          filter(year == target_year) %>% 
          group_by(state) %>% 
          summarise(total = sum(total))
      } else {
        
        realTitle <- paste0('Heatmap of gun-related ', target_incident ,' incidents in US (', target_year ,')')
        
        filtered_df <- final_df %>% 
          filter(incident_characteristics == target_incident & year == target_year) %>% 
          group_by(state) %>% 
          summarise(total = sum(total))
      }
      

    }
    
    final_data <- left_join(map_base, filtered_df, by = c('region' = 'state'))
    
    ggplot(data = final_data,
                   mapping = aes(x = long,
                                 y = lat,
                                 group = group))+
      geom_polygon(aes(fill = total),
                   colour = 'black')+
      scale_fill_continuous(low='white', high='darkred')+
        labs(title = realTitle,
             x = NULL,
             y = NULL,
             fill = 'Incidents')+
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks = element_blank())
      
    
    # ggplotly(plot)
    
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)


