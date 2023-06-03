source("global.R", local = TRUE)
# A list of age groups
age_group_list <- c('Child 0-11', 'Teen 12-17', 'Adult 18+')

gender_list <- c('Male', 'Female', 'Unknown')

# Separate columns into rows
separated_data <- excel_data %>% 
  select(incident_id, incident_date, state, participant_age_group) %>% 
  separate_longer_delim(participant_age_group, delim = '||') %>%
  separate_longer_delim(participant_age_group, delim = '|') %>% 
  rename(age_group = participant_age_group)

# Extract partial string and assign different age groups
final_data <- separated_data %>% 
  mutate(age_group = ifelse(grepl('Child', age_group), age_group_list[1], age_group)) %>%
  mutate(age_group = ifelse(grepl('Teen', age_group), age_group_list[2], age_group)) %>%
  mutate(age_group = ifelse(grepl('Adult', age_group), age_group_list[3], age_group))

# The data is a mess and had to clean it
good_data <- final_data %>% 
  filter(!age_group %in% age_group_list) %>% 
  filter(grepl('::[0-9]{2}', age_group)) %>% 
  mutate(age_group = str_extract(age_group, '[0-9]{2}')) %>%
  mutate(age_group = ifelse(as.integer(age_group) <= 11 & as.integer(age_group) > 0, age_group_list[1],
                            ifelse(as.integer(age_group) <= 17 & as.integer(age_group) > 11, age_group_list[2],
                                   ifelse(as.integer(age_group) >= 18, age_group_list[3], age_group))))

# Filter out gibberish rows and assign 'Unknown'
unknown_data <- final_data %>% 
  filter(!age_group %in% age_group_list) %>% 
  filter(!grepl('::[0-9]{2}', age_group)) %>%
  mutate(age_group = 'Unknown')

# Merge everything into one tibble
final_data <- final_data %>% 
  filter(age_group %in% age_group_list) %>% 
  bind_rows(good_data) %>% 
  bind_rows(unknown_data) %>% 
  mutate(incident_date = year(incident_date)) %>%
  rename(year = incident_date)

# Gender data
gender_data <- excel_data %>%
  select(incident_id, incident_date, state, participant_gender) %>%
  rename(year = incident_date) %>% 
  mutate(year = year(year)) %>% 
  separate_longer_delim(participant_gender, delim = stringr::regex('\\|\\||[:digit:]::|\\|')) %>%
  mutate(participant_gender = replace_na(participant_gender, 'Unknown')) %>%
  filter(complete.cases(.), participant_gender %in% gender_list)

# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel(""),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      selectInput(inputId = 'selectType',
                  label = 'Type',
                  choices = c('Age', 'Gender'),
                  selected = 'Age',
                  multiple = FALSE,
                  selectize = FALSE,
                  width = NULL,
                  size = NULL
      ),
      sliderInput(inputId = "yearInput",
                  label = "Year",
                  min = min(final_data$year),
                  max = max(final_data$year),
                  value = min(final_data$year),
                  sep = ''),
      selectInput(inputId = 'selectInput',
        label = 'Ranking',
        choices = c('Top', 'Bottom'),
        selected = 'Top',
        multiple = FALSE,
        selectize = FALSE,
        width = NULL,
        size = NULL
      ),
      sliderInput(inputId = "numOfStatesInput",
                  label = "Number of States",
                  min = 5,
                  max = 20,
                  value = 10,
                  sep = '')
      
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
    titleText <- ''
    type <- ''
    if(input$selectType == 'Gender'){
      final_data <- gender_data %>% 
        rename(target = participant_gender)
      
      type <- 'Gender'
      titleText <- paste('Gender of participants in the US in', input$yearInput)
      
    } else if (input$selectType == 'Age') {
      final_data <- final_data %>% 
        rename(target = age_group)
      
      type <- 'Age Group'
      titleText <- paste('Participants of different age groups in the US in', input$yearInput)
    }
    
    # Filter the data by year input
    filtered_state <- final_data %>% 
      filter(year == input$yearInput) %>%
      group_by(state) %>% 
      summarise(count = n()) %>% 
      arrange(desc(count))
    
    if(input$selectInput == 'Top') {
      filtered_state <- filtered_state %>% 
        slice_head(n = input$numOfStatesInput)
    } else {
      filtered_state <- filtered_state %>% 
        slice_tail(n = input$numOfStatesInput)
    }
    
    filtered_data <- final_data %>% 
      filter(year == input$yearInput) %>%
      filter(state %in% filtered_state$state)
    
    # Plotting
    ggplot_plot <- ggplot(data = filtered_data, 
           mapping = aes(x = state,
                         fill = target)) +
      geom_bar()+
      labs(title = titleText,
           x = 'State',
           y = 'Number of Participants',
           fill = type)+
      scale_y_continuous(labels=scales::comma)+
      theme_bw()+
      theme(axis.text.x = element_text(angle = 90))

    
    # Change to plotly to make it interactive
    ggplotly(ggplot_plot)

  })
  
  
}

# Create Shiny app ----
shinyApp(ui, server)